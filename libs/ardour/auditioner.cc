/*
    Copyright (C) 2001 Paul Davis

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include <glibmm/threads.h>

#include "pbd/error.h"

#include "ardour/audio_diskstream.h"
#include "ardour/audioregion.h"
#include "ardour/audioengine.h"
#include "ardour/delivery.h"
#include "ardour/route.h"
#include "ardour/session.h"
#include "ardour/auditioner.h"
#include "ardour/audioplaylist.h"
#include "ardour/audio_port.h"
#include "ardour/data_type.h"
#include "ardour/region_factory.h"

using namespace std;
using namespace ARDOUR;
using namespace PBD;

#include "i18n.h"

Auditioner::Auditioner (Session& s)
	: AudioTrack (s, "auditioner", Route::Auditioner)
        , current_frame (0)
        , _auditioning (0)
        , length (0)
        , _seek_frame (-1)
        , _seeking (false)
        , _seek_complete (false)
        , via_monitor (false)
{
}

int
Auditioner::init ()
{
        if (Track::init ()) {
                return -1;
        }
	
	if (connect ()) {
		return -1;
	}

	_output->changed.connect_same_thread (*this, boost::bind (&Auditioner::output_changed, this, _1, _2));

        return 0;
}

Auditioner::~Auditioner ()
{
}

int
Auditioner::connect ()
{
	string left = Config->get_auditioner_output_left();
	string right = Config->get_auditioner_output_right();

	vector<string> outputs;
	_session.engine().get_physical_outputs (DataType::AUDIO, outputs);

	via_monitor = false;

	if (left.empty() || left == "default") {
                if (_session.monitor_out()) {
                        left = _session.monitor_out()->input()->audio (0)->name();
                        via_monitor = true;
                } else {
			if (outputs.size() > 0) {
				left = outputs[0];
			}
                }
	}

	if (right.empty() || right == "default") {
                if (_session.monitor_out()) {
                        right = _session.monitor_out()->input()->audio (1)->name();
                        via_monitor = true;
                } else {
			if (outputs.size() > 1) {
				right = outputs[1];
			}
                }
	}

	_output->disconnect (this);

	if (left.empty() && right.empty()) {
		if (_output->n_ports().n_audio() == 0) {
			/* ports not set up, so must be during startup */
			warning << _("no outputs available for auditioner - manual connection required") << endmsg;
		}
	} else {

		if (_output->n_ports().n_audio() == 0) {

			/* create (and connect) new ports */

			_main_outs->defer_pan_reset ();
			
			if (left.length()) {
				_output->add_port (left, this, DataType::AUDIO);
			}
			
			if (right.length()) {
				_output->add_port (right, this, DataType::AUDIO);
			}
			
			_main_outs->allow_pan_reset ();
			_main_outs->reset_panner ();

		} else {
			
			/* reconnect existing ports */

			boost::shared_ptr<Port> oleft (_output->nth (0));
			boost::shared_ptr<Port> oright (_output->nth (1));
			if (oleft) {
				oleft->connect (left);
			}
			if (oright) {
				oright->connect (right);
			}
		}
			
	}

	return 0;
}

AudioPlaylist&
Auditioner::prepare_playlist ()
{
	// FIXME auditioner is still audio-only
	boost::shared_ptr<AudioPlaylist> apl = boost::dynamic_pointer_cast<AudioPlaylist>(_diskstream->playlist());
	assert(apl);

	apl->clear ();
	return *apl;
}

void
Auditioner::audition_region (boost::shared_ptr<Region> region)
{
	if (g_atomic_int_get (&_auditioning)) {
		/* don't go via session for this, because we are going
		   to remain active.
		*/
		cancel_audition ();
	}

	if (boost::dynamic_pointer_cast<AudioRegion>(region) == 0) {
		error << _("Auditioning of non-audio regions not yet supported") << endmsg;
		return;
	}

	Glib::Threads::Mutex::Lock lm (lock);

	/* copy it */

	boost::shared_ptr<AudioRegion> the_region (boost::dynamic_pointer_cast<AudioRegion> (RegionFactory::create (region)));
	the_region->set_position (0);

	_diskstream->playlist()->drop_regions ();
	_diskstream->playlist()->add_region (the_region, 0, 1);

	if (_diskstream->n_channels().n_audio() < the_region->n_channels()) {
		audio_diskstream()->add_channel (the_region->n_channels() - _diskstream->n_channels().n_audio());
	} else if (_diskstream->n_channels().n_audio() > the_region->n_channels()) {
		audio_diskstream()->remove_channel (_diskstream->n_channels().n_audio() - the_region->n_channels());
	}

        ProcessorStreams ps;
	{
		Glib::Threads::Mutex::Lock lm (AudioEngine::instance()->process_lock ());

		if (configure_processors (&ps)) {
			error << string_compose (_("Cannot setup auditioner processing flow for %1 channels"),
						 _diskstream->n_channels()) << endmsg;
			return;
		}
	}

	/* force a panner reset now that we have all channels */

	_main_outs->reset_panner();

	_seek_frame = -1;
	_seeking = false;
	length = the_region->length();

	int dir;
	framecnt_t offset = the_region->sync_offset (dir);

	/* can't audition from a negative sync point */

	if (dir < 0) {
		offset = 0;
	}

	_diskstream->seek (offset);
	current_frame = offset;

	g_atomic_int_set (&_auditioning, 1);
}

int
Auditioner::play_audition (framecnt_t nframes)
{
	bool need_butler = false;
	framecnt_t this_nframes;
	int ret;

	if (g_atomic_int_get (&_auditioning) == 0) {
		silence (nframes);
		return 0;
	}

#if 0 // TODO
	if (_seeking && _seek_complete) {
		// set FADE-IN
	} else if (_seek_frame >= 0 && _seek_frame < length && !_seeking) {
		// set FADE-OUT -- use/override amp? || use region-gain ?
	}
#endif

	if (_seeking && _seek_complete) {
		_seek_complete = false;
		_seeking = false;
		_seek_frame = -1;
	}

	if(!_seeking) {
		/* process audio */
		this_nframes = min (nframes, length - current_frame);

		if ((ret = roll (this_nframes, current_frame, current_frame + nframes, false, need_butler)) != 0) {
			silence (nframes);
			return ret;
		}

		current_frame += this_nframes;

	} else {
		silence (nframes);
	}

	if (_seek_frame >= 0 && _seek_frame < length && !_seeking) {
		_seek_complete = false;
		_seeking = true;
		need_butler = true;
	}

	if (!_seeking) {
		AuditionProgress(current_frame, length); /* emit */
	}

	if (current_frame >= length) {
		_session.cancel_audition ();
		return 0;
	} else {
		return need_butler ? 1 : 0;
	}
}

void
Auditioner::output_changed (IOChange change, void* /*src*/)
{
	if (change.type & IOChange::ConnectionsChanged) {
		string phys;
		vector<string> connections;
		vector<string> outputs;
		_session.engine().get_physical_outputs (DataType::AUDIO, outputs);
		if (_output->nth (0)->get_connections (connections)) {
			if (outputs.size() > 0) {
				phys = outputs[0];
			}
			if (phys != connections[0]) {
				Config->set_auditioner_output_left (connections[0]);
			} else {
				Config->set_auditioner_output_left ("default");
			}
		} else {
			Config->set_auditioner_output_left ("");
		}

		connections.clear ();

		if (_output->nth (1)->get_connections (connections)) {
			if (outputs.size() > 1) {
				phys = outputs[1];
			}
			if (phys != connections[0]) {
				Config->set_auditioner_output_right (connections[0]);
			} else {
				Config->set_auditioner_output_right ("default");
			}
		} else {
			Config->set_auditioner_output_right ("");
		}
	}
}

ChanCount
Auditioner::input_streams () const
{
        /* auditioner never has any inputs - its channel configuration
           depends solely on the region we are auditioning.
        */

        if (audio_diskstream()) {
                return audio_diskstream()->n_channels();
        }

        return ChanCount ();
}

MonitorState 
Auditioner::monitoring_state () const
{
	return MonitoringDisk;
}
