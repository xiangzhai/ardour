/*
 * Copyright (C) 2018 Paul Davis (paul@linuxaudiosystems.com)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "pbd/i18n.h"

#include "ardour/audioengine.h"
#include "ardour/debug.h"
#include "ardour/session.h"
#include "ardour/transport_master_manager.h"

using namespace ARDOUR;
using namespace PBD;

const std::string TransportMasterManager::state_node_name = X_("TransportMasters");
TransportMasterManager* TransportMasterManager::_instance = 0;

TransportMasterManager::TransportMasterManager()
	: _master_speed (0)
	, _master_position (0)
	, _ui_transport_master (0)
	, _current_master (0)
	, _session (0)
{
}

TransportMasterManager::~TransportMasterManager ()
{
	clear ();
}

int
TransportMasterManager::init ()
{
	try {
		/* setup default transport masters. Most people will never need any
		   others, and likely most people won't use anything except the
		   UI/Internal pseudo-master
		*/
		add (Engine, X_("JACK Transport"));
		add (MTC, X_("MTC"));
		add (LTC, X_("LTC"));
		add (MIDIClock, X_("MIDI Clock"));
		add (UI, X_("Internal"));

		/* Use the UI/internal master by default. ::set_state() may reset this
		 * later
		 */
	} catch (...) {
		return -1;
	}

	_current_master = _transport_masters.back();
	_ui_transport_master = boost::dynamic_pointer_cast<UI_TransportMaster> (_transport_masters.back());

	return 0;
}

void
TransportMasterManager::set_session (Session* s)
{
	/* Called by AudioEngine in process context */

	Glib::Threads::RWLock::ReaderLock lm (lock);

	_session = s;

	for (TransportMasters::iterator tm = _transport_masters.begin(); tm != _transport_masters.end(); ++tm) {
		(*tm)->set_session (s);
	}
}

TransportMasterManager&
TransportMasterManager::instance()
{
	if (!_instance) {
		_instance = new TransportMasterManager();
	}
	return *_instance;
}

// Called from AudioEngine::process_callback() BEFORE Session::process() is called. Each transport master has processed any incoming data for this cycle,
// and this method computes the transport speed that Ardour should use to get into and remain in sync with the master.
//
double
TransportMasterManager::pre_process_transport_masters (pframes_t nframes, samplepos_t session_transport_position)
{
	Glib::Threads::RWLock::ReaderLock lm (lock, Glib::Threads::TRY_LOCK);

	if (lm.locked()) {
		for (TransportMasters::iterator tm = _transport_masters.begin(); tm != _transport_masters.end(); ++tm) {
			(*tm)->pre_process (nframes);
		}

		if (_session && _current_master) {
			return compute_matching_master_speed (nframes, _session->transport_sample());
		}
	}

	return 0.0;
}

double
TransportMasterManager::compute_matching_master_speed (pframes_t nframes, samplepos_t session_transport_position)
{
  restart:
	if (!_current_master->speed_and_position (_master_speed, _master_position)) {

		assert (_current_master->type() != UI);

		/* switch back to default master - our own UI */
		TransportMasterManager::instance().set_current (UI);

		/* now do it all again with the UI/internal master */
		goto restart;
	}

	/* compute delta or "error" between session and the master's
	 * position. Note: master_position is computed for right now, whereas
	 * _transport_sample was updated during the last process cycle. They
	 * should both indicate the same time. Any discrepancy is fed back into
	 * the DLL so that we get/remain more accurate.
	 */

	const double e = _master_position - session_transport_position;

	/* inject DLL with new data */

	t0 = t1;
	t1 += b * e + e2;
	e2 += c * e;

	double matching_master_speed = (t0 - t1) / nframes;

	DEBUG_TRACE (DEBUG::Slave, string_compose ("slave @ %2 speed %3 cur delta %4 matching speed %5\n",
	                                           _master_position, _master_speed, e, matching_master_speed));

	if (_current_master->sample_clock_synced() && matching_master_speed != 0.0f) {

		/* if the master is synced to our audio interface via word-clock or similar, then we assume that its speed is binary: 0.0 or 1.0
		   (since our sample clock cannot change with respect to it).
		*/
		matching_master_speed = matching_master_speed > 0.0 ? 1.0 : -1.0f;
	} else if (e > _current_master->resolution()) {
		/* session will not play disk material, so do not varispeed yet */
		matching_master_speed = matching_master_speed > 0.0 ? 1.0 : -1.0f;
	}

	/* speed is set, we're locked, and good to go */
	DEBUG_TRACE (DEBUG::Slave, string_compose ("%1: computed speed-to-follow-master as %2\n", _current_master->name(), matching_master_speed));
	return matching_master_speed;
}

void
TransportMasterManager::post_process_transport_masters (pframes_t nframes)
{
	Glib::Threads::RWLock::ReaderLock lm (lock, Glib::Threads::TRY_LOCK);

	if (lm.locked()) {
		for (TransportMasters::iterator tm = _transport_masters.begin(); tm != _transport_masters.end(); ++tm) {
			(*tm)->post_process (nframes);
		}
	}
}

void
TransportMasterManager::init_transport_master_dll (int direction, samplepos_t pos)
{
	/* the bandwidth of the DLL is a trade-off,
	 * because the max-speed of the transport in ardour is
	 * limited to +-8.0, a larger bandwidth would cause oscillations
	 *
	 * But this is only really a problem if the user performs manual
	 * seeks while transport is running and slaved to some timecode-y master.
	 */

	AudioEngine* ae = AudioEngine::instance();

	double const omega = 2.0 * M_PI * double(ae->samples_per_cycle()) / 2.0 / double(ae->sample_rate());
	b = 1.4142135623730950488 * omega;
	c = omega * omega;

	e2 = double (direction * ae->sample_rate());
	t0 = double (pos);
	t1 = t0 + e2;

	DEBUG_TRACE (DEBUG::Slave, string_compose ("[re-]init DLL %1 %2 %3\n", t0,  t1, e2));
}

int
TransportMasterManager::add (SyncSource type, std::string const & name)
{
	Glib::Threads::RWLock::WriterLock lm (lock);
	return add_locked (TransportMaster::factory (type, name));
}

int
TransportMasterManager::add_locked (boost::shared_ptr<TransportMaster> tm)
{
	if (!tm) {
		return -1;
	}

	for (TransportMasters::const_iterator t = _transport_masters.begin(); t != _transport_masters.end(); ++t) {
		if ((*t)->name() == tm->name()) {
			error << string_compose (_("There is already a transport master named \"%1\" - not duplicated"), tm->name()) << endmsg;
			return -1;
		}
	}


	_transport_masters.push_back (tm);

	return 0;
}

int
TransportMasterManager::remove (std::string const & name)
{
	Glib::Threads::RWLock::WriterLock lm (lock);

	for (TransportMasters::iterator t = _transport_masters.begin(); t != _transport_masters.end(); ++t) {
		if ((*t)->name() == name) {
			_transport_masters.erase (t);
			return 0;
		}
	}

	return -1;
}

int
TransportMasterManager::set_current (boost::shared_ptr<TransportMaster> c)
{
	Glib::Threads::RWLock::WriterLock lm (lock);
	return set_current_locked (c);
}

int
TransportMasterManager::set_current_locked (boost::shared_ptr<TransportMaster> c)
{
	if (find (_transport_masters.begin(), _transport_masters.end(), c) == _transport_masters.end()) {
		warning << string_compose (X_("programming error: attempt to use unknown transport master named \"%1\"\n"), c->name());
		return -1;
	}

	_current_master = c;
	_master_speed = 0;
	_master_position = 0;

	return 0;
}

int
TransportMasterManager::set_current (SyncSource ss)
{
	Glib::Threads::RWLock::WriterLock lm (lock);

	for (TransportMasters::iterator t = _transport_masters.begin(); t != _transport_masters.end(); ++t) {
		if ((*t)->type() == ss) {
			return set_current_locked (*t);
		}
	}

	return -1;
}


int
TransportMasterManager::set_current (std::string const & str)
{
	Glib::Threads::RWLock::WriterLock lm (lock);

	for (TransportMasters::iterator t = _transport_masters.begin(); t != _transport_masters.end(); ++t) {
		if ((*t)->name() == str) {
			return set_current_locked (*t);
		}
	}

	return -1;
}


void
TransportMasterManager::clear ()
{
	Glib::Threads::RWLock::WriterLock lm (lock);
	_transport_masters.clear ();
}

int
TransportMasterManager::set_state (XMLNode const & node, int version)
{
	{
		Glib::Threads::RWLock::WriterLock lm (lock);

		assert (node.name() == state_node_name);

		XMLNodeList const & children = node.children();

		if (!children.empty()) {
			_transport_masters.clear ();
		}

		for (XMLNodeList::const_iterator c = children.begin(); c != children.end(); ++c) {

			boost::shared_ptr<TransportMaster> tm = TransportMaster::factory (**c);

			if (add_locked (tm)) {
				continue;
			}

			/* we know it is the last thing added to the list of masters */

			_transport_masters.back()->set_state (**c, version);
		}
	}

	std::string current_master;
	if (node.get_property (X_("current"), current_master)) {
		set_current (current_master);
	} else {
		set_current (UI);
	}


	return 0;
}

XMLNode&
TransportMasterManager::get_state ()
{
	XMLNode* node = new XMLNode (state_node_name);

	node->set_property (X_("current"), _current_master->name());

	Glib::Threads::RWLock::ReaderLock lm (lock);

	for (TransportMasters::iterator t = _transport_masters.begin(); t != _transport_masters.end(); ++t) {
		node->add_child_nocopy ((*t)->get_state());
	}

	return *node;
}
