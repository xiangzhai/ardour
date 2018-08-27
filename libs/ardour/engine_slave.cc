/*
    Copyright (C) 2004 Paul Davis

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

#include <iostream>
#include <cerrno>

#include "pbd/i18n.h"

#include "ardour/audioengine.h"
#include "ardour/audio_backend.h"
#include "ardour/transport_master.h"

using namespace std;
using namespace ARDOUR;

Engine_TransportMaster::Engine_TransportMaster (AudioEngine& e)
	: TransportMaster (Engine, X_("JACK"))
	, engine (e)
	, _starting (false)
{
}

Engine_TransportMaster::~Engine_TransportMaster ()
{
}

bool
Engine_TransportMaster::locked() const
{
	return true;
}

bool
Engine_TransportMaster::ok() const
{
	return true;
}

void
Engine_TransportMaster::pre_process (pframes_t, samplepos_t)
{
	/* nothing to do */
}

bool
Engine_TransportMaster::speed_and_position (double& sp, samplepos_t& position, samplepos_t /* now */)
{
	boost::shared_ptr<AudioBackend> backend = engine.current_backend();

	/* 3rd argument (now) doesn't matter here because we're always being
	 * called synchronously with the engine.
	 */

	if (backend && backend->speed_and_position (sp, position)) {
		return true;
	}

	return false;
}

std::string
Engine_TransportMaster::position_string () const
{
	return std::string();
}

std::string
Engine_TransportMaster::delta_string () const
{
	return std::string();
}
