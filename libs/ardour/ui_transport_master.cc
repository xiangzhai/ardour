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

#include <glib.h>

#include "pbd/i18n.h"

#include "ardour/audioengine.h"
#include "ardour/debug.h"
#include "ardour/session.h"
#include "ardour/transport_master.h"

using namespace ARDOUR;
using namespace PBD;

UI_TransportMaster::UI_TransportMaster ()
	: TransportMaster (UI, _("Internal"))
	, _speed (0)
	, _position (0)
	, _session_owns_this (0)
	, _requested_speed (0)
	, _requested_position (0)
	, _loop_location (0)
{
}

void
UI_TransportMaster::pre_process (pframes_t)
{
	Glib::Threads::RWLock::WriterLock lm (speed_pos_lock, Glib::Threads::TRY_LOCK);

	if (lm.locked()) {
		_speed = _requested_speed;
		_position = _requested_position;
	} else {
		/* catch it next time */
	}

	/* don't need the lock for this */

	_session_owns_this = _position;
}

void
UI_TransportMaster::post_process (pframes_t nframes)
{
	/* if this fails, it means that some UI reset the position
	 * (i.e. locate) during the process cycle. If that happens, next cycle
	 * will trigger a locate, and we don't have to care that the cmp&exch
	 * failed.
	 */

	_position.compare_exchange_strong (_session_owns_this, _session_owns_this + nframes);
}

void
UI_TransportMaster::set_speed (double speed)
{
	Glib::Threads::RWLock::WriterLock lm (speed_pos_lock);
	_requested_speed = speed;
}

double
UI_TransportMaster::speed() const
{
	return _requested_speed;
}

void
UI_TransportMaster::set_position (samplepos_t position)
{
	Glib::Threads::RWLock::WriterLock lm (speed_pos_lock);
	_requested_position = position;
}

samplepos_t
UI_TransportMaster::position() const
{
	return _position.load ();
}

bool
UI_TransportMaster::speed_and_position (double & speed, samplepos_t & pos)
{
	/* always called from process context */

	speed = _speed;
	pos = _position.load();
	return true;
}

void
UI_TransportMaster::set_loop_location (Location* loc)
{
	_loop_location = loc;
}

Location*
UI_TransportMaster::loop_location() const
{
	if (_session) {
		return _session->locations()->auto_loop_location ();
	}
	return 0;
}
