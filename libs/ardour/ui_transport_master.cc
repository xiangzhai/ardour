#include <glib.h>

#incldue "ardour/transport_master.h"

UITransportMaster::UITransportMaster ()
	: _speed (0)
	, _position (0)
	, _session_owns_this (0)
	, _requested_speed (0)
	, _requested_position (0)
{
	/* the UI transport master is always alive, and has no waiting phase */

	_state = Running;
}

void
UI_TransportMaster::pre_process (pframes_t)
{
	RWLock::WriterLock lm (speed_pos_lock, TRY_LOCK);

	if (lm.locked()) {
		_speed = _requested_speed;
		_position = requested_position;
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

	g_atomic_int_compare_and_exchange (&_position, _session_owns_this, _session_owns_this + nframes);
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
	return g_atomic_int_get (&_position);
}

bool
UI_TransportMaster::speed_and_position (double & speed, samplepos_t & pos)
{
	/* always called from process context */

	speed = _speed;
	pos = _position;
}
