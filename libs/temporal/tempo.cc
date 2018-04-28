/*
    Copyright (C) 2017 Paul Davis

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

#include "pbd/error.h"
#include "pbd/i18n.h"
#include "pbd/compose.h"
#include "pbd/enumwriter.h"
#include "pbd/failed_constructor.h"
#include "pbd/stacktrace.h"

#include "temporal/debug.h"
#include "temporal/tempo.h"

using namespace PBD;
using namespace Temporal;
using std::cerr;
using std::cout;
using std::endl;
using Temporal::superclock_t;

std::string Tempo::xml_node_name = X_("Tempo");
std::string Meter::xml_node_name = X_("Meter");

void
Point::add_state (XMLNode & node) const
{
	node.set_property (X_("sclock"), _sclock);
	node.set_property (X_("quarters"), _quarters);
	node.set_property (X_("bbt"), _bbt);
}

Point::Point (XMLNode const & node)
{
	if (!node.get_property (X_("sclock"), _sclock)) {
		throw failed_constructor();
	}
	if (!node.get_property (X_("quarters"), _quarters)) {
		throw failed_constructor();
	}
	if (!node.get_property (X_("bbt"), _bbt)) {
		throw failed_constructor();
	}
}

Tempo::Tempo (XMLNode const & node)
{
	assert (node.name() == xml_node_name);
	if (!node.get_property (X_("scpnt-start"), _superclocks_per_note_type)) {
		throw failed_constructor ();
	}
	if (!node.get_property (X_("scpnt-end"), _end_superclocks_per_note_type)) {
		throw failed_constructor ();
	}
	if (!node.get_property (X_("note-type"), _note_type)) {
		throw failed_constructor ();
	}
	if (!node.get_property (X_("type"), _type)) {
		throw failed_constructor ();
	}
	if (!node.get_property (X_("active"), _active)) {
		throw failed_constructor ();
	}
}

bool
Tempo::set_ramped (bool)
{
#warning implement Tempo::set_ramped
	return true;
}

bool
Tempo::set_clamped (bool)
{
#warning implement Tempo::set_clamped
	return true;
}

XMLNode&
Tempo::get_state () const
{
	XMLNode* node = new XMLNode (xml_node_name);

	node->set_property (X_("scpnt-start"), superclocks_per_note_type());
	node->set_property (X_("scpnt-end"), end_superclocks_per_note_type());
	node->set_property (X_("note-type"), note_type());
	node->set_property (X_("type"), type());
	node->set_property (X_("active"), active());

	return *node;
}

Meter::Meter (XMLNode const & node)
{
	assert (node.name() == xml_node_name);
	if (!node.get_property (X_("note-value"), _note_value)) {
		throw failed_constructor ();
	}
	if (!node.get_property (X_("divisions-per-bar"), _divisions_per_bar)) {
		throw failed_constructor ();
	}
}

XMLNode&
Meter::get_state () const
{
	XMLNode* node = new XMLNode (xml_node_name);
	node->set_property (X_("note-value"), note_value());
	node->set_property (X_("divisions-per-bar"), divisions_per_bar());
	return *node;
}

Temporal::BBT_Time
Meter::bbt_add (Temporal::BBT_Time const & bbt, Temporal::BBT_Offset const & add) const
{
	int32_t bars = bbt.bars;
	int32_t beats = bbt.beats;
	int32_t ticks = bbt.ticks;

	if ((bars ^ add.bars) < 0) {
		/* signed-ness varies */
		if (abs(add.bars) >= abs(bars)) {
			/* addition will change which side of "zero" the answer is on;
			   adjust bbt.bars towards zero to deal with "unusual" BBT math
			*/
			if (bars < 0) {
				bars++;
			} else {
				bars--;
			}
		}
	}

	if ((beats ^ add.beats) < 0) {
		/* signed-ness varies */
		if (abs (add.beats) >= abs (beats)) {
			/* adjust bbt.beats towards zero to deal with "unusual" BBT math */
			if (beats < 0) {
				beats++;
			} else {
				beats--;
			}
		}
	}

	Temporal::BBT_Offset r (bars + add.bars, beats + add.beats, ticks + add.ticks);

	if (r.ticks >= Temporal::Beats::PPQN) {
		r.beats += r.ticks / Temporal::Beats::PPQN;
		r.ticks %= Temporal::Beats::PPQN;
	}

	if (r.beats > _divisions_per_bar) {
		r.bars += r.beats / _divisions_per_bar;
		r.beats %= _divisions_per_bar;
	}

	if (r.beats == 0) {
		r.beats = 1;
	}

	if (r.bars == 0) {
		r.bars = 1;
	}

	return Temporal::BBT_Time (r.bars, r.beats, r.ticks);
}

Temporal::BBT_Time
Meter::bbt_subtract (Temporal::BBT_Time const & bbt, Temporal::BBT_Offset const & sub) const
{
	int32_t bars = bbt.bars;
	int32_t beats = bbt.beats;
	int32_t ticks = bbt.ticks;

	if ((bars ^ sub.bars) < 0) {
		/* signed-ness varies */
		if (abs (sub.bars) >= abs (bars)) {
			/* adjust bbt.bars towards zero to deal with "unusual" BBT math */
			if (bars < 0) {
				bars++;
			} else {
				bars--;
			}
		}
	}

	if ((beats ^ sub.beats) < 0) {
		/* signed-ness varies */
		if (abs (sub.beats) >= abs (beats)) {
			/* adjust bbt.beats towards zero to deal with "unusual" BBT math */
			if (beats < 0) {
				beats++;
			} else {
				beats--;
			}
		}
	}

	Temporal::BBT_Offset r (bars - sub.bars, beats - sub.beats, ticks - sub.ticks);

	if (r.ticks < 0) {
		r.beats -= 1 - (r.ticks / Temporal::Beats::PPQN);
		r.ticks = Temporal::Beats::PPQN + (r.ticks % Temporal::Beats::PPQN);
	}

	if (r.beats <= 0) {
		r.bars -= 1 - (r.beats / _divisions_per_bar);
		r.beats = _divisions_per_bar + (r.beats % _divisions_per_bar);
	}

	if (r.beats == 0) {
		r.beats = 1;
	}

	if (r.bars <= 0) {
		r.bars -= 1;
	}

	return Temporal::BBT_Time (r.bars, r.beats, r.ticks);
}

Temporal::BBT_Offset
Meter::bbt_delta (Temporal::BBT_Time const & a, Temporal::BBT_Time const & b) const
{
	return Temporal::BBT_Offset (a.bars - b.bars, a.beats - b.beats, a.ticks - b.ticks);
}

Temporal::BBT_Time
Meter::round_to_bar (Temporal::BBT_Time const & bbt) const
{
	Temporal::BBT_Time b = bbt.round_up_to_beat ();
	if (b.beats > _divisions_per_bar/2) {
		b.bars++;
	}
	b.beats = 1;
	return b;
}


Temporal::Beats
Meter::to_quarters (Temporal::BBT_Offset const & offset) const
{
	Temporal::Beats b;

	b += (offset.bars * _divisions_per_bar * 4) / _note_value;
	b += (offset.beats * 4) / _note_value;
	b += Temporal::Beats::ticks (offset.ticks);

	return b;
}

/*
Ramp Overview

In these notes, we have two units that are reciprocally related: T and S.

   T = 1/S

T is tempo or "beat-things per minute"
S is speed or "minutes per beat-thing"

Most equation-like expressions are expressed both in terms of T and S.

      |                     *
Tempo |                   *
Tt----|-----------------*|
Ta----|--------------|*  |
      |            * |   |
      |         *    |   |
      |     *        |   |
T0----|*             |   |
  *   |              |   |
      _______________|___|____
      time           a   t (next tempo)
      [        c         ] defines c

Duration in beats at time a is the integral of some Tempo function.
In our case, the Tempo function (Tempo at time t) is
T(t) = T0(e^(ct))

>>1/S(t) = (1/S0)(e^ct) => (1/S)(t) = (e^(ct))/S0 => S(t) = S0/(e^(ct))

with function constant
c = log(Ta/T0)/a

>>c = log ((1/Sa)/(1/S0)) / a => c = log (S0/Sa) / a

so
a = log(Ta/T0)/c

>>a = log ((1/Ta)/(1/S0) / c => a = log (S0/Sa) / c

The integral over t of our Tempo function (the beat function, which is the duration in beats at some time t) is:
b(t) = T0(e^(ct) - 1) / c

>>b(t) = 1/S0(e^(ct) - 1) / c  => b(t) = (e^(ct) - 1) / (c * S0)

To find the time t at beat duration b, we use the inverse function of the beat function (the time function) which can be shown to be:
t(b) = log((c.b / T0) + 1) / c

>>t(b) = log((c*b / (1/S0)) + 1) / c => t(b) = log ((c*b * S0) + 1) / c

The time t at which Tempo T occurs is a as above:
t(T) = log(T / T0) / c

>> t(1/S) = log ((1/S) / (1/S0) /c => t(1/S) = log (S0/S) / c

The beat at which a Tempo T occurs is:
b(T) = (T - T0) / c

>> b(1/S) = (1/S - 1/S0) / c

The Tempo at which beat b occurs is:
T(b) = b.c + T0

>> T(b) = b.c + (1/S0)

We define c for this tempo ramp by placing a new tempo section at some time t after this one.
Our problem is that we usually don't know t.
We almost always know the duration in beats between this and the new section, so we need to find c in terms of the beat function.
Where a = t (i.e. when a is equal to the time of the next tempo section), the beat function reveals:
t = b log (Ta / T0) / (T0 (e^(log (Ta / T0)) - 1))

By substituting our expanded t as a in the c function above, our problem is reduced to:
c = T0 (e^(log (Ta / T0)) - 1) / b

>> c = (1/S0) (e^(log ((1/Sa) / (1/S0))) - 1) / b => c = (1/S0) (e^(log (S0/Sa)) - 1) / b => c (e^(log (S0/Sa)) - 1) / (b * S0)

Of course the word 'beat' has been left loosely defined above.
In music, a beat is defined by the musical pulse (which comes from the tempo)
and the meter in use at a particular time (how many  pulse divisions there are in one bar).
It would be more accurate to substitute the work 'pulse' for 'beat' above.

 */

/* equation to compute c is:
 *
 *    c = log (Ta / T0) / a
 *
 * where
 *
 *   a : time into section (from section start
 *  T0 : tempo at start of section
 *  Ta : tempo at time a into section
 *
 * THE UNITS QUESTION
 *
 * log (Ta / T0) / (time-units) => C is in per-time-units (1/time-units)
 *
 * so the question is what are the units of a, and thus c?
 *
 * we could use ANY time units (because we can measure a in any time units)
 * but whichever one we pick dictates how we can use c in the future since
 * all subsequent computations will need to use the same time units.
 *
 * options:
 *
 *    pulses        ... whole notes, possibly useful, since we can use it for any other note_type
 *    quarter notes ... linearly related to pulses
 *    beats         ... not a fixed unit of time
 *    minutes       ... linearly related to superclocks
 *    samples       ... needs sample rate
 *    superclocks   ... frequently correct
 *
 * so one answer might be to compute c in two different units so that we have both available.
 *
 * hence, compute_c_superclocks() and compute_c_pulses()
 */

XMLNode&
TempoPoint::get_state () const
{
	XMLNode& base (Tempo::get_state());
	Point::add_state (base);
	return base;
}

TempoPoint::TempoPoint (XMLNode const & node)
	: Tempo (node)
	, Point (node)
	, _c_per_quarter (0)
	, _c_per_superclock (0)
{
}

void
TempoPoint::compute_c_superclock (samplecnt_t sr, superclock_t end_scpqn, superclock_t superclock_duration)
{
	if ((superclocks_per_quarter_note() == end_scpqn) || !ramped()) {
		_c_per_superclock = 0.0;
		return;
	}

	_c_per_superclock = log ((double) superclocks_per_quarter_note () / end_scpqn) / superclock_duration;
}
void
TempoPoint::compute_c_quarters (samplecnt_t sr, superclock_t end_scpqn, Temporal::Beats const & quarter_duration)
{
	if ((superclocks_per_quarter_note () == end_scpqn) || !ramped()) {
		_c_per_quarter = 0.0;
		return;
	}

	_c_per_quarter = log (superclocks_per_quarter_note () / (double) end_scpqn) /  quarter_duration.to_double();
}

superclock_t
TempoPoint::superclock_at_qn (Temporal::Beats const & qn) const
{
	if (_c_per_quarter == 0.0) {
		/* not ramped, use linear */
		return _sclock + llrint (superclocks_per_quarter_note () * qn.to_double());
	}

	cerr << "Compute S-at-qn " << qn << " starting at " << _sclock << " + " << llrint (superclocks_per_quarter_note() * (log1p (_c_per_quarter * qn.to_double()) / _c_per_quarter)) << endl;

	return _sclock + llrint (superclocks_per_quarter_note() * (log1p (_c_per_quarter * qn.to_double()) / _c_per_quarter));
}

MeterPoint::MeterPoint (XMLNode const & node)
	: Meter (node)
	, Point (node)
{
}

XMLNode&
MeterPoint::get_state () const
{
	XMLNode& base (Meter::get_state());
	Point::add_state (base);
	return base;
}

MusicTimePoint::MusicTimePoint (XMLNode const & node)
	: Point (node)
{
}

XMLNode&
MusicTimePoint::get_state () const
{
	XMLNode* node = new XMLNode (X_("MusicTime"));
	Point::add_state (*node);
	return *node;
}

timepos_t
TempoMapPoint::time() const
{
	switch (_map->time_domain()) {
	case AudioTime:
		return timepos_t (sample());
	case BeatTime:
	case BarTime:
		return timepos_t (bbt());
	}
	/*NOTREACHED*/
	abort();
	/*NOTREACHED*/
	return timepos_t();
}

samplepos_t
TempoMapPoint::sample() const
{
	return superclock_to_samples (_sclock, _map->sample_rate());
}

void
TempoMapPoint::start_float ()
{
	_floating = true;
}

void
TempoMapPoint::end_float ()
{
	_floating = false;
}

Temporal::Beats
TempoMapPoint::quarters_at (superclock_t sc) const
{
	/* This TempoMapPoint must already have a fully computed metric and position */

	if (!_tempo->ramped()) {
		return _quarters + Temporal::Beats ((sc - _sclock) / (double) (metric().superclocks_per_quarter_note ()));
	}

	return _quarters + Temporal::Beats (expm1 (metric().c_per_superclock() * (sc - _sclock)) / (metric().c_per_superclock() * metric().superclocks_per_quarter_note ()));
}

Temporal::Beats
TempoMapPoint::quarters_at (Temporal::BBT_Time const & bbt) const
{
	/* This TempoMapPoint must already have a fully computed metric and position */

	Temporal::BBT_Offset offset = metric().bbt_delta (bbt, _bbt);
	return _quarters + metric().to_quarters (offset);
}

Temporal::BBT_Time
TempoMapPoint::bbt_at (Temporal::Beats const & qn) const
{
	/* This TempoMapPoint must already have a fully computed metric and position */

	Temporal::Beats quarters_delta = qn - _quarters;
	int32_t ticks_delta = quarters_delta.to_ticks (Temporal::Beats::PPQN);
	return metric().bbt_add (_bbt, Temporal::BBT_Offset (0, 0,  ticks_delta));
}

Temporal::BBT_Time
TempoMapPoint::bbt_at (samplepos_t pos) const
{
	/* This TempoMapPoint must already have a fully computed metric and position */

	superclock_t sclock_delta = sclock() - samples_to_superclock (pos, _map->sample_rate());
	int32_t ticks_delta = sclock_delta / metric().superclocks_per_ppqn ();
	return metric().bbt_add (_bbt, Temporal::BBT_Offset (0, 0,  ticks_delta));
}

/* TEMPOMAP */

struct TraceableWriterLock : public Glib::Threads::RWLock::WriterLock
{
	TraceableWriterLock (Glib::Threads::RWLock & lock) : Glib::Threads::RWLock::WriterLock (lock) { std::cerr << "LOCK " << this << std::endl; PBD::stacktrace (std::cerr, 20); }
	~TraceableWriterLock() { std::cerr << "UNLOCK " << this << std::endl; }
};

TempoMap::TempoMap (Tempo const & initial_tempo, Meter const & initial_meter, samplecnt_t sr)
	: _sample_rate (sr)
	, _dirty (false)
	, _generation (0) // XXX needs to be reloaded from saved state??
{
	TraceableWriterLock lm (_lock);

	_tempos.push_back (TempoPoint (initial_tempo, 0, Beats(), BBT_Time()));
	_meters.push_back (MeterPoint (initial_meter, 0, Beats(), BBT_Time()));

	_points.push_back (TempoMapPoint (this, TempoMapPoint::Flag (TempoMapPoint::ExplicitMeter|TempoMapPoint::ExplicitTempo), _tempos.front(), _meters.front(), 0, Beats(), BBT_Time()));

	set_dirty (true);

	cerr << "Initial...\n";
	dump_locked (cerr);
}

TempoMap::~TempoMap()
{
}

#define S2Sc(s) (samples_to_superclock ((s), _sample_rate))

void
TempoMap::set_dirty (bool yn)
{
	DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("tempo map @ %1 dirty set to %2\n", this, yn));
	_dirty = yn;
}

void
TempoMap::maybe_rebuild ()
{
	if (_dirty && !_points.empty()) {
		rebuild (_points.back().sclock());
	}
}

Meter const &
TempoMap::meter_at (timepos_t const & time) const
{
	switch (time.lock_style()) {
	case AudioTime:
		return meter_at (time.sample());
		break;
	case BarTime:
		return meter_at (time.bbt());
		break;
	case BeatTime:
		return meter_at (time.beats());
		break;
	}
	/*NOTREACHED*/
	return meter_at (0);
}

Meter const &
TempoMap::meter_at (samplepos_t s) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return meter_at_locked (S2Sc (s));
}

Meter const &
TempoMap::meter_at (Temporal::Beats const & b) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return meter_at_locked (b);
}

Meter const &
TempoMap::meter_at (Temporal::BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return meter_at_locked (bbt);
}

Tempo const &
TempoMap::tempo_at (timepos_t const & time) const
{
	switch (time.lock_style()) {
	case AudioTime:
		return tempo_at (time.sample());
		break;
	case BarTime:
		return tempo_at (time.bbt());
		break;
	case BeatTime:
		return tempo_at (time.beats());
		break;
	}
	/*NOTREACHED*/
	return tempo_at (0);
}

Tempo const &
TempoMap::tempo_at (samplepos_t s) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return tempo_at_locked (S2Sc(s));
}

Tempo const &
TempoMap::tempo_at (Temporal::Beats const &b) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return tempo_at_locked (b);
}

Tempo const &
TempoMap::tempo_at (Temporal::BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return tempo_at_locked (bbt);
}

void
TempoMap::set_time_domain (TimeDomain td)
{

	if (td == time_domain()) {
		return;
	}

#warning paul tempo_map::set_time_domain needs implementing
#if 0
	switch (td) {
	case AudioTime:
		for (Tempos::iterator t = _tempos.begin(); t != _tempos.end(); ++t) {
			t->set_sclock (t->superclock_at_qn (t->beats ()));
		}
		for (Meters::iterator m = _meters.begin(); m != _meters.end(); ++m) {
			m->set_sclock (m->superclock_at_qn (m->beats ()));
		}
		break;

	default:
		for (Tempos::iterator t = _tempos.begin(); t != _tempos.end(); ++t) {
			t->set_beats (t->quarters_at (t->sclock()));
		}
		for (Meters::iterator m = _meters.begin(); m != _meters.end(); ++m) {
			m->set_beats (m->quarters_at (m->sclock()));
		}
	}
#endif

	_time_domain = td;
	set_dirty (true);
}

void
TempoMap::rebuild (superclock_t limit)
{
	/* CALLER MUST HOLD LOCK */

	assert (!_tempos.empty());
	assert (!_meters.empty());

	if (limit == 0) { /* which would make no sense */
		limit = _points.back().sclock();
	}

	_points.clear ();

	/* iterators t (tempo) and m (meter) can only advance to the final element in their respective containers

	   iterator b (bartimes) can advance (or even begin) to the ::end() of their respective container
	*/

	Tempos::iterator t = _tempos.begin();
	Meters::iterator m = _meters.begin();
	MusicTimes::iterator b = _bartimes.begin();
	Tempos::iterator nxt_tempo = t; ++nxt_tempo;
	TempoPoint* current_tempo = &*t;
	MeterPoint* current_meter = &*m;

	/* verify that initial tempo+meter are at the same (zero) time */
	assert (t->sclock() == m->sclock());
	assert (t->sclock() == 0);

	/* default initial BBT time is 1|1|0 but there might be a bartime marker right at the beginning of the map
	   to change that.
	*/

	BBT_Time bbt_pos;

	if (b != _bartimes.end()) {
		if (b->sclock() == 0) {
			bbt_pos = b->bbt();
		}
	}

	/* now add explicit map points for all tempo, meter and bartime points */

	while ((t != _tempos.end()) || (m != _meters.end()) || (b != _bartimes.end())) {

		/* UPDATE RAMP COEFFICIENTS WHEN NECESSARY */

		if (t->ramped() && nxt_tempo != _tempos.end()) {
			switch (time_domain ()) {
			case AudioTime:
				t->compute_c_superclock (_sample_rate, nxt_tempo->superclocks_per_quarter_note (), nxt_tempo->sclock() - t->sclock());
			case BeatTime:
				t->compute_c_quarters (_sample_rate, nxt_tempo->superclocks_per_quarter_note (), nxt_tempo->beats() - t->beats());
			}
		}

		/* figure out which of the 1, 2 or 3 possible iterators defines the next explicit point (we want the earliest on the timeline,
		   but there may be more than 1 at the same location).
		*/

		Point first_of_three (std::numeric_limits<superclock_t>::max(), Beats(), BBT_Time()); /* don't care about the beats or BBT values */

		if (m != _meters.end() && m->sclock() < first_of_three.sclock()) {
			first_of_three = *m;
		}

		if (t != _tempos.end() && t->sclock() < first_of_three.sclock()) {
			first_of_three = *t;
		}

		if (b != _bartimes.end() && (b->sclock() < first_of_three.sclock())) {
			first_of_three = *b;
		}

		/* Determine whether a tempo or meter or bartime point (or any combination thereof) is defining this new point */

		TempoMapPoint::Flag flag = TempoMapPoint::Flag (0);
		bool advance_meter = false;
		bool advance_tempo = false;
		bool advance_bartime = false;

		if (m->sclock() == first_of_three.sclock()) {
			flag = TempoMapPoint::Flag (flag|TempoMapPoint::ExplicitMeter);
			advance_meter = true;
			current_meter = &*m;
			cerr << "THIS POINT WILL DEFINE A METER (" << *m << ")\n";
		}

		if (t->sclock() == first_of_three.sclock()) {
			flag = TempoMapPoint::Flag (flag|TempoMapPoint::ExplicitTempo);
			advance_tempo = true;
			current_tempo = &*t;
			cerr << "THIS POINT WILL DEFINE A TEMPO (" << *t << ")\n";
		}

		if ((b != _bartimes.end()) && (b->sclock() == first_of_three.sclock())) {
			flag = TempoMapPoint::Flag (flag|TempoMapPoint::ExplicitPosition);
			advance_bartime = true;
			cerr << "THIS POINT WILL DEFINE A BBT position\n";
		}

		/* add an explicit point right here */
		cerr << "Adding new point with tempo = " << *current_tempo << " meter = " << *current_meter << endl;
		_points.push_back (TempoMapPoint (this, flag, *current_tempo, *current_meter, first_of_three.sclock(), first_of_three.beats(), first_of_three.bbt()));
		cerr << "ADDED: " << _points.back() << endl;

		if (advance_meter && (m != _meters.end())) {
			cerr << "used a meter\n";
			++m;
		}
		if (advance_tempo && (t != _tempos.end())) {
			cerr << "used a tempo\n";
			++t;
			if (nxt_tempo != _tempos.end()) {
				++nxt_tempo;
			}
		}
		if (advance_bartime && (b != _bartimes.end())) {
			cerr << "used a bartime\n";
			++b;
		}
	}

	DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("explicit tempo map contains %1 points\n", _points.size()));

	/* now fill in all the implicit points until the next explicit one */

	for (TempoMapPoints::iterator p = _points.begin(); p != _points.end(); ) {
		TempoMapPoints::iterator nxt = p;
		++nxt;

		if (p->is_explicit_meter()) {
			current_meter = &p->meter();
		}

		if (p->is_explicit_tempo()) {
			current_tempo = &p->tempo();
		}

		assert (current_tempo);
		assert (current_meter);

		superclock_t superclock_step = p->tempo().superclocks_per_note_type ();
		superclock_t sc = p->sclock();

		sc += superclock_step;

		BBT_Time bbt (p->bbt());

		while ((nxt != _points.end() && sc < nxt->sclock()) || sc < limit) {

			sc += superclock_step;
			Beats beats = p->quarters_at (sc);
			bbt = p->meter().bbt_add (bbt, Temporal::BBT_Offset (0, 1, 0));

			_points.insert (nxt, TempoMapPoint (this, TempoMapPoint::Flag (0), *current_tempo, *current_meter, sc, beats, bbt));
		}

		/* skip over the points we just inserted, to continue with the next explicit point originally
		   added earlier.
		*/
		p = nxt;
	}

	DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("rebuild: completed, %1 points\n", _points.size()));
	dump_locked (cerr);

	_generation++;
	set_dirty (false);
}

void
TempoMap::full_rebuild ()
{
	TraceableWriterLock lm (_lock);
	assert (!_points.empty());
	rebuild (_points.back().sclock());
}

void
TempoMap::extend (superclock_t limit)
{
	TraceableWriterLock lm (_lock);

	DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("extend map to %1 from %2 with %3\n", limit, _points.back().sclock(), _points.size()));

	TempoMapPoint const & last_point (_points.back());

	TempoMetric metric (last_point.metric());

	const Beats qn_step = (Temporal::Beats (1,0) * 4) / metric.note_value();
	const superclock_t superclock_step = metric.superclocks_per_note_type ();

	superclock_t sc = last_point.sclock();
	Beats qn (last_point.quarters ());
	BBT_Time bbt (last_point.bbt());

	while (1) {
		qn += qn_step;
		sc += superclock_step;

		if (sc >= limit) {
			break;
		}

		bbt = metric.bbt_add (bbt, Temporal::BBT_Offset (0, 1, 0));

		_points.push_back (TempoMapPoint (this, TempoMapPoint::Flag (0), last_point.tempo(), last_point.meter(), sc, qn, bbt));
	}

	dump_locked (cerr);
}

MeterPoint*
TempoMap::add_meter (MeterPoint const & mp)
{
	/* CALLER MUST HOLD LOCK */

	Meters::iterator m;

	switch (time_domain()) {
	case AudioTime:
		m = upper_bound (_meters.begin(), _meters.end(), mp, Point::sclock_comparator());
		break;
	case BeatTime:
		m = upper_bound (_meters.begin(), _meters.end(), mp, Point::beat_comparator());
		break;
	case BarTime:
		m = upper_bound (_meters.begin(), _meters.end(), mp, Point::bbt_comparator());
		break;
	}

	bool replaced = false;
	MeterPoint* ret = 0;

	if (m != _meters.end()) {
		if (m->sclock() == mp.sclock()) {
			/* overwrite Meter part of this point */
			*((Meter*)&(*m)) = mp;
			ret = &(*m);
			replaced = true;
		}
	}

	if (!replaced) {
		ret = &(*(_meters.insert (m, mp)));
	}

	return ret;
}

void
TempoMap::change_tempo (TempoPoint & p, Tempo const & t)
{
	TraceableWriterLock lm (_lock);

	*((Tempo*)&p) = t;

	set_dirty (true);
}

TempoMapPoint const &
TempoMap::set_tempo (Tempo const & t, superclock_t sc)
{
	TraceableWriterLock lm (_lock);

	assert (!_points.empty());

	DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set tempo @ %1 to %2\n", sc, t));

	Beats beats;
	BBT_Time bbt;

	TempoMapPoint const &point (const_point_at (sc));

	/* tempo changes must be on beat */

	beats = point.quarters_at (sc).round_to_beat ();
	bbt = point.bbt_at (beats);

	/* recompute superclock position of rounded beat */
	sc = point.tempo().superclock_at_qn (beats);

	TempoPoint tp (t, sc, beats, bbt);
	add_tempo (tp);

	rebuild ();

	return const_point_at (tp.sclock());
}

TempoMapPoint const &
TempoMap::set_tempo (Tempo const & t, BBT_Time const & bbt)
{
	TraceableWriterLock lm (_lock);

	DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set tempo @ %1 to %2\n", bbt, t));

	/* tempo changes are required to be on-beat */

	BBT_Time on_beat = bbt.round_to_beat();
	superclock_t sc;
	Beats beats;

	TempoMapPoint const &point (const_point_at (on_beat));

	beats = point.quarters_at (on_beat);
	sc = point.tempo().superclock_at_qn (beats);

	TempoPoint tp (t, sc, beats, on_beat);
	(void) add_tempo (tp);

	rebuild ();

	return const_point_at (tp.sclock());
}

TempoMapPoint const &
TempoMap::set_tempo (Tempo const & t, timepos_t const & time)
{
	switch (time.lock_style()) {
	case AudioTime:
		return set_tempo (t, time.sample());
		break;
	default:
		break;
	}

	return set_tempo (t, time.beats());
}

TempoPoint*
TempoMap::add_tempo (TempoPoint const & tp)
{
	/* CALLER MUST HOLD LOCK */

	Tempos::iterator t;

	switch (time_domain()) {
	case AudioTime:
		t = upper_bound (_tempos.begin(), _tempos.end(), tp, Point::sclock_comparator());
		break;
	case BeatTime:
		t = upper_bound (_tempos.begin(), _tempos.end(), tp, Point::beat_comparator());
		break;
	case BarTime:
		t = upper_bound (_tempos.begin(), _tempos.end(), tp, Point::bbt_comparator());
		break;
	}

	bool replaced = false;
	TempoPoint* ret = 0;

	if (t != _tempos.end()) {
		if (t->sclock() == tp.sclock()) {
			/* overwrite Tempo part of this point */
			*((Tempo*)&(*t)) = tp;
			ret = &(*t);
			replaced = true;
		}
	}

	if (!replaced) {
		ret = &(*(_tempos.insert (t, tp)));
	}

	set_dirty (true);

	return ret;
}

void
TempoMap::remove_tempo (TempoPoint const & tp)
{
	TraceableWriterLock lm (_lock);

	Tempos::iterator t = upper_bound (_tempos.begin(), _tempos.end(), tp, Point::sclock_comparator());
	if (t->sclock() != tp.sclock()) {
		/* error ... no tempo point at the time of tp */
		return;
	}
	_tempos.erase (t);
	set_dirty (true);
}

bool
TempoMap::move_tempo (TempoPoint const & tp, timepos_t const & when, bool push)
{
	Tempos::iterator t;
	Tempos::iterator next;
	Tempos::iterator prev;
	superclock_t sc;
	bool moved = false;
	Beats beats;

	assert (when.lock_style() == time_domain());

	switch (time_domain()) {
	case AudioTime:

		/* find iterator for existing tempo point */

		sc = S2Sc (when.sample());

		/* only the superclock time matters for the comparator */
		t = upper_bound (_tempos.begin(), _tempos.end(), TempoPoint (tp, sc, Beats(), BBT_Time()), Point::sclock_comparator());

		/* if insert position is end, or the found tempo is at a
		   different time than the passed in TempoPoint, do nothing
		*/
		if (t == _tempos.end() || (t->sclock() != tp.sclock())) {
			/* no tempo at this time */
			return false;
		}

		next = t; ++next;

		if (t == _tempos.begin()) {
			if (sc < next->sclock()) {
				/* no earlier position, and we can just slide
				 * it to the new position, since it remains
				 * ordered before the following TempoPoint.
				 */
				t->set_sclock (sc);
				moved = true;
			}
		} else {
			prev = t; --prev;
			if (prev->sclock() < sc && sc < next->sclock()) {
				/* we can just slide it along, since it
				 * remains positioned between the adjacent
				 * TempoPoints
				 */
				t->set_sclock (sc);
				moved = true;
			}
		}
		if (!moved) {
			_tempos.erase (t);
			set_dirty (true);
			set_tempo (tp, sc);
		}
		break;

	case BeatTime:
		/* find existing tempo point */

		beats = when.beats ();

		/* only the beat time matters for the comparator */
		t = upper_bound (_tempos.begin(), _tempos.end(), TempoPoint (tp, 0, beats, BBT_Time()), Point::beat_comparator());

		/* if insert position is end, or the found tempo is at a
		   different time than the passed in TempoPoint, do nothing
		*/
		if (t == _tempos.end() || (t->beats() != tp.beats())) {
			/* no tempo at this time */
			return false;
		}

		next = t; ++next;

		if (t == _tempos.begin()) {
			if (beats < next->beats()) {
				/* no earlier position, and we can just slide
				 * it to the new position, since it remains
				 * ordered before the following TempoPoint.
				 */
				t->set_beats (beats);
				moved = true;
			}
		} else {
			prev = t; --prev;
			if (prev->beats() < beats && beats < next->beats()) {
				/* we can just slide it along, since it
				 * remains positioned between the adjacent
				 * TempoPoints
				 */
				t->set_beats (beats);
				moved = true;
			}
		}
		if (!moved) {
			_tempos.erase (t);
			set_dirty (true);
			set_tempo (tp, beats);
		}
		break;

	case BarTime:
		break;
	}

	set_dirty (true);

	return true;
}

TempoMapPoint const &
TempoMap::set_meter (Meter const & m, timepos_t const & time)
{
	switch (time.lock_style()) {
	case AudioTime:
		cerr << "set meter via sample " << time << endl;
		return set_meter (m, S2Sc (time.sample()));
	default:
		break;
	}

	cerr << "set meter via BBT " << time.bbt() << endl;
	return set_meter (m, time.bbt());
}


TempoMapPoint const &
TempoMap::set_meter (Meter const & t, BBT_Time const & bbt)
{
	TempoMapPoint const * ret = 0;

	{
		TraceableWriterLock lm (_lock);

		DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set meter @ %1 to %2\n", bbt, t));

		TempoMapPoint const &point (const_point_at (bbt));
		superclock_t sc;
		Beats beats;
		BBT_Time rounded_bbt;

		/* meter changes are required to be on-bar */

		cerr << "Using point @ " << point << endl;

		rounded_bbt = point.metric().round_to_bar (bbt);
		beats = point.quarters_at (rounded_bbt);
		sc = point.tempo().superclock_at_qn (beats);

		MeterPoint mp (t, sc, beats, rounded_bbt);

		cerr << "Adding " << mp << endl;

		add_meter (mp);

		rebuild ();

		ret = &const_point_at (rounded_bbt);
	}

	Changed (0, _points.back().sample());

	return *ret;
}

TempoMapPoint const &
TempoMap::set_meter (Meter const & m, superclock_t sc)
{
	TempoMapPoint const * ret = 0;

	{
		TraceableWriterLock lm (_lock);

		assert (!_points.empty());

		DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set meter @ %1 to %2\n", sc, m));

		Beats beats;
		BBT_Time bbt;

		TempoMapPoint const &point (const_point_at (sc));

		/* meter changes must be on bar */

		bbt = point.bbt_at (beats);
		bbt = point.metric().round_to_bar (bbt);

		/* compute beat position */
		beats = point.quarters_at (bbt);

		/* recompute superclock position of bar-rounded position */
		sc = point.tempo().superclock_at_qn (beats);

		MeterPoint mp (m, sc, beats, bbt);
		add_meter (mp);

		rebuild ();

		ret = &const_point_at (sc);
	}

	Changed (0, _points.back().sample());

	return *ret;
}

void
TempoMap::remove_meter (MeterPoint const & mp)
{
	{
		TraceableWriterLock lm (_lock);

		Meters::iterator m = upper_bound (_meters.begin(), _meters.end(), mp, Point::sclock_comparator());
		if (m->sclock() != mp.sclock()) {
			/* error ... no meter point at the time of mp */
			return;
		}
		_meters.erase (m);
		set_dirty (true);
	}

	Changed (0, _points.back().sample());

}

TempoMapPoints::iterator
TempoMap::iterator_at (superclock_t sc)
{
	/* CALLER MUST HOLD LOCK */

	if (_points.empty()) {
		throw EmptyTempoMapException();
	}

	if (_points.size() == 1) {
		return _points.begin();
	}

	/* Construct an arbitrary TempoMapPoint. The only property we care about is it's superclock time,
	   so other values used in the constructor are arbitrary and irrelevant.
	*/

	maybe_rebuild ();

	const TempoMapPoint tp (this, TempoMapPoint::Flag (0), _points.front().tempo(), _points.front().meter(), sc, Temporal::Beats(), Temporal::BBT_Time());
	TempoMapPoint::SuperClockComparator scmp;

	TempoMapPoints::iterator tmp = upper_bound (_points.begin(), _points.end(), tp, scmp);

	if (tmp != _points.begin()) {
		return --tmp;
	}

	return tmp;
}

TempoMapPoints::iterator
TempoMap::iterator_at (Temporal::Beats const & qn)
{
	/* CALLER MUST HOLD LOCK */

	if (_points.empty()) {
		throw EmptyTempoMapException();
	}

	if (_points.size() == 1) {
		return _points.begin();
	}

	maybe_rebuild ();

	/* Construct an arbitrary TempoMapPoint. The only property we care about is its quarters time,
	   so other values used in the constructor are arbitrary and irrelevant.
	*/

	const TempoMapPoint tp (this, TempoMapPoint::Flag (0), _points.front().tempo(), _points.front().meter(), 0, qn, Temporal::BBT_Time());
	TempoMapPoint::QuarterComparator bcmp;

	TempoMapPoints::iterator tmp = upper_bound (_points.begin(), _points.end(), tp, bcmp);

	if (tmp != _points.begin()) {
		return --tmp;
	}

	return tmp;
}

TempoMapPoints::iterator
TempoMap::iterator_at (Temporal::BBT_Time const & bbt)
{
	/* CALLER MUST HOLD LOCK */

	if (_points.empty()) {
		throw EmptyTempoMapException();
	}

	if (_points.size() == 1) {
		return _points.begin();
	}

	maybe_rebuild ();

	/* Construct an arbitrary TempoMapPoint. The only property we care about is its bbt time,
	   so other values used in the constructor are arbitrary and irrelevant.
	*/

	const TempoMapPoint tp (this, TempoMapPoint::Flag (0), _points.front().tempo(), _points.front().meter(), 0, Temporal::Beats(), bbt);
	TempoMapPoint::BBTComparator bcmp;

	TempoMapPoints::iterator tmp = upper_bound (_points.begin(), _points.end(), tp, bcmp);

	if (tmp != _points.begin()) {
		return --tmp;
	}

	return tmp;
}

Temporal::BBT_Time
TempoMap::bbt_at (timepos_t const & pos) const
{
	switch (pos.lock_style()) {
	case BarTime:
		return pos.bbt();
	case AudioTime:
		return bbt_at (pos.sample());
	default:
		break;
	}

	return bbt_at (pos.beats());
}

Temporal::BBT_Time
TempoMap::bbt_at (samplepos_t s) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return bbt_at_locked (S2Sc (s));
}

Temporal::BBT_Time
TempoMap::bbt_at_locked (superclock_t sc) const
{
	TempoMapPoint point (const_point_at (sc));
	Temporal::Beats b ((sc - point.sclock()) / (double) point.metric().superclocks_per_quarter_note());
	return point.metric().bbt_add (point.bbt(), Temporal::BBT_Offset (0, b.get_beats(), b.get_ticks()));
}

Temporal::BBT_Time
TempoMap::bbt_at (Temporal::Beats const & qn) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return bbt_at_locked (qn);
}

Temporal::BBT_Time
TempoMap::bbt_at_locked (Temporal::Beats const & qn) const
{
	TempoMapPoint const & point (const_point_at (qn));
	Temporal::Beats delta (qn - point.quarters());
	return point.metric().bbt_add (point.bbt(), Temporal::BBT_Offset (0, delta.get_beats(), delta.get_ticks()));
}

samplepos_t
TempoMap::sample_at (Temporal::Beats const & qn) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return sample_at_locked (qn);
}

samplepos_t
TempoMap::sample_at_locked (Temporal::Beats const & qn) const
{
	TempoMapPoints::const_iterator i = const_iterator_at (qn);

	/* compute distance from reference point to b. Remember that Temporal::Beats is always interpreted as quarter notes */

	const Temporal::Beats q_delta = qn - i->quarters();
	superclock_t sclock_delta = i->metric().superclock_at_qn (q_delta);
	return superclock_to_samples (i->sclock() + sclock_delta, _sample_rate);
}

samplepos_t
TempoMap::sample_at (Temporal::BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return sample_at_locked (bbt);
}

samplepos_t
TempoMap::sample_at_locked (BBT_Time const & bbt) const
{
	TempoMapPoints::const_iterator i = const_iterator_at (bbt);

	/* this computes the distance from the point, in beats whose size is
	   determined by the meter.
	*/

	const Temporal::BBT_Offset delta = i->metric().bbt_delta (bbt, i->bbt());

	/* convert to quarter notes */
	const int32_t ticks = delta.ticks + (Temporal::Beats::PPQN * delta.beats * 4) / i->metric().note_value();

	/* get distance in superclocks */
	return superclock_to_samples (i->sclock() + i->metric().superclock_at_qn (Temporal::Beats::ticks (ticks)), _sample_rate);
}

samplepos_t
TempoMap::sample_at (timepos_t const & pos) const
{
	switch (pos.lock_style()) {
	case BarTime:
		return sample_at (pos.bbt());
	case BeatTime:
		return sample_at (pos.beats ());
	case AudioTime:
		break;
	}
	return pos.sample();
}

samplepos_t
TempoMap::samplepos_plus_bbt (samplepos_t pos, BBT_Time op) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	BBT_Time pos_bbt = bbt_at (pos);

	pos_bbt.ticks += op.ticks;
	if (pos_bbt.ticks >= ticks_per_beat) {
		++pos_bbt.beats;
		pos_bbt.ticks -= ticks_per_beat;
	}
	pos_bbt.beats += op.beats;

	double divisions_per_bar = meter_at_locked (pos_bbt).divisions_per_bar();
	while (pos_bbt.beats >= divisions_per_bar + 1) {
		++pos_bbt.bars;
		divisions_per_bar = meter_at_locked (pos_bbt).divisions_per_bar();
		pos_bbt.beats -= divisions_per_bar;
	}
	pos_bbt.bars += op.bars;

	return sample_at (pos_bbt);
}

/** Count the number of beats that are equivalent to distance when going forward,
    starting at pos.
*/
Temporal::Beats
TempoMap::samplewalk_to_quarters (samplepos_t pos, samplecnt_t distance) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	TempoMapPoint const & first (const_point_at (pos));
	TempoMapPoint const & last (const_point_at (pos+distance));
	Temporal::Beats a = first.quarters_at (S2Sc (pos));
	Temporal::Beats b = last.quarters_at (S2Sc (pos+distance));
	return b - a;
}

Temporal::Beats
TempoMap::samplewalk_to_quarters (Temporal::Beats const & pos, samplecnt_t distance) const
{
	/* XXX this converts from beats to samples and back to beats... undesirable */
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	samplepos_t s = sample_at_locked (pos);
	s += distance;
	return const_point_at (distance).quarters_at (s);

}

Temporal::Beats
TempoMap::bbtwalk_to_quarters (Beats const & pos, BBT_Offset const & distance) const
{
	return quarter_note_at (bbt_walk (bbt_at (pos), distance)) - pos;
}

void
TempoMap::set_sample_rate (samplecnt_t new_sr)
{
	TraceableWriterLock lm (_lock);
	double ratio = new_sr / (double) _sample_rate;

	for (TempoMapPoints::iterator i = _points.begin(); i != _points.end(); ++i) {
		i->map_reset_set_sclock_for_sr_change (llrint (ratio * i->sclock()));
	}
}

void
TempoMap::dump (std::ostream& ostr)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	dump_locked (ostr);
}

void
TempoMap::dump_locked (std::ostream& ostr)
{
	ostr << "\n\n------------\n";
	for (TempoMapPoints::iterator i = _points.begin(); i != _points.end(); ++i) {
		ostr << *i << std::endl;
	}
}

void
TempoMap::dump_fundamental (std::ostream& ostr) const
{
	for (Tempos::const_iterator t = _tempos.begin(); t != _tempos.end(); ++t) {
		ostr << *t << endl;
	}

	for (Meters::const_iterator m = _meters.begin(); m != _meters.end(); ++m) {
		ostr << *m << endl;
	}
}

void
TempoMap::get_points (TempoMapPoints& ret) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit()) {
			ret.push_back (*p);
		}
	}
}

void
TempoMap::get_tempos (TempoMapPoints& ret) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit_tempo()) {
			ret.push_back (*p);
		}
	}

	cerr << "get tempos found " << ret.size() << endl;
}

void
TempoMap::get_meters (TempoMapPoints& ret) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit_meter()) {
			ret.push_back (*p);
		}
	}

	cerr << "get meters found " << ret.size() << endl;
}

void
TempoMap::get_grid (TempoMapPoints& ret, samplepos_t s, samplepos_t e, Temporal::Beats const & resolution)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	superclock_t start = S2Sc (s);
	superclock_t end = S2Sc (e);

	TempoMapPoints::iterator p = iterator_at (start);

	DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("get grid between %1..%2 at resolution = %3; current end: %4\n",start, end, resolution, _points.back().sclock()));

	/* extend as necessary to fill out to e */

	if (e > _points.back().sample()) {
		/* need to drop reader lock while we rebuild since that
		   requires the writer lock.
		*/
		lm.release ();
		extend (end);
		lm.acquire ();
	}

	/* advance to the first point at or later than start (since by
	 * definition, p may point at the point before start.
	 */

	while (p != _points.end() && p->sclock() < start) {
		++p;
	}

	Temporal::Beats prev_beats;

	while ((p != _points.end()) && (p->sclock() < end)) {

		if (prev_beats) {
			if ((p->quarters() - prev_beats) >= resolution) {
				ret.push_back (*p); /* makes a copy */
			}
		} else {
			ret.push_back (*p);
		}

		prev_beats = p->quarters ();
		++p;
	}
}


void
TempoMap::get_bar_grid (TempoMapPoints& ret, samplepos_t s, samplepos_t e, int32_t bar_gap)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	superclock_t start = S2Sc (s);
	superclock_t end = S2Sc (e);

	for (TempoMapPoints::iterator p = iterator_at (start); (p != _points.end()) && (p->sclock() < end); ++p) {

		if ((p->sclock() >= start) && (p->bbt().beats == 1) && ((p->bbt().bars == 1) || (p->bbt().bars % bar_gap == 0))) {
			ret.push_back (TempoMapPoint (this, TempoMapPoint::Flag (TempoMapPoint::ExplicitMeter|TempoMapPoint::ExplicitTempo), p->tempo(), p->meter(), p->sclock(), p->quarters(), p->bbt()));
		}
	}
}

std::ostream&
std::operator<<(std::ostream& str, Meter const & m)
{
	return str << m.divisions_per_bar() << '/' << m.note_value();
}

std::ostream&
std::operator<<(std::ostream& str, Tempo const & t)
{
	return str << t.note_types_per_minute() << " 1/" << t.note_type() << " notes per minute (" << t.superclocks_per_note_type() << " sc-per-1/" << t.note_type() << ')';
}

std::ostream&
std::operator<<(std::ostream& str, Point const & p)
{
	return str << "P@" << p.sclock() << '/' << p.beats() << '/' << p.bbt();
}

std::ostream&
std::operator<<(std::ostream& str, MeterPoint const & m)
{
	return str << *((Meter const *) &m) << ' ' << *((Point const *) &m);
}

std::ostream&
std::operator<<(std::ostream& str, TempoPoint const & t)
{
	return str << *((Tempo const *) &t) << ' '  << *((Point const *) &t);
}

std::ostream&
std::operator<<(std::ostream& str, TempoMapPoint const & tmp)
{
	str << '@' << std::setw (12) << tmp.sclock() << ' ' << tmp.sclock() / (double) superclock_ticks_per_second
	    << " flags " << hex << tmp.flags() << dec
	    << (tmp.is_explicit_tempo() ? " EXP-T" : " imp-t")
	    << (tmp.is_explicit_meter() ? " EXP-M" : " imp-m")
	    << (tmp.is_explicit_position() ? " EXP-P" : " imp-p")
	    << " qn " << tmp.quarters ()
	    << " bbt " << tmp.bbt()
		;

	if (tmp.is_explicit_tempo()) {
		str << " tempo " << tmp.tempo();
	}

	if (tmp.is_explicit_meter()) {
		str << " meter " << tmp.meter();
	}

	if (tmp.is_explicit_tempo() && tmp.tempo().ramped()) {
		str << " ramp c/sc = " << tmp.metric().c_per_superclock() << " c/qn " << tmp.metric().c_per_quarter();
	}

	return str;
}

samplepos_t
TempoMap::sample_plus_quarters_as_samples (samplepos_t start, Temporal::Beats const & distance) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	TempoMapPoints::const_iterator i = const_iterator_at (start);

	const Temporal::Beats start_qn = i->quarters_at (start);
	const Temporal::Beats end_qn = start_qn + distance;

	cerr << "SPQAS start " << start << " qns = " << start_qn <<  " end via " << distance << " = " << end_qn << endl;

	TempoMapPoints::const_iterator e = const_iterator_at (end_qn);

	cerr << "\t\tend iter @ " << e->sample() << " qn = " << e->quarters() << " diff = " << end_qn - e->quarters() << endl;

	return superclock_to_samples (e->metric().superclock_at_qn (end_qn), _sample_rate);
}

Temporal::Beats
TempoMap::sample_delta_as_quarters (samplepos_t start, samplepos_t distance) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return quarter_note_at (start + distance) - quarter_note_at (start);
}

Temporal::superclock_t
TempoMap::sample_quarters_delta_as_samples (samplepos_t start, Temporal::Beats const & distance) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	const Temporal::Beats start_qn = const_iterator_at (start)->quarters_at (S2Sc (start));
	return sample_at_locked (start_qn + distance);
}

void
TempoMap::update_one_domain_from_another (timepos_t const & src, void* dst, TimeDomain target_domain) const
{
	switch (target_domain) {
	case AudioTime:
		*((samplepos_t*) dst) = sample_at (src);
		break;
	case BeatTime:
		*((Beats*) dst) = quarter_note_at (src);
		break;
	case BarTime:
		*((BBT_Time*) dst) = bbt_at (src);
		break;
	}
}

int
TempoMap::update_music_times (int generation, samplepos_t pos, Temporal::Beats & b, Temporal::BBT_Time & bbt, bool force)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	if (!force && (generation == _generation)) {
		return _generation;
	}

	const superclock_t sc = S2Sc (pos);

	TempoMapPoints::iterator i = iterator_at (sc);

	cerr << "update music times for " << pos << " sc = " << sc << " iterator = " << *i << endl;

	b = i->quarters_at (sc);
	bbt = i->bbt_at (pos);

	cerr << "computed BBT as " << bbt << endl;

	return _generation;
}

int
TempoMap::update_samples_and_bbt_times (int generation, Temporal::Beats const & b, samplepos_t & pos, Temporal::BBT_Time & bbt, bool force)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	if (!force && (generation == _generation)) {
		return _generation;
	}

	TempoMapPoints::iterator i = iterator_at (b);

	pos = sample_at (b);
	bbt = i->bbt_at (b);

	return _generation;
}

int
TempoMap::update_samples_and_beat_times (int generation, Temporal::BBT_Time const & bbt, samplepos_t & pos, Temporal::Beats & b, bool force)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	if (!force && (generation == _generation)) {
		return _generation;
	}

	TempoMapPoints::iterator i = iterator_at (bbt);

	pos = sample_at (bbt);
	b = i->quarters_at (bbt);

	return _generation;
}

samplecnt_t
TempoMap::samples_per_quarter_note_at (samplepos_t samples) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return superclock_to_samples (const_iterator_at (samples_to_superclock (samples, _sample_rate))->metric().superclocks_per_quarter_note (), _sample_rate);
}

BBT_Time
TempoMap::bbt_walk (BBT_Time const & bbt, BBT_Offset const & offset) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	// TempoMapPoint const & start (const_point_at (bbt));

#warning TempoMap::bbt_walk() is not yet implemented

	/* common case: start + finish are both defined by the same TempoMetric */

	/* uncommon case: at least one tempo and/or meter change between start
	 * and finish ... have to walk.
	 */
	return BBT_Time ();
}

Temporal::Beats
TempoMap::quarter_note_at (timepos_t const & pos) const
{
	switch (pos.lock_style()) {
	case BeatTime:
		return pos.beats();
	case AudioTime:
		return quarter_note_at (pos.sample());
	default:
		break;
	}

	return quarter_note_at (pos.bbt());
}

Temporal::Beats
TempoMap::quarter_note_at (Temporal::BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return const_iterator_at (bbt)->quarters_at (bbt);
}

Temporal::Beats
TempoMap::quarter_note_at (samplepos_t pos) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return const_iterator_at (pos)->quarters_at (S2Sc (pos));
}

XMLNode&
TempoMap::get_state ()
{
	XMLNode* node = new XMLNode (X_("TempoMap"));

	Glib::Threads::RWLock::ReaderLock lm (_lock);

	node->set_property (X_("time-domain"), _time_domain);
	node->set_property (X_("superclocks-per-second"), superclock_ticks_per_second);
	node->set_property (X_("extent"), _points.back().sclock());

	XMLNode* children;

	children = new XMLNode (X_("Tempos"));
	node->add_child_nocopy (*children);
	for (Tempos::const_iterator t = _tempos.begin(); t != _tempos.end(); ++t) {
		children->add_child_nocopy (t->get_state());
	}

	children = new XMLNode (X_("Meters"));
	node->add_child_nocopy (*children);
	for (Meters::const_iterator m = _meters.begin(); m != _meters.end(); ++m) {
		children->add_child_nocopy (m->get_state());
	}

	children = new XMLNode (X_("MusicTimes"));
	node->add_child_nocopy (*children);
	for (MusicTimes::const_iterator b = _bartimes.begin(); b != _bartimes.end(); ++b) {
		children->add_child_nocopy (b->get_state());
	}

	return *node;
}

int
TempoMap::set_state (XMLNode const & node, int /*version*/)
{
	TraceableWriterLock lm (_lock);

	/* global map properties */

	/* XXX this should probably be at the global level in the session file because it affects a lot more than just the tempo map, potentially */
	node.get_property (X_("superclocks-per-second"), superclock_ticks_per_second);

	node.get_property (X_("time-domain"), _time_domain);

	XMLNodeList const & children (node.children());

	for (XMLNodeList::const_iterator c = children.begin(); c != children.end(); ++c) {
		if ((*c)->name() == X_("Tempos")) {
			if (set_tempos_from_state (**c)) {
				return -1;
			}
		}

		if ((*c)->name() == X_("Meters")) {
			if (set_meters_from_state (**c)) {
				return -1;
			}
		}

		if ((*c)->name() == X_("MusicTimes")) {
			if (set_music_times_from_state (**c)) {
				return -1;
			}
		}
	}

	/* now fill in the map */

	superclock_t extent;
	node.get_property (X_("extent"), extent);

	rebuild (extent);

	return 0;
}

int
TempoMap::set_music_times_from_state (XMLNode const& tempos_node)
{
	return 0;
}

int
TempoMap::set_tempos_from_state (XMLNode const& tempos_node)
{
	/* CALLER MUST HOLD LOCK */

	XMLNodeList const & children (tempos_node.children());

	try {
		_tempos.clear ();
		for (XMLNodeList::const_iterator c = children.begin(); c != children.end(); ++c) {
			_tempos.push_back (TempoPoint (**c));
		}
		cerr << "Added " << _tempos.size() << " tempos from " << children.size() << endl;
	} catch (...) {
		cerr << "EXCEPTION creating tempos\n";
		_tempos.clear (); /* remove any that were created */
		return -1;
	}

	return 0;
}

int
TempoMap::set_meters_from_state (XMLNode const& meters_node)
{
	/* CALLER MUST HOLD LOCK */

	XMLNodeList const & children (meters_node.children());

	try {
		_meters.clear ();
		for (XMLNodeList::const_iterator c = children.begin(); c != children.end(); ++c) {
			_meters.push_back (MeterPoint (**c));
		}
		cerr << "Added " << _meters.size() << " meters from " << children.size() << endl;
	} catch (...) {
		cerr << "EXCEPTION creating meters\n";
		_meters.clear (); /* remove any that were created */
		return -1;
	}

	return 0;
}

bool
TempoMap::can_remove (Tempo const & t) const
{
	return !is_initial (t);
}

bool
TempoMap::is_initial (Tempo const & t) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	assert (!_points.empty());
	return &t == &_points.front().tempo();
}

bool
TempoMap::is_initial (Meter const & m) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	assert (!_points.empty());
	return &m == &_points.front().meter();
}

bool
TempoMap::can_remove (Meter const & m) const
{
	return !is_initial (m);
}

/** returns the sample duration of the supplied BBT time at a specified sample position in the tempo map.
 * @param pos the frame position in the tempo map.
 * @param bbt the distance in BBT time from pos to calculate.
 * @param dir the rounding direction..
 * @return the duration in frames between pos and bbt
*/
samplecnt_t
TempoMap::bbt_duration_at (samplepos_t pos, const BBT_Time& bbt, int /* dir_ignored */ ) const
{
	return full_duration_at (pos, timecnt_t (bbt, pos), AudioTime).samples();
}

/** Takes a duration (in any time domain) and considers it as a distance from the given position.
 *  Returns a distance in the requested domain, taking tempo changes into account.
 *
 *  Obviously, if the given distance is in the same time domain as the requested domain,
 *  the returned distance is identical to the given one.
 */

timecnt_t
TempoMap::full_duration_at (timepos_t const & pos, timecnt_t const & duration, TimeDomain return_domain) const
{
	timepos_t p (pos);

	if (return_domain == duration.style()) {
		return duration;
	}

	switch (return_domain) {
	case AudioTime:
		switch (duration.style()) {
		case AudioTime:
			/*NOTREACHED*/
			break;
		case BeatTime:
			switch (p.lock_style()) {
			case BeatTime:
				break;
			case BarTime:
				abort ();
			case AudioTime:
				p.update_audio_and_beat_times(); /* XXX optimize by just fetching beats */
				break;
			}
			p += duration;
			return timecnt_t (p.sample() - pos.sample(), pos);
			break;
		case BarTime:
			/* we're not doing this yet, if ever */
			abort ();
			/*NOTREACHED*/
			break;
		}
		break;
	case BeatTime:
		switch (duration.style()) {
		case AudioTime:
			p.update_music_times ();
			p += duration;
			p.update_music_times ();
			return timecnt_t (p.beats () - pos.beats(), pos);
			break;
		case BeatTime:
			/*NOTREACHED*/
			break;
		case BarTime:
			/* we're not doing this yet, if ever */
			abort ();
			/*NOTREACHED*/
			break;
		}
		break;
	case BarTime:
		/* we're not doing this yet, if ever */
		abort ();
		/*NOTREACHED*/
		break;
	}
	/*NOTREACHED*/
	abort ();
	/*NOTREACHED*/
	return timecnt_t (0, timepos_t());

}

Tempo const *
TempoMap::next_tempo (Tempo const & t) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	TempoMapPoints::const_iterator p = _points.begin();

	while (p != _points.end()) {
		if (&t == &p->tempo()) {
			break;
		}
		++p;
	}

	if (p != _points.end()) {
		++p;

		if (p != _points.end()) {
			return &p->tempo();
		}
	}

	return 0;
}

uint32_t
TempoMap::n_meters () const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	uint32_t n = 0;

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit_meter()) {
			++n;
		}
	}

	return n;
}

uint32_t
TempoMap::n_tempos () const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	uint32_t n = 0;

	for (TempoMapPoints::const_iterator p = _points.begin(); p != _points.end(); ++p) {
		if (p->is_explicit_tempo()) {
			++n;
		}
	}

	return n;
}

void
TempoMap::insert_time (timepos_t const & pos, timecnt_t const & duration)
{
	TraceableWriterLock lm (_lock);
	TempoMapPoints::iterator i;

	switch (duration.style()) {
	case AudioTime:
		i = iterator_at (S2Sc (pos.sample()));
		if (i->sample() < pos.sample()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_sclock (S2Sc (i->sample() + duration.samples()));
			++i;
		}
		break;
	case BeatTime:
		i = iterator_at (pos.beats());
		if (i->quarters() < pos.beats()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_quarters (i->quarters() + duration.beats());
			++i;
		}
		break;
	case BarTime:
		i = iterator_at (pos.bbt());
		if (i->bbt() < pos.bbt()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_bbt (bbt_walk (i->bbt(), duration.bbt()));
			++i;
		}
		break;
	}
}

bool
TempoMap::remove_time (timepos_t const & pos, timecnt_t const & duration)
{
	TraceableWriterLock lm (_lock);
	TempoMapPoints::iterator i;

	bool moved = false;

	switch (duration.style()) {
	case AudioTime:
		i = iterator_at (S2Sc (pos.sample()));
		if (i->sample() < pos.sample()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_sclock (S2Sc (i->sample() - duration.samples()));
			++i;
			moved = true;
		}
		break;
	case BeatTime:
		i = iterator_at (pos.beats());
		if (i->quarters() < pos.beats()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_quarters (i->quarters() - duration.beats());
			++i;
			moved = true;
		}
		break;
	case BarTime:
		i = iterator_at (pos.bbt());
		if (i->bbt() < pos.bbt()) {
			++i;
		}

		while (i != _points.end()) {
			i->set_bbt (bbt_walk (i->bbt(), -duration.bbt()));
			++i;
			moved = true;
		}
		break;
	}

	return moved;
}

TempoMapPoint const *
TempoMap::previous_tempo (TempoMapPoint const & point) const
{
	bool seen_current_tempo = false;
	TempoMapPoints::const_iterator i = const_iterator_at (point.sclock());

	while (i != _points.begin()) {
		if (i->is_explicit_tempo ()) {
			if (!seen_current_tempo) {
				seen_current_tempo = true;
			} else {
				return &(*i);
			}
		}
		--i;
	}

	return 0;
}
