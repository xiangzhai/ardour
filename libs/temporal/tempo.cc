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

Point::Point (TempoMap const & map, XMLNode const & node)
	: _map (&map)
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

samplepos_t
Point::sample() const
{
	return superclock_to_samples (_sclock, _map->sample_rate());
}

timepos_t
Point::time() const
{
	switch (_map->time_domain()) {
	case AudioTime:
		return timepos_t (sample());
	case BeatTime:
		return timepos_t (beats());
	case BarTime:
		/*NOTREACHED*/
		break;
	}
	/*NOTREACHED*/
	abort();
	/*NOTREACHED*/
	return timepos_t();
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

	/* ticks-per-bar-division; PPQN is ticks-per-quarter note */

	const int32_t tpg = ticks_per_grid ();

	if (r.ticks >= tpg) {

		/* ticks per bar */
		const int32_t tpb = tpg * _divisions_per_bar;

		if (r.ticks >= tpb) {
			r.bars += r.ticks / tpb;
			r.ticks %= tpb;
		}

		if (r.ticks >= tpg) {
			r.beats += r.ticks / tpg;
			r.ticks %= tpg;
		}
	}

	if (r.beats > _divisions_per_bar) {

		/* adjust to zero-based math, since that's what C++ operators expect */

		r.beats -= 1;
		r.bars += r.beats / _divisions_per_bar;
		r.beats %= _divisions_per_bar;

		/* adjust back */

		r.beats += 1;
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

	/* ticks-per-bar-division; PPQN is ticks-per-quarter note */

	const int32_t tpg = ticks_per_grid ();

	if (r.ticks < 0) {
		r.beats -= (r.ticks / tpg);
		r.ticks = tpg + (r.ticks % Temporal::Beats::PPQN);
	}

	if (r.beats < 0) {

		r.beats += 1;

		r.bars -= r.beats / _divisions_per_bar;
		r.beats = r.beats % _divisions_per_bar;

		r.beats -= 1;
	}

	if (r.bars <= 0) {
		r.bars -= 1;
	}

	return Temporal::BBT_Time (r.bars, r.beats, r.ticks);
}

Temporal::BBT_Time
Meter::round_to_bar (Temporal::BBT_Time const & bbt) const
{
	Temporal::BBT_Time b = bbt.round_to_beat ();
	if (b.beats > _divisions_per_bar/2) {
		b.bars++;
	}
	b.beats = 1;
	return b;
}

Temporal::BBT_Time
Meter::round_up_to_bar (Temporal::BBT_Time const & bbt) const
{
	if (bbt.ticks == 0 && bbt.beats == 1) {
		return bbt;
	}
	BBT_Time b = bbt.round_up_to_beat ();
	if (b.beats > 1) {
		b.bars += 1;
		b.beats = 1;
	}
	return b;
}

Temporal::BBT_Time
Meter::round_down_to_bar (Temporal::BBT_Time const & bbt) const
{
	if (bbt.ticks == 0 && bbt.beats == 1) {
		return bbt;
	}
	BBT_Time b = bbt.round_down_to_beat ();
	if (b.beats > 1) {
		b.beats = 1;
	}
	return b;
}

Temporal::BBT_Time
Meter::round_up_to_beat (Temporal::BBT_Time const & bbt) const
{
	Temporal::BBT_Time b = bbt.round_up_to_beat ();
	if (b.beats > _divisions_per_bar) {
		b.bars++;
		b.beats = 1;
	}
	return b;
}

Temporal::Beats
Meter::to_quarters (Temporal::BBT_Offset const & offset) const
{
	int64_t ticks = 0;

	ticks += (Beats::PPQN * offset.bars * _divisions_per_bar * 4) / _note_value;
	ticks += (Beats::PPQN * offset.beats * 4) / _note_value;

	/* "parts per bar division" */

	const int tpg = ticks_per_grid ();

	if (offset.ticks > tpg) {
		ticks += Beats::PPQN * offset.ticks / tpg;
		ticks += offset.ticks % tpg;
	} else {
		ticks += offset.ticks;
	}

	return Beats (ticks/Beats::PPQN, ticks%Beats::PPQN);
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
 * hence, compute_c_superclocks() and compute_c_quarters()
 */

XMLNode&
TempoPoint::get_state () const
{
	XMLNode& base (Tempo::get_state());
	Point::add_state (base);
	return base;
}

TempoPoint::TempoPoint (TempoMap const & map, XMLNode const & node)
	: Tempo (node)
	, Point (map, node)
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
TempoPoint::superclock_at (Temporal::Beats const & qn) const
{
	if (!ramped()) {
		/* not ramped, use linear */
		return _sclock + llrint (superclocks_per_quarter_note () * (qn - _quarters).to_double());
	}

	return _sclock + llrint (superclocks_per_quarter_note() * (log1p (_c_per_quarter * (qn - _quarters).to_double()) / _c_per_quarter));
}

Temporal::Beats
TempoPoint::quarters_at (superclock_t sc) const
{
	if (!ramped()) {
		return _quarters + Temporal::Beats::from_double ((sc - _sclock) / (double) (superclocks_per_quarter_note ()));
	}

	return _quarters + Temporal::Beats::from_double (expm1 (_c_per_superclock * (sc - _sclock)) / _c_per_superclock * superclocks_per_quarter_note ());
}

MeterPoint::MeterPoint (TempoMap const & map, XMLNode const & node)
	: Meter (node)
	, Point (map, node)
{
}

/* Given a time in BBT_Time, compute the equivalent Beat Time.
 *
 * Computation assumes that the Meter is in effect at the time specified as
 * BBT_Time (i.e. there is no other MeterPoint between this one and the specified
 * time.
 */
Temporal::Beats
MeterPoint::quarters_at (Temporal::BBT_Time const & bbt) const
{
	Temporal::BBT_Offset offset = bbt_delta (bbt, _bbt);
	return _quarters + to_quarters (offset);
}

/* Given a time in Beats, compute the equivalent BBT Time.
 *
 * Computation assumes that the Meter is in effect at the time specified in
 * Beats (i.e. there is no other MeterPoint between this one and the specified
 * time.
 */

Temporal::BBT_Time
MeterPoint::bbt_at (Temporal::Beats const & qn) const
{
	return bbt_add (_bbt, Temporal::BBT_Offset (0, 0,  (qn - _quarters).to_ticks()));
}

XMLNode&
MeterPoint::get_state () const
{
	XMLNode& base (Meter::get_state());
	Point::add_state (base);
	return base;
}

Temporal::BBT_Time
TempoMetric::bbt_at (superclock_t sc) const
{
	if (_tempo->sclock() > _meter->sclock()) {
		const superclock_t sc_delta = sc - _tempo->sclock();
		const superclock_t sppqn = superclocks_per_ppqn();
		return _meter->bbt_add (_tempo->bbt(), Temporal::BBT_Offset (0, 0, (sc_delta / sppqn) + (sc_delta % sppqn ? 1 : 0)));
	} else {
		const superclock_t sc_delta = sc - _meter->sclock();
		const superclock_t sppqn = superclocks_per_ppqn();
		return _meter->bbt_add (_meter->bbt(), Temporal::BBT_Offset (0, 0, (sc_delta / sppqn) + (sc_delta % sppqn ? 1 : 0)));
	}
}

superclock_t
TempoMetric::superclock_at (BBT_Time const & bbt) const
{
	return _tempo->superclock_at (_meter->quarters_at (bbt));
}

MusicTimePoint::MusicTimePoint (TempoMap const & map, XMLNode const & node)
	: Point (map, node)
{
}

XMLNode&
MusicTimePoint::get_state () const
{
	XMLNode* node = new XMLNode (X_("MusicTime"));
	Point::add_state (*node);
	return *node;
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

/* TEMPOMAP */

struct TraceableWriterLock : public Glib::Threads::RWLock::WriterLock
{
	TraceableWriterLock (Glib::Threads::RWLock & lock) : Glib::Threads::RWLock::WriterLock (lock) { }
	~TraceableWriterLock() { }
};

TempoMap::TempoMap (Tempo const & initial_tempo, Meter const & initial_meter, samplecnt_t sr)
	: _sample_rate (sr)
	, _dirty (false)
	, _generation (0) // XXX needs to be reloaded from saved state??
{
	TraceableWriterLock lm (_lock);

	_tempos.push_back (TempoPoint (*this, initial_tempo, 0, Beats(), BBT_Time()));
	_meters.push_back (MeterPoint (*this, initial_meter, 0, Beats(), BBT_Time()));

	set_dirty (true);
}

TempoMap::~TempoMap()
{
}

#define S2Sc(s) (samples_to_superclock ((s), _sample_rate))
#define Sc2S(s) (superclock_to_samples ((s), _sample_rate))

void
TempoMap::set_dirty (bool yn)
{
	DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("tempo map @ %1 dirty set to %2\n", this, yn));
	_dirty = yn;
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
			t->set_sclock (t->superclock_at (t->beats ()));
		}
		for (Meters::iterator m = _meters.begin(); m != _meters.end(); ++m) {
			m->set_sclock (m->superclock_at (m->beats ()));
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

MeterPoint*
TempoMap::add_meter (MeterPoint const & mp)
{
	/* CALLER MUST HOLD LOCK */

	Meters::iterator m;

	switch (time_domain()) {
	case AudioTime:
		for (m = _meters.begin(); m != _meters.end() && m->sclock() < mp.sclock(); ++m);
		break;
	case BeatTime:
		for (m = _meters.begin(); m != _meters.end() && m->beats() < mp.beats(); ++m);
		break;
	case BarTime:
		for (m = _meters.begin(); m != _meters.end() && m->bbt() < mp.bbt(); ++m);
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

	reset_starting_at (mp.sclock());
	set_dirty (true);

	return ret;
}

void
TempoMap::change_tempo (TempoPoint & p, Tempo const & t)
{
	TraceableWriterLock lm (_lock);

	*((Tempo*)&p) = t;

	set_dirty (true);
}

TempoPoint &
TempoMap::set_tempo (Tempo const & t, superclock_t sc)
{
	TempoPoint * ret;

	{
		TraceableWriterLock lm (_lock);

		DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set tempo @ %1 to %2\n", sc, t));

		Beats beats;
		BBT_Time bbt;

		TempoMetric tm (metric_at_locked (sc, false));

		/* tempo changes must be on beat */

		beats = tm.quarters_at (sc).round_to_beat ();
		bbt = tm.bbt_at (beats);

		/* recompute superclock position of rounded beat */
		sc = tm.superclock_at (beats);

		TempoPoint tp (*this, t, sc, beats, bbt);
		ret = add_tempo (tp);
	}

	Changed ();

	return *ret;
}

TempoPoint &
TempoMap::set_tempo (Tempo const & t, BBT_Time const & bbt)
{
	TempoPoint * ret = 0;

	{
		TraceableWriterLock lm (_lock);

		DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set tempo @ %1 to %2\n", bbt, t));

		/* tempo changes are required to be on-beat */

		BBT_Time on_beat = bbt.round_to_beat();
		superclock_t sc;
		Beats beats;

		TempoMetric metric (metric_at_locked (on_beat, false));

		beats = metric.quarters_at (on_beat);
		sc = metric.superclock_at (on_beat);

		TempoPoint tp (*this, t, sc, beats, on_beat);
		ret = add_tempo (tp);
	}

	Changed ();

	return *ret;
}

TempoPoint &
TempoMap::set_tempo (Tempo const & t, Beats const & beats)
{
	TempoPoint * ret;

	{
		TraceableWriterLock lm (_lock);

		DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set tempo @ %1 to %2\n", beats, t));

		/* tempo changes are required to be on-beat */

		Beats on_beat = beats.round_to_beat();
		superclock_t sc;
		BBT_Time bbt;

		TempoMetric metric (metric_at_locked (on_beat, false));

		bbt = metric.bbt_at (on_beat);
		sc = metric.superclock_at (on_beat);

		TempoPoint tp (*this, t, sc, on_beat, bbt);
		ret = add_tempo (tp);
	}

	Changed ();

	return *ret;
}

TempoPoint &
TempoMap::set_tempo (Tempo const & t, timepos_t const & time)
{
	switch (time.time_domain()) {
	case AudioTime:
		return set_tempo (t, time.sample());
		break;
	case BeatTime:
		return set_tempo (t, time.beats());
		break;
	}

	return set_tempo (t, time.bbt());
}

TempoPoint*
TempoMap::add_tempo (TempoPoint const & tp)
{
	/* CALLER MUST HOLD LOCK */

	Tempos::iterator t;

	switch (time_domain()) {
	case AudioTime:
		for (t = _tempos.begin(); t != _tempos.end() && t->sclock() < tp.sclock(); ++t);
		break;
	case BeatTime:
		for (t = _tempos.begin(); t != _tempos.end() && t->beats() < tp.beats(); ++t);
		break;
	case BarTime:
		for (t = _tempos.begin(); t != _tempos.end() && t->bbt() < tp.bbt(); ++t);
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

	reset_starting_at (tp.sclock());

	set_dirty (true);

	return ret;
}

void
TempoMap::remove_tempo (TempoPoint const & tp)
{
	{
		TraceableWriterLock lm (_lock);
		superclock_t sc (tp.sclock());
		Tempos::iterator t;
		for (t = _tempos.begin(); t != _tempos.end() && t->sclock() < tp.sclock(); ++t);
		if (t->sclock() != tp.sclock()) {
			/* error ... no tempo point at the time of tp */
			return;
		}
		_tempos.erase (t);
		reset_starting_at (sc);
		set_dirty (true);
	}

	Changed ();
}

/** Given a superclock time, compute the Beat time and BBT_Time using only Tempo and
 * Meter points
 */
void
TempoMap::solve (superclock_t sc, Beats & beats, BBT_Time & bbt) const
{
	/* CALLER MUST HOLD LOCK */

	TempoMetric tm (metric_at_locked (sc, false));

	beats = tm.quarters_at (sc);
	bbt = tm.bbt_add (tm.meter().bbt(), Temporal::BBT_Offset (0, 0, (beats - tm.meter().beats()).to_ticks()));
}

/** Given a Beat time, compute the superclock and BBT_Time using only Tempo and
 * Meter points
 */
void
TempoMap::solve (Beats const & beats, superclock_t & sc, BBT_Time & bbt) const
{
	/* CALLER MUST HOLD LOCK */

	TempoMetric tm (metric_at_locked (beats, false));

	sc = tm.superclock_at (beats);
	bbt = tm.bbt_add (tm.meter().bbt(), Temporal::BBT_Offset (0, 0, (beats - tm.meter().beats()).to_ticks()));
}

void
TempoMap::reset_starting_at (superclock_t sc)
{
	/* CALLER MUST HOLD LOCK */

	Tempos::iterator t;
	Meters::iterator m;
	MusicTimes::iterator b;

	assert (!_tempos.empty());
	assert (!_meters.empty());

	TempoPoint* current_tempo = 0;
	MeterPoint* current_meter = 0;

	assert (!_tempos.empty());
	assert (!_meters.empty());

	/* our task:

	   1) set t, m and b to the iterators for the tempo, meter and bartime
	   markers (if any) closest to but after @param sc.

	   2) set current_tempo and current_meter to point to the tempo and
	   meter in effect at @param sc
	*/

	if (sc) {
		for (t = _tempos.begin(); t != _tempos.end() && t->sclock() <= sc; ++t) {
			current_tempo = &*t;
		}
		for (m = _meters.begin(); m != _meters.end() && m->sclock() <= sc; ++m) {
			current_meter = &*m;
		}
		for (b = _bartimes.begin(); t != _tempos.end() && b->sclock() <= sc; ++b);
	} else {
		t = _tempos.begin();
		m = _meters.begin();
		b = _bartimes.begin();

		current_meter = &*m;
		current_tempo = &*t;
	}

	Tempos::iterator nxt_tempo = _tempos.end();

	while ((t != _tempos.end()) || (m != _meters.end()) || (b != _bartimes.end())) {

               /* UPDATE RAMP COEFFICIENTS WHEN NECESSARY */

	       if (t->ramped() && nxt_tempo != _tempos.end()) {
		       /* this is an audio time domain reset, so compute c_per_superclock */
		       t->compute_c_superclock (_sample_rate, nxt_tempo->superclocks_per_quarter_note (), nxt_tempo->sclock() - t->sclock());
	       }

	       /* figure out which of the 1, 2 or 3 possible iterators defines the next explicit point (we want the earliest on the timeline,
                  but there may be more than 1 at the same location).
               */

	       Point* first_of_three = 0;
	       superclock_t limit = INT64_MAX;
	       bool is_bartime = false;

	       if (m != _meters.end() && m->sclock() < limit) {
                       first_of_three = &*m;
                       limit = m->sclock();
               }

	       if (t != _tempos.end() && t->sclock() < limit) {
                       first_of_three = &*t;
                       limit = t->sclock();
               }

               if (b != _bartimes.end() && b->sclock() < limit) {
                       first_of_three = &*b;
                       limit = b->sclock();
                       is_bartime = true;
               }

               assert (first_of_three);

               /* Determine whether a tempo or meter or bartime point (or any combination thereof) is defining this new point */

               bool advance_meter = false;
               bool advance_tempo = false;
               bool advance_bartime = false;

               TempoMetric metric (*current_tempo, *current_meter);

               if (m->sclock() == first_of_three->sclock()) {
                       advance_meter = true;
                       current_meter = &*m;
               }

               if (t->sclock() == first_of_three->sclock()) {
                       advance_tempo = true;
                       current_tempo = &*t;
               }

               if ((b != _bartimes.end()) && (b->sclock() == first_of_three->sclock())) {
                       advance_bartime = true;
               }

               if (time_domain() == AudioTime) {
	               if (!is_bartime) {
		               /* Each tempo and meter marker is locked to its BBT
		                  time, so recompute where that is in audio and beat time.
		               */
		               const superclock_t sc = metric.superclock_at (first_of_three->bbt());
		               const Beats beats = metric.quarters_at (first_of_three->bbt());
		               first_of_three->set (sc, beats, first_of_three->bbt());
	               } else {
		               /* ??? */
	               }
               } else {
	               /* ??? */
               }

               if (advance_meter && (m != _meters.end())) {
                       ++m;
               }
               if (advance_tempo && (t != _tempos.end())) {
                       ++t;
                       if (nxt_tempo != _tempos.end()) {
                               ++nxt_tempo;
                       }
               }
               if (advance_bartime && (b != _bartimes.end())) {
                       ++b;
               }
       }

}

bool
TempoMap::move_meter (MeterPoint const & mp, timepos_t const & when, bool push)
{
	{
		TraceableWriterLock lm (_lock);

		assert (time_domain() != BarTime);
		assert (!_tempos.empty());
		assert (!_meters.empty());

		if (_meters.size() < 2 || mp == _meters.front()) {
			/* not movable */
			return false;
		}

		superclock_t sc;
		Beats beats;
		BBT_Time bbt;
		TimeDomain td (time_domain());
		bool round_up;

		/* if time domains differ, then asking for the "when" value in
		 * the map domain may involve a tempo map lookup. This requires
		 * a lock, so we have to drop the lock while we do this.
		 */

		if (when.time_domain() != time_domain()) {
			lm.release ();
		}

		switch (td) {
		case AudioTime:
			sc = S2Sc (when.sample());
			if (sc > mp.sclock()) {
				round_up = true;
			} else {
				round_up = false;
			}
			break;
		case BeatTime:
			beats = when.beats ();
			if (beats > mp.beats ()) {
				round_up = true;
			} else {
				round_up = false;
			}
			break;
		}

		if (when.time_domain() != time_domain()) {
			lm.acquire ();
		}

		/* Do not allow moving a meter marker to the same position as
		 * an existing one.
		 */

		Tempos::iterator t, prev_t;
		Meters::iterator m, prev_m;

		switch (time_domain()) {
		case AudioTime: {
			for (t = _tempos.begin(), prev_t = _tempos.end(); t != _tempos.end() && t->sclock() < sc; ++t) { prev_t = t; }
			for (m = _meters.begin(), prev_m = _meters.end(); m != _meters.end() && m->sclock() < sc && *m != mp; ++m) { prev_m = m; }
			assert (prev_m != _meters.end());
			if (prev_t == _tempos.end()) { prev_t = _tempos.begin(); }
			TempoMetric metric (*prev_t, *prev_m);
			bbt = metric.bbt_at (sc);
			bbt = metric.meter().round_to_bar (bbt);
			for (m = _meters.begin(), prev_m = _meters.end(); m != _meters.end() && m->bbt() < bbt && *m != mp; ++m) {prev_m = m; }
			for (t = _tempos.begin(), prev_t = _tempos.end(); t != _tempos.end() && t->bbt() < bbt; ++t) { prev_t = t; }
			assert (prev_m != _meters.end());
			if (prev_t == _tempos.end()) { prev_t = _tempos.begin(); }
			metric = TempoMetric (*prev_t, *prev_m);
			sc = metric.superclock_at (bbt);
			for (m = _meters.begin(), prev_m = _meters.end(); m != _meters.end(); ++m) {
				if (&*m != &mp) {
					if (m->sclock() == sc) {
						return false;
					}
				}
			}
			beats = metric.quarters_at (bbt);
			break;
		}

		case BeatTime: {
			/* meter changes must be on bar */
			for (t = _tempos.begin(), prev_t = _tempos.end(); t != _tempos.end() && t->beats() < beats; ++t) { prev_t = t; }
			for (m = _meters.begin(), prev_m = _meters.end(); m != _meters.end() && m->beats() < beats && *m != mp; ++m) { prev_m = m; }
			assert (prev_m != _meters.end());
			if (prev_t == _tempos.end()) { prev_t = _tempos.begin(); }
			TempoMetric metric (*prev_t, *prev_m);
			bbt = metric.bbt_at (beats);
			if (round_up) {
				bbt = metric.meter().round_up_to_bar (bbt);
			} else {
				bbt = metric.meter().round_down_to_bar (bbt);
			}
			for (t = _tempos.begin(), prev_t = _tempos.end(); t != _tempos.end() && t->bbt() < bbt; ++t) { prev_t = t; }
			for (m = _meters.begin(), prev_m = _meters.end(); m != _meters.end() && m->bbt() < bbt && *m != mp; ++m) { prev_m = m; }
			assert (prev_m != _meters.end());
			if (prev_t == _tempos.end()) { prev_t = _tempos.begin(); }
			metric = TempoMetric (*prev_t, *prev_m);
			beats = metric.quarters_at (bbt);
			for (m = _meters.begin(), prev_m = _meters.end(); m != _meters.end(); ++m) {
				if (&*m != &mp) {
					if (m->beats() == beats) {
						return false;
					}
				}
			}
			sc = metric.superclock_at (bbt);
			break;
		}

		default:
			/* NOTREACHED */
			return false;
		}

		if (mp.sclock() == sc && mp.beats() == beats && mp.bbt() == bbt) {
			return false;
		}

		const superclock_t old_sc = mp.sclock();

		Meters::iterator current = _meters.end();
		Meters::iterator insert_before = _meters.end();

		for (Meters::iterator m = _meters.begin(); m != _meters.end(); ++m) {
			if (*m == mp) {
				current = m;
			}
			if (insert_before == _meters.end() && (m->sclock() > sc)) {
				insert_before = m;
			}
		}

		/* existing meter must have been found */
		assert (current != _meters.end());

		/* reset position of this meter */
		current->set (sc, beats, bbt);
		/* reposition in list */
		_meters.splice (insert_before, _meters, current);
		/* recompute 3 domain positions for everything after this */
		reset_starting_at (std::min (sc, old_sc));
	}

	Changed ();

	return true;
}

bool
TempoMap::move_tempo (TempoPoint const & tp, timepos_t const & when, bool push)
{
	{
		TraceableWriterLock lm (_lock);

		assert (time_domain() != BarTime);
		assert (!_tempos.empty());

		if (_tempos.size() < 2 || tp == _tempos.front()) {
			/* not movable */
			return false;
		}

		superclock_t sc;
		Beats beats;
		BBT_Time bbt;
		TimeDomain td (time_domain());

		/* if time domains differ, then asking for the "when" value in
		 * the map domain may involve a tempo map lookup. This requires
		 * a lock, so we have to drop the lock while we do this.
		 */

		if (when.time_domain() != time_domain()) {
			lm.release ();
		}

		switch (td) {
		case AudioTime:
			sc = S2Sc (when.sample());
			break;
		case BeatTime:
			beats = when.beats ();
			break;
		}

		if (when.time_domain() != time_domain()) {
			lm.acquire ();
		}


		Tempos::iterator t, prev_t;
		Meters::iterator m, prev_m;

		switch (time_domain()) {
		case AudioTime: {
			for (t = _tempos.begin(), prev_t = _tempos.end(); t != _tempos.end() && t->sclock() < sc && *t != tp; ++t) { prev_t = t; }
			for (m = _meters.begin(), prev_m = _meters.end(); m != _meters.end() && m->sclock() < sc; ++m) { prev_m = m; }
			assert (prev_t != _tempos.end());
			if (prev_m == _meters.end()) { prev_m = _meters.begin(); }
			TempoMetric metric (*prev_t, *prev_m);
			beats = metric.quarters_at (sc);
			/* tempo changes must be on beat, so round and then
			 * recompute superclock and BBT with rounded result
			 */
			beats = beats.round_to_beat ();
			for (t = _tempos.begin(), prev_t = _tempos.end(); t != _tempos.end() && t->sclock() < sc && *t != tp; ++t) { prev_t = t; }
			for (m = _meters.begin(), prev_m = _meters.end(); m != _meters.end() && m->sclock() < sc; ++m) { prev_m = m; }
			assert (prev_t != _tempos.end());
			if (prev_m == _meters.end()) { prev_m = _meters.begin(); }
			metric = TempoMetric (*prev_t, *prev_m);
			sc = metric.superclock_at (beats);
			bbt = metric.bbt_at (beats);
			break;
		}

		case BeatTime: {
			/* tempo changes must be on beat */
			beats = beats.round_to_beat ();
			for (t = _tempos.begin(), prev_t = _tempos.end(); t != _tempos.end() && t->beats() < beats && *t != tp; ++t) { prev_t = t; }
			for (m = _meters.begin(), prev_m = _meters.end(); m != _meters.end() && m->beats() < beats; ++m) { prev_m = m; }
			assert (prev_t != _tempos.end());
			assert (prev_m != _meters.end());
			TempoMetric metric (*prev_t, *prev_m);
			sc = metric.superclock_at (beats);
			bbt = metric.bbt_at (beats);
			break;
		}

		default:
			/* NOTREACHED */
			return false;
		}

		if (tp.sclock() == sc && tp.beats() == beats && tp.bbt() == bbt) {
			return false;
		}

		if (_tempos.size() == 2) {
			/* must be the 2nd of two, so just move it */
			_tempos.back().set (sc, beats, bbt);
		} else {

			Tempos::iterator t = find (_tempos.begin(), _tempos.end(), tp);
			assert (t != _tempos.end());
			_tempos.erase (t);
			TempoPoint new_tp (*this, tp, sc, beats, bbt);
			add_tempo (new_tp);
		}
	}

	Changed ();

	return true;
}

MeterPoint &
TempoMap::set_meter (Meter const & m, timepos_t const & time)
{
	switch (time.time_domain()) {
	case AudioTime:
		return set_meter (m, S2Sc (time.sample()));
	case BarTime:
		return set_meter (m, time.beats());
	default:
		break;
	}

	return set_meter (m, time.bbt());
}


MeterPoint &
TempoMap::set_meter (Meter const & t, BBT_Time const & bbt)
{
	MeterPoint * ret = 0;

	{
		TraceableWriterLock lm (_lock);

		DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set meter @ %1 to %2\n", bbt, t));

		TempoMetric metric (metric_at_locked (bbt));
		superclock_t sc;
		Beats beats;
		BBT_Time rounded_bbt;

		/* meter changes are required to be on-bar */

		rounded_bbt = metric.round_to_bar (bbt);
		beats = metric.quarters_at (rounded_bbt);
		sc = metric.superclock_at (beats);

		MeterPoint mp (*this, t, sc, beats, rounded_bbt);

		ret = add_meter (mp);

		reset_starting_at (mp.sclock());
	}

	Changed ();

	return *ret;
}

MeterPoint &
TempoMap::set_meter (Meter const & t, Beats const & beats)
{
	MeterPoint * ret = 0;

	{
		TraceableWriterLock lm (_lock);

		DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set meter @ %1 to %2\n", beats, t));

		TempoMetric metric (metric_at_locked (beats));

		/* meter changes are required to be on-bar */

		BBT_Time rounded_bbt = metric.bbt_at (beats);
		rounded_bbt = metric.round_to_bar (rounded_bbt);

		const Beats rounded_beats = metric.quarters_at (rounded_bbt);
		const superclock_t sc = metric.superclock_at (rounded_beats);

		MeterPoint mp (*this, t, sc, rounded_beats, rounded_bbt);

		ret = add_meter (mp);
	}

	Changed ();

	return *ret;
}

MeterPoint &
TempoMap::set_meter (Meter const & m, superclock_t sc)
{
	MeterPoint * ret = 0;

	{
		TraceableWriterLock lm (_lock);

		DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("Set meter @ %1 to %2\n", sc, m));

		Beats beats;
		BBT_Time bbt;

		TempoMetric metric (metric_at_locked (sc));

		/* meter changes must be on bar */

		bbt = metric.bbt_at (beats);
		bbt = metric.round_to_bar (bbt);

		/* compute beat position */
		beats = metric.quarters_at (bbt);

		/* recompute superclock position of bar-rounded position */
		sc = metric.superclock_at (beats);

		MeterPoint mp (*this, m, sc, beats, bbt);
		ret = add_meter (mp);
	}

	Changed ();

	return *ret;
}

void
TempoMap::remove_meter (MeterPoint const & mp)
{
	{
		TraceableWriterLock lm (_lock);
		superclock_t sc = mp.sclock();
		Meters::iterator m = upper_bound (_meters.begin(), _meters.end(), mp, Point::sclock_comparator());
		if (m->sclock() != mp.sclock()) {
			/* error ... no meter point at the time of mp */
			return;
		}
		_meters.erase (m);
		reset_starting_at (sc);
		set_dirty (true);
	}

	Changed ();

}

Temporal::BBT_Time
TempoMap::bbt_at (timepos_t const & pos) const
{
	switch (pos.time_domain()) {
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
	return metric_at_locked (S2Sc (s)).bbt_at (S2Sc (s));
}

Temporal::BBT_Time
TempoMap::bbt_at (Temporal::Beats const & qn) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return metric_at_locked (qn).bbt_at (qn);
}

samplepos_t
TempoMap::sample_at (Temporal::Beats const & qn) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return superclock_to_samples (metric_at_locked (qn).superclock_at (qn), _sample_rate);
}

samplepos_t
TempoMap::sample_at (Temporal::BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return samples_to_superclock (metric_at_locked (bbt).superclock_at (bbt), _sample_rate);
}

samplepos_t
TempoMap::sample_at (timepos_t const & pos) const
{
	switch (pos.time_domain()) {
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

	double divisions_per_bar = metric_at_locked (pos_bbt).divisions_per_bar();
	while (pos_bbt.beats >= divisions_per_bar + 1) {
		++pos_bbt.bars;
		divisions_per_bar = metric_at_locked (pos_bbt).divisions_per_bar();
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
	TempoMetric first (metric_at (pos));
	TempoMetric last (metric_at (pos+distance));
	Temporal::Beats a = first.quarters_at (S2Sc (pos));
	Temporal::Beats b = last.quarters_at (S2Sc (pos+distance));
	return b - a;
}

Temporal::Beats
TempoMap::samplewalk_to_quarters (Temporal::Beats const & pos, samplecnt_t distance) const
{
	/* XXX this converts from beats to samples and back to beats... undesirable */
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	superclock_t s = metric_at_locked (pos).superclock_at (pos);
	s += S2Sc (distance);
	return metric_at_locked (s).quarters_at (s);

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
	const double ratio = new_sr / (double) _sample_rate;

	for (Tempos::iterator t = _tempos.begin(); t != _tempos.end(); ++t) {
		t->map_reset_set_sclock_for_sr_change (llrint (ratio * t->sclock()));
	}

	for (Meters::iterator m = _meters.begin(); m != _meters.end(); ++m) {
		m->map_reset_set_sclock_for_sr_change (llrint (ratio * m->sclock()));
	}

	for (MusicTimes::iterator p = _bartimes.begin(); p != _bartimes.end(); ++p) {
		p->map_reset_set_sclock_for_sr_change (llrint (ratio * p->sclock()));
	}
}

void
TempoMap::dump (std::ostream& ostr) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	dump_locked (ostr);
}

void
TempoMap::dump_locked (std::ostream& ostr) const
{
	for (Tempos::const_iterator t = _tempos.begin(); t != _tempos.end(); ++t) {
		ostr << &*t << ' ' << *t << endl;
	}

	for (Meters::const_iterator m = _meters.begin(); m != _meters.end(); ++m) {
		ostr << &*m << ' ' << *m << endl;
	}
}

void
TempoMap::get_grid (TempoMapPoints& ret, samplepos_t s, samplepos_t e, uint32_t bar_mod)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	DEBUG_TRACE (DEBUG::TemporalMap, string_compose (">>> GRID START %1 .. %2\n", S2Sc (s), S2Sc (e)));

	const superclock_t end = S2Sc (e);
	superclock_t pos = S2Sc (s);
	TempoMetric metric = metric_at_locked (pos, false);
	TempoMetric emetric = metric_at_locked (end, false);
	BBT_Time bbt = metric.bbt_at (pos);
	BBT_Time ebbt = metric_at_locked (end).bbt_at (end);

	DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("get grid between %1..%2 [ %4 .. %5 ] { %6 .. %7 } at bar_mod = %3\n",
	                                                 pos, end, bar_mod, s, e, bbt, ebbt));

	if ((metric.quarters_at (bbt) - metric.quarters_at (pos)).abs() > Beats::ticks (1)) {
		cerr << "MM1: " << pos << " vs. " << metric.superclock_at (bbt) << " delta " << pos - metric.superclock_at (bbt) << endl;
		abort ();
	}

	if ((emetric.quarters_at (ebbt) - emetric.quarters_at (end)).abs() > Beats::ticks (1)) {
		cerr << "MM2: " << end << " vs. " << metric_at_locked (end).superclock_at (ebbt) << " delta " << end - metric_at_locked(end).superclock_at (ebbt) << endl;
		abort ();
	}

#ifndef NDEBUG
	if (DEBUG_ENABLED(PBD::DEBUG::TemporalMap)) {
		dump (cerr);
	}
#endif

	if (bar_mod == 0) {
		/* this could change the tempo in effect */
		bbt = metric.meter().round_up_to_beat (bbt);
		metric = metric_at_locked (bbt, false);
		pos = metric.superclock_at (bbt);

		if (pos < S2Sc (s)) {
			abort ();
		}

	} else {

		/* this rounding cannot change the meter in effect, because it
		   remains within the bar. But it could change the tempo (which
		   are only quantized to grid positions within a bar).
		*/

		BBT_Time bar = bbt.round_down_to_bar ();
		if (bar_mod != 1) {
			bar.bars -= bar.bars % bar_mod;
			++bar.bars;
		}
		bbt = bar;
		metric = metric_at_locked (bbt, false);
		pos = metric.superclock_at (bbt);
	}

	if (pos < end) {

		do {

			/* complete the triplet of (superclock,quarters,bbt) */

			const Temporal::Beats beats = metric.quarters_at (pos);

			/* add point to grid */

			ret.push_back (TempoMapPoint (*this, metric, pos, beats, bbt));
			DEBUG_TRACE (DEBUG::TemporalMap, string_compose ("G %1\t       %2\n", metric, ret.back()));

			/* Advance by the meter note value size */

			if (bar_mod == 0) {
				pos += metric.superclocks_per_grid (_sample_rate);
			} else {
				pos += bar_mod * metric.superclocks_per_bar (_sample_rate);
			}

			if (pos >= end) {
				break;
			}

			/* metric may be different for new position */
			metric = metric_at_locked (pos);

			/* compute bbt. it appears that one could just add some number
			 * of beats and/or bars to the current value, but this may
			 * cross metric boundaries, at which point it gets
			 * ... complicated.
			 */

			bbt = metric.bbt_at (pos);

		} while (true);
	}

	DEBUG_TRACE (DEBUG::TemporalMap, "<<< GRID DONE\n");
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
std::operator<<(std::ostream& str, TempoMetric const & tm)
{
	return str << tm.tempo() << ' '  << tm.meter();
}

std::ostream&
std::operator<<(std::ostream& str, TempoMapPoint const & tmp)
{
	str << '@' << std::setw (12) << tmp.sclock() << ' ' << tmp.sclock() / (double) superclock_ticks_per_second
	    << (tmp.is_explicit_tempo() ? " EXP-T" : " imp-t")
	    << (tmp.is_explicit_meter() ? " EXP-M" : " imp-m")
	    << (tmp.is_explicit_position() ? " EXP-P" : " imp-p")
	    << " qn " << tmp.beats ()
	    << " bbt " << tmp.bbt()
		;

	if (tmp.is_explicit_tempo()) {
		str << " tempo " << tmp.tempo();
	}

	if (tmp.is_explicit_meter()) {
		str << " meter " << tmp.meter();
	}

	if (tmp.is_explicit_tempo() && tmp.tempo().ramped()) {
		str << " ramp c/sc = " << tmp.tempo().c_per_superclock() << " c/qn " << tmp.tempo().c_per_quarter();
	}

	return str;
}

samplepos_t
TempoMap::sample_plus_quarters_as_samples (samplepos_t start, Temporal::Beats const & distance) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	TempoMetric metric (metric_at_locked (S2Sc (start)));

	const Temporal::Beats start_qn = metric.quarters_at (start);
	const Temporal::Beats end_qn = start_qn + distance;

	TempoMetric end_metric (metric_at (end_qn));

	return superclock_to_samples (end_metric.superclock_at (end_qn), _sample_rate);
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

	Temporal::Beats start_qn = metric_at_locked (start).quarters_at (S2Sc (start));
	start_qn += distance;
	return superclock_to_samples (metric_at_locked (start_qn).superclock_at (start_qn), _sample_rate);
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

	if (force || (generation != _generation)) {

		const superclock_t sc = S2Sc (pos);
		TempoMetric tm (metric_at_locked (sc));

		b = tm.quarters_at (sc);
		bbt = tm.bbt_at (sc);
	}

	return _generation;
}

int
TempoMap::update_samples_and_bbt_times (int generation, Temporal::Beats const & b, samplepos_t & pos, Temporal::BBT_Time & bbt, bool force)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	if (force || (generation != _generation)) {

		TempoMetric metric (metric_at_locked (b));

		pos = samples_to_superclock (metric.superclock_at (b), _sample_rate);
		bbt = metric.bbt_at (b);
	}

	return _generation;
}

int
TempoMap::update_samples_and_beat_times (int generation, Temporal::BBT_Time const & bbt, samplepos_t & pos, Temporal::Beats & b, bool force)
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);

	if (force || (generation != _generation)) {
		TempoMetric metric = metric_at_locked (bbt);

		pos = superclock_to_samples (metric.superclock_at (bbt), _sample_rate);
		b = metric.quarters_at (bbt);
	}

	return _generation;
}

samplecnt_t
TempoMap::samples_per_quarter_note_at (samplepos_t samples) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return superclock_to_samples (metric_at_locked (samples_to_superclock (samples, _sample_rate)).superclocks_per_quarter_note (), _sample_rate);
}

BBT_Time
TempoMap::bbt_walk (BBT_Time const & bbt, BBT_Offset const & o) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	BBT_Offset offset (o);
	Tempos::const_iterator t, prev_t, next_t;
	Meters::const_iterator m, prev_m, next_m;

	assert (!_tempos.empty());
	assert (!_meters.empty());

	/* trivial (and common) case: single tempo, single meter */

	if (_tempos.size() == 1 && _meters.size() == 1) {
		return _meters.front().bbt_add (bbt, o);
	}

	/* Find tempo,meter pair for bbt, and also for the next tempo and meter
	 * after each (if any)
	 */

	/* Yes, linear search because the typical size of _tempos and _meters
	 * is 1, and extreme sizes are on the order of 10
	 */

	next_t = _tempos.end();
	next_m = _meters.end();

	for (t = _tempos.begin(), prev_t = t; t != _tempos.end() && t->bbt() < bbt;) {
		prev_t = t;
		++t;

		if (t != _tempos.end()) {
			next_t = t;
			++next_t;
		}
	}

	for (m = _meters.begin(), prev_m = m; m != _meters.end() && m->bbt() < bbt;) {
		prev_m = m;
		++m;

		if (m != _meters.end()) {
			next_m = m;
			++next_m;
		}
	}

	/* may have found tempo and/or meter precisely at the tiem given */

	if (t != _tempos.end() && t->bbt() == bbt) {
		prev_t = t;
	}

	if (m != _meters.end() && m->bbt() == bbt) {
		prev_m = m;
	}

	/* see ::metric_at_locked() for comments about the use of const_cast here
	 */

	TempoMetric metric (*const_cast<TempoPoint*>(&*prev_t), *const_cast<MeterPoint*>(&*prev_m));
	superclock_t pos = metric.superclock_at (bbt);

	/* normalize possibly too-large ticks count */

	const int32_t tpg = metric.meter().ticks_per_grid ();

	if (offset.ticks > tpg) {
		/* normalize */
		offset.beats += offset.ticks / tpg;
		offset.ticks %= tpg;
	}

	/* add tick count, now guaranteed to be less than 1 grid unit */

	if (offset.ticks) {
		pos += metric.superclocks_per_ppqn () * offset.ticks;
	}

	/* add each beat, 1 by 1, rechecking to see if there's a new
	 * TempoMetric in effect after each addition
	 */

#define TEMPO_CHECK_FOR_NEW_METRIC                                      \
	if (((next_t != _tempos.end()) && (pos >= next_t->sclock())) || \
	    ((next_m != _meters.end()) && (pos >= next_m->sclock()))) { \
		/* need new metric */ \
		if (pos >= next_t->sclock()) { \
			if (pos >= next_m->sclock()) { \
				metric = TempoMetric (*const_cast<TempoPoint*>(&*next_t), *const_cast<MeterPoint*>(&*next_m)); \
				++next_t; \
				++next_m; \
			} else { \
				metric = TempoMetric (*const_cast<TempoPoint*>(&*next_t), metric.meter()); \
				++next_t; \
			} \
		} else if (pos >= next_m->sclock()) { \
			metric = TempoMetric (metric.tempo(), *const_cast<MeterPoint*>(&*next_m)); \
			++next_m; \
		} \
	}

	for (int32_t b = 0; b < offset.beats; ++b) {

		TEMPO_CHECK_FOR_NEW_METRIC;
		pos += metric.superclocks_per_grid (_sample_rate);
	}

	/* add each bar, 1 by 1, rechecking to see if there's a new
	 * TempoMetric in effect after each addition
	 */

	for (int32_t b = 0; b < offset.bars; ++b) {

		TEMPO_CHECK_FOR_NEW_METRIC;

		pos += metric.superclocks_per_bar (_sample_rate);
	}

	return metric.bbt_at (pos);
}

Temporal::Beats
TempoMap::quarter_note_at (timepos_t const & pos) const
{
	switch (pos.time_domain()) {
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
	return metric_at_locked (bbt).quarters_at (bbt);
}

Temporal::Beats
TempoMap::quarter_note_at (samplepos_t pos) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return metric_at_locked (pos).quarters_at (S2Sc (pos));
}

XMLNode&
TempoMap::get_state ()
{
	XMLNode* node = new XMLNode (X_("TempoMap"));

	Glib::Threads::RWLock::ReaderLock lm (_lock);

	node->set_property (X_("time-domain"), _time_domain);
	node->set_property (X_("superclocks-per-second"), superclock_ticks_per_second);

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
	}

	Changed ();

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
			_tempos.push_back (TempoPoint (*this, **c));
		}
	} catch (...) {
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
			_meters.push_back (MeterPoint (*this, **c));
		}
	} catch (...) {
		_meters.clear (); /* remove any that were created */
		return -1;
	}

	return 0;
}

bool
TempoMap::can_remove (TempoPoint const & t) const
{
	return !is_initial (t);
}

bool
TempoMap::is_initial (TempoPoint const & t) const
{
	return t.sclock() == 0;
}

bool
TempoMap::is_initial (MeterPoint const & m) const
{
	return m.sclock() == 0;
}

bool
TempoMap::can_remove (MeterPoint const & m) const
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
			switch (p.time_domain()) {
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

	Tempos::const_iterator p = _tempos.begin();

	while (p != _tempos.end()) {
		if (&t == &*p) {
			break;
		}
		++p;
	}

	if (p != _tempos.end()) {
		++p;

		if (p != _tempos.end()) {
			return &*p;;
		}
	}

	return 0;
}

uint32_t
TempoMap::n_meters () const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return _meters.size();
}

uint32_t
TempoMap::n_tempos () const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return _tempos.size();
}

void
TempoMap::insert_time (timepos_t const & pos, timecnt_t const & duration)
{
	assert (time_domain() != BarTime);
	assert (!_tempos.empty());
	assert (!_meters.empty());

	if (pos == std::numeric_limits<timepos_t>::min()) {
		/* can't insert time at the front of the map: those entries are fixed */
		return;
	}

	{
		TraceableWriterLock lm (_lock);

		Tempos::iterator     t (_tempos.begin());
		Meters::iterator     m (_meters.begin());
		MusicTimes::iterator b (_bartimes.begin());

		TempoPoint current_tempo = *t;
		MeterPoint current_meter = *m;
		MusicTimePoint current_time_point (*this);

		if (_bartimes.size() > 0) {
			current_time_point = *b;
		}

		superclock_t sc;
		Beats beats;
		BBT_Time bbt;

		/* set these to true so that we set current_* on our first pass
		 * through the while loop(s)
		 */

		bool moved_tempo = true;
		bool moved_meter = true;
		bool moved_bartime = true;

		switch (duration.style()) {
		case AudioTime:
			sc = S2Sc (pos.sample());

			/* handle a common case quickly */

			if ((_tempos.size() < 2 || sc > _tempos.back().sclock()) &&
			    (_meters.size() < 2 || sc > _meters.back().sclock()) &&
			    (_bartimes.size() < 2 || (_bartimes.empty() || sc > _bartimes.back().sclock()))) {

				/* only one tempo, plus one meter and zero or
				   one bartimes, or insertion point is after last
				   item. nothing to do here.
				*/

				return;
			}

			/* advance fundamental iterators to correct position */

			while (t != _tempos.end()   && t->sclock() < sc) ++t;
			while (m != _meters.end()   && m->sclock() < sc) ++m;
			while (b != _bartimes.end() && b->sclock() < sc) ++b;

			while (t != _tempos.end() && m != _meters.end() && b != _bartimes.end()) {

				if (moved_tempo && t != _tempos.end()) {
					current_tempo = *t;
					moved_tempo = false;
				}
				if (moved_meter && m != _meters.end()) {
					current_meter = *m;
					moved_meter = false;
				}
				if (moved_bartime && b != _bartimes.end()) {
					current_time_point = *b;
					moved_bartime = false;
				}

				/* for each of t, m and b:

				   if the point is earlier than the other two,
				   recompute the superclock, beat and bbt
				   positions, and reset the point.
				*/

				if (t->sclock() < m->sclock() && t->sclock() < b->sclock()) {

					sc = t->sclock() + S2Sc (duration.samples());
					beats = current_tempo.quarters_at (sc);
					/* round tempo to beats */
					beats = beats.round_to_beat ();
					sc = current_tempo.superclock_at (beats);
					bbt = current_meter.bbt_at (beats);

					t->set (sc, beats, bbt);
					++t;
					moved_tempo = true;
				}

				if (m->sclock() < t->sclock() && m->sclock() < b->sclock()) {

					sc = m->sclock() + S2Sc (duration.samples());
					beats = current_tempo.quarters_at (sc);
					/* round meter to bars */
					bbt = current_meter.bbt_at (beats);
					beats = current_meter.quarters_at (current_meter.round_to_bar(bbt));
					/* recompute */
					sc = current_tempo.superclock_at (beats);

					m->set (sc, beats, bbt);
					++m;
					moved_meter = true;
				}

				if (b->sclock() < t->sclock() && b->sclock() < m->sclock()) {

					sc = b->sclock() + S2Sc (duration.samples());
					beats = current_tempo.quarters_at (sc);
					/* round bartime to beats */
					beats = beats.round_to_beat();
					sc = current_tempo.superclock_at (beats);
					bbt = current_meter.bbt_at (beats);

					m->set (sc, beats, bbt);
					++m;
					moved_meter = true;
				}

			}
			break;

		case BeatTime:
			break;
		}

		set_dirty (true);
	}

	Changed ();
}

bool
TempoMap::remove_time (timepos_t const & pos, timecnt_t const & duration)
{
	bool moved = false;

	if (moved) {
		Changed ();
	}

	return moved;
}

TempoPoint const *
TempoMap::previous_tempo (TempoPoint const & point) const
{
	Tempos::const_iterator t = _tempos.begin();
	Tempos::const_iterator prev = _tempos.end();

	while (t != _tempos.end()) {
		if (t->sclock() == point.sclock()) {
			if (prev != _tempos.end()) {
				return &*prev;
			}
		}
		prev = t;
		++t;
	}

	return 0;
}

TempoMetric
TempoMap::metric_at (timepos_t const & pos) const
{
	switch (pos.time_domain()) {
	case AudioTime:
		return metric_at (pos.sample());
	case BeatTime:
		return metric_at (pos.beats());
	default:
		break;
	}

	return metric_at (pos.bbt());
}

TempoMetric
TempoMap::metric_at (samplepos_t s) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return metric_at_locked (S2Sc (s));
}

TempoMetric
TempoMap::metric_at (Beats const & b) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return metric_at_locked (b);
}

TempoMetric
TempoMap::metric_at (BBT_Time const & bbt) const
{
	Glib::Threads::RWLock::ReaderLock lm (_lock);
	return metric_at_locked (bbt);
}

TempoMetric
TempoMap::metric_at_locked (superclock_t sc, bool can_match) const
{
	Tempos::const_iterator t, prev_t;
	Meters::const_iterator m, prev_m;

	assert (!_tempos.empty());
	assert (!_meters.empty());

	/* Yes, linear search because the typical size of _tempos and _meters
	 * is 1, and extreme sizes are on the order of 10
	 */

	for (t = _tempos.begin(), prev_t = t; t != _tempos.end() && t->sclock() < sc; ++t) { prev_t = t; }
	for (m = _meters.begin(), prev_m = m; m != _meters.end() && m->sclock() < sc; ++m) { prev_m = m; }

	if (can_match || sc == 0) {
		/* may have found tempo and/or meter precisely at @param sc */

		if (t != _tempos.end() && t->sclock() == sc) {
			prev_t = t;
		}

		if (m != _meters.end() && m->sclock() == sc) {
			prev_m = m;
		}
	}

	/* I hate doing this const_cast<>, but making this method non-const
	 * propagates into everything that just calls metric_at(), and that's a
	 * bit ridiculous. Yes, the TempoMetric returned here can be used to
	 * change the map, and that's bad, but the non-const propagation is
	 * worse.
	 */

	return TempoMetric (*const_cast<TempoPoint*>(&*prev_t), *const_cast<MeterPoint*> (&*prev_m));
}

TempoMetric
TempoMap::metric_at_locked (Beats const & b, bool can_match) const
{
	Tempos::const_iterator t, prev_t;
	Meters::const_iterator m, prev_m;

	assert (!_tempos.empty());
	assert (!_meters.empty());

	/* Yes, linear search because the typical size of _tempos and _meters
	 * is 1, and extreme sizes are on the order of 10
	 */

	for (t = _tempos.begin(), prev_t = t; t != _tempos.end() && t->beats() < b; ++t) { prev_t = t; }
	for (m = _meters.begin(), prev_m = m; m != _meters.end() && m->beats() < b; ++m) { prev_m = m; }

	if (can_match || b == Beats()) {
		/* may have found tempo and/or meter precisely at @param b */
		if (t != _tempos.end() && t->beats() == b) {
			prev_t = t;
		}

		if (m != _meters.end() && m->beats() == b) {
			prev_m = m;
		}
	}

	/* I hate doing this const_cast<>, but making this method non-const
	 * propagates into everything that just calls metric_at(), and that's a
	 * bit ridiculous. Yes, the TempoMetric returned here can be used to
	 * change the map, and that's bad, but the non-const propagation is
	 * worse.
	 */

	return TempoMetric (*const_cast<TempoPoint*>(&*prev_t), *const_cast<MeterPoint*> (&*prev_m));
}

TempoMetric
TempoMap::metric_at_locked (BBT_Time const & bbt, bool can_match) const
{
	Tempos::const_iterator t, prev_t;
	Meters::const_iterator m, prev_m;

	assert (!_tempos.empty());
	assert (!_meters.empty());

	/* Yes, linear search because the typical size of _tempos and _meters
	 * is 1, and extreme sizes are on the order of 10
	 */

	for (t = _tempos.begin(), prev_t = t; t != _tempos.end() && t->bbt() < bbt; ++t) { prev_t = t; }
	for (m = _meters.begin(), prev_m = m; m != _meters.end() && m->bbt() < bbt; ++m) { prev_m = m; }

	if (can_match || bbt == BBT_Time()) {
		/* may have found tempo and/or meter precisely at @param bbt */

		if (t != _tempos.end() && t->bbt() == bbt) {
			prev_t = t;
		}

		if (m != _meters.end() && m->bbt() == bbt) {
			prev_m = m;
		}
	}

	/* I hate doing this const_cast<>, but making this method non-const
	 * propagates into everything that just calls metric_at(), and that's a
	 * bit ridiculous. Yes, the TempoMetric returned here can be used to
	 * change the map, and that's bad, but the non-const propagation is
	 * worse.
	 */

	return TempoMetric (*const_cast<TempoPoint*>(&*prev_t), *const_cast<MeterPoint*> (&*prev_m));
}
