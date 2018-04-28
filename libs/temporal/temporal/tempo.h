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

#ifndef __temporal_tempo_h__
#define __temporal_tempo_h__

#include <list>
#include <string>
#include <vector>
#include <cmath>
#include <exception>

#include <glibmm/threads.h>

#include "pbd/enum_convert.h"
#include "pbd/signals.h"
#include "pbd/statefuldestructible.h"

#include "temporal/visibility.h"
#include "temporal/beats.h"
#include "temporal/bbt_time.h"
#include "temporal/superclock.h"
#include "temporal/timeline.h"
#include "temporal/types.h"

/* A tempo map is built from 4 types of entities

   1) points that define tempo
   2) points that define meter
   3) points that define the position of a specific beat
   4) labels attached to points that provide their BBT time

   Each point that defines tempo and meter must also be a point that defines
   the position of a beat - that is, tempo and meter changes always fall on
   beat positions. In addition, meter changes must always be on a point that
   begins a new measure or bar.

   The user can directly manipulate any of these 4 entities.

   Tempo: tempo related properties and position can be changed.
   Meter: meter related properties and position can be changed.
   Beat locations: position can be changed.
   BBT labels: BBT time can be changed.

   All points have a position defined by a superclock_t value.

   Beats increase monotonically throughout the tempo map (BBT may not). Each
   beat value of a point is 1 beat more than the previous point. Beats start at
   zero. If the user moved a point, it will retain the same beat value, but
   will have a new superclock_t position.

   The map has a single time domain at any time, and can only be using either
   AudioTime or BeatTime. BarTime is not legal as a map time domain.

   When the map is using AudioTime as its time domain, .... [ what? ]

   When the map is using BarTime as its time domain, .... [ what? ]

   The visible tempo map consists of a set of TempoMapPoints, one for every
   beat spanning a duration from zero to some extent defined by the map's
   user.

   Rebuilding the map consists of the following steps:

   1) traverse the list of tempos, and recompute any ramps between them
   2) create a set of "explicit" points from the set of tempo, meter and beat positions
   3) fill in between them and until the final requested extent with "implicit" points

   This rebuild can theoretically be optimized by only rebuilding from a
   certain point in time (e.g. the first explicit (or implicit?) point marked "dirty").


   Step 1 requires an intermediate data type: a tempo combined with a
   duration.
*/


namespace Temporal {

class Meter;
class TempoMap;

/* Conceptually, Point is similar to timepos_t. However, whereas timepos_t can
 * use the TempoMap to translate between time domains, Point cannot. Why not?
 * Because Point is foundational in building the tempo map, and we cannot
 * create a circular functional dependency between them. So a Point always has
 * its superclock and beat time defined and no translation between them is possible.
 *
 * In general, most Points in a TempoMap will use the AudioTime
 * domain. Currently (March 2018) this is an alias for superclock_t rather than
 * samples. They are linearly related anyway and at the code (and machine)
 * level, they are the same type.
 */

class LIBTEMPORAL_API Point {
  public:
	Point (superclock_t sc, Beats const & b, BBT_Time const & bbt) : _sclock (sc), _quarters (b), _bbt (bbt) {}
	Point (XMLNode const &);

	virtual ~Point() {}

	superclock_t sclock() const      { return _sclock; }
	virtual void set_sclock (superclock_t sc) { _sclock = sc; }

	Beats const & beats() const { return _quarters; }
	virtual void  set_beats (Beats const & b) { _quarters = b; }

	BBT_Time const & bbt() const { return _bbt; }
	virtual void     set_bbt (BBT_Time const & bbt) { _bbt = bbt; }

	struct sclock_comparator {
		bool operator() (Point const & a, Point const & b) const {
			return a.sclock() < b.sclock();
		}
	};

	struct beat_comparator {
		bool operator() (Point const & a, Point const & b) const {
			return a.beats() < b.beats();
		}
	};

	struct bbt_comparator {
		bool operator() (Point const & a, Point const & b) const {
			return a.bbt() < b.bbt();
		}
	};

  protected:
	superclock_t          _sclock;
	Beats                 _quarters;
	BBT_Time              _bbt;

	void add_state (XMLNode &) const;
};

/** Tempo, the speed at which musical time progresses (BPM).
 */

class LIBTEMPORAL_API Tempo {
  public:
	enum Type {
		Ramped,
		Constant
	};

	static std::string xml_node_name;

	Tempo (XMLNode const &);

	/**
	 * @param npm Note Types per minute
	 * @param note_type Note Type (default `4': quarter note)
	 */
	Tempo (double npm, int note_type = 4)
		: _superclocks_per_note_type (double_npm_to_sc (npm))
		, _end_superclocks_per_note_type (double_npm_to_sc (npm))
		, _note_type (note_type)
		, _active (true)
		, _locked_to_meter (false)
		, _clamped (false)
		, _type (Tempo::Constant) {}

	Tempo (double npm, double enpm, int note_type = 4)
		: _superclocks_per_note_type (double_npm_to_sc (npm))
		, _end_superclocks_per_note_type (double_npm_to_sc (enpm))
		, _note_type (note_type)
		, _active (true)
		, _locked_to_meter (false)
		, _clamped (false)
		, _type (npm != enpm ? Tempo::Ramped : Tempo::Constant) {}

	/* these five methods should only be used to show and collect information to the user (for whom
	 * bpm as a floating point number is the obvious representation)
	 */
	double note_types_per_minute () const { return (superclock_ticks_per_second * 60.0) / _superclocks_per_note_type; }
	double end_note_types_per_minute () const { return (superclock_ticks_per_second * 60.0) / _end_superclocks_per_note_type; }
	double quarter_notes_per_minute() const { return (superclock_ticks_per_second * 60.0 * 4.0) / (_note_type * _superclocks_per_note_type); }
	double samples_per_note_type(samplecnt_t sr) const { return superclock_to_samples (superclocks_per_note_type (), sr); }
	double samples_per_quarter_note(samplecnt_t sr) const { return superclock_to_samples (superclocks_per_quarter_note(), sr); }
	void   set_note_types_per_minute (double npm) { _superclocks_per_note_type = double_npm_to_sc (npm); }

	int note_type () const { return _note_type; }

	superclock_t superclocks_per_note_type () const {
		return _superclocks_per_note_type;
	}
	superclock_t end_superclocks_per_note_type () const {
		return _end_superclocks_per_note_type;
	}
	superclock_t superclocks_per_note_type (int note_type) const {
		return (_superclocks_per_note_type * _note_type) / note_type;
	}
	superclock_t superclocks_per_quarter_note () const {
		return superclocks_per_note_type (4);
	}
	superclock_t superclocks_per_ppqn () const {
		return superclocks_per_quarter_note() / ticks_per_beat;
	}

	bool active () const { return _active; }
	void set_active (bool yn) { _active = yn; }

	bool locked_to_meter ()  const { return _locked_to_meter; }
	void set_locked_to_meter (bool yn) { _locked_to_meter = yn; }

	bool clamped() const { return _clamped; }
	bool set_clamped (bool yn);

	Type type() const { return _type; }

	bool ramped () const { return _type != Constant; }
	bool set_ramped (bool yn);

	XMLNode& get_state () const;

  protected:
	superclock_t _superclocks_per_note_type;
	superclock_t _end_superclocks_per_note_type;
	int8_t       _note_type;
	bool         _active;
	bool         _locked_to_meter; /* XXX name has unclear meaning with nutempo */
	bool         _clamped;
	Type         _type;

	static inline double       sc_to_double_npm (superclock_t sc) { return (superclock_ticks_per_second * 60.0) / sc; }
	static inline superclock_t double_npm_to_sc (double npm) { return llrint ((superclock_ticks_per_second / npm) * 60.0); }
};

/** Meter, or time signature (subdivisions per bar, and which note type is a single subdivision). */
class LIBTEMPORAL_API Meter {
  public:

	static std::string xml_node_name;

	Meter (XMLNode const &);
	Meter (int8_t dpb, int8_t nv) : _note_value (nv), _divisions_per_bar (dpb) {}

	int divisions_per_bar () const { return _divisions_per_bar; }
	int note_value() const { return _note_value; }

	inline bool operator==(const Meter& other) { return _divisions_per_bar == other.divisions_per_bar() && _note_value == other.note_value(); }
	inline bool operator!=(const Meter& other) { return _divisions_per_bar != other.divisions_per_bar() || _note_value != other.note_value(); }

	Meter& operator=(Meter const & other) {
		if (&other != this) {
			_divisions_per_bar = other._divisions_per_bar;
			_note_value = other._note_value;
		}
		return *this;
	}

	BBT_Time   bbt_add (BBT_Time const & bbt, BBT_Offset const & add) const;
	BBT_Time   bbt_subtract (BBT_Time const & bbt, BBT_Offset const & sub) const;
	BBT_Offset bbt_delta (BBT_Time const & bbt, BBT_Time const & sub) const;

	BBT_Time round_to_bar (BBT_Time const &) const;

	Beats to_quarters (BBT_Offset const &) const;

	XMLNode& get_state () const;

  protected:
	/** The type of "note" that a division represents.  For example, 4 is
	    a quarter (crotchet) note, 8 is an eighth (quaver) note, etc.
	*/
	int8_t _note_value;
	/* how many of '_note_value' make up a bar or measure */
	int8_t _divisions_per_bar;
};

/* A MeterPoint is literally just the combination of a Meter with a Point
 */
class LIBTEMPORAL_API MeterPoint : public Meter, public Point
{
  public:
	MeterPoint (Meter const & m, superclock_t sc, Beats const & b, BBT_Time const & bbt) : Meter (m), Point (sc, b, bbt) {}
	MeterPoint (XMLNode const &);

	XMLNode& get_state () const;
};

/* A TempoPoint is a combination of a Tempo with a Point. However, if the temp
 * is ramped, then at some point we will need to compute the ramp coefficients
 * (c-per-quarter and c-per-superclock) and store them so that we can compute
 * time-at-quarter-note on demand.
 */

class LIBTEMPORAL_API TempoPoint : public Tempo, public Point
{
  public:
	TempoPoint (Tempo const & t, superclock_t sc, Beats const & b, BBT_Time const & bbt) : Tempo (t), Point (sc, b, bbt), _c_per_quarter (0), _c_per_superclock (0) {}
	TempoPoint (Tempo const & t, Point const & p) : Tempo (t), Point (p), _c_per_quarter (0), _c_per_superclock (0) {}
	TempoPoint (XMLNode const &);

	/* just change the tempo component, without moving */
	TempoPoint& operator=(Tempo const & t) {
		*((Tempo*)this) = t;
		return *this;
	}

	superclock_t superclock_at_qn (Beats const & qn) const;

	double c_per_superclock () const { return _c_per_superclock; }
	double c_per_quarter () const { return _c_per_quarter; }

	void compute_c_superclock (samplecnt_t sr, superclock_t end_superclocks_per_note_type, superclock_t duration);
	void compute_c_quarters (samplecnt_t sr, superclock_t end_superclocks_per_note_type, Beats const & duration);

	XMLNode& get_state () const;

  private:
	double _c_per_quarter;
	double _c_per_superclock;
};

/** Helper class to perform computations that require both Tempo and Meter
    at a given point in time.
*/
class LIBTEMPORAL_API TempoMetric : public TempoPoint, public Meter {
  public:
	TempoMetric (TempoPoint const & tp, Meter const & m) : TempoPoint (tp), Meter (m) {}
	TempoMetric (Tempo const & t, Meter const & m, Point const & p) : TempoPoint (t, p), Meter (m) {}
	~TempoMetric () {}

	superclock_t superclocks_per_bar (samplecnt_t sr) const {
		return superclocks_per_grid (sr) * _divisions_per_bar;
	}
	superclock_t superclocks_per_grid (samplecnt_t sr) const {
		return (superclock_ticks_per_second * Meter::note_value()) / (note_types_per_minute() / Tempo::note_type());
	}

	superclock_t superclock_per_note_type_at_superclock (superclock_t sc) const {
		return superclocks_per_note_type () * expm1 (c_per_superclock() * sc);
	}

	/* technically this is returning samplecnt_t but that type is not available here */
	superclock_t samples_per_bar (samplecnt_t sr) const {
		return superclock_to_samples (superclocks_per_bar (sr), sr);
	}
};

/* A music time point is a place where BBT time (BarTime) is reset from
 * whatever it would be when just inferring from the usual counting. Its
 * position is given by a Point that might use superclock or Beats, and the
 * Point's BBT time member is overwritten.
 */
class LIBTEMPORAL_API MusicTimePoint : public Point
{
  public:
	MusicTimePoint (BBT_Time const & bbt_time, Point const & p) : Point (p) { _bbt = bbt_time; }
	MusicTimePoint (XMLNode const &);

	XMLNode & get_state () const;
};

/** Tempo Map - mapping of timecode to musical time.
 * convert audio-samples, sample-rate to Bar/Beat/Tick, Meter/Tempo
 */

/* TempoMap concepts

   we have several different ways of talking about time:

   * PULSE : whole notes, just because. These are linearly related to any other
             note type, so if you know a number of pulses (whole notes), you
             know the corresponding number of any other note type (e.g. quarter
             notes).

   * QUARTER NOTES : just what the name says. A lot of MIDI software and
                     concepts assume that a "beat" is a quarter-note.

   * BEAT : a fraction of a PULSE. Defined by the meter in effect, so requires
            meter (time signature) information to convert to/from PULSE or QUARTER NOTES.
            In a 5/8 time, a BEAT is 1/8th note. In a 4/4 time, a beat is quarter note.
            This means that measuring time in BEATS is potentially non-linear (if
            the time signature changes, there will be a different number of BEATS
            corresponding to a given time in any other unit).

   * SUPERCLOCK : a very high resolution clock whose frequency
                  has as factors all common sample rates and all common note
                  type divisors. Related to MINUTES or SAMPLES only when a
                  sample rate is known. Related to PULSE or QUARTER NOTES only
                  when a tempo is known.

   * MINUTES : wallclock time measurement. related to SAMPLES or SUPERCLOCK
               only when a sample rate is known.


   * SAMPLES : audio time measurement. Related to MINUTES or SUPERCLOCK only
               when a sample rate is known

   * BBT : bars|beats|ticks ... linearly related to BEATS but with the added
           semantics of bars ("measures") added, in which beats are broken up
           into groups of bars ("measures"). Requires meter (time signature)
           information to compute to/from a given BEATS value. Contains no
           additional time information compared to BEATS, but does have
           additional semantic information.

  Nick sez: not every note onset is on a tick
  Paul wonders: if it's 8 samples off, does it matter?
  Nick sez: it should not phase with existing audio

 */

class LIBTEMPORAL_API TempoMapPoint : public Point
{
  public:
	enum Flag {
		ExplicitTempo =    0x1,
		ExplicitMeter =    0x2,
		ExplicitPosition = 0x4,
	};

	TempoMapPoint (TempoMap* map, Flag f, TempoPoint& t, MeterPoint& m, superclock_t sc, Beats const & q, BBT_Time const & bbt)
		: Point (sc, q, bbt), _flags (f), _map (map), _tempo (&t), _meter (&m), _floating (false) {}
	~TempoMapPoint () {}

	TempoMap*    map()  const  { return _map; }
	Flag         flags() const { return _flags; }
	TempoPoint & tempo() const { return *_tempo; }
	MeterPoint & meter() const { return *_meter; }

	/* called by a GUI that is manipulating the position of this point */
	void start_float ();
	void end_float ();
	bool floating() const { return _floating; }

	bool is_explicit_tempo ()    const { return _flags & ExplicitTempo; }
	bool is_explicit_meter ()    const { return _flags & ExplicitMeter; }
	bool is_explicit_position () const { return _flags & ExplicitPosition; }
	bool is_explicit() const           { return _flags & (ExplicitMeter|ExplicitTempo|ExplicitPosition); }
	bool is_implicit() const           { return _flags == Flag (0); }

	superclock_t superclocks_per_note_type (int8_t note_type) const {
		return _tempo->superclocks_per_note_type (note_type);
	}

	struct BadTempoMetricLookup : public std::exception {
		virtual const char* what() const throw() { return "cannot obtain non-const Metric from implicit map point"; }
	};

	superclock_t        sclock() const      { return _sclock; }
	timepos_t           time() const;
	samplepos_t         sample() const;
	Beats const &       quarters() const  { return _quarters; }
	BBT_Time  const &   bbt() const       { return _bbt; }

	superclock_t        meter_sclock() const   { return _meter->sclock(); }
	samplepos_t         meter_sample() const;
	Beats const &       meter_quarters() const { return _meter->beats(); }
	BBT_Time const &    meter_bbt() const      { return _meter->bbt(); }

	superclock_t        tempo_sclock() const   { return _tempo->sclock(); }
	samplepos_t         tempo_sample() const;
	Beats const &       tempo_quarters() const { return _tempo->beats(); }
	BBT_Time const &    tempo_bbt() const      { return _tempo->bbt(); }


	/* None of these properties can be set for an Implicit point, because
	 * they are determined by prior Explicit points.
	 */

	void set_sclock (superclock_t  sc) { if (is_explicit()) { Point::set_sclock (sc); } }
	void set_quarters (Beats const & q) { if (is_explicit()) { Point::set_beats (q); } }
	void set_bbt (BBT_Time const & bbt) {  if (is_explicit()) { Point::set_bbt (bbt); } }

	Beats quarters_at (superclock_t sc) const;
	Beats quarters_at (BBT_Time const &) const;

	BBT_Time bbt_at (Beats const &) const;
	BBT_Time bbt_at (superclock_t) const;

	struct SuperClockComparator {
		bool operator() (TempoMapPoint const & a, TempoMapPoint const & b) const { return a.sclock() < b.sclock(); }
	};

	struct QuarterComparator {
		bool operator() (TempoMapPoint const & a, TempoMapPoint const & b) const { return a.quarters() < b.quarters(); }
	};

	struct BBTComparator {
		bool operator() (TempoMapPoint const & a, TempoMapPoint const & b) const { return a.bbt() < b.bbt(); }
	};

	superclock_t  walk_to_superclock (superclock_t start, Beats const & distance) const;
	Beats walk_to_quarters (superclock_t start, superclock_t distance) const;

	TempoPoint & nonconst_tempo() const { return *_tempo; }
	MeterPoint & nonconst_meter() const { return *_meter; }

	TempoMetric metric() const { return TempoMetric (tempo(), meter()); }

  protected:
	friend class TempoMap;
	void map_reset_set_sclock_for_sr_change (superclock_t sc) { _sclock = sc; }

  private:
	Flag         _flags;
	TempoMap*    _map;
	TempoPoint * _tempo;
	MeterPoint * _meter;
	bool         _floating;
};

typedef std::list<TempoMapPoint> TempoMapPoints;

class LIBTEMPORAL_API TempoMap : public PBD::StatefulDestructible
{
   public:
	TempoMap (Tempo const & initial_tempo, Meter const & initial_meter, samplecnt_t sr);
	~TempoMap();

	void set_dirty (bool yn);

	void set_sample_rate (samplecnt_t sr);
	samplecnt_t sample_rate() const { return _sample_rate; }

	void insert_time (timepos_t const & pos, timecnt_t const & duration);
	bool remove_time (timepos_t const & pos, timecnt_t const & duration);

	void change_tempo (TempoPoint&, Tempo const &);

	TempoMapPoint const & set_tempo (Tempo const &, BBT_Time const &);
	TempoMapPoint const & set_tempo (Tempo const &, Beats const &);
	TempoMapPoint const & set_tempo (Tempo const &, timepos_t const &);

	void remove_tempo (TempoPoint const &);

	TempoMapPoint const & set_meter (Meter const &, BBT_Time const &);
	TempoMapPoint const & set_meter (Meter const &, Beats const &);
	TempoMapPoint const & set_meter (Meter const &, timepos_t const &);

	void remove_meter (MeterPoint const &);

	/* these are a convenience method that just wrap some odd semantics */
	bool move_tempo (TempoPoint const & point, timepos_t const & destination, bool push = false);
	bool move_meter (MeterPoint const & point, timepos_t const & destination, bool push = false);

	bool can_remove (Tempo const &) const;
	bool can_remove (Meter const &) const;

	bool is_initial (Tempo const &) const;
	bool is_initial (Meter const &) const;

	uint32_t n_meters() const;
	uint32_t n_tempos() const;

	Tempo const * next_tempo (Tempo const &) const;
	Meter const * next_meter (Meter const &) const;

	Meter const & meter_at (samplepos_t sc) const;
	Meter const & meter_at (Beats const & b) const;
	Meter const & meter_at (BBT_Time const & bbt) const;
	Meter const & meter_at (timepos_t const &) const;

	Tempo const & tempo_at (samplepos_t sc) const;
	Tempo const & tempo_at (Beats const &b) const;
	Tempo const & tempo_at (BBT_Time const & bbt) const;
	Tempo const & tempo_at (timepos_t const & t) const;

	TempoMetric metric_at (samplepos_t sc) const { return  const_point_at (sc).metric(); }
	TempoMetric metric_at (Beats const &b) const { return const_point_at (b).metric(); }
	TempoMetric metric_at (BBT_Time const & bbt) const {return const_point_at (bbt).metric(); }

	TempoMapPoint const * previous_tempo (TempoMapPoint const &) const;

	/* convenience function */
	BBT_Time round_to_bar (BBT_Time const & bbt) const {
		return const_point_at (bbt).metric().round_to_bar (bbt);
	}

	BBT_Time bbt_at (samplepos_t sc) const;
	BBT_Time bbt_at (Beats const &) const;
	BBT_Time bbt_at (timepos_t const &) const;

	Beats quarter_note_at (samplepos_t sc) const;
	Beats quarter_note_at (BBT_Time const &) const;
	Beats quarter_note_at (timepos_t const &) const;

	samplepos_t sample_at (Beats const &) const;
	samplepos_t sample_at (BBT_Time const &) const;
	samplepos_t sample_at (timepos_t const &) const;

	int update_music_times (int gen, samplepos_t, Beats & b, BBT_Time & bbt, bool force);
	int update_samples_and_beat_times (int gen, BBT_Time const & bbt, samplepos_t & pos, Beats & b, bool force);
	int update_samples_and_bbt_times (int gen, Beats const & b, samplepos_t & pos, BBT_Time & bbt, bool force);
	void update_one_domain_from_another (timepos_t const & src, void* dst, TimeDomain) const;

	/* ways to walk along the tempo map, measure distance between points,
	 * etc.
	 */

	Beats sample_delta_as_quarters (samplepos_t start, samplepos_t distance) const;
	Beats samplewalk_to_quarters (samplepos_t pos, samplecnt_t distance) const;
	Beats samplewalk_to_quarters (Beats const & pos, samplecnt_t distance) const;
	samplepos_t sample_plus_quarters_as_samples (samplepos_t start, Beats const & distance) const;
	samplepos_t sample_quarters_delta_as_samples (samplepos_t start, Beats const & distance) const;
	samplepos_t samplepos_plus_bbt (samplepos_t pos, BBT_Time op) const;

	samplecnt_t bbt_duration_at (samplepos_t pos, const BBT_Time& bbt, int dir) const;
	Beats bbtwalk_to_quarters (Beats const & start, BBT_Offset const & distance) const;

	samplecnt_t samples_per_quarter_note_at (samplepos_t) const;

	Temporal::timecnt_t full_duration_at (Temporal::timepos_t const &, Temporal::timecnt_t const & duration, Temporal::TimeDomain domain) const;

	BBT_Time bbt_walk (BBT_Time const &, BBT_Offset const &) const;

	TempoMapPoint const & const_point_at (samplepos_t s) const { return *const_iterator_at (samples_to_superclock (s, _sample_rate)); }
	TempoMapPoint const & const_point_at (Beats const & b) const { return *const_iterator_at (b); }
	TempoMapPoint const & const_point_at (BBT_Time const & bbt) const { return *const_iterator_at (bbt); }

	TempoMapPoint const & const_point_after (samplepos_t sc) const;
	TempoMapPoint const & const_point_after (Beats const & b) const;
	TempoMapPoint const & const_point_after (BBT_Time const & bbt) const;

	TimeDomain time_domain() const { return _time_domain; }
	void set_time_domain (TimeDomain td);

	/* If resolution == Beats() (i.e. zero), then the grid that is
	   returned will contain a mixture of implicit and explicit points,
	   and will only be valid as long as this map remains unchanged
	   (because the implicit points may reference explicit points in the
	   map.

	   If resolution != Beats() (i.e. non-zero), then the in-out @param
	   grid will contain only explicit points that do not reference this
	   map in anyway.
	*/

	void get_grid (TempoMapPoints& points, samplepos_t start, samplepos_t end, Beats const & resolution);
	void get_bar_grid (TempoMapPoints& points, samplepos_t start, samplepos_t end, int32_t bar_gap);

	/* returns all points with ExplicitMeter and/or ExplicitTempo */
	void get_points (TempoMapPoints& points) const;
	void get_tempos (TempoMapPoints& points) const;
	void get_meters (TempoMapPoints& points) const;

	template<class T> void apply_with_points (T& obj, void (T::*method)(TempoMapPoints &)) {
		Glib::Threads::RWLock::ReaderLock lm (_lock);
		(obj.*method)(_points);
	}

	struct EmptyTempoMapException : public std::exception {
		virtual const char* what() const throw() { return "TempoMap is empty"; }
	};

	void dump (std::ostream&);
	void dump_fundamental (std::ostream&) const;
	void extend (superclock_t limit);
	void rebuild (superclock_t limit = 0);
	void full_rebuild ();

	PBD::Signal2<void,samplepos_t,samplepos_t> Changed;

	XMLNode& get_state();
	int set_state (XMLNode const&, int version);

	typedef std::list<TempoPoint> Tempos;
	typedef std::list<MeterPoint> Meters;
	typedef std::list<MusicTimePoint> MusicTimes;

   private:
	Tempos       _tempos;
	Meters       _meters;
	MusicTimes   _bartimes;

	TempoMapPoints                _points;
	samplecnt_t                   _sample_rate;
	mutable Glib::Threads::RWLock _lock;
	bool                          _dirty;
	int                           _generation;
	TimeDomain                     _time_domain;

	/* these return an iterator that refers to the TempoMapPoint at or most immediately preceding the given position.
	 *
	 * Conceptually, these could be const methods, but C++ prevents them returning a non-const iterator in that case.
	 *
	 * Note that they cannot return an invalid iterator (e.g. _points.end()) because:
	 *
	 *    - if the map is empty, an exception is thrown
	 *    - if the given time is before the first map entry, _points.begin() is returned
	 *    - if the given time is after the last map entry, the equivalent of _points.rbegin() is returned
	 *    - if the given time is within the map entries, a valid iterator will be returned
	 *
	 * The caller MUST hold a read or write lock on the map.
	 */

	TempoMapPoints::iterator iterator_at (superclock_t sc);
	TempoMapPoints::iterator iterator_at (Beats const &);
	TempoMapPoints::iterator iterator_at (BBT_Time const &);

	TempoMapPoints::const_iterator const_iterator_at (superclock_t sc) const { return const_cast<TempoMap*>(this)->iterator_at (sc); }
	TempoMapPoints::const_iterator const_iterator_at (Beats const & b) const { return const_cast<TempoMap*>(this)->iterator_at (b); }
	TempoMapPoints::const_iterator const_iterator_at (BBT_Time const & bbt) const { return const_cast<TempoMap*>(this)->iterator_at (bbt); }

	/* Returns the TempoMapPoint at or most immediately preceding the given time. If the given time is
	 * before the first map entry, then the first map entry will be returned, which underlies the semantics
	 * that the first map entry's values propagate backwards in time if not at absolute zero.
	 *
	 * As for iterator_at(), define both const+const and non-const variants, because C++ won't let us return a non-const iterator
	   from a const method (which is a bit silly, but presumably aids compiler reasoning).
	*/

	TempoMapPoint & point_at (superclock_t sc) { return *iterator_at (sc); }
	TempoMapPoint & point_at (Beats const & b) { return *iterator_at (b); }
	TempoMapPoint & point_at (BBT_Time const & bbt) { return *iterator_at (bbt); }

	Meter const & meter_at_locked (superclock_t sc) const { return const_point_at (sc).meter(); }
	Meter const & meter_at_locked (Beats const & b) const { return const_point_at (b).meter(); }
	Meter const & meter_at_locked (BBT_Time const & bbt) const { return const_point_at (bbt).meter(); }
	Tempo const & tempo_at_locked (superclock_t sc) const { return const_point_at (sc).tempo(); }
	Tempo const & tempo_at_locked (Beats const &b) const { return const_point_at (b).tempo(); }
	Tempo const & tempo_at_locked (BBT_Time const & bbt) const { return const_point_at (bbt).tempo(); }

	BBT_Time      bbt_at_locked (superclock_t sc) const;
	BBT_Time      bbt_at_locked (Beats const &) const;
	samplepos_t   sample_at_locked (Beats const &) const;
	samplepos_t   sample_at_locked (BBT_Time const &) const;

	int set_tempos_from_state (XMLNode const &);
	int set_meters_from_state (XMLNode const &);
	int set_music_times_from_state (XMLNode const &);

	TempoMapPoint const & set_tempo (Tempo const &, superclock_t);
	TempoMapPoint const & set_meter (Meter const &, superclock_t);

	void maybe_rebuild();
	void rebuild_locked (superclock_t limit);
	void dump_locked (std::ostream&);

	TempoPoint* add_tempo (TempoPoint const &);
	MeterPoint* add_meter (MeterPoint const &);
	MusicTimePoint* add_music_time_point (MusicTimePoint const &);
};

} /* end of namespace Temporal */

#ifdef COMPILER_MSVC
#pragma warning(disable:4101)
#endif

namespace PBD {
DEFINE_ENUM_CONVERT(Temporal::Tempo::Type);
DEFINE_ENUM_CONVERT(Temporal::TimeDomain);
} /* namespace PBD */


namespace std {
std::ostream& operator<<(std::ostream& str, Temporal::TempoMapPoint const &);
std::ostream& operator<<(std::ostream& str, Temporal::Tempo const &);
std::ostream& operator<<(std::ostream& str, Temporal::Meter const &);
std::ostream& operator<<(std::ostream& str, Temporal::Point const &);
std::ostream& operator<<(std::ostream& str, Temporal::TempoPoint const &);
std::ostream& operator<<(std::ostream& str, Temporal::MeterPoint const &);
}

#endif /* __temporal_tempo_h__ */
