/*
    Copyright (C) 2002 Paul Davis

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

#ifdef WAF_BUILD
#include "gtk2ardour-config.h"
#endif

#include <cstdio> // for sprintf, grrr
#include <cstdlib>
#include <cmath>
#include <string>
#include <climits>

#include "pbd/error.h"
#include "pbd/memento_command.h"

#include "temporal/tempo.h"

#include <gtkmm2ext/utils.h>
#include <gtkmm2ext/gtk_ui.h>

#include "ardour/session.h"
#include "ardour/tempo.h"
#include <gtkmm2ext/doi.h>
#include <gtkmm2ext/utils.h>

#include "canvas/canvas.h"
#include "canvas/item.h"
#include "canvas/line_set.h"

#include "editor.h"
#include "marker.h"
#include "tempo_dialog.h"
#include "rgb_macros.h"
#include "gui_thread.h"
#include "time_axis_view.h"
#include "tempo_lines.h"
#include "ui_config.h"

#include "pbd/i18n.h"

using namespace std;
using namespace ARDOUR;
using namespace PBD;
using namespace Gtk;
using namespace Gtkmm2ext;
using namespace Editing;
using namespace Temporal;

void
Editor::remove_metric_marks ()
{
	/* don't delete these while handling events, just punt till the GUI is idle */

	for (Marks::iterator x = metric_marks.begin(); x != metric_marks.end(); ++x) {
		delete_when_idle (*x);
	}
	metric_marks.clear ();

	for (Curves::iterator x = tempo_curves.begin(); x != tempo_curves.end(); ++x) {
		delete (*x);
	}
	tempo_curves.clear ();
}
struct CurveComparator {
	bool operator() (TempoCurve const * a, TempoCurve const * b) {
		return a->position() < b->position();
	}
};
void
Editor::draw_metric_marks (Temporal::TempoMapPoints & points)
{
	char buf[64];
	TempoMapPoints::const_iterator prev_ts = points.end();
	double max_tempo = 0.0;
	double min_tempo = DBL_MAX;

	remove_metric_marks (); // also clears tempo curves

	for (TempoMapPoints::iterator i = points.begin(); i != points.end(); ++i) {

		if (!i->is_explicit()) {
			continue;
		}

		Temporal::TempoMetric const & metric (i->metric());

		if (i->is_explicit_meter()) {

			snprintf (buf, sizeof(buf), "%d/%d", metric.divisions_per_bar(), metric.note_value ());

			cerr << "NEW METER MARKER using " << buf << endl;

			if (i->map()->time_domain() != Temporal::AudioTime) {
				metric_marks.push_back (new MeterMarker (*this, *meter_group, UIConfiguration::instance().color ("meter marker music"), buf, i->meter()));
			} else {
				metric_marks.push_back (new MeterMarker (*this, *meter_group, UIConfiguration::instance().color ("meter marker"), buf, i->meter()));
			}
		}

		if (i->is_explicit_tempo()) {

			max_tempo = max (max_tempo, metric.tempo.note_types_per_minute());
			max_tempo = max (max_tempo, metric.tempo.end_note_types_per_minute());
			min_tempo = min (min_tempo, metric.tempo.note_types_per_minute());
			min_tempo = min (min_tempo, metric.tempo.end_note_types_per_minute());
			uint32_t const tc_color = UIConfiguration::instance().color ("tempo curve");

			tempo_curves.push_back (new TempoCurve (*this, *tempo_group, tc_color, *i, i->sample(), false));

			const std::string tname (X_(""));
			if (i->map()->time_domain() != Temporal::AudioTime) {
				metric_marks.push_back (new TempoMarker (*this, *tempo_group, UIConfiguration::instance().color ("tempo marker music"), tname, i->tempo()));
			} else {
				metric_marks.push_back (new TempoMarker (*this, *tempo_group, UIConfiguration::instance().color ("tempo marker"), tname, i->tempo()));
			}

			if (prev_ts != points.end() && abs (prev_ts->metric().tempo.end_note_types_per_minute() - i->metric().tempo.note_types_per_minute()) < 1.0) {
				metric_marks.back()->set_points_color (UIConfiguration::instance().color ("tempo marker music"));
			} else {
				metric_marks.back()->set_points_color (UIConfiguration::instance().color ("tempo marker"));
			}

			prev_ts = i;
		}

	}
	tempo_curves.sort (CurveComparator());

	const double min_tempo_range = 5.0;
	const double tempo_delta = fabs (max_tempo - min_tempo);

	if (tempo_delta < min_tempo_range) {
		max_tempo += min_tempo_range - tempo_delta;
		min_tempo += tempo_delta - min_tempo_range;
	}

	for (Curves::iterator x = tempo_curves.begin(); x != tempo_curves.end(); ) {
		Curves::iterator tmp = x;
		(*x)->set_max_tempo (max_tempo);
		(*x)->set_min_tempo (min_tempo);
		++tmp;
		if (tmp != tempo_curves.end()) {
			(*x)->set_position ((*x)->point().sample(), (*tmp)->point().sample());
		} else {
			(*x)->set_position ((*x)->point().sample(), UINT32_MAX);
		}

		if (!(*x)->point().metric().tempo.active()) {
			(*x)->hide();
		} else {
			(*x)->show();
		}

		++x;
	}

	for (Marks::iterator x = metric_marks.begin(); x != metric_marks.end(); ++x) {
		TempoMarker* tempo_marker;

		if ((tempo_marker = dynamic_cast<TempoMarker*> (*x)) != 0) {
			tempo_marker->update_height_mark ((tempo_marker->tempo().note_types_per_minute() - min_tempo) / max (10.0, max_tempo - min_tempo));
		}
	}
}


void
Editor::tempo_map_property_changed (const PropertyChange& /*ignored*/)
{
	tempo_map_changed (0, 0);
}

void
Editor::tempo_map_changed (samplepos_t, samplepos_t)
{
	cerr << "TEMPO MAP CHANGED IN GUI\n";

	if (!_session) {
		return;
	}

	if (!_session) {
		return;
	}

	compute_bbt_ruler_scale (_leftmost_sample, _leftmost_sample + current_page_samples());
	TempoMapPoints grid;

	if (bbt_ruler_scale != bbt_show_many) {
		compute_current_bbt_points (grid, _leftmost_sample, _leftmost_sample + current_page_samples());
	}

	_session->tempo_map().apply_with_points (*this, &Editor::draw_metric_marks); // redraw metric markers
	draw_measures (grid);
	update_tempo_based_rulers ();

	Temporal::TempoPoint const * prev_point = 0;
	double max_tempo = 0.0;
	double min_tempo = DBL_MAX;

	for (Marks::iterator x = metric_marks.begin(); x != metric_marks.end(); ++x) {
		TempoMarker* tempo_marker;
		MeterMarker* meter_marker;

		if ((tempo_marker = dynamic_cast<TempoMarker*> (*x)) != 0) {
			Temporal::TempoPoint & tempo (tempo_marker->tempo());

			tempo_marker->set_position (timepos_t (tempo.beats()));

			if (prev_point && abs (prev_point->end_note_types_per_minute() - tempo.note_types_per_minute()) < 1.0) {
				tempo_marker->set_points_color (UIConfiguration::instance().color ("tempo marker music"));
			} else {
				tempo_marker->set_points_color (UIConfiguration::instance().color ("tempo marker"));
			}

			max_tempo = max (max_tempo, tempo.note_types_per_minute());
			max_tempo = max (max_tempo, tempo.end_note_types_per_minute());
			min_tempo = min (min_tempo, tempo.note_types_per_minute());
			min_tempo = min (min_tempo, tempo.end_note_types_per_minute());

			prev_point = &tempo;

		} else if ((meter_marker = dynamic_cast<MeterMarker*> (*x)) != 0) {
			meter_marker->set_position (timepos_t (meter_marker->point().beats ()));
		}
	}

	tempo_curves.sort (CurveComparator());

	const double min_tempo_range = 5.0;
	const double tempo_delta = fabs (max_tempo - min_tempo);

	if (tempo_delta < min_tempo_range) {
		max_tempo += min_tempo_range - tempo_delta;
		min_tempo += tempo_delta - min_tempo_range;
	}

	for (Curves::iterator x = tempo_curves.begin(); x != tempo_curves.end(); ) {
		Curves::iterator tmp = x;
		(*x)->set_max_tempo (max_tempo);
		(*x)->set_min_tempo (min_tempo);
		++tmp;
		if (tmp != tempo_curves.end()) {
			(*x)->set_position ((*x)->point().sample(), (*tmp)->point().sample());
		} else {
			(*x)->set_position ((*x)->point().sample(), UINT32_MAX);
		}

		if (!(*x)->point().metric().tempo.active()) {
			(*x)->hide();
		} else {
			(*x)->show();
		}

		++x;
	}

	for (Marks::iterator x = metric_marks.begin(); x != metric_marks.end(); ++x) {
		TempoMarker* tempo_marker;
		if ((tempo_marker = dynamic_cast<TempoMarker*> (*x)) != 0) {
			tempo_marker->update_height_mark ((tempo_marker->tempo().note_types_per_minute() - min_tempo) / max (max_tempo - min_tempo, 10.0));
		}
	}

	compute_bbt_ruler_scale (_leftmost_sample, _leftmost_sample + current_page_samples());

	if (bbt_ruler_scale != bbt_show_many) {
		compute_current_bbt_points (grid, _leftmost_sample, _leftmost_sample + current_page_samples());
	}

	draw_measures (grid);
	update_tempo_based_rulers ();
}

void
Editor::redisplay_tempo (bool immediate_redraw)
{
	if (!_session) {
		return;
	}

	if (immediate_redraw) {

//only recalculate bbt_ruler_scale on a zoom or snap-change; not every redraw; if a case is found where this is necessary, uncomment this line.
//		compute_bbt_ruler_scale (_leftmost_sample, _leftmost_sample + current_page_samples());

		TempoMapPoints grid;

		if (bbt_ruler_scale != bbt_show_many) {
			compute_current_bbt_points (grid, _leftmost_sample, _leftmost_sample + current_page_samples());
		}

		draw_measures (grid);
		update_tempo_based_rulers (); // redraw rulers and measure lines

	} else {
		Glib::signal_idle().connect (sigc::bind_return (sigc::bind (sigc::mem_fun (*this, &Editor::redisplay_tempo), true), false));
	}
}
void
Editor::tempo_curve_selected (TempoMapPoint const * p, bool yn)
{
	if (p == 0) {
		return;
	}

	for (Curves::iterator x = tempo_curves.begin(); x != tempo_curves.end(); ++x) {
		if (&(*x)->point() == p) {
			if (yn) {
				(*x)->set_color_rgba (UIConfiguration::instance().color ("location marker"));
			} else {
				(*x)->set_color_rgba (UIConfiguration::instance().color ("tempo curve"));
			}
			break;
		}
	}
}

/* computes a grid starting a beat before and ending a beat after leftmost and rightmost respectively */
void
Editor::compute_current_bbt_points (Temporal::TempoMapPoints& grid, samplepos_t leftmost, samplepos_t rightmost)
{
	if (!_session) {
		return;
	}

	/* note: we need prevent negative values of leftmost from creeping into tempomap
	 */

	switch (bbt_ruler_scale) {
	case bbt_show_beats:
	case bbt_show_ticks:
	case bbt_show_ticks_detail:
	case bbt_show_ticks_super_detail:
		_session->tempo_map().get_grid (grid, max (samplepos_t (0), leftmost), rightmost, Temporal::Beats (1, 0));
		break;

	case bbt_show_1:
		_session->tempo_map().get_grid (grid, max (samplepos_t (0), leftmost), rightmost, Temporal::Beats (1, 0));
		break;

	case bbt_show_4:
		_session->tempo_map().get_grid (grid, max (samplepos_t (0), leftmost), rightmost, Temporal::Beats (4, 0));
		break;

	case bbt_show_16:
		_session->tempo_map().get_grid (grid, max (samplepos_t (0), leftmost), rightmost, Temporal::Beats (16, 0));
		break;

	case bbt_show_64:
		_session->tempo_map().get_grid (grid, max (samplepos_t (0), leftmost), rightmost, Temporal::Beats (64, 0));
		break;

	default:
		/* bbt_show_many */
		_session->tempo_map().get_grid (grid, max (samplepos_t (0), leftmost), rightmost, Temporal::Beats (128, 0));
		break;
	}
}

void
Editor::hide_measures ()
{
	if (tempo_lines) {
		tempo_lines->hide();
	}
}

void
Editor::draw_measures (TempoMapPoints& grid)
{
	if (_session == 0 || _show_measures == false || grid.empty()) {
		return;
	}

	if (tempo_lines == 0) {
		tempo_lines = new TempoLines (time_line_group);
	}

	const unsigned divisions = get_grid_beat_divisions ();
	tempo_lines->draw (grid, divisions, _leftmost_sample, _session->sample_rate());
}

void
Editor::mouse_add_new_tempo_event (timepos_t const & position)
{
	if (_session == 0) {
		return;
	}

	TempoMap& map(_session->tempo_map());
	TempoDialog tempo_dialog (map, position, _("add"));

	switch (tempo_dialog.run ()) {
	case RESPONSE_ACCEPT:
		break;
	default:
		return;
	}

	double bpm = tempo_dialog.get_bpm ();
	double end_bpm = tempo_dialog.get_end_bpm ();
	double note_type = tempo_dialog.get_note_type ();

	Temporal::BBT_Time requested;
	tempo_dialog.get_bbt_time (requested);

	begin_reversible_command (_("add tempo mark"));
	XMLNode &before = map.get_state();
	map.set_tempo (Tempo (bpm, end_bpm, note_type), requested);
	XMLNode &after = map.get_state();
	_session->add_command(new MementoCommand<TempoMap>(map, &before, &after));
	commit_reversible_command ();
}

void
Editor::mouse_add_new_meter_event (timepos_t const & position)
{
	if (_session == 0) {
		return;
	}

	TempoMap& map(_session->tempo_map());
	MeterDialog meter_dialog (map, position, _("add"));

	switch (meter_dialog.run ()) {
	case RESPONSE_ACCEPT:
		break;
	default:
		return;
	}

	double bpb = meter_dialog.get_bpb ();
	bpb = max (1.0, bpb);

	double note_type = meter_dialog.get_note_type ();

	Temporal::BBT_Time requested;
	meter_dialog.get_bbt_time (requested);

	begin_reversible_command (_("add meter mark"));
	XMLNode &before = map.get_state();

	map.set_meter (Meter (bpb, note_type), timepos_t (requested));

	_session->add_command(new MementoCommand<TempoMap>(map, &before, &map.get_state()));
	commit_reversible_command ();
}

void
Editor::remove_tempo_marker (ArdourCanvas::Item* item)
{
	ArdourMarker* marker;
	TempoMarker* tempo_marker;

	if ((marker = reinterpret_cast<ArdourMarker *> (item->get_data ("marker"))) == 0) {
		fatal << _("programming error: tempo marker canvas item has no marker object pointer!") << endmsg;
		abort(); /*NOTREACHED*/
	}

	if ((tempo_marker = dynamic_cast<TempoMarker*> (marker)) == 0) {
		fatal << _("programming error: marker for tempo is not a tempo marker!") << endmsg;
		abort(); /*NOTREACHED*/
	}

	if (!tempo_marker->tempo().locked_to_meter() && tempo_marker->tempo().active()) {
		real_remove_tempo_marker (tempo_marker->tempo());
	}
}

void
Editor::edit_meter_section (Temporal::MeterPoint const & point)
{
	MeterDialog meter_dialog (_session->tempo_map(), point, _("done"));

	switch (meter_dialog.run()) {
	case RESPONSE_ACCEPT:
		break;
	default:
		return;
	}

	double bpb = meter_dialog.get_bpb ();
	bpb = max (1.0, bpb); // XXX is this a reasonable limit?

	double const note_type = meter_dialog.get_note_type ();
	const Meter meter (bpb, note_type);

	Temporal::BBT_Time when;
	meter_dialog.get_bbt_time (when);

	begin_reversible_command (_("replace meter mark"));
	XMLNode &before = _session->tempo_map().get_state();

	_session->tempo_map().set_meter (meter, when);

	XMLNode &after = _session->tempo_map().get_state();
	_session->add_command(new MementoCommand<TempoMap>(_session->tempo_map(), &before, &after));
	commit_reversible_command ();
}

void
Editor::edit_tempo_section (Temporal::TempoPoint const & point)
{
	TempoDialog tempo_dialog (_session->tempo_map(), point, _("done"));

	switch (tempo_dialog.run ()) {
	case RESPONSE_ACCEPT:
		break;
	default:
		return;
	}

	double bpm = tempo_dialog.get_bpm ();
	double end_bpm = tempo_dialog.get_end_bpm ();
	double nt = tempo_dialog.get_note_type ();
	bpm = max (0.01, bpm);
	Tempo tempo (bpm, end_bpm, nt);

	Temporal::BBT_Time when;
	tempo_dialog.get_bbt_time (when);

	begin_reversible_command (_("replace tempo mark"));
	XMLNode &before = _session->tempo_map().get_state();

	_session->tempo_map().set_tempo (tempo, when);

	XMLNode &after = _session->tempo_map().get_state();
	_session->add_command (new MementoCommand<TempoMap>(_session->tempo_map(), &before, &after));
	commit_reversible_command ();
}

void
Editor::edit_tempo_marker (TempoMarker& tm)
{
	edit_tempo_section (tm.tempo());
}

void
Editor::edit_meter_marker (MeterMarker& mm)
{
	edit_meter_section (mm.meter());
}

gint
Editor::real_remove_tempo_marker (TempoPoint const & point)
{
	begin_reversible_command (_("remove tempo mark"));
	XMLNode &before = _session->tempo_map().get_state();
	_session->tempo_map().remove_tempo (point);
	XMLNode &after = _session->tempo_map().get_state();
	_session->add_command(new MementoCommand<TempoMap>(_session->tempo_map(), &before, &after));
	commit_reversible_command ();

	return FALSE;
}

void
Editor::remove_meter_marker (ArdourCanvas::Item* item)
{
	ArdourMarker* marker;
	MeterMarker* meter_marker;

	if ((marker = reinterpret_cast<ArdourMarker *> (item->get_data ("marker"))) == 0) {
		fatal << _("programming error: meter marker canvas item has no marker object pointer!") << endmsg;
		abort(); /*NOTREACHED*/
	}

	if ((meter_marker = dynamic_cast<MeterMarker*> (marker)) == 0) {
		fatal << _("programming error: marker for meter is not a meter marker!") << endmsg;
		abort(); /*NOTREACHED*/
	}

	if (!_session->tempo_map().is_initial (meter_marker->meter())) {
		real_remove_meter_marker (meter_marker->meter());
	}
}

gint
Editor::real_remove_meter_marker (Temporal::MeterPoint const & point)
{
	begin_reversible_command (_("remove tempo mark"));
	XMLNode &before = _session->tempo_map().get_state();
	_session->tempo_map().remove_meter (point);
	XMLNode &after = _session->tempo_map().get_state();
	_session->add_command(new MementoCommand<TempoMap>(_session->tempo_map(), &before, &after));
	commit_reversible_command ();

	return FALSE;
}
