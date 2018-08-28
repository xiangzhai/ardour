/*
    Copyright (C) 2018 Paul Davis

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

#include "pbd/enumwriter.h"
#include "pbd/i18n.h"

#include "temporal/time.h"

#include "ardour/audioengine.h"
#include "ardour/session.h"
#include "ardour/transport_master.h"
#include "ardour/transport_master_manager.h"

#include "gtkmm2ext/utils.h"

#include "ardour_ui.h"
#include "transport_masters_dialog.h"

using namespace std;
using namespace Gtk;
using namespace Gtkmm2ext;
using namespace ARDOUR;
using namespace PBD;

TransportMastersDialog::TransportMastersDialog ()
	: ArdourDialog (_("Transport Masters"))
	, table (4, 7)
{
	get_vbox()->pack_start (table);

	col1_title.set_markup (string_compose ("<span weight=\"bold\">%1</span>", _("Type")));
	col2_title.set_markup (string_compose ("<span weight=\"bold\">%1</span>", _("Name")));
	col3_title.set_markup (string_compose ("<span weight=\"bold\">%1</span>", _("Format")));
	col4_title.set_markup (string_compose ("<span weight=\"bold\">%1</span>", _("Current")));
	col5_title.set_markup (string_compose ("<span weight=\"bold\">%1</span>", _("Timestamp")));
	col6_title.set_markup (string_compose ("<span weight=\"bold\">%1</span>", _("Delta")));
	col7_title.set_markup (string_compose ("<span weight=\"bold\">%1</span>", _("Collect")));
	col8_title.set_markup (string_compose ("<span weight=\"bold\">%1</span>", _("Use")));

	table.set_spacings (6);

	rebuild ();
}

TransportMastersDialog::~TransportMastersDialog ()
{
	for (vector<Row*>::iterator r = rows.begin(); r != rows.end(); ++r) {
		delete *r;
	}
}

void
TransportMastersDialog::rebuild ()
{
	TransportMasterManager::TransportMasters const & masters (TransportMasterManager::instance().transport_masters());

	container_clear (table);

	for (vector<Row*>::iterator r = rows.begin(); r != rows.end(); ++r) {
		delete *r;
	}

	rows.clear ();
	table.resize (masters.size()+1, 8);

	table.attach (col1_title, 0, 1, 0, 1);
	table.attach (col2_title, 1, 2, 0, 1);
	table.attach (col3_title, 2, 3, 0, 1);
	table.attach (col4_title, 3, 4, 0, 1);
	table.attach (col5_title, 4, 5, 0, 1);
	table.attach (col6_title, 5, 6, 0, 1);
	table.attach (col7_title, 6, 7, 0, 1);
	table.attach (col8_title, 7, 8, 0, 1);

	uint32_t n = 1;

	for (TransportMasterManager::TransportMasters::const_iterator m = masters.begin(); m != masters.end(); ++m, ++n) {

		Row* r = new Row;
		rows.push_back (r);

		r->tm = *m;
		r->label.set_text ((*m)->name());
		r->type.set_text (enum_2_string  ((*m)->type()));

		r->use_button.set_group (use_button_group);

		if (TransportMasterManager::instance().current() == r->tm) {
			r->use_button.set_active (true);
		}

		table.attach (r->type, 0, 1, n, n+1);
		table.attach (r->label, 1, 2, n, n+1);
		table.attach (r->format, 2, 3, n, n+1);
		table.attach (r->current, 3, 4, n, n+1);
		table.attach (r->timestamp, 4, 5, n, n+1);
		table.attach (r->delta, 5, 6, n, n+1);
		table.attach (r->collect_button, 6, 7, n, n+1);
		table.attach (r->use_button, 7, 8, n, n+1);
	}
}

void
TransportMastersDialog::Row::update (Session* s, samplepos_t now)
{
	using namespace Timecode;

	samplepos_t pos;
	double speed;
	stringstream ss;
	Time t;
	boost::shared_ptr<TimecodeTransportMaster> ttm;

	if (s) {
		tm->speed_and_position (speed, pos, now);
		sample_to_timecode (pos, t, false, false, 25, false, AudioEngine::instance()->sample_rate(), 100, false, 0);

		if ((ttm = boost::dynamic_pointer_cast<TimecodeTransportMaster> (tm))) {
			format.set_text (timecode_format_name (ttm->apparent_timecode_format()));
		} else {
			format.set_text ("");
		}
		current.set_text (Timecode::timecode_format_time (t));
		timestamp.set_markup (tm->position_string());
		delta.set_markup (tm->delta_string ());
	}
}

void
TransportMastersDialog::update (samplepos_t audible)
{
	samplepos_t now = AudioEngine::instance()->sample_time ();

	for (vector<Row*>::iterator r = rows.begin(); r != rows.end(); ++r) {
		(*r)->update (_session, now);
	}
}

void
TransportMastersDialog::on_map ()
{
	update_connection = ARDOUR_UI::Clock.connect (sigc::mem_fun (*this, &TransportMastersDialog::update));
	ArdourDialog::on_map ();
}

void
TransportMastersDialog::on_unmap ()
{
	update_connection.disconnect ();
	ArdourDialog::on_unmap ();
}
