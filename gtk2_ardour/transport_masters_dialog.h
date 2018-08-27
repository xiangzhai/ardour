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

#ifndef __ardour_gtk_transport_masters_dialog_h__
#define __ardour_gtk_transport_masters_dialog_h__

#include <gtkmm/button.h>
#include <gtkmm/radiobutton.h>
#include <gtkmm/label.h>
#include <gtkmm/table.h>

namespace ARDOUR {
	class TransportMaster;
}

#include "ardour_dialog.h"

class TransportMastersDialog : public ArdourDialog
{
  public:
	TransportMastersDialog ();
	~TransportMastersDialog ();

	void update (ARDOUR::samplepos_t);

  protected:
	void on_map ();
	void on_unmap ();

  private:

	struct Row {
		Gtk::Label label;
		Gtk::Label type;
		Gtk::Label current;
		Gtk::Label timestamp;
		Gtk::Label delta;
		Gtk::CheckButton collect_button;
		Gtk::RadioButton use_button;
		boost::shared_ptr<ARDOUR::TransportMaster> tm;

		void update (ARDOUR::Session*, ARDOUR::samplepos_t);

		Row () {};
	};

	std::vector<Row*> rows;

	Gtk::RadioButtonGroup use_button_group;
	Gtk::Table table;
	Gtk::Label col1_title;
	Gtk::Label col2_title;
	Gtk::Label col3_title;
	Gtk::Label col4_title;
	Gtk::Label col5_title;
	Gtk::Label col6_title;
	Gtk::Label col7_title;

	sigc::connection update_connection;

	void rebuild ();
};

#endif /* __ardour_gtk_transport_masters_dialog_h__ */
