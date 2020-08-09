/*
 * Copyright (C) 2020 Robin Gareus <robin@gareus.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef __ardour_vst3_x11_plugin_ui_h__
#define __ardour_vst3_x11_plugin_ui_h__

#ifdef WAF_BUILD
#include "gtk2ardour-config.h"
#endif

#ifdef VST3_SUPPORT

#include <gtkmm/widget.h>

#include "plugin_ui.h"

namespace ARDOUR {
	class PluginInsert;
	class VST2Plugin;
}

class VST3X11PluginUI : public PlugUIBase, public Gtk::VBox
{
public:
	VST3X11PluginUI (boost::shared_ptr<ARDOUR::PluginInsert>, boost::shared_ptr<ARDOUR::VST3Plugin>);
	~VST3X11PluginUI ();

	gint get_preferred_height ();
	gint get_preferred_width ();
	bool resizable ();

	bool start_updating (GdkEventAny*);
	bool stop_updating (GdkEventAny*);

	int package (Gtk::Window&);

private:
	boost::shared_ptr<ARDOUR::PluginInsert> _pi;
	boost::shared_ptr<ARDOUR::VST3Plugin>   _vst3;
	Gtk::Widget*                            _gui_widget;
};

#endif // VST3_SUPPORT
#endif
