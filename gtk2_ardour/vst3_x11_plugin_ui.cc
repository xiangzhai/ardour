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


#include "ardour/plugin_insert.h"
#include "ardour/vst3_plugin.h"

#include "vst3_x11_plugin_ui.h"

#include <X11/X.h>
#include <X11/Xlib.h>

using namespace ARDOUR;
using namespace Steinberg;

VST3X11PluginUI::VST3X11PluginUI (boost::shared_ptr<PluginInsert> pi, boost::shared_ptr<VST3Plugin> vst3)
	: PlugUIBase (pi)
	, _pi (pi)
	, _vst3 (vst3)
{
	IPlugView* view = vst3->view ();
#if 0
	// -> compare vstfx_run_editor
	Window window;
	if (kResultOk != view->attached ((void*)window, "X11EmbedWindowID")) {
		;
	}
#endif
}

VST3X11PluginUI::~VST3X11PluginUI ()
{
}

gint
VST3X11PluginUI::get_preferred_height ()
{
	return 0;
}

gint
VST3X11PluginUI::get_preferred_width ()
{
	return 0;
}

bool
VST3X11PluginUI::resizable ()
{
	return false;
}

bool
VST3X11PluginUI::start_updating (GdkEventAny*)
{
	return false;
}

bool
VST3X11PluginUI::stop_updating (GdkEventAny*)
{
	return false;
}

int
VST3X11PluginUI::package (Gtk::Window&)
{
	return 0;
}
