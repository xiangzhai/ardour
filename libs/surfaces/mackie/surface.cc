#include <sstream>
#include <iomanip>
#include <iostream>
#include <cstdio>
#include <cmath>

#include "midi++/port.h"
#include "midi++/manager.h"

#include "ardour/debug.h"
#include "ardour/route.h"
#include "ardour/panner.h"
#include "ardour/panner_shell.h"
#include "ardour/rc_configuration.h"

#include "control_group.h"
#include "surface_port.h"
#include "surface.h"
#include "strip.h"
#include "mackie_control_protocol.h"
#include "mackie_jog_wheel.h"

#include "strip.h"
#include "button.h"
#include "led.h"
#include "pot.h"
#include "fader.h"
#include "jog.h"
#include "meter.h"

#include "i18n.h"

using namespace std;
using namespace PBD;
using namespace Mackie;
using ARDOUR::Route;
using ARDOUR::Panner;
using ARDOUR::Pannable;
using ARDOUR::PannerShell;

// The MCU sysex header
static MidiByteArray mackie_sysex_hdr  (5, MIDI::sysex, 0x0, 0x0, 0x66, 0x10);

// The MCU extender sysex header
static MidiByteArray mackie_sysex_hdr_xt  (5, MIDI::sysex, 0x0, 0x0, 0x66, 0x11);

static MidiByteArray empty_midi_byte_array;

Surface::Surface (MackieControlProtocol& mcp, jack_client_t* jack, const std::string& device_name, uint32_t number, surface_type_t stype)
	: _mcp (mcp)
	, _stype (stype)
	, _number (number)
	, _active (false)
	, _connected (false)
	, _jog_wheel (0)
{
	DEBUG_TRACE (DEBUG::MackieControl, "Surface::init\n");
	
	MIDI::Manager * mm = MIDI::Manager::instance();

	MIDI::Port * input = new MIDI::Port (string_compose (_("%1 in"), device_name), MIDI::Port::IsInput, jack);
	MIDI::Port * output =new MIDI::Port (string_compose (_("%1 out"), device_name), MIDI::Port::IsOutput, jack);

	input->set_centrally_parsed (false);
	output->set_centrally_parsed (false);
	
	mm->add_port (input);
	mm->add_port (output);

	DEBUG_TRACE (DEBUG::MackieControl, string_compose ("surface has ports named %1 and %2\n",
							   input->name(), output->name()));

	_port = new SurfacePort (*this, *input, *output);
	_port->open();
	_port->inactive_event.connect_same_thread (*this, boost::bind (&Surface::handle_port_inactive, this, _port));

	switch (stype) {
	case mcu:
		init_controls ();
		_jog_wheel = new Mackie::JogWheel (_mcp);
		break;
	default:
		break;
	}

	switch (stype) {
	case mcu:
	case ext:
		strips.resize (8);
		init_strips ();
		break;
	default:
		break;
	}

	connect_to_signals ();

	DEBUG_TRACE (DEBUG::MackieControl, "Surface::init finish\n");
}

Surface::~Surface ()
{
	DEBUG_TRACE (DEBUG::MackieControl, "Surface: destructor\n");

	zero_all ();

	// delete groups
	for (Groups::iterator it = groups.begin(); it != groups.end(); ++it) {
		delete it->second;
	}
	
	// delete controls
	for (Controls::iterator it = controls.begin(); it != controls.end(); ++it) {
		delete *it;
	}
	
	delete _jog_wheel;
	delete _port;
}

const MidiByteArray& 
Surface::sysex_hdr() const
{
	switch  (_stype) {
	case mcu: return mackie_sysex_hdr;
	case ext: return mackie_sysex_hdr_xt;
	}
	cout << "SurfacePort::sysex_hdr _port_type not known" << endl;
	return mackie_sysex_hdr;
}

static GlobalControlDefinition mackie_global_controls[] = {
	{ "jog", 0x3c, Jog::factory, "none" },
	{ "external", 0x2e, Pot::factory, "none" },
	{ "io", 0x28, Button::factory, "assignment" },
	{ "sends", 0x29, Button::factory, "assignment" },
	{ "pan", 0x2a, Button::factory, "assignment" },
	{ "plugin", 0x2b, Button::factory, "assignment" },
	{ "eq", 0x2c, Button::factory, "assignment" },
	{ "dyn", 0x2d, Button::factory, "assignment" },
	{ "left", 0x2e, Button::factory, "bank" },
	{ "right", 0x2f, Button::factory, "bank" },
	{ "channel_left", 0x30, Button::factory, "bank" },
	{ "channel_right", 0x31, Button::factory, "bank" },
	{ "flip", 0x32, Button::factory, "none" },
	{ "edit", 0x33, Button::factory, "none" },
	{ "name_value", 0x34, Button::factory, "display" },
	{ "timecode_beats", 0x35, Button::factory, "display" },
	{ "F1", Button::F1, Button::factory, "none" },
	{ "F2", Button::F2, Button::factory, "none" },
	{ "F3", Button::F3, Button::factory, "none" },
	{ "F4", Button::F4, Button::factory, "none" },
	{ "F5", Button::F5, Button::factory, "none" },
	{ "F6", Button::F6, Button::factory, "none" },
	{ "F7", Button::F7, Button::factory, "none" },
	{ "F8", Button::F8, Button::factory, "none" },
	{ "F9", Button::F9, Button::factory, "none" },
	{ "F10", Button::F10, Button::factory, "none" },
	{ "F11", Button::F11, Button::factory, "none" },
	{ "F12", Button::F12, Button::factory, "none" },
	{ "F13", Button::F13, Button::factory, "none" },
	{ "F14", Button::F14, Button::factory, "none" },
	{ "F15", Button::F15, Button::factory, "none" },
	{ "F16", Button::F16, Button::factory, "none" },
	{ "shift", 0x46, Button::factory, "modifiers" },
	{ "option", 0x47, Button::factory, "modifiers" },
	{ "control", 0x48, Button::factory, "modifiers" },
	{ "cmd_alt", 0x49, Button::factory, "modifiers" },
	{ "on", 0x4a, Button::factory, "automation" },
	{ "rec_ready", 0x4b, Button::factory, "automation" },
	{ "undo", 0x4c, Button::factory, "functions" },
	{ "save", Button::Save, Button::factory, "automation" },
	{ "touch", Button::Touch, Button::factory, "automation" },
	{ "redo", Button::Redo, Button::factory, "functions" },
	{ "marker", Button::Marker, Button::factory, "functions" },
	{ "enter", Button::Enter, Button::factory, "functions" },
	{ "cancel", Button::Cancel, Button::factory, "functions" },
	{ "mixer", Button::Mixer, Button::factory, "functions" },
	{ "frm_left", 0x54, Button::factory, "transport" },
	{ "frm_right", 0x55, Button::factory, "transport" },
	{ "loop", Button::Loop, Button::factory, "transport" },
	{ "punch_in", 0x57, Button::factory, "transport" },
	{ "punch_out", 0x58, Button::factory, "transport" },
	{ "home", Button::Home, Button::factory, "transport" },
	{ "end", Button::End, Button::factory, "transport" },
	{ "rewind", Button::Rewind, Button::factory, "transport" },
	{ "ffwd", Button::Ffwd, Button::factory, "transport" },
	{ "stop", Button::Stop, Button::factory, "transport" },
	{ "play", Button::Play, Button::factory, "transport" },
	{ "record", Button::Record, Button::factory, "transport" },
	{ "cursor_up", Button::CursorUp, Button::factory, "cursor" },
	{ "cursor_down", Button::CursorDown, Button::factory, "cursor" },
	{ "cursor_left", Button::CursorLeft, Button::factory, "cursor" },
	{ "cursor_right", Button::CursorRight, Button::factory, "cursor" },
	{ "zoom", Button::Zoom, Button::factory, "none" },
	{ "scrub", Button::Scrub, Button::factory, "none" },
	{ "user_a", Button::UserA, Button::factory, "user" },
	{ "user_b", Button::UserB, Button::factory, "user" },
	{ "fader_touch", 0x70, Led::factory, "master" },
	{ "timecode", 0x71, Led::factory, "none" },
	{ "beats", 0x72, Led::factory, "none" },
	{ "solo", 0x73, Led::factory, "none" },
	{ "relay_click", 0x73, Led::factory, "none" },
	{ "", 0, Button::factory, "" }
};
	
void 
Surface::init_controls()
{
	Group* group;

	groups["assignment"] = new Group  ("assignment");
	groups["automation"] = new Group  ("automation");
	groups["bank"] = new Group  ("bank");
	groups["cursor"] = new Group  ("cursor");
	groups["display"] = new Group  ("display");
	groups["functions"] = new Group  ("functions");
	groups["modifiers"] = new Group  ("modifiers");
	groups["none"] = new Group  ("none");
	groups["transport"] = new Group  ("transport");
	groups["user"] = new Group  ("user");
	groups["master"] = new Group ("master");

	for (uint32_t n = 0; mackie_global_controls[n].name[0]; ++n) {
		group = groups[mackie_global_controls[n].group_name];
		Control* control = mackie_global_controls[n].factory (*this, mackie_global_controls[n].id, mackie_global_controls[n].name, *group);
		controls_by_name[mackie_global_controls[n].name] = control;
		group->add (*control);
	}
}

static StripControlDefinition mackie_strip_controls[] = {
	{ "gain", Control::fader_base_id, Fader::factory, },
	{ "vpot", Control::pot_base_id, Pot::factory, },
	{ "recenable", Control::recenable_button_base_id, Button::factory, },
	{ "solo", Control::solo_button_base_id, Button::factory, },
	{ "mute", Control::mute_button_base_id, Button::factory, },
	{ "select", Control::select_button_base_id, Button::factory, },
	{ "vselect", Control::vselect_button_base_id, Button::factory, },
	{ "fader_touch", Control::fader_touch_button_base_id, Button::factory, },
	{ "meter", 0, Meter::factory, },
	{ "", 0, Button::factory, }
};

void 
Surface::init_strips ()
{
	for (uint32_t i = 0; i < 8; ++i) {

		char name[32];
		
		snprintf (name, sizeof (name), "strip_%d", (8* _number) + i);

		Strip* strip = new Strip (*this, name, i, mackie_strip_controls);
		
		groups[name] = strip;
		strips[i] = strip;
	}
}

float 
Surface::scaled_delta (const ControlState & state, float current_speed)
{
	return state.sign *  (std::pow (float(state.ticks + 1), 2) + current_speed) / 100.0;
}

void 
Surface::display_bank_start (uint32_t current_bank)
{
	if  (current_bank == 0) {
		// send Ar. to 2-char display on the master
		_port->write (two_char_display ("Ar", ".."));
	} else {
		// write the current first remote_id to the 2-char display
		_port->write (two_char_display (current_bank));
	}
}

void 
Surface::blank_jog_ring ()
{
	Control* control = controls_by_name["jog"];

	if (control) {
		Pot* pot = dynamic_cast<Pot*> (control);
		if (pot) {
			_port->write (pot->set_onoff (false));
		}
	}
}

bool 
Surface::has_timecode_display () const
{
	return false;
}

float
Surface::scrub_scaling_factor () const
{
	return 100.0;
}

void 
Surface::connect_to_signals ()
{
	if (!_connected) {

		DEBUG_TRACE (DEBUG::MackieControl, string_compose ("Surface %1 connecting to signals on port %2\n", 
								   number(), _port->input_port().name()));

		MIDI::Parser* p = _port->input_port().parser();

		/* V-Pot messages are Controller */
		p->controller.connect_same_thread (*this, boost::bind (&Surface::handle_midi_controller_message, this, _1, _2));
		/* Button messages are NoteOn */
		p->note_on.connect_same_thread (*this, boost::bind (&Surface::handle_midi_note_on_message, this, _1, _2));
		/* Fader messages are Pitchbend */
		p->channel_pitchbend[0].connect_same_thread (*this, boost::bind (&Surface::handle_midi_pitchbend_message, this, _1, _2, 0U));
		p->channel_pitchbend[1].connect_same_thread (*this, boost::bind (&Surface::handle_midi_pitchbend_message, this, _1, _2, 1U));
		p->channel_pitchbend[2].connect_same_thread (*this, boost::bind (&Surface::handle_midi_pitchbend_message, this, _1, _2, 2U));
		p->channel_pitchbend[3].connect_same_thread (*this, boost::bind (&Surface::handle_midi_pitchbend_message, this, _1, _2, 3U));
		p->channel_pitchbend[4].connect_same_thread (*this, boost::bind (&Surface::handle_midi_pitchbend_message, this, _1, _2, 4U));
		p->channel_pitchbend[5].connect_same_thread (*this, boost::bind (&Surface::handle_midi_pitchbend_message, this, _1, _2, 5U));
		p->channel_pitchbend[6].connect_same_thread (*this, boost::bind (&Surface::handle_midi_pitchbend_message, this, _1, _2, 6U));
		p->channel_pitchbend[7].connect_same_thread (*this, boost::bind (&Surface::handle_midi_pitchbend_message, this, _1, _2, 7U));
		
		_connected = true;
	}
}

void
Surface::handle_midi_pitchbend_message (MIDI::Parser&, MIDI::pitchbend_t pb, uint32_t fader_id)
{
	DEBUG_TRACE (DEBUG::MackieControl, string_compose ("handle_midi pitchbend on port %3, fader = %1 value = %2\n", 
							   fader_id, pb, _number));
	
	Fader* fader = faders[fader_id];

	if (fader) {
		Strip* strip = dynamic_cast<Strip*> (&fader->group());
		if (strip) {
			float midi_pos = pb >> 4; // only the top 10 bytes are used
			strip->handle_fader (*fader, midi_pos/1023.0);
		} else {
			/* master fader */
		}
	} else {
		DEBUG_TRACE (DEBUG::MackieControl, "fader not found\n");
	}
}

void 
Surface::handle_midi_note_on_message (MIDI::Parser &, MIDI::EventTwoBytes* ev)
{
	DEBUG_TRACE (DEBUG::MackieControl, string_compose ("SurfacePort::handle_note_on %1 = %2\n", ev->note_number, ev->velocity));

	Button* button = buttons[ev->note_number];

	if (button) {
		Strip* strip = dynamic_cast<Strip*> (&button->group());

		if (strip) {
			strip->handle_button (*button, ev->velocity == 0x7f ? press : release);
		} else {
			/* global button */
			DEBUG_TRACE (DEBUG::MackieControl, string_compose ("global button %1\n", button->raw_id()));
			_mcp.handle_button_event (*this, *button, ev->velocity == 0x7f ? press : release);
		}
	} 
}

void 
Surface::handle_midi_controller_message (MIDI::Parser &, MIDI::EventTwoBytes* ev)
{
	DEBUG_TRACE (DEBUG::MackieControl, string_compose ("SurfacePort::handle_midi_controller %1 = %2\n", ev->controller_number, ev->value));

	Pot* pot = pots[ev->controller_number];

	if (!pot && ev->controller_number == Control::jog_base_id) {
		pot = dynamic_cast<Pot*> (controls_by_name["jog"]);
	}

	if (pot) {
		ControlState state;
		
		// bytes[2] & 0b01000000 (0x40) give sign
		state.sign = (ev->value & 0x40) == 0 ? 1 : -1; 
		// bytes[2] & 0b00111111 (0x3f) gives delta
		state.ticks = (ev->value & 0x3f);
		if (state.ticks == 0) {
			/* euphonix and perhaps other devices send zero
			   when they mean 1, we think.
			*/
			state.ticks = 1;
		}
		state.delta = float (state.ticks) / float (0x3f);
		
		/* Pots only emit events when they move, not when they
		   stop moving. So to get a stop event, we need to use a timeout.
		*/

		_mcp.add_in_use_timeout (*this, *pot, pot);

		Strip* strip = dynamic_cast<Strip*> (&pot->group());
		if (strip) {
			strip->handle_pot (*pot, state);
		} else {
			JogWheel* wheel = dynamic_cast<JogWheel*> (pot);
			if (wheel) {
				DEBUG_TRACE (DEBUG::MackieControl, string_compose ("Jog wheel moved %1\n", state.ticks));
				wheel->jog_event (*_port, *pot, state);
			} else {
				DEBUG_TRACE (DEBUG::MackieControl, string_compose ("External controller moved %1\n", state.ticks));
				cout << "external controller" << state.ticks * state.sign << endl;
			}
		}
	} else {
		DEBUG_TRACE (DEBUG::MackieControl, "pot not found\n");
	}
}

void 
Surface::handle_port_inactive (SurfacePort * port)
{
	_active = false;
}

void 
Surface::write_sysex (const MidiByteArray & mba)
{
	if (mba.empty()) {
		return;
	}

	MidiByteArray buf;
	buf << sysex_hdr() << mba << MIDI::eox;
	_port->write (buf);
}

void 
Surface::write_sysex (MIDI::byte msg)
{
	MidiByteArray buf;
	buf << sysex_hdr() << msg << MIDI::eox;
	_port->write (buf);
}

void
Surface::drop_routes ()
{
	for (Strips::iterator s = strips.begin(); s != strips.end(); ++s) {
		(*s)->set_route (boost::shared_ptr<Route>());
	}
}

uint32_t
Surface::n_strips () const
{
	return strips.size();
}

Strip*
Surface::nth_strip (uint32_t n) const
{
	if (n > n_strips()) {
		return 0;
	}
	return strips[n];
}

void
Surface::zero_all ()
{
	// TODO turn off Timecode displays

	// zero all strips
	for (Strips::iterator it = strips.begin(); it != strips.end(); ++it) {
		_port->write ((*it)->zero());
	}

	// turn off global buttons and leds
        // global buttons are only ever on mcu_port, so we don't have
	// to figure out which port.

	for (Controls::iterator it = controls.begin(); it != controls.end(); ++it) {
		Control & control = **it;
		if (!control.group().is_strip() && control.accepts_feedback()) {
			_port->write (control.zero());
		}
	}

	// any hardware-specific stuff
	// clear 2-char display
	_port->write (two_char_display ("  "));

	// and the led ring for the master strip
	blank_jog_ring ();
}

void
Surface::periodic ()
{
	for (Strips::iterator s = strips.begin(); s != strips.end(); ++s) {
		(*s)->periodic ();
	}

}

void
Surface::write (const MidiByteArray& data) 
{
	_port->write (data);
}

void 
Surface::jog_wheel_state_display (JogWheel::State state)
{
	switch (state) {
	case JogWheel::zoom:
			_port->write (two_char_display ("Zm"));
			break;
		case JogWheel::scroll:
			_port->write (two_char_display ("Sc"));
			break;
		case JogWheel::scrub:
			_port->write (two_char_display ("Sb"));
			break;
		case JogWheel::shuttle:
			_port->write (two_char_display ("Sh"));
			break;
		case JogWheel::speed:
			_port->write (two_char_display ("Sp"));
			break;
		case JogWheel::select:
			_port->write (two_char_display ("Se"));
			break;
	}
}

void
Surface::map_routes (const vector<boost::shared_ptr<Route> >& routes)
{
	vector<boost::shared_ptr<Route> >::const_iterator r;
	Strips::iterator s;

	for (s = strips.begin(); s != strips.end(); ++s) {
		(*s)->set_route (boost::shared_ptr<Route>());
	}

	for (r = routes.begin(), s = strips.begin(); r != routes.end() && s != strips.end(); ++r, ++s) {
		(*s)->set_route (*r);
	}
}

static char translate_seven_segment (char achar)
{
	achar = toupper (achar);
	if  (achar >= 0x40 && achar <= 0x60)
		return achar - 0x40;
	else if  (achar >= 0x21 && achar <= 0x3f)
      return achar;
	else
      return 0x00;
}

MidiByteArray 
Surface::two_char_display (const std::string & msg, const std::string & dots)
{
	if  (msg.length() != 2) throw MackieControlException ("MackieMidiBuilder::two_char_display: msg must be exactly 2 characters");
	if  (dots.length() != 2) throw MackieControlException ("MackieMidiBuilder::two_char_display: dots must be exactly 2 characters");
	
	MidiByteArray bytes (6, 0xb0, 0x4a, 0x00, 0xb0, 0x4b, 0x00);
	
	// chars are understood by the surface in right-to-left order
	// could also exchange the 0x4a and 0x4b, above
	bytes[5] = translate_seven_segment (msg[0]) +  (dots[0] == '.' ? 0x40 : 0x00);
	bytes[2] = translate_seven_segment (msg[1]) +  (dots[1] == '.' ? 0x40 : 0x00);
	
	return bytes;
}

MidiByteArray 
Surface::two_char_display (unsigned int value, const std::string & /*dots*/)
{
	ostringstream os;
	os << setfill('0') << setw(2) << value % 100;
	return two_char_display (os.str());
}

void 
Surface::display_timecode (const std::string & timecode, const std::string & timecode_last)
{
	if (has_timecode_display()) {
		_port->write (timecode_display (timecode, timecode_last));
	}
}

MidiByteArray 
Surface::timecode_display (const std::string & timecode, const std::string & last_timecode)
{
	// if there's no change, send nothing, not even sysex header
	if  (timecode == last_timecode) return MidiByteArray();
	
	// length sanity checking
	string local_timecode = timecode;

	// truncate to 10 characters
	if  (local_timecode.length() > 10) {
		local_timecode = local_timecode.substr (0, 10);
	}

	// pad to 10 characters
	while  (local_timecode.length() < 10) { 
		local_timecode += " ";
	}
		
	// find the suffix of local_timecode that differs from last_timecode
	std::pair<string::const_iterator,string::iterator> pp = mismatch (last_timecode.begin(), last_timecode.end(), local_timecode.begin());
	
	MidiByteArray retval;
	
	// sysex header
	retval << sysex_hdr();
	
	// code for timecode display
	retval << 0x10;
	
	// translate characters. These are sent in reverse order of display
	// hence the reverse iterators
	string::reverse_iterator rend = reverse_iterator<string::iterator> (pp.second);
	for  (string::reverse_iterator it = local_timecode.rbegin(); it != rend; ++it) {
		retval << translate_seven_segment (*it);
	}
	
	// sysex trailer
	retval << MIDI::eox;
	
	return retval;
}

