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

#include <vector>

#include "pbd/i18n.h"

#include "ardour/audioengine.h"
#include "ardour/midi_port.h"
#include "ardour/session.h"
#include "ardour/transport_master.h"
#include "ardour/utils.h"

using namespace ARDOUR;

const std::string TransportMaster::state_node_name = X_("TransportMaster");


TransportMaster::TransportMaster (SyncSource t, std::string const & name)
	: _type (t)
	, _name (name)
	, _session (0)
{
	ARDOUR::AudioEngine::instance()->PortConnectedOrDisconnected.connect_same_thread (port_connection, boost::bind (&TransportMaster::connection_handler, this, _1, _2, _3, _4, _5));
}

TransportMaster::~TransportMaster()
{
	delete _session;
}

bool
TransportMaster::connection_handler (boost::weak_ptr<ARDOUR::Port>, std::string name1, boost::weak_ptr<ARDOUR::Port>, std::string name2, bool yn)
{
	if (!_port) {
		return false;
	}

	const std::string fqn = ARDOUR::AudioEngine::instance()->make_port_name_non_relative (_port->name());

	if (fqn != name1 && fqn != name2) {
		/* not our port */
		return false;
	}

	_connected = yn;
	return true;
}

void
TransportMaster::set_session (Session* s)
{
	_session = s;
}

int
TransportMaster::set_state (XMLNode const & node, int /* version */)
{
	XMLNode* pnode = node.child (X_("Port"));

	if (pnode) {
		XMLNodeList const & children = pnode->children();
		for (XMLNodeList::const_iterator ci = children.begin(); ci != children.end(); ++ci) {

			XMLProperty const *prop;

			if ((*ci)->name() == X_("Connection")) {
				if ((prop = (*ci)->property (X_("other"))) == 0) {
					continue;
				}
				_port->connect (prop->value());
			}
		}
	}

	return 0;
}

XMLNode&
TransportMaster::get_state ()
{
	XMLNode* node = new XMLNode (state_node_name);
	node->set_property (X_("type"), _type);
	node->set_property (X_("name"), _name);


	if (_port) {
		std::vector<std::string> connections;

		XMLNode* pnode = new XMLNode (X_("Port"));

		if (_port->get_connections (connections)) {

			std::vector<std::string>::const_iterator ci;
			std::sort (connections.begin(), connections.end());

			for (ci = connections.begin(); ci != connections.end(); ++ci) {

				/* if its a connection to our own port,
				   return only the port name, not the
				   whole thing. this allows connections
				   to be re-established even when our
				   client name is different.
				*/

				XMLNode* cnode = new XMLNode (X_("Connection"));

				cnode->set_property (X_("other"), AudioEngine::instance()->make_port_name_relative (*ci));
				pnode->add_child_nocopy (*cnode);
			}
		}

		node->add_child_nocopy (*pnode);
	}

	return *node;
}

boost::shared_ptr<TransportMaster>
TransportMaster::factory (XMLNode const & node)
{
	if (node.name() != TransportMaster::state_node_name) {
		return boost::shared_ptr<TransportMaster>();
	}

	SyncSource type;
	std::string name;

	if (!node.get_property (X_("type"), type)) {
		return boost::shared_ptr<TransportMaster>();
	}

	if (!node.get_property (X_("name"), name)) {
		return boost::shared_ptr<TransportMaster>();
	}

	return factory (type, name);
}

boost::shared_ptr<TransportMaster>
TransportMaster::factory (SyncSource type, std::string const& name)
{
	/* XXX need to count existing sources of a given type */

	switch (type) {
	case MTC:
		return boost::shared_ptr<TransportMaster> (new MTC_TransportMaster (sync_source_to_string (type)));
	case LTC:
		return boost::shared_ptr<TransportMaster> (new LTC_TransportMaster (sync_source_to_string (type)));
	case MIDIClock:
		return boost::shared_ptr<TransportMaster> (new MIDIClock_TransportMaster (sync_source_to_string (type)));
	case Engine:
		return boost::shared_ptr<TransportMaster> (new Engine_TransportMaster (*AudioEngine::instance()));
	default:
		break;
	}

	return boost::shared_ptr<TransportMaster>();
}

boost::shared_ptr<Port>
TransportMasterViaMIDI::create_midi_port (std::string const & port_name)
{
	boost::shared_ptr<Port> p;

	if ((p = AudioEngine::instance()->register_input_port (DataType::MIDI, port_name)) == 0) {
		return boost::shared_ptr<Port> ();
	}

	_midi_port = boost::dynamic_pointer_cast<MidiPort> (p);

	return p;
}

void
TransportMasterViaMIDI::update_from_midi (pframes_t nframes, samplepos_t now)
{
	_midi_port->read_and_parse_entire_midi_buffer_with_no_speed_adjustment (nframes, parser, now);
}
