/*
 * Copyright (C) 2019-2020 Robin Gareus <robin@gareus.org>
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

#ifndef _ardour_vst3_host_h_
#define _ardour_vst3_host_h_

#include <map>
#include <vector>

#include <glib.h>

#include <boost/shared_ptr.hpp>

#include "ardour/libardour_visibility.h"
#include "vst3/vst3.h"

#define QUERY_INTERFACE_IMPL(Interface)                  \
tresult queryInterface (const TUID _iid, void** obj)     \
{                                                        \
  QUERY_INTERFACE (_iid, obj, FUnknown::iid, Interface)  \
  QUERY_INTERFACE (_iid, obj, Interface::iid, Interface) \
  *obj = nullptr;                                        \
  return kNoInterface;                                   \
}

#if defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wnon-virtual-dtor"
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#    pragma GCC diagnostic push
#    pragma GCC diagnostic ignored "-Wnon-virtual-dtor"
#endif

namespace Steinberg {

LIBARDOUR_API extern std::string tchar_to_utf8 (Vst::TChar const* s);
LIBARDOUR_API extern bool utf8_to_tchar (Vst::TChar* rv, const char* s, size_t l = 0);
LIBARDOUR_API extern bool utf8_to_tchar (Vst::TChar* rv, std::string const& s, size_t l = 0);

class LIBARDOUR_API HostAttribute
{
public:
	enum Type {
		kInteger,
		kFloat,
		kString,
		kBinary
	};

	HostAttribute (int64 value)
		: _size (0)
		, _type (kInteger)
	{
		v.intValue = value;
	}

	HostAttribute (double value)
		: _size (0)
		, _type (kFloat)
	{
		v.floatValue = value;
	}

	HostAttribute (const Vst::TChar* value, uint32 size)
		: _size (size)
		, _type (kString)
	{
		v.stringValue = new Vst::TChar[_size];
		memcpy (v.stringValue, value, _size * sizeof (Vst::TChar));
	}

	HostAttribute (const void* value, uint32 size)
		: _size (size)
		, _type (kBinary)
	{
		v.binaryValue = new char[_size];
		memcpy (v.binaryValue, value, _size);
	}

	~HostAttribute ()
	{
		if (_size) {
			delete[] v.binaryValue;
		}
	}

	Type getType ()      const { return _type; }
	int64 intValue ()    const { return v.intValue; }
	double floatValue () const { return v.floatValue; }

	const Vst::TChar* stringValue (uint32& stringSize)
	{
		stringSize = _size;
		return v.stringValue;
	}

	const void* binaryValue (uint32& binarySize)
	{
		binarySize = _size;
		return v.binaryValue;
	}

protected:
	union v {
		int64       intValue;
		double      floatValue;
		Vst::TChar* stringValue;
		char*       binaryValue;
	} v;

	uint32 _size;
	Type   _type;

private:
	/* prevent copy construction */
	HostAttribute (HostAttribute const& other);
};

class LIBARDOUR_API RefObject : public FUnknown
{
public:
	RefObject ();
	virtual ~RefObject () {}
	uint32 addRef ();
	uint32 release ();

private:
	gint _cnt; // atomic
};

class LIBARDOUR_API HostAttributeList : public Vst::IAttributeList, public RefObject
{
public:
	HostAttributeList ();
	virtual ~HostAttributeList ();

	QUERY_INTERFACE_IMPL (Vst::IAttributeList);
	uint32 addRef () SMTG_OVERRIDE { return RefObject::addRef (); }
	uint32 release () SMTG_OVERRIDE { return RefObject::release (); }

	tresult setInt (AttrID aid, int64 value) SMTG_OVERRIDE;
	tresult getInt (AttrID aid, int64& value) SMTG_OVERRIDE;
	tresult setFloat (AttrID aid, double value) SMTG_OVERRIDE;
	tresult getFloat (AttrID aid, double& value) SMTG_OVERRIDE;
	tresult setString (AttrID aid, const Vst::TChar* string) SMTG_OVERRIDE;
	tresult getString (AttrID aid, Vst::TChar* string, uint32 size) SMTG_OVERRIDE;
	tresult setBinary (AttrID aid, const void* data, uint32 size) SMTG_OVERRIDE;
	tresult getBinary (AttrID aid, const void*& data, uint32& size) SMTG_OVERRIDE;

protected:
	void removeAttrID (AttrID aid);

	std::map<std::string, HostAttribute*> list;
};

class LIBARDOUR_API HostMessage : public Vst::IMessage, public RefObject
{
public:
	HostMessage ();
	virtual ~HostMessage ();

	QUERY_INTERFACE_IMPL (Vst::IMessage);
	uint32 addRef () SMTG_OVERRIDE { return RefObject::addRef (); }
	uint32 release () SMTG_OVERRIDE { return RefObject::release (); }

	const char* getMessageID () SMTG_OVERRIDE;
	void setMessageID (const char* messageID) SMTG_OVERRIDE;
	Vst::IAttributeList* getAttributes () SMTG_OVERRIDE;

protected:
	char*                                _messageId;
	boost::shared_ptr<HostAttributeList> _attribute_list;
};

class LIBARDOUR_API PlugInterfaceSupport : public Vst::IPlugInterfaceSupport
{
public:
	PlugInterfaceSupport ();
	QUERY_INTERFACE_IMPL (Vst::IPlugInterfaceSupport);
	uint32 addRef () SMTG_OVERRIDE { return 1; }
	uint32 release () SMTG_OVERRIDE { return 1; }

	tresult isPlugInterfaceSupported (const TUID) SMTG_OVERRIDE;
	void    addPlugInterfaceSupported (const TUID);

private:
	std::vector<FUID> _interfaces;
};

class LIBARDOUR_API HostApplication : public Vst::IHostApplication
{
public:
	static Vst::IHostApplication* getHostContext ()
	{
		static HostApplication* app = new HostApplication;
		return app;
	}

	HostApplication ();
	virtual ~HostApplication () {}
	tresult queryInterface (const TUID _iid, void** obj) SMTG_OVERRIDE;

	uint32 addRef () SMTG_OVERRIDE { return 1; }
	uint32 release () SMTG_OVERRIDE { return 1; }

	tresult getName (Vst::String128 name) SMTG_OVERRIDE;
	tresult createInstance (TUID cid, TUID _iid, void** obj) SMTG_OVERRIDE;

protected:
	boost::shared_ptr<PlugInterfaceSupport> _plug_interface_support;
};

class ParamValueQueue : public Vst::IParamValueQueue
{
public:
	QUERY_INTERFACE_IMPL (Vst::IParamValueQueue);
	uint32 addRef () SMTG_OVERRIDE { return 1; }
	uint32 release () SMTG_OVERRIDE { return 1; }

	static const int maxNumPoints = 64;

	ParamValueQueue() {
		_values.reserve (maxNumPoints);
		_id = Vst::kNoParamId;
	}

	Vst::ParamID getParameterId() SMTG_OVERRIDE { return _id; }

	void setParameterId (Vst::ParamID id) {
		_values.clear();
		_id = id;
	}

	int32   getPointCount() SMTG_OVERRIDE { return _values.size(); }
	tresult getPoint (int32 index, int32&, Vst::ParamValue&) SMTG_OVERRIDE;
	tresult addPoint (int32, Vst::ParamValue, int32&) SMTG_OVERRIDE;

protected:
	struct Value {
		Value (Vst::ParamValue v, int32 offset)
			: value(v)
			, sampleOffset(offset)
		{}
		Vst::ParamValue value;
		int32 sampleOffset;
	};

	std::vector<Value> _values;
	Vst::ParamID _id;
};

class ParameterChanges : public Vst::IParameterChanges
{
public:
	QUERY_INTERFACE_IMPL (Vst::IParameterChanges);
	uint32 addRef () SMTG_OVERRIDE { return 1; }
	uint32 release () SMTG_OVERRIDE { return 1; }

	ParameterChanges () {
		clear ();
	}

	void set_n_params (int n) {
		_queue.resize(n);
	}

	void clear () {
		_used_queue_count = 0;
	}

	int32 getParameterCount() SMTG_OVERRIDE {
		return _used_queue_count;
	}

	Vst::IParamValueQueue* getParameterData (int32 index) SMTG_OVERRIDE;
	Vst::IParamValueQueue* addParameterData (Vst::ParamID const& id, int32& index) SMTG_OVERRIDE;

protected:
	std::vector<ParamValueQueue> _queue;
	int _used_queue_count;
};

class EventList : public Vst::IEventList {
public:
	EventList() {
		_events.reserve (128);
	}

	QUERY_INTERFACE_IMPL (Vst::IEventList)
	uint32 addRef () SMTG_OVERRIDE { return 1; }
	uint32 release () SMTG_OVERRIDE { return 1; }

	int32 PLUGIN_API getEventCount() {
		return _events.size ();
	}

	tresult PLUGIN_API getEvent (int32 index, Vst::Event& e) {
		if (index >= 0 && index < (int32)_events.size ()) {
			e = _events[index];
			return kResultTrue;
		} else {
			return kResultFalse;
		}
	}

	tresult PLUGIN_API addEvent (Vst::Event& e) {
		_events.push_back (e);
		return kResultTrue;
	}

	void clear() {
		_events.clear();
	}

protected:
	std::vector<Vst::Event> _events;
};

#if defined(__clang__)
#    pragma clang diagnostic pop
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#    pragma GCC diagnostic pop
#endif

} // namespace Steinberg
#endif
