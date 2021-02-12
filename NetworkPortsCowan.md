## Network Ports

Network ports are a mild abstraction of TCP sockets. They are a subtype of ports.

This proposal depends on [SettingsListsCowan](SettingsListsCowan.md).  Settings lists passed to procedures in this proposal MUST contain either the `path` key (for local domain sockets), or the keys `host` and `port` defined below (for TCP sockets).


## Client Procedures

The procedures in this section take a specification for a TCP socket and return a input/output port connected to the socket.

`(open-network-client `*settings-list*`)`

Returns an input/output port connected to the host and port specified by *settings-list*.

## Server Procedures

`(make-network-listener `*settings-list*`)`

Returns an opaque *listener* object (which may be of any type) that will accept connections to the port and host (which must be a local address) specified in *settings-list*.  If the host is not specified, the listener will accept connections that are made to *port* on any local address.

`(open-network-server `*listener*`)`

Waits for a client to connect to *listener*, and returns an input/output port connected to that client.  When the port is closed, this procedure may be invoked again on the same listener.

`(close-network-listener `*listener*`)`

Close *listener* and abandon any further attempts to listen for collections.

## Settings-list Keys

Implementations MUST support the following keys:

`host`::

> Specifies the host as a string.  It may be an IPv4 dotted-decimal address, an IPv6 colon-hexadecimal address if supported by the operating system, or a host name to be looked up according to whatever operating system conventions exist, if any.  Appropriate strings may refer to broadcast or multicast addresses.

> The host can also be specified as a bytevector of length 4 or 16.

`port`::

> Specifies the port number as an exact integer or the port name as a string.  The meaning of a string is implementation-dependent, but is intended to be a standardized service name.

`network-endpoint`::

> Specifies both the host and the port number in the form of a network endpoint object.  See [NetworkEndpointsCowan](NetworkEndpointsCowan.md).
