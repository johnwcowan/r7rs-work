Network endpoints are a disjoint type representing a network host name or address plus a port.

== Definitions ==

In the following descriptions, ''host'' can be:

 * a string in the form of an IPv4 dotted-decimal address

 * a string in the form of an IPv6 colon-hexadecimal address (if supported by the operating system)

 * a string representing a host name to be looked up according to whatever operating system conventions exist, if any

 * a bytevector of the appropriate length, such as 4 for IPv4 or 16 for IPv6, representing a host address in binary format.

 * `#t`, which means "any of the IP addresses of the local host"

Note that appropriate values may refer to broadcast or multicast addresses.

''Network-port'' can be a positive exact integer or a string.  The meaning of a string is implementation-dependent, but is intended to be a standardized service name.  Note that these ports are not Scheme ports.

== Procedures ==

`(make-network-endpoint `[''host'']` `[''network-port'']`)`

Returns a network endpoint, which may be newly allocated.  If ''network-port'' is omitted or 0, then it is an error unless ''host'' refers to the local host, and the endpoint specifies an unused port to be assigned by the operating system.  If ''network-port'' is present but ''host'' is omitted or has the value `#t`, the endpoint specifies the given port on the local host accessed by any address.  Otherwise, the endpoint specifies a particular host and port.  If there is just one argument, it is ''network-port'', as this is the more common case.

`(network-endpoint? `''obj''`)`

Returns `#t` if ''obj'' is a network endpoint and `#f` otherwise.

`(network-endpoint-bytevector-address `''endpoint''`)`

Returns the host specified by ''endpoint'' in bytevector format.  The value returned need not be the same as the value passed to `make-network-endpoint`, but must be acceptable to another invocation of `make-network-endpoint`.  If the host was specified as `#t`, returns `#t`.

`(network-endpoint-string-address `''endpoint''`)`

Returns the host specified by ''endpoint'' in dotted-decimal or dotted-hexadecimal format.  The value returned need not be the same as the value passed to `make-network-endpoint`, but must be acceptable to another invocation of `make-network-endpoint`.  If the host was specified as `#t`, returns `#t`.

`(network-endpoint-host `''endpoint''`)`

Returns the host specified by ''endpoint'' as a host name, or if the host name is not known or does not exist, in dotted-decimal or dotted-hexadecimal format.  The value returned need not be the same as the value passed to `make-network-endpoint`, but must be acceptable to another invocation of `make-network-endpoint`.  If the host was specified as `#t`, returns `#t`.

`(network-endpoint-port `''endpoint''`)`

Returns the network port specified by ''endpoint'' as a number.  The value returned need not be the same as the value passed to `make-network-endpoint`, but must be acceptable to another invocation of `make-network-endpoint`.
