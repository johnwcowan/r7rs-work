## Specification

`(write-log `*message* *dictionary*`)`

Write *message* to the syslog using entries in *dictionary* to format it.
*Message* is a string containing any Unicode characters except the ASCII control
characters.


`(open-log ` *transport dictionary*`)`

Initializes the logging system; if not called, the first call to `write-log` implies it.

*Transport* specifies the log transport to be used, with `#t` meaning the system default.
Other possibilities are `udp`, `tcp`, `tls`.
(The key `windows-event-system` might be possibility,
though it is designed for C++ and hairy beyond belief).
or `#t` if the implementation default is fine.
In addition, `err` causes the output to be sent to the
port that is the value of `(current-error-port)`,
but in a slightly different format (see below).

*Dictionary* contains key-value pairs; the pairs passed to `open-log` provide defaults
for those not passed to `write-log`.
Typically `application` and `application-type` (see below)
would be set in `open-log`.

## Standard dictionary keys

`application`: a string containing the name of the current application.
Defaults to `(car (command-line))`.  ASCII only, no controls, no spaces.

`application-type`: an open set of symbols, including at least
`client`, `server`, `auth` (anything security related),
`auth-priv` (anything that might contain a credential
and needs to go into a log only special people can read),
and `default` (= syslog local7). ASCII only, no controls, no spaces.

`message-type`: any symbol representing the general type of this message.
ASCII only, no control characters, no spaces.

`severity`: the closed set of symbols
`emergency`, `alert`, `critical`, `error`, `warning`, `notice`, `info`, `debug`.
The default is `info`.

The reason it's closed is that it represents decreasing priority of importance,
and if the caller uses some other symbol we won't know how to prioritize the message.
So if the value is unknown, change it to `error`.

## Wire protocol

Each log is packed into a UDP packet and set to port 514,
typically but not always on localhost.  No ack is expected.
If the port is unreachable, use stderr instead.  It is also possible
to make a TCP connection to port 514 or a TLS connection to port 6514.
Alternatively the Posix logging calls can be used through an FFI.

Here are the fields of the packet, separated by a single space:

  * Message length in bytes using ASCII digits (not used for the UDP transport)
  * Priority: see below
  * ISO 8601 timestamp
  * Sending host, max 48 ASCII characters:  fully qualified domain name,
    hostname, IP address, or "-" if completely unknown.
  * Application name, max 48 ASCII characters; "-" if not known at all
  * Process id, max 128 ASCII digits or anything else, or "-" if not known at all
  * Message type: max 32 ASCII characters
  * Reserved field, value is "-"
  * Message in UTF-8

The priority field is a decimal number expressed in ASCII, like `<nnn>`,
where `nnn` is 1-3 ASCII digits without leading 0s.
Note that the angle brackets are literal.
These represent the `severity` as a numeric value 0 to 7
where 0 is `emergency`,
plus the following values for `application-type`:

  * 1*8 for client
  * 3*8 for server
  * 4*8 for auth
  * 10*8 for auth-priv
  * 23*8 for default
  
However, when writing to the `err` transport, the priority value is replaced
by the severity as a capitalized string and the application type as a
lower-case string, separated by a space character.
This format is easier for humans to read.

Host should be truncated from the left if necessary, but other fields from the right.
