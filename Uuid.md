[RFC 4122](https://tools.ietf.org/html/rfc4122) UUIDs are represented by a disjoint type.

## Predicates

`(uuid? `*obj*`)`

Return `#t` if *obj* is a UUID, and `#f` otherwise.

## Constructors

Note:  UUIDs of versions 1 and 2 leak information about the creating machine's MAC address
and so have bad privacy implications.  UUIDs of version 3 use the broken MD5 hash scheme.
Therefore, this SRFI does not provide methods of creating them.
Use version 4 instead of versions 1 or 2, and version 5 instead of version 3.

`(make-random-uuid)`

Returns a random version 4 UUID.
The source of randomness used should be cryptographically strong.

`(make-relative-uuid `*namespace-uuid name*`)`

Returns a version 5 UUID generated non-randomly from *namespace-uuid*,
which can be any UUID, and *name*, which is either a bytevector of
arbitrary length or a string of arbitrary length.  Strings are converted
to bytevectors using UTF-8 encoding.

`dns-namespace-uuid`

Constant whose value is the UUID `6ba7b810-9dad-11d1-80b4-00c04fd430c8`,
to be used to create version 5 UUIDs where *name* is a
fully qualified domain name.

`url-namespace-uuid`

Constant whose value is the UUID `6ba7b811-9dad-11d1-80b4-00c04fd430c8`,
to be used to create version 5 UUIDs where *name* is an
absolute URL, URI, or IRI.

`oid-namespace-uuid`

Constant whose value is the UUID `6ba7b812-9dad-11d1-80b4-00c04fd430c8`,
to be used to create version 5 UUIDs where *name* is a
ITU X.660 object identifier with either integer or non-integer labels.

`x500-namespace-uuid`

Constant whose value is the UUID `6ba7b814-9dad-11d1-80b4-00c04fd430c8`,
to be used to create version 5 UUIDs where *name* is a
X.500 distinguished name in either DER format or text format.

`nil-uuid`
Constant whose value is the UUID `00000000-0000-0000-0000-0000000000000`,
representing the absence of a UUID.  It is inadvisable to use this UUID
as a namespace UUID.

## Accessors

`(uuid-version `*uuid*`)`

Return the version of *uuid* as an exact integer from 0 to 15 inclusive.
Normally 0 is returned only for the nil UUID.  If the UUID is not an
RFC 4122 variant, the result is unspecified.

## Conversion

`(uuid->string `*uuid*`)`

Returns a newly allocated string representing *uuid* in the canonical format
`xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`, where each `x` is an
uppercase hex digit.

To generate a corresponding URN, prepend "urn:uuid:" to the result.

`(string->uuid `*string*`)`

Returns the UUID that results from parsing *string*.  In
addition to canonical format, hyphens can be omitted and
lowercase hex digits used.  Any other string returns `#f`.

`(uuid->bytevector `*uuid*`)`
Returns a newly allocated bytevector representation of *uuid*.
The length of the result is always 16 bytes.

`(bytevector->uuid `*bytevector*`)`

Returns the UUID specified by *bytevector*.
It is an error if *bytevector* is not 16 bytes long.

## Implementation

See [Gōran Weinholt's library](https://github.com/weinholt/uuid) as a basis.

