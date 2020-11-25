# Abstract

URI references are a fundamental datatype for any sort of Internet work.
They are defined in detail by [RFC 3986](https://tools.ietf.org/html/rfc3986).
This SRFI explains how to convert a URI reference in the form of a string
into an object that is easier to interpret and manipulate.

# Issues

 1. Should the accessor functions attempt to do sufficient parsing
    to get a value if the value is `#f`?'
  
 2. (closed)
   
 3. (closed)

# Specification

Terminological note: this SRFI refers to the objects passed or
returned in these procedures as URI objects for conciseness.
However, they can hold either a URI or a relative reference,
which are jointly known in RFC 3986 as a URI reference.
The predicates use standard terminology.

A URI object has up to 14 components arranged into the following tree:

```
whole-+-scheme
      |
      +-specific--+--authority-+--userinfo--+
      |           |            |            +--username
      |           |            |            +--password
      |           |            +--host
      |           |            +--port
      |           +--path---+
      |           |         +--path-list
      |           +--query--+
      |                     +--query-alist
      +--fragment
```
This means that if `specific` is not present, then none of
`authority`, `path`, and `query` can be present.
But the reverse is not true.  Consequently, a URI object is
parsed lazily when the programmer has determined that
a component should be present rather than all at once.

If a component is present, its value is a string
(except as noted below);
if it is absent, its value is `#f`.

## Predicates

`(uri-object? `*obj*`)`

Returns `#t` if *obj* is a URI object, and `#f` otherwise.

`(uri? `*uri-object*`)`

Returns `#t` if *uri-object* is a URI
as defined in [RFC 3986, Section ](https://tools.ietf.org/html/rfc3986#section-4.3

`(uri-absolute? `*uri-object*`)`

Returns `#t` if *uri-object* represents an absolute URI
as defined in [RFC 3986, Section 4.3](https://tools.ietf.org/html/rfc3986#section-4.3)
This implies that it represents a URI that has no `fragment` component.

`(uri-relative-reference? `*uri-object*`)`

Returns `#t` if *uri-object* represents a relative reference
as defined in [RFC 3986, Section 4.2](https://tools.ietf.org/html/rfc3986#section-4.2).

## Constructors

`(make-uri-object `*arg* ...`)`

Returns a newly allocated URI object whose components are
initialized by the *args*, which are an alternation of
component names represented as symbols and component
values represented as strings (except as noted below).
All unspecified components are initialized to `#f`.

In order to create a URI object from a URI-reference string,
specify the `whole` component only.

It is an error if both a component and any of its ancestral
or descendent components are passed as arguments.
For example, if `userinfo` is specified, then none of
`username`, `password`, `authority`, `specific`, and `whole` can also be specified.
URIs that violate these rules will produce unpredictable results
if passed to any of the procedures of this SRFI.

## Parsers
 
 These parsers decompose certain components into
 their sub-components, mutating the
 *uri-object* to reflect the results of parsing.
 If parsing fails, *uri-object* is unchanged,
 and an error satisfying `uri-error?` is signaled.

`(uri-parse-whole! `*uri-object*)`

If the `whole` component of *uri-object* is `#f`,
does nothing.
Otherwise, the component is parsed into
`scheme`, `specific`, and `fragment` components, and
*uri-object* is mutated to contain them.
All descendant components of these are set to `#f`.

In addition, %-escapes are decoded as follows
(no decoding is done in the `whole` component):

 * Any that contain lower-case hex digits are
   normalized to upper case.
   
 * Any that specify generally unreserved characters
   (that is, ASCII letters or digits, hyphen, period,
   underscore, or tilde) are decoded to the
   corresponding ASCII character.
   
 * Any that are greater than hex 80 and are followed
   by one, two, or three additional %-escapes are decoded to bytes and
   interpreted as UTF-8.  If they are valid, they are
   decoded to the corresponding Unicode character.
   Otherwise, they are left unchanged.
   
 * Any remaining %-escapes in the `scheme` or `fragment` portions
   are decoded to the corresponding ASCII character.
   
 * Any others are left unchanged.
 
`(uri-parse-specific! `*uri-object*`)`

If the `specific` component of *uri-object* is `#f`,
does nothing.
Otherwise, the component is parsed into
`authority`, `userinfo`, `username`, `password`, `host`, `port`,
`path`, `path-list`, and `query` components, and
*uri-object* is mutated to contain them.

The `path-list` component is set to a list of strings.
The first is either `""` or `"/"`, depending
on how the path begins.  The remaining components
are set to the slash-separated segments of the path.
Any %-escapes representing slashes in the individual
path components are decoded to a slash.

Returns an unspecified value.

In addition, %-escapes are decoded as follows:

 * In the `authority` part, all except those
   representing at-sign and colon are decoded
   to the corresponding ASCII character.
   
 * In the `path` part, all except those representing
   slashes are decoded
   to the corresponding ASCII character.
   This must be done before parsing the
   path into separate `path-list` components.
   
 * In the `authority` part, all except those
   representing colon and at-sign are decoded
   to the corresponding ASCII character.
   This must be done before parsing into
   the `userinfo`, `host`, and `port` components.
   
 * In the `userinfo` part, all except those
   representing colon are decoded
   to the corresponding ASCII character.
   This must be done before parsing into
   the `username` and `password` components.
   
 * In the `username`, `password`, `host`, and `part`
   components, all are decoded
   to the corresponding ASCII character.
   
 * Any others are left unchanged.

`(uri-parse-query! `*uri-object* [*plus*]`)`

The `query` component is parsed into name-value
pairs delimited by `;` or `&`, and these are
then parsed into names and values separated by `=`.
These are then used to construct an alist,
and the `query-alist` components is mutated to contain it.

Returns an unspecified value.

By no means are all query strings in this format,
so `uri-parse-query!` should only be called
if the caller knows what query strings look like
in a particular application.

Any remaining escape sequences are decoded
to the corresponding ASCII character.
If the *plus* argument is true, plus signs
in values are normalized to spaces.

# Accessors

`(uri->string `*uri-object*`)`

Returns the `whole` component of *uri-object*,
but if this is `#f`, assembles a string
from the other components and returns that.

`(uri-scheme `*uri-object*`)`  
`(uri-specific `*uri-object*`)`  
`(uri-authority `*uri-object*`)`  
`(uri-userinfo `*uri-object*`)`  
`(uri-username `*uri-object*`)`  
`(uri-password `*uri-object*`)`  
`(uri-host `*uri-object*`)`  
`(uri-port `*uri-object*`)`  
`(uri-path `*uri-object*`)`  
`(uri-path-list `*uri-object*`)`  
`(uri-query `*uri-object*`)`  
`(uri-query-list `*uri-object*`)`  
`(uri-fragment `*uri-object*`)`

Extracts the specified component from *uri-object*
as a string if present, and as `#f` if not present.

## Updaters

`(uri-set-scheme `*uri-object string*`)`  
`(uri-set-specific `*uri-object string*`)`  
`(uri-set-authority `*uri-object string*`)`  
`(uri-set-userinfo `*uri-object string*`)`  
`(uri-set-username `*uri-object string*`)`  
`(uri-set-password `*uri-object string*`)`  
`(uri-set-host `*uri-object string*`)`  
`(uri-set-portet `*uri-object string*`)`  
`(uri-set-pathet `*uri-object string*`)`  
`(uri-set-path-list `*uri-object list*`)`  
`(uri-set-query `*uri-object string*`)`  
`(uri-set-query-alist `*uri-object alist*`)`  
`(uri-set-fragment `*uri-object*`)`


These procedures return a new URI in which all components
are equal to the corresponding components of *uri-object*,
except that the specified component is set to *new-value*
and all descendant components are set to `#f`.

## URI resolution

`(uri-merge `*uri-object base-uri-object*`)`

Merges `uri-object` with `base-uri-object`
according to the rules for URI resolution
in [RFC 3986, Section 5.2](https://tools.ietf.org/html/rfc3986#section-5.2)
and returns a newly allocated URI.
Members of `path-list` containing `.` or `..`
are normalized.

## Data URIs

`(uri-parse-data `*uri-object*`)`

If the `scheme` component of *uri-object*
is not `"data"`, returns two values, both `#f`.
Otherwise, returns a media type as the first value
and decoded data as the second value, according
to the rules of [RFC 2397](https://tools.ietf.org/html/rfc2397).
If the media type begins with `text`, the decoded data
is a string according to the specified encoding;
otherwise, it is a bytevector.

If the parse fails, an error satisfying `uri-error?` is signaled.

## Exceptions

`(uri-parse-error `*obj*`)`

Returns `#t` if *obj* is an object raised by any of the
parsing procedures, and `#f` otherwise.

## Acknowledgements

This is mostly based on
[Gauche's `rfc.uri` library](http://practical-scheme.net/gauche/man/gauche-refe/URI-parsing-and-construction.html#URI-parsing-and-construction),
with some input from the Chicken [uri-generic](http://wiki.call-cc.org/eggref/5/uri-generic) and
[uri-common](http://wiki.call-cc.org/eggref/5/uri-common) eggs.
