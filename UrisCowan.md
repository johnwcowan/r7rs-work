# Abstract

URIs are a fundamental datatype for any sort of Internet work.
They are defined in detail by [RFC 3986](https://tools.ietf.org/html/rfc3986).
This SRFI explains how to convert a URI in the form of a string
into an object that is easier to interpret and manipulate.

# Issues

 1. Should the accessor functions attempt to do sufficient parsing
    to get a value if the value is `#f`?'
  
 2. Should `parse-specific!` do `parse-authority` too?
    In principle, some authorities might not take the usual
    username-password-host-port form, but it seems unlikely
    that happens in the Real World.

# Specification

A URI object has up to 14 components arranged into the following tree:

```
uri-+-scheme
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
But the converse is not true.  Consequently, a URI is
parsed lazily when the programmer has determined that
a component is present rather than all at once.

If a component is present, its value is a string
(except as noted below);
if it is absent, its value is `#f`.

## Predicates

`(uri-object? `*obj*`)`

Returns `#t` if *obj* is a URI object, and `#f` otherwise.

`(uri-absolute? `*uri-object*`)`

Returns `#t` if *uri-object* represents an absolute URI;
that is, if the `fragment` component is `#f`.

`(uri-relative? `*uri-object*`)`

Returns `#t` if *uri-object* represents a relative reference
as defined in RFC 3986.

## Constructors

`(make-uri `*arg* ...`)`

Returns a newly allocated URI object whose components are
initialized by the *args*, which are an alternation of
component names represented as symbols and component
values represented as strings.  All unspecified components
are initialized to `#f`.

It is an error if both a component and any of its ancestral components
are passed as arguments.  For example, if either of `username`
or `password` is specified, then none of
`userinfo`, `authority`, and `specific` can be specified.
URIs that violate these rules will produce unpredictable results
if any of the following procedures are passed to them.

`(parse-uri `*string*`)`

Returns a newly allocated URI object that results from parsing *string*.
The `uri` component is set to *string*`.
The `scheme`, `specific`, and `fragment` components are
set to a string if they are present in *string*.
All other components are initialized to `#f`.

In addition, %-escapes are decoded as follows:

 * Any that contain lower-case hex digits are
   normalized to upper case.
   
 * Any that specify generally unreserved characters
   (that is, ASCII letters or digits, hyphen, period,
   underscore, or tilde) are normalized to the
   corresponding ASCII character.
   
 * Any that are greater than hex 80 and are followed
   by one, two, or three %-escapes are decoded to bytes and
   interpreted as UTF-8.  If they are valid, they are
   normalized to the corresponding Unicode character.
   Otherwise, they are left unchanged.
   
 * Any remaining %-escapes in the `scheme` or `fragment` portions
   are converted to the corresponding ASCII character.
   
 * Any others are left unchanged.
   
`(uri-parse-specific! `*uri-object*`)`

If the `specific` component of *uri-object* is `#f`,
does nothing.
Otherwise, the component is parsed into
`authority`, `path`, `path-list`, and `query` components, and
*uri-object* is mutated to contain them.

The `path-list` component is set to a list of strings.
The first is either `""` or `"/"` or `"//", depending
on how the path begins.  The remaining components
are set to the slash-separated segments of the path.
Any %-escapes representing slashes in the individual
path components are normalized to a slash.

Returns an unspecified value.

In addition, %-escapes are decoded as follows:

 * In the `authority` part, all except those
   representing at-sign and colon are normalized
   to the corresponding ASCII character.
   
 * In the `path` part, all except those representing
   slashes are normalized
   to the corresponding ASCII character.
   This must be done before parsing the
   path into separate `path-list` components.
   
 * Any others are left unchanged.

`(uri-parse-authority! `*uri-object*`)`

If the `authority` component of *uri-object* is `#f`,
does nothing.
Otherwise, the component is parsed into
`userinfo`, `username, `password`, `host`, and `port` components, and
*uri-object* is mutated to contain them.
Returns an unspecified value.

In addition, %-escapes are decoded as follows:

 * In the `userinfo` part, all except those
   representing colon are normalized
   to the corresponding ASCII character.
   This must be done before parsing into
   the `username` and `password` components.
   
 * In the `host` part, all except those
   representing colon are normalized
   to the corresponding ASCII character.
   
 * In the `port` part, all are normalized
   to the corresponding ASCII character.
   
 * Any others are left unchanged.

`(uri-parse-query! `*uri-object* [*plus*]`)`

The `query` component is parsed into name-value
pairs delimited by `;` or `&`, and these are
then parsed into names and values separated by `=`.
If the `query`
component does not follow this pattern or is `#f`,
the `query-alist` component is set to `#f`.
Returns an unspecified value.

By no means are all query strings in this format,
so `uri-parse-query` should only be called
if the caller knows what query strings look like
in a particular application.

Any remaining escape sequences are normalized
to the corresponding ASCII character.
If the *plus* argument is true, plus signs
in values are normalized to spaces.

# Accessors

`uri-ref`, `uri-scheme-ref`, ..., `uri-fragment-ref`.

## Updaters

These procedures return a new URI in which all components
are equal to *uri-object*, except that the specified component
is set to *new-value* and all descendant components are
set to `#f`.

`uri-scheme-set`, ..., `uri-fragment-set`.

## URI resolution

`(uri-merge `*uri-object base-uri-object*`)`

Merges `uri-object` with `base-uri-object`
according to the rules for URI resolution
in RFC 3986 and returns a newly allocated URI.
The members of `path-list` containing `.` or `..`
are normalized.

## Data URIs

`(uri-parse-data `*uri-object*`)`

If the `scheme` component of *uri-object*
is not `"data"`, returns two values, both `#f`.
Otherwise, returns a media type as the first value
and decoded data as the second value.  If the
media type begins with `text`, the decoded data
is a string according to the specified encoding;
otherwise, it is a bytevector.
