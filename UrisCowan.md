# Abstract

URIs are a fundamental datatype for any sort of Internet work.
They are defined in detail by [RFC 3986](https://tools.ietf.org/html/rfc3986).
This SRFI explains how to convert a URI in the form of a string
into an object that is easier to interpret and manipulate.

# Issues

 1. Should the accessor functions attempt to do sufficient parsing
    to get a value if the value is `#f`?'
  
 2. Should `parse-specific!` do `parse-authority!` too?
    In principle, some authorities might not take the usual
    username-password-host-port form, but it seems unlikely
    that happens in the Real World.
   
 3. Should Punycode decoding
    (see [RFC 3492](https://www.rfc-editor.org/rfc/rfc3492.txt))
    be done in `parse-authority!`?

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

Returns `#t` if *uri-object* represents an absolute URI
as defined in [RFC 3986, Section 4.3](https://tools.ietf.org/html/rfc3986#section-4.3)
This implies that it has at
least a scheme and an authority and definitely does not
have a fragment.

`(uri-relative? `*uri-object*`)`

Returns `#t` if *uri-object* represents a relative reference
as defined in [RFC 3986, Section 4.2](https://tools.ietf.org/html/rfc3986#section-4.2).

## Constructors

`(make-uri `*arg* ...`)`

Returns a newly allocated URI object whose components are
initialized by the *args*, which are an alternation of
component names represented as symbols and component
values represented as strings.  All unspecified components
are initialized to `#f`.

In order to create a URI object from a string, specify
the `uri` component only.

It is an error if both a component and any of its ancestral
or descendent components
are passed as arguments.
For example, if `userinfo` is specified, then none of
`username`, `password`, `authority`, `specific`, and `uri` can also be specified.
URIs that violate these rules will produce unpredictable results
if passed to any of the procedures of this SRFI.

## Parsers
 
 These parsers decompose certain components into
 their sub-components, mutating the
 *uri-object*.
   
`(uri-parse! `*uri-object*)`

If the `uri` component of *uri-object* is `#f`,
does nothing.
Otherwise, the component is parsed into
`scheme`, `specific`, and `fragment` components, and
*uri-object* is mutated to contain them.

In addition, %-escapes are decoded as follows
(no decoding is done in the `uri` component):

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
`authority`, `path`, `path-list`, and `query` components, and
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
   representing colon are decoded
   to the corresponding ASCII character.
   This must be done before parsing into
   the `username` and `password` components.
   
 * In the `host` part, all except those
   representing colon are decoded
   to the corresponding ASCII character.
   
 * In the `port` part, all are decoded
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

Any remaining escape sequences are decoded
to the corresponding ASCII character.
If the *plus* argument is true, plus signs
in values are normalized to spaces.

# Accessors

`(uri->string `*uri-object*`)`

Returns the `uri` component of *uri-object*.
Note that this will be `#f` unless *uri-object*
was created with `(make-uri 'uri some-string)`
or with `(parse-uri some-string)`.

`(uri-scheme `*url-object*`)`  
`(uri-specific `*url-object*`)`  
`(uri-authority `*url-object*`)`  
`(uri-userinfo `*url-object*`)`  
`(uri-username `*url-object*`)`  
`(uri-password `*url-object*`)`  
`(uri-host `*url-object*`)`  
`(uri-port `*url-object*`)`  
`(uri-path `*url-object*`)`  
`(uri-path-list `*url-object*`)`  
`(uri-query `*url-object*`)`  
`(uri-query-list `*url-object*`)`  
`(uri-fragment `*url-object*`)`

Extracts the specified component from *uri-object*
as a string if present, and as `#f` if not present.

## Updaters

`(uri-scheme-set `*url-object string*`)`  
`(uri-specific-set `*url-object string*`)`  
`(uri-authority-set `*url-object string*`)`  
`(uri-userinfo-set `*url-object string*`)`  
`(uri-username-set `*url-object string*`)`  
`(uri-password-set `*url-object string*`)`  
`(uri-host-set `*url-object string*`)`  
`(uri-port-set `*url-object string*`)`  
`(uri-path-set `*url-object string*`)`  
`(uri-path-list-set `*url-object list*`)`  
`(uri-query-set `*url-object string*`)`  
`(uri-query-list-set `*url-object alist*`)`  
`(uri-fragment-set `*url-object*`)`


These procedures return a new URI in which all components
are equal to *uri-object*, except that the specified component
is set to *new-value* and all descendant components are
set to `#f`.

## URI resolution

`(uri-merge `*uri-object base-uri-object*`)`

Merges `uri-object` with `base-uri-object`
according to the rules for URI resolution
in [RFC 3986, Section 5.2](https://tools.ietf.org/html/rfc3986#section-5.2)
 and returns a newly allocated URI.
The members of `path-list` containing `.` or `..`
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

## Acknowledgements

This is mostly based on
[Gauche's `rfc.uri` library](http://practical-scheme.net/gauche/man/gauche-refe/URI-parsing-and-construction.html#URI-parsing-and-construction),
with some input from the Chicken [uri-generic](http://wiki.call-cc.org/eggref/5/uri-generic) and
[uri-common](http://wiki.call-cc.org/eggref/5/uri-common) eggs.
