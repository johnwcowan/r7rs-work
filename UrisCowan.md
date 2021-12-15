# Abstract

URI references are a fundamental datatype for any sort of Internet work.
They are defined in detail by [RFC 3986](https://tools.ietf.org/html/rfc3986).
This SRFI explains how to convert a URI reference in the form of a string
into an object that is easier to interpret and manipulate.

# Issues

None at present.

# Specification

Terminological note: this SRFI refers to the objects passed or
returned in these procedures as URI objects for conciseness.
However, they can hold either a URI or a relative reference,
which are jointly known in RFC 3986 as a URI reference.
The predicates use standard terminology.

A URI object has up to 12 components arranged into the following tree:

```
whole-+-scheme
      |
      +-specific--+--authority-+--userinfo--+
      |           |            |            +--username
      |           |            |            +--password
      |           |            +--host
      |           |            +--port
      |           +--path---+
      |           +--query--+
      +--fragment
```
This means that if `specific` is not present, then none of
`authority`, `path`, and `query` can be present.
But the reverse is not true.  Consequently, a URI object is
parsed lazily when the programmer has determined that
a component should be present rather than parsing all at once.

If a component is present, its value is a string
(except as noted below);
if it is absent, its value is `#f`.

## Predicates

`(uri-object? `*obj*`)`

Returns `#t` if *obj* is a URI object, and `#f` otherwise.

`(uri-reference? `*uri-object*`)`

Returns `#t` if *uri-object* is a URI reference
as defined in [RFC 3986, Section 4.1](https://tools.ietf.org/html/rfc3986#section-4.1).

`(uri-absolute? `*uri-object*`)`

Returns `#t` if *uri-object* represents an absolute URI
as defined in [RFC 3986, Section 4.3](https://tools.ietf.org/html/rfc3986#section-4.3)

`(uri-relative-reference? `*uri-object*`)`

Returns `#t` if *uri-object* represents a relative reference
as defined in [RFC 3986, Section 4.2](https://tools.ietf.org/html/rfc3986#section-4.2).

## Constructors

`(make-uri-object `*arg* ...`)`

Returns a newly allocated URI object whose cached components are
initialized by the *args*, which are an alternation of
component names represented as symbols and component
values represented as strings (except as noted below).
All unspecified components are initialized to `#f`.

It is an error if both a component and any of its ancestral
or descendent components are passed as arguments.
For example, if `userinfo` is specified, then none of
`username`, `password`, `authority`, `specific`, and `whole` can also be specified.
It is also an error to have `password` without `username, `port` without `host`,
or `query` with neither `authority` nor `host`.
URIs that violate these rules will produce unpredictable results
if passed to any of the procedures of this SRFI.

`(string->uri-object `*string*`)`

Decodes %-escapes in *string* as follows:

 * Upper-case hex digits are replaced by lower-case ones.
 
 * Sequences of %-escapes representing
   non-ASCII characters are replaced by the corresponding
   Unicode character.
   
 * %-escapes that specify generally unreserved characters
   (that is, ASCII letters or digits, hyphen, period,
   underscore, or tilde) are replaced by the
   corresponding ASCII character.
   
 * All other %-escapes are left unchanged.

The result is then stored in the `whole` component
of a newly allocated URI object, which is returned.

## Accessors
 
 These procedures return the relevant component
 of *uri-object* as a string or number,
 caching the result in *uri-object*.
 
 If the component is `#f` but the parent component
 is not, then the parent component is parsed into all
 his child components.  If the parent component is also
 `#f`, parsing is performed recursively on the ancestors.
 Any %-escapes of characters that are not reserved
 for the component are replaced by the corresponding ASCII
 character.
 
 By the same token, if the
 component is `#f` but some of its child components
 are not, the desired component is assembled from them.
 See the tree above to understand the parent-child
 relationships. 

 If parsing fails, *uri-object* is unchanged,
 and an error satisfying `uri-error?` is signaled.
 The same is true if it is not possible to construct
 a well-formed component from its children, as when
 there is a `password` but no `username`, a `port`
 without a `host`, or a `query` without either a `path`
 or an `authority`.

`(uri-whole `*uri-object*)`
`(uri-scheme `*uri-object*)`
`(uri-specific `*uri-object*)`
`(uri-authority `*uri-object*)`
`(uri-userinfo `*uri-object*)`
`(uri-username `*uri-object*)`
`(uri-password `*uri-object*)`
`(uri-host `*uri-object*)`
`(uri-port `*uri-object*)`
`(uri-path `*uri-object*)`
`(uri-query `*uri-object*)`
`(uri-fragment `*uri-object*)`

Return the specified component of *uri-object*,
parsing as needed.  Returns `#f` if the component
is not present.

## Convenience functions

`(uri-parse-path `*uri-object*`)`

Retrieves the `path` component of *uri-object*,
breaks it on `/` characters, and returns a list of them.
The first element is `""` if the path is relative,
or "/" if it is absolute.
It is an error to mutate this list or any of its elements.
  
`(uri-parse-query `*uri-object* [*plus*]`)`

The `query` component is parsed into name-value
pairs delimited by `;` or `&`, and these are
then parsed into names and values separated by `=`.
These are then used to construct an alist
where the names are symbols and the values are string.
which is returned.
It is an error to mutate this list or any of its elements.

By no means are all query strings in this format,
so `uri-parse-query` should only be called
if the caller knows what query strings look like
in a particular application.

If the *plus* argument is true, plus signs
in values are decoded to spaces.
Any remaining escape sequences are decoded
to the corresponding ASCII character.

## URI resolution

`(uri-merge `*uri-object base-uri-object*`)`

Merges `uri-object` with `base-uri-object`
according to the rules for URI resolution
in [RFC 3986, Section 5.2](https://tools.ietf.org/html/rfc3986#section-5.2)
and returns a newly allocated URI.
Path components consisting of `.` or `..`
are normalized when possible.

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

`(uri-parse-error? `*obj*`)`

Returns `#t` if *obj* is an object raised by any of the
parsing procedures, and `#f` otherwise.

## Acknowledgements

This is partly based on
[Gauche's `rfc.uri` library](http://practical-scheme.net/gauche/man/gauche-refe/URI-parsing-and-construction.html#URI-parsing-and-construction),
with some input from the Chicken [uri-generic](http://wiki.call-cc.org/eggref/5/uri-generic) and
[uri-common](http://wiki.call-cc.org/eggref/5/uri-common) eggs.
