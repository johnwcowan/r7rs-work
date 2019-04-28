## Path objects

A path object is a list of strings which can be created from a Posix or Windows pathname and can be manipulated conveniently
and converted back into a pathname.  Every path object has a *drive* (the first element), a *root*
(the second element), and a possibly empty sequence of *components* (the remaining elements),
all of which are strings.  The *filename* is the last component

## Parsing

`(parse-posix-path `*string*`)`

Parses *string* as a Posix pathname and returns a corresponding path object, setting the components
to the slash-separated substrings of *string*.  The drive and root are set according to the following examples:

```
;; Absolute path
(parse-posix-path "/etc/passwd) => ("" "/" "etc" "passwd"

;; Relative path
(parse-posix-path "foo") => ("" "" "foo")

;; Implementation-defined path
(parse-posix-path "//foo/bar") => ("" "//" "foo" "bar")
```

Except for the case of two initial slashes, consecutive slashes are collapsed,
and components consisting of a single period are removed.  However, components consisting of
two periods are not removed, as this would produce the wrong result in the presence of symbolic links.

`(parse-windows-path `*string*`)`

Parses *string* as a Windows pathname and returns a corresponding path object,
setting the components to the slash-separated or backslash-separated substrings of *string*.
The drive and root are set according to the following examples:

```
;; Absolute path
(parse-windows-path "C:\\Windows) => ("c:" "/" "Windows")

;; UNC path
(parse-windows-path "\\\\host\\share\\dir\\file") => ("//host/share" "/" "dir" "file")

;; Current-drive-relative path
(make-windows-path "\\Windows\\System32\stop.exe") => "" "/" "Windows" "System32" "stop.exe")

;; Relative path
(parse-windows-path "foo") => ("" "" "foo")

;; Relative-to-drive path
(parse-windows-path "C:foo")) => ("C:" "" "foo")
```

The last format is meaningful because Windows maintains a separate current directory
on each drive, so `"C:foo"` means the file named
`foo` relative to the current directory on drive `C:`,
regardless of whether that is the current drive.

All backslashes are converted to slashes,
consecutive slashes are collapsed
except for two initial slashes,
drives ending in a colon are downcased,
and components consisting of a single period are removed.
However, components consisting of two periods are not removed,
as this would produce the wrong result in the presence of symbolic links.

If any of the characters of *string* are illegal in Windows pathnames,
namely `< > " : | ? *`, an error satisfying `path-error?` is signaled.

## Conversion

`(posix-path->pathname `*path*`)`

Returns a Posix pathname based on the contents of *path* using slash as the separator.
If *path* is an absolute Windows path, the result is implementation-dependent.

`(windows-path->pathname `*path*`)`

Returns a string pathname based on the contents of *path* using backslash as the separator.

`(path->file-uri `*path*`)`

Returns a file URI corresponding to *path*.  If *path* is not absolute, an error is signaled.
Note that in a UNC pathname, the UNC host corresponds to the URI host, so such file URIs
begin with two slashes rather than three.

## Path operations

`(path-reserved? `*path*`)`

Returns `#t` if any part of *path* contains a character invalid in a Windows path
(see `parse-windows-path`), or contains a component
`CON, PRN, AUX, NUL, COM`*d*, or `LPT`*d*, where d is a digit,
or any of these followed by a period and any other characters.
The comparison is case-independent.  In all other cases, returns `#f`.

`(path-parent `*path*`)`

Returns a path representing the parent directory of *path*, or *path* itself if it is a root directory.

`(path-filename `*path*`)`

Returns the filename (last component) of *path*, or the empty string if there are no components.

`(path-join `*basepath path* ...`)`

If a single *path* argument is given, `path-join` returns a path object representing the results of appending
the components of the *path* elements to *basepath* in order.  However,
if the *path* argument has a non-empty drive, *path* is returned.
If the drive of *path* is empty, but the root is non-empty, *path* is appended to the drive of *basepath*.

If two or more *path* arguments are given, `path-join` returns
what `(path-join (path-join `*basepath* *path1*`)` *path* ...`)` returns.

`(path-match `*path glob ci?*`)`

Returns `#t` if *path* matches the glob pattern in *glob* (a path object).
Glob paths may contain the wildcards `*`, `?`, and `[...]` where `...` represents a set of characters to match.
If *glob* is relative, the path can be either relative or absolute, and matching is done from the right.
If *glob* is absolute, the path must be absolute, and the whole path must match.
If *ci?* is true, matching is done case-insensitively;
if it is false or missing, matching is done case-sensitively.

`(path-relative-to `*path1 path1*`)`

Returns a version of *path1* that is relative to *path2*.
If it is not possible to do so without introducing double-period components, `#f` is returned.

`(path-with-filename `*path filename*`)`

Returns a path object based on *path* with the filename (replaced by *filename* (a string).
If the path does not contain a filename, an error satisfying `path-error?` is signaled.

`(path-with-suffix `*path suffix*`)`

Returns a path object based on *path* with the suffix of the filename (everything to the
right of the final period replaced by *suffix*.  If there is no period, a period followed
by *suffix* is appended to the filename.

`(path-normalize `*path*`)`

Returns a path object which is the same as *path*, except that if any component other than the first is the string 
`".."`, then that component and the preceding component are removed from the returned path object.

`(path-error? `*obj*`)`

Returns `#t` if *obj* is an object raised by the procedures above and `#f` otherwise.
