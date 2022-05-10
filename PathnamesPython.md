This pathname library is based on the portable parts of Python's [os.path](https://docs.python.org/3/library/os.path.html) library.

## Issues

Should path objects be opaque, or is list-of-strings fine?

## Path objects

A path object belongs to a disjoint type that can be created from a Posix or Windows pathname
(a string, a bytevector, or a u16vector) and can be manipulated conveniently
and converted back into a pathname.  Every path object has a *drive* , a *root*,
and a list of *components*, all of which are likewise strings, bytevectors, or u16vectors.
The *filename* is the last component.

## Parsing

`(parse-posix-pathname `*pathname*`)`

Parses *pathname* as a Posix pathname and returns a corresponding path object,
setting the components to the slash-separated substrings of *string*.
A "slash" in this context is the character `#\/`, the byte `#2F`, or the
unsigned 16-bit value `#x002F` as the case may be.
The drive and root are set according to the following normative examples:

```
;; Absolute path
(define p (parse-posix-pathname "/etc/passwd))
(pathname-drive p) => ""
(pathname-root p) => "etc"
(pathname-components p) => ("passwd")

;; Relative path  
(define p (parse-posix-pathname "foo"))
(pathname-drive p) => ""
(pathname-root p) => ""
(pathname-componentss p) => "" ("foo")

;; Implementation-defined path (used by Cygwin for UNC paths)
(define p (parse-posix-pathname "//foo/bar/baz")
(pathname-drive p) => "//foo"
(pathname-root p) => "/"
(pathname-comopnents ("bar baz")
```

Except for the case of two initial slashes, consecutive slashes are collapsed,
and components consisting of a single period are removed.  However, components consisting of
two periods are not removed, as this would produce the wrong result in the presence of symbolic links.

`(parse-windows-pathname `*string*`)`

Parses *string* as a Windows pathname and returns a corresponding path object,
setting the components to the slash-separated or backslash-separated substrings of *string*,
where "slash" (and by the same token "backslash") has the same significance
as in the description of `parse-posix-pathname`.
The drive and root are set according to the following normative examples:

```
;; Absolute path
(define p (parse-windows-pathname "C:\\Windows)
(pathname-drive p) => "c:"
(pathname-root p) => "/"
(pathname-components p) => ("Windows")

;; UNC path
(define p (parse-windows-pathname "\\\\host\\share\\dir\\file"
(pathname-drive p) => "//host"
(pathname-root p) => "/"
(pathname-components p) => ("share" "dir" "file")

;; Current-drive-relative path
(define p (make-windows-pathname "\\Windows\\System32\\stop.exe"))
(pathname-drive p) => ""
(pathname-root p) => "/"
(pathname-components p) => ("Windows" "System32" "stop.exe")

;; Relative path
(define p parse-windows-pathname "foo"))
(pathname-drive p) => ""
(pathname-root p) => ""
(pathname-components p) => ("foo")

;; Relative-to-drive path
(define p parse-windows-pathname "C:foo"))
(pathname-drive p) => "C:"
(pathname-root p) => ""
(pathname-components p) => ("foo")
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

If any of the characters of *pathname* are illegal in Windows pathnames,
namely `< > " : | ? *`, an error satisfying `path-error?` is signaled.

## Conversion

`(posix-pathname `*pathobj* [*drive-mapper*]`)`

Returns a Posix-style pathname based on the contents of *path* using slash as the separator.
If the drive is not empty, it is passed through *drive-mapper*, a procedure
which accepts a string, bytevector, or u16vector and returns an object of the same type.
Whatever is returned will be prepended to the path.

If *drive-mapper* is omitted, the behavior is implementation-dependent.
For example, it might map `"c:"` to `"/cygdrive/c"` on Cygwin,
or to `"/mnt/c"` on Windows Subsystem for Linux,
return its argument unchanged on Windows,
or simply return the empty string, or raise an error.

`(windows-pathname `*pathobj*`)`

Returns a Windows-style string pathname based on the contents of *path* using backslash as the separator.
Slashes in the drive and root are converted to backslashes.

`(path->file-uri `*pathobj*`)`

Returns a file URI corresponding to *path*.  If *path* is not absolute, an error is signaled.
Note that in a UNC pathname, the UNC host corresponds to the URI host, so such file URIs
contain two slashes rather than three after "file:"

## Predicates

`(path-reserved? `*pathobj*`)`

Returns `#t` if any part of *path* contains a character invalid in a Windows path
(see `parse-windows-path`), or contains a component
`CON, PRN, AUX, NUL, COM`*d*, or `LPT`*d*, where d is a digit,
or any of these followed by a period and any other characters,
or ends in a period.
The comparison is case-independent.  In all other cases, returns `#f`.

`(path-absolute-posix? `*pathobj*`)`

Returns `#t` if the root is not empty, and `#f` otherwise.

`(path-absolute-windows? `*pathobj*`)`

Returns `#t` if the drive and root are not both empty, and `#f` otherwise.

`(path-portable? `*pathobj*`)`

Returns `#t` if *pathobj* represents a maximally portable path, and `#f` otherwise.
Specifically, a maximally portable path is one in which:

  * `path-reserved?` returns `#f`

  *  the drive and root are both empty strings
  
  * each component contains only lower-case letters,
    digits, and hyphens except as noted below
	
  * each component contains 1 to 8 characters except as noted below
	
  * the filename, unlike the other components, can contain a single period,
    in which case there need to be 1 to 8 characters before it and 1 to 3 characters after it.
	
The concept of a maximally portable path is based loosely on Common
Lisp logical pathnames, but is even more restrictive.

`(pathname-hidden? `*pathobj*`)`

Returns `#t` if the filename begins with a period.
   
## Accessors

`(path-parent `*pathobj*`)`

Returns a path representing the parent directory of *path*, or *path* itself if it is a root directory.

`(path-filename `*pathobj*`)`

Returns the filename of *path*, or the empty string if there are no components.

`(path-match `*pathobj glob ci?*`)`

Returns `#t` if *path* matches the glob pattern in *glob* (a path object).
Glob components (but not the drive or root) may contain the wildcards `*`, `?`, and `[...]`,
where `...` represents a set of characters to match.

 * If *glob* is relative, the path can be either relative or absolute, and matching is done from the right.
 * If *glob* is absolute, the path must be absolute, and the whole path must match.
 * If *ci?* is true, matching is done case-insensitively;
 * If it is false or missing, matching is done case-sensitively.

`(path-relative-to `*pathobj1 pathobj2*`)`

Returns a version of *path1* that is relative to *path2*.
If it is not possible to do so without introducing double-period components, `#f` is returned.

`(path-suffix `*pathobj*`)`

Returns the suffix of the filename (everything to the
right of the last period) as a string.  If there is no period
or no components, returns `#f`.
An initial period is not treated as a suffix delimiter.

Another name for the suffix is the extension.

`(path-with-suffix `*pathobj suffix*`)`

Returns a path object based on *path* with the suffix of the filename
(everything to the right of the last period) replaced by *suffix*.
If there is no period or the only period is initial,
a period followed by *suffix* is appended to the filename.

`(path-without-suffix `*pathobj suffix*`)`

Returns a path object based on *pathobj* with the suffix of the filename
(everything to the right of the last period), plus the period itself, removed.
If there is no period or the only period is initial,
the result is equal to *path*.

## Path merging

`(path-join `*basepathobj pathobj* ...`)`

If a single *pathobj* argument is given, `path-join` returns a path object
representing the results of appending
the components of the *path* elements to *basepath* in order.  However,
if the *path* argument has a non-empty drive, *path* is returned.
If the drive of *path* is empty, but the root is non-empty,
*path* is appended to the drive of *basepathobj*.

If two or more *pathobj* arguments are given, `path-join` returns
what `(path-join (path-join `*basepathobj* *pathobj1*`)` *pathobj* ...`)` returns.

`(path-with-filename `*pathobj filename*`)`

Returns a path object based on *pathobj* with the filename (replaced by *filename* (a string).
If the path does not contain a filename, an error satisfying `path-error?` is signaled.

`(path-normalize `*pathobj*`)`

Returns a path object which is the same as *path*,
except that if any component other than the first is the string 
`".."`, then that component and the preceding component are removed from the returned path object.

## Error handling

`(path-error? `*obj*`)`

Returns `#t` if *obj* is an object raised by the procedures above and `#f` otherwise.
