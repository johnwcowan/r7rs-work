## Path objects

Path objects represent filesystem pathnames with semantics appropriate for different operating systems.  This proposal is essentially a straight transliteration into Scheme of the pure portion of the Python [pathlib](http://docs.python.org/3/library/pathlib.html) library (also documented in [PEP 428](http://legacy.python.org/dev/peps/pep-0428)).  As a result, it does not provide any operations on the file system; convert the path object to a string to pass it to `open-input-file` or any other procedure expecting a pathname.

Path objects are immutable objects of a disjoint type.  They are divided into two subtypes, Posix paths and Windows paths.  Most procedures in this proposal operate correctly on both subtypes.

Every path object has a *drive*, a *root*, and a possibly empty sequence of *components*, all of which are strings.  The *anchor* of a pathname is the drive concatenated with the root.  The *filename* is the last component, and the *suffix* is the part of the filename after the rightmost period.

## Constructors

`(make-posix-path `*string*`)`

Parses *string* as a Posix pathname and returns a corresponding path object, setting the components to the slash-separated substrings of *string*.  The drive and root are set according to the following examples:

```
(define absolute (make-posix-path "/etc/passwd))
(path-drive absolute) => ""
(path-root absolute) => "/"

(define relative (make-posix-path "foo"))
(path-drive relative) => ""
(path-root relative) => ""

(define implementation-defined (make-posix-path "//foo/bar"))
(path-drive implementation-defined) => ""
(path-root implementation-defined) => "//"
```

Except for the case of two initial slashes, consecutive slashes and backslashes are collapsed, and components consisting of a single period are removed.  However, components consisting of two periods are not removed, as this would produce the wrong result in the presence of symbolic links.


`(make-windows-path `*string*`)`

Parses *string* as a Windows pathname and returns a corresponding path object, setting the components to the slash-separated or backslash-separated substrings of *string*.  The drive and root are set according to the following examples:

```
(define absolute (make-windows-path "C:\\Windows))
(path-drive absolute) => "C:"
(path-root absolute) => "/"

(define unc (make-windows-path "\\\\host\\share\\file"))
(path-drive unc) => "//host/share"
(path-root unc) => "/"

(define current-drive-relative (make-windows-path "\\Windows\\System32"))
(path-drive current-drive-relative) => ""
(path-root current-drive-relative) => "/"

(define relative (make-windows-path "foo"))
(path-drive relative) => ""
(path-root relative) => ""

(define relative-with-drive (make-windows-path "C:foo"))
(path-drive relative-with-drive) => "C:"
(path-root relative-with-drive) => ""
```

The last format is meaningful because Windows maintains a separate current directory
on each drive, so `"C:foo"` means the file named `foo` relative to the current directory on drive `C:`, regardless of whether that is the current drive.

Consecutive slashes or backslashes are collapsed, and components consisting of a single period are removed.  However, components consisting of two periods are not removed, as this would produce the wrong result in the presence of symbolic links.

If any of the characters of *string* are illegal in Windows pathnames, namely `< > " : | ? *`, an error is signaled.

`(path `*windows? device root component* ...`)`

Returns a path object with the specified *device*, *root*, and *components*.  If *windows?* is true, a Windows path object is returned; otherwise, a Posix path object is returned.

## Predicates

`(path? `*obj*`)`

Returns `#t` if *obj* is a path object and `#f` otherwise.

`(posix-path? `*obj*`)`

Returns `#t` if *obj* is a Posix path object and `#f` otherwise.

`(windows-path? `*obj*`)`

Returns `#t` if *obj* is a Windows path object and `#f` otherwise.

`(path-absolute? `*path*`)`

Returns `#t` if *path* represents an absolute path and `#f` otherwise.  On Posix, a path is absolute if its root is non-empty.  On Windows, a path is absolute if the anchor is non-empty.

`(path-reserved? `*path*`)`

Returns `#t` if *path* is a Windows path object containing any of the reserved components `CON PRN AUX NUL COM1 COM2 COM3 COM4 COM5 COM6 COM7 COM8 COM9 LPT1 LPT2 LPT3 LPT4 LPT5 LPT6 LPT7 LPT8 LPT9` [from this list](http://msdn.microsoft.com/en-us/library/aa365247.aspx) with or without a suffix, and `#f` otherwise.  Posix path objects always return `#f`.

`(path=? `*path,,1,, path,,2,,*`)`

Returns `#t` if *path,,1,,* and *path,,2,,* represent the same path; that is, the drive, root, and components are equal.  Posix paths are compared case-sensitively; Windows paths are compared case-insensitively.  It is implementation-defined whether a Windows path is ever `path=?` to a Posix path; for example, a Cygwin implementation might choose to treat `(make-posix-path "/cygdrive/c/Windows")` and `(make-windows-path "C:\\Windows")` as `path=?`.  In all other cases, `path=?` returns `#f`.

## Accessors

`(path-drive `*path*`)`

Returns the drive part of *path*.

`(path-root `*path*`)`

Returns the root part of *path*.

`(path-anchor `*path*`)`

Returns the drive and root parts of *path* concatenated.

`(path-parts `*path*`)`

Returns a list whose car is the anchor of *path* and whose cdr is a list of the components of *path*.

`(path-suffix `*path*`)`

Returns the suffix (the rightmost part after a period) of the filename of *path*, or the empty string if there are no periods.

`(path-suffixes `*path*`)`

Returns a list of all the suffixes of the filename of *path*.

## Conversion

`(path->string `*path*`)`

Returns a string pathname based on the contents of *path*.  Windows paths use backslash as the separator.

`(path->slashed-string `*path*`)`

Returns a string pathname based on the contents of *path*.  Windows paths use slash as the separator.

`(path->file-uri `*path*`)`

Returns a file URI corresponding to *path*.  If *path* is not absolute, an error is signaled.

## Path operations

`(path-parent `*path*`)`

Returns a path representing the parent directory of *path*, or *path* itself if it is a root directory.

`(path-filename `*path*`)`

Returns the filename (last component) of *path*, or the empty string if there are no components.

`(path-stem `*path*`)`

Returns the filename of *path* with all suffixes removed.

`(path-join `path string-or-path* ...`)`

If a single *string-or-path* argument is given, `path-join` returns a path object representing the results of appending the components of the *string-or-path* arguments to *path* in order.  If the *string-or-path* argument is a string, `path-join` behaves as if it was converted to a path first by `make-posix-path` or `make-windows-path`, depending on the subtype of *path*.

If the *string-or-path* argument has a non-empty drive, the drive, root, and components of *path* are discarded.  If the drive is empty, but the root is non-empty, the root and components of *path* are discarded.

If two *string-or-path* arguments are given, `path-join` returns what `(path-join (path-join `*path* *string-or-path,,1,,*`)` *string-or-path,,2,,*`)` returns, and so on for any additional arguments.

`(path-match `*path glob*`)`

Returns `#t` if *path* matches the glob pattern in *glob* (a path object).  Glob paths may contain the wildcards `*`, `?`, and `[...]` where `...` represents a set of characters to match.  If *glob* is relative, the path can be either relative or absolute, and matching is done from the right.  If *glob* is absolute, the path must be absolute, and the whole path must match.  Posix paths are compared case-sensitively; Windows paths are compared case-insensitively.  It is an error to attempt to match a Windows path object against a Posix glob pattern or vice versa.

`(path-relative-to `*path,,1,, path,,2,,*`)`

Returns a version of *path,,1,,* that is relative to *path,,2,,*.  If it is not possible to do so without introducing double-period components, `#f` is returned.

`(path-with-filename `*path filename*`)`

Returns a path object based on *path* with the filename (including any suffixes) replaced by *filename* (a string).  If the path does not contain a filename, an error is signaled.

`(path-with-suffix `*path suffix*`)`

Returns a path object based on *path* with the final suffix of the filename replaced by *suffix*.  If the filename does not contain a suffix, *suffix* is appended after a separating period.  If the path does not contain a filename, `#f` is returned.

`(path-normalize `*path*`)`

Returns a path object which is the same as *path*, except that if any component other than the first is the string `".."`, then that component and the preceding component are removed from the returned path object.

`(path-without-suffix `*path*`)`

Returns a path object which is the same as *path*, except that if the filename ends in a suffix, that suffix is removed from the returned path object.

`(path-without-suffixes `*path*`)`

Returns a path object which is the same as *path*, except that if the filename ends in one or more suffixes, they are removed from the returned path object.


