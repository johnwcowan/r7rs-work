This is a proposed repository format for R7RS.  It is intended to address the major issues and leave room for growth.  The idea is that anyone can provide their own repository, and implementations can write their own tools (or modify existing ones) to download packages from a repo. 

There will be a central repo and a common tool which will aim to support all R7RS 
implementations.

{{{
;; A minimal repository describing a single package containing two
;; libraries.  The tarball must expand to a single directory, and the
;; paths, pointing to files containing a single `define-library' form,
;; are relative to that directory.
(repository
 (package
  (url "http://www.wonderland.org/repo/wonderland.tgz")
  (library
   (name (wonderland cheshire cat))
   (path "cheshire/cat.pkg"))
  (library
   (name (wonderland mad hatter))
   (path "mad/hatter.pkg"))))

;; The following is the same repository with all options described.
(repository

 ;; Descriptive time of how often this repository should automatically
 ;; be refreshed.  Tools should honor this recommendation, but may
 ;; override it in response to explicit update requests.  The duration
 ;; is expressed as an amount and a unit of time, which defaults to
 ;; `day'.  Other valid units are: second, minute, hour, week, month,
 ;; year.
 (refresh (amount 1) (unit-of-time day))

 ;; A sibling is a pointer to another repository, which the client may
 ;; choose to search as well.  The URL points to a file containing
 ;; another `repository' sexp.  The optional trust level is a real
 ;; number in [0..1] indicating the current repository's trust in the
 ;; sibling - the user's trust level is the product of all trust
 ;; levels along the shortest path to a source.  The default is 0.5.
 ;;
 ;; Siblings may be used to build a distributed network of
 ;; repositories, to split the current repository into several URLs
 ;; for performance (e.g. moving seldom changing packages or publisher
 ;; lists to a repo with a longer refresh), or to provide translated
 ;; versions of the repo.  Clients are free to ignore siblings and/or
 ;; maintain their own lists of repositories.
 (sibling
  (name "Some Other Repo")
  (url "http://some-other-repository.org/packages.scm"))
 (sibling
  (name "My Translations")
  ;; The URL can be customized per locale as discussed below.
  (url
   (xx "http://yet-another-repository.co.xx/scheme/translated/packages.scm")))

 ;; A package is a collection of one or more related libraries that
 ;; are installed together.  Any library-specified properties can be
 ;; included at the package level, and apply to all contained
 ;; libraries.  Note multiple entries for the same package can occur
 ;; in the same or other repository, and the fields should be merged
 ;; (or the tool should warn if they are incompatible).
 (package
  ;; The name of a package uses the same namespace as the library
  ;; names.  Often, common libraries will be grouped under a common
  ;; prefix, following a pattern such as (pet-name project lib1),
  ;; (pet-name project lib2), etc.  If the package name is not
  ;; provided, then the longest common prefix of the libraries may be
  ;; used provided it is unambiguous.  In particular, in the common
  ;; case that there is only a single library in the package, the
  ;; library and package have the same name.  If the longest common
  ;; prefix is empty, or could apply to multiple packages, then the
  ;; package can only be referred to by the individual library names.
  (name (wonderland))
  ;; The URL points to a tarred, gzipped file containing a single
  ;; directory - any other format is invalid (optionally alternate compression may be supported).
  ;; All path references below are relative to that directory.
  (url "http://www.wonderland.org/repo/cheshire-cat.tgz")
  ;; Any number of checksums may be provided, and tools should require
  ;; confirmation or refuse to install for a package with an invalid
  ;; checksum.  Currently defined checksums include md5, sha1,
  ;; sha2224, sha256, sha384, sha12.  It is recommended all packages
  ;; use a sha256 checksum.  Checksums are always written as hex
  ;; strings.  Checksums always apply to the _unzipped_ tarball, so
  ;; that transparent recompression can be applied.
  (checksums (md5 "0123...")
             (sha-256 "0123..."))
  ;; The size of the package tarball in bytes, for information prior
  ;; to downloading and as an additional checksum (since this is
  ;; a checksum this refers to the size of the unzipped tarball).
  (size 1234)
  ;; A package can contain zero or more signatures.  Each signature
  ;; contains the identity of a publisher (discussed below), and an
  ;; RSA signature of one or more of the checksums as hex strings.
  (signature (email "hatter@wonderland.org")
             (digest sha-256)
             (sha-256 "0123...")
             (rsa "0123..."))

  ;; Each package contains one or more library descriptions.
  (library
   ;; Every library must have a name.
   (name (wonderland cheshire cat))
   ;; Each library must point to a single library description file
   ;; within the tarball.  It is an error if this path is absolute.
   ;; The extension has no specific meaning and can be chosen at will.
   (path "cheshire/cat.pkg")
   ;; A list of libraries which are required to install the given
   ;; library - installation tools should determine and install the
   ;; transitive closure of dependencies when any library is
   ;; installed.
   (depends
    (rabbit holes)
    ;; Libraries may also be suffixed with a version string following
    ;; Debian version string comparison rules:
    ;; http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version.
    ;; Boolean and/or/not or inequality >=, <= version requirements
    ;; may also be used as in R6RS.  Note these dependencies exist
    ;; outside the library system itself, which knows nothing about
    ;; versions.
    (magic potions "1.3")
    (monsters jabberwocky (>= "2.0.6")))
   ;; A library may "provide" another library (possibly one with no
   ;; direct implementation), in which case it will satisfy a
   ;; non-versioned dependency for that library.  This again is
   ;; outside the library system itself, however a library can
   ;; effectively choose between several libraries to import with
   ;; cond-expand, thus allowing for a poor-man's interface system.
   (provides (invisible cat))
   ;; Foreign dependencies can be specified, referring to libraries
   ;; the FFI may need or programs that may be called out to.  The
   ;; registry for these names is currently unspecified, and they
   ;; exist only as a reminder to the user.
   (foreign-depends "libmysql")
   ;; The list of platforms the library is known to work on.  Each
   ;; platform can either be a single symbol indicating the OS, or a
   ;; list of (OS ARCH).  Known operating system identifiers include:
   ;;   * windows
   ;;   * osx
   ;;   * linux
   ;;   * bsd
   ;;   * unix
   ;;   * plan9
   ;; while the architectures include:
   ;;   * x86
   ;;   * x86-64
   ;;   * arm
   (platforms linux (osx x86-64))
   ;; A list of cond-expand features required to install the library.
   (features utf8 bignum)
   ;; List of phases this library is used for, where <phase> can be any
   ;; of install, build or test.  The default is just install.  If used by other
   ;; phases, this library will be installed in a temporary location for use
   ;; during the given phase.
   (use-for install)
   ;; Library containing tests to run before installing this library.
   ;; The second argument is the entry point, which should be a thunk
   ;; which returns a true value if all tests pass, and defaults to run-tests.
   (test-library (wonderland cheshire cat test) run-tests)
   ;; Program containing tests to run before installing this library.
   ;; The program should exit with a success value (0 on POSIX systems)
   ;; if all tests pass.  Tools should fail or warn before installing libraries
   ;; for which any tests fail.
   (test-program "relative/path/to/test-program.scm")
   (test-program (program name))
   ;; A list of author names.
   (authors "Alice Caroll")
   ;; A list of maintainers if different from the authors.
   (maintainers "Alice Caroll")
   ;; The homepage for the library.  This is a "locale customizable"
   ;; value, which means it can be either a single string, as in the
   ;; homepage example below, or an alist as in the docs example
   ;; described below.
   (homepage "http://www.wonderland.org/scheme/index.html")
   ;; A URL pointing to the manual for the library.  This is a "locale
   ;; customizable" value, which in this case is an alist which tools
   ;; should use to determine the appropriate value for the user.  The
   ;; keys of the alist are two-letter ISO 639 language codes as
   ;; symbols (optionally followed by ISO 3166 standard two-letter
   ;; country codes), and the values are the URL for that language and
   ;; country.  The last key may be `else', which is chosen if no
   ;; other values matched.
   (manual
    (fr "http://www.wonderland.fr/scheme/manual.html")
    (zh-tw "http://www.wonderland.fr/scheme/manual.html")
    (else "http://www.wonderland.org/scheme/manual.html"))
   ;; A list of screenshots for the library.
   (screenshots "http://www.wonderland.org/scheme/cat.png")
   ;; An extended description of the library.  The first line will
   ;; often be used in concise lists of libraries.  This is a "locale
   ;; customizable" value.
   (description "...")
   ;; A list of keywords relevant to the library.
   (keywords 'invisible 'cat)
   ;; A list of licenses, either from a known symbol or a list
   ;; indicating the name, a URL pointing to the full text, and
   ;; an optional checksum of that text.  Known licenses include:
   ;;  * gpl2
   ;;  * gpl3
   ;;  * lgpl
   ;;  * mit
   ;;  * bsd
   ;;  * artistic
   ;;  * apache
   ;;  * public-domain
   (licenses 'bsd
             (looking-glass-license
              (url "http://www.wonderland.org/scheme/license.txt")
              (checksum (sha256 "0123..."))))
   ;; Creation and latest update date times in RFC 3339 format.
   (created "2011-08-03T22:44:00+00:00")
   (updated "2011-09-20T00:31:00+00:00")
   ;; Current version string.
   (version "1.2.3")
   ;; User-provided ratings of the library on a [0..1] scale across
   ;; five categories:
   ;;   * speed - the runtime performance of the library
   ;;   * stability - how stable and tested the library is
   ;;   * ease-of-use - how easy it is to use the library
   ;;   * extensibility - how easy it is to build on the library
   ;;   * documentation - the quality of the documentation
   ;; Each rating may be a single number indicating the mean of
   ;; ratings this repository has registered, or it may be a list
   ;; separately indicating any of the mean, median, variance, or
   ;; count of votes received.  Ratings are optional meta-data
   ;; maintained by the repo from signed votes (i.e. the repo may
   ;; choose not to maintain them at all), and are not generated by
   ;; packaging tools, however the original author can submit a
   ;; measure of the "intended" stability:
   ;;      <= 0.1 - pre-alpha
   ;;   0.1 - 0.3 - alpha
   ;;   0.3 - 0.5 - beta
   ;;   0.5 - 0.7 - release
   ;;   0.7 - 0.9 - stable
   ;;   0.9 - 1.0 - offer to pay for finding any remaining bugs
   (rating
    (speed (mean 0.7) (median 0.6) (variance 0.0) (count 23))
    (stability 0.5)
    (extensibility 0.5)
    (ease-of-use 0.5)
    (documentation 0.5))
   ;; The following are reserved for more complex installation options
   ;; in the future.
   (config-files "...")
   (data-files "...")
   (pre-install "...")
   (post-install "...")
   (pre-uninstall "...")
   (post-uninstall "..."))

  ;; The second library in this package.
  (library
   (name (wonderland mad hatter))
   (path "mad/hatter.pkg"))

  ;; A program has all the same fields as a library, except the name
  ;; is optional, and the path indicates a file containing a single
  ;; top-level program to install in a binary directory.  A package
  ;; can contain a mix of programs and libraries.
  (program
   ...))

 ;; A publisher is anyone who signs keys in the given repository.  A
 ;; repository should include the public keys for publishers who sign
 ;; its packages (though not necessarily in the main package list).
 (publisher
  ;; The name is typically the same as in the authors and maintainers
  ;; list - it is not used for security.  Tools should detect and warn
  ;; when multiple publishers use the same or similar names.
  (name "Alice Caroll")
  ;; The identity is self-chosen and intended to be unique, but the
  ;; true determination of a publisher is determined by combination of
  ;; identity and public-key.  Tools should detect and warn when the
  ;; same identity has multiple public-keys (which can happen
  ;; legitimately).
  (identity "org.wonderlang.alice")
  ;; The public key consists of the modulus and exponent of an RSA
  ;; public key.  The values hexstrings for readability, and in
  ;; deference to implementations with no bignum support.
  (public-key (modulus "<n>")
              (exponent "<e>"))
  ;; Optional creation and expiration dates for the public key in RFC
  ;; 3339 format.
  (created "2011-01-01")
  (expires "2013-01-01")
  ;; The email address should not be published to unauthenticated
  ;; users, and even then probably only with captcha confirmation.
  (email "...")
  ;; The homepage of the publisher.
  (homepage "http://www.wonderland.org/"))

 ;; Implementations may also be listed in the repository for
 ;; reference, and potentially for automatic installing and
 ;; cross-testing in the future.
 (implementation
  ;; The name of the implementation.
  (name "Chibi-Scheme")
  ;; The feature an implementation recognizes as itself in cond-expand
  ;; forms.
  (feature chibi)
  ;; The homepage for the implementation.
  (homepage "http://synthcode.com/wiki/chibi-scheme")
  ;; The URL for the latest release of the implementation.
  (url "http://chibi-scheme.googlecode.com/files/chibi-scheme-latest.tgz")))

;; Summary of the structure.

;; Every field is named and there are no positional arguments.
;; The entire format is enclosed in a `repository' sexp, in which
;; `sibling', `package' and `publisher' may occur multiple times.
;; Likewise within a package `library' and `program' may occur
;; multiple times.

;; The `name' and `provides' in a library or package follow the R7RS
;; library naming conventions.  The `depends' values use a variant of
;; this with optional version suffix.

;; `unit-of-time' uses a closed list of symbols.  `features',
;; `licenses' and `platforms' are semi-closed lists of symbols (users
;; can add new values as needed), and `keywords' is an open list of
;; symbols.

;; `amount', `count' and `size' are exact integers.  `trust', `mean'
;; and `median' are real numbers in the range [0..1], and `variance'
;; is a non-negative real number.

;; The remaining values are strings, all of which are opaque with the
;; following exceptions.  `url', `homepage', `manual' and
;; `screenshots' indicate an RFC 3986 URL.  `path' indicates a
;; filesystem path as an ASCII string - non-ASCII paths are currently
;; unsupported.  `version' indicates a Debian-style version string.
;; `created', `updated' and `expires' are RFC 3339 date-time strings.
;; `modulus', `exponent' and the checksum values and signatures are
;; hexidecimal strings.
}}}
