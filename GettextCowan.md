## Gettext package

Gettext is a package for the localization of strings.  To substitute the translated version of a string for the original, write `(_ "string")` instead of `"string"`, and the Right Thing should happen -- provided you have installed the appropriate translation object correctly in advance.

## Basic procedures

There is really only one `_` procedure, but it is specified as multiple cases here.

`(_ `[*translation*] *string*`)`

Look up *string* in *translation* and return the translated version as a string.  If *translation* is not specified, use the installed translation.

`(_ `[*translation*] *string*` `*encoding*`)`

Look up *string* in *translation* and return the translated version as a bytevector encoded according to *encoding*.  If *encoding* is `#t`, use the encoding of the translation.  If *encoding* is a string, use it as the name of the encoding to be used.  If the system does not know that encoding, signal an error.  If *translation* is not specified, use the installed translation.

`(_ `[*translation*] *singular*` `*plural*` `*n*`)`

If *n* equals 1, look up *singular* in *translation* and return the translated version as a string.  If *n* does not equal 1, look up *singular* and return the appropriate associated plural translation (some languages need more than one plural translation depending on the value of *n*).  See `null-translation` for an explanation of the *plural* argument.  If *translation* is not specified, use the installed translation.

`(_ `[*translation*] *singular*` `*plural*` `*n*` `*encoding*`)`

The same as the previous case, except that the returned value is a bytevector.  The contents of the bytevector are determined by *encoding* as explained above.

## Loading and installing translations

A translation is an immutable object of a disjoint type.

`(make-translation `*domain*` `[#*localedir*|[*languages* [*fallback?* [*encoding*]]]]]`)`

Locates and returns a translation for *domain* (a string, typically the name of an application or a library).  *Localedir* is a directory in which to search for translations; if not specified or `#f`, an implementation-dependent directory is used.  *Languages* is a list of strings which are language tags; if not specified or `#f`, an implementation-dependent list is used, usually derived from an environment variable.  *Encoding* is used to specify the name of the encoding used by the translation to create bytevectors; if not specified or `#f`, an implementation-defined encoding is used.  If *fallback?* is true and no appropriate translation object can be located, a null translation object is returned; if it is not specified or `#f`, an error is signaled.

`(null-translation)`

Returns a null translation object that translates all strings into themselves.  When a singular/plural translation is requested, the object translates *singular* to itself if *n* is equal to 1 and translates *singular* to *plural* otherwise.  This is the only case in which the *plural* argument to `_` is used.

The domain of a null translation object is `""`.  The encoding used by the null translation object to create bytevectors is system-defined.

`(translation? `*obj*`)`

Returns `#t` if *obj* is a translation object, and `#f` otherwise.

`(translation-domain `*translation*`)`

Returns the domain of the translation as a string.  It is an error to modify this string.

`(translation-properties `*translation*`)`

Returns an a-list containing the properties of the translation; these are implementation-dependent, and the list may be empty.

`(translation-encoding `*translation*`)`

Returns the string name of the native encoding of the translation.

`(translation-output-encoding `*translation*`)`

Returns the string name of the encoding that was specified to `make-translation`.

`(install-translation! `*translation*`)`

Causes *translation* to be installed as the default translation.

`(installed-translation)`

Return the currently installed translation object.
