
##symbols

& package-name package-name ... name -> symbol[syntax]

&& package-name package-name ... name -> symbol [syntax]

package [package] package-names -> package

homeless-symbol? symbol -> boolean

plain-symbol? symbol -> boolean

make-symbol package name -> symbol

symbol-name symbol -> name

symbol-package symbol -> package

symbol-plist -> plist

symbol-property-ref symbol indicator -> value

symbol-property-set! symbol indicator value

symbol-property-remove! symbol indicator

symbol-indicator? symbol indicator -> boolean

symbol-indicator-add! symbol indicator

symbol-indicator-remove! symbol indicator

symbol-delete! symbol

##packages

symbol-export! package symbol

package-search [package] name -> symbol

package-search-all [package] name -> symbols

symbol-import! package symbol

package-search name -> symbols

symbol-shadow! package name

symbol-shadowing-import! package symbol

package-delete! package

make-package [package] package-name -> package

symbol-unexport! package symbol

symbol-unintern! package symbol

package-import! importing-package imported-package

package-unimport! importing-package imported-package

package-map proc package -> objects

package-map-external proc package -> objects

package-map-accessible proc package -> objects

package-for-each proc package

package-for-each-external proc package

package-for-each-accessible proc package

symbol-intern! package symbol

package-name package -> package-name

package-shadowing-symbols -> symbols

package-imports -> packages

package-imported-by -> packages

package? obj -> boolean

package-error? obj -> boolean

symbol-conflict? obj -> boolean

symbol-conflict-symbols symbol-conflict -> symbols

symbol-conflict-packages symbol-conflict -> packages

package-parent package -> package

package-children package -> packages

