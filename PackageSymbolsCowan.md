
##symbols

& package-name package-name ... name -> symbol[syntax]

&& package-name package-name ... name -> symbol [syntax]

make-symbol name -> symbol

homeless-symbol? symbol -> boolean

plain-symbol? symbol -> boolean

symbol-name symbol -> name

symbol-package symbol -> package

symbol-plist symbol -> plist

symbol-property-ref symbol indicator -> value

symbol-property-set! symbol indicator value

symbol-property-remove! symbol indicator

symbol-indicator? symbol indicator -> boolean

symbol-indicator-add! symbol indicator

symbol-indicator-remove! symbol indicator

symbol->fancy symbol -> symbol

fancy->symbol symbol -> symbol

symbol-delete! symbol

##packages

make-package [package] package-name -> package

package? obj -> boolean

package [package] package-names -> package

package-name package -> package-name

package-path package -> package-names

package-shadowing-symbols -> symbols

package-imports -> packages

package-imported-by -> packages

package-parent package -> package

package-children [package] -> packages

package-descendants [package] -> packages

package-map proc package -> objects

package-map-external proc package -> objects

package-map-accessible proc package -> objects

package-for-each proc package

package-for-each-external proc package

package-for-each-accessible proc package

package-search package name -> symbol status-symbol

package-search-all [package] name -> symbols

##package system

symbol-intern! package name

symbol-import! package symbol

symbol-shadowing-import! package symbol

symbol-export! package symbol

symbol-shadow! package name

package-import! importing-package imported-package

package-unimport! importing-package imported-package

symbol-unexport! package symbol

symbol-unintern! package symbol

package-delete! package

##exceptions

package-error? obj -> boolean

symbol-conflict? obj -> boolean

symbol-conflict-action symbol-conflict -> symbol

symbol-conflict-symbols symbol-conflict -> symbols

symbol-conflict-packages symbol-conflict -> packages

