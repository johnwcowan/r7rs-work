
##symbols

& package-name package-name ... symbol-name -> symbol[syntax]

&& package-name package-name ... symbol-name -> symbol [syntax]

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

##packages

symbol-export

symbol-find-all

symbol-import

package-list

symbol-shadow

symbol-shadowing-import

package-delete

make-package

symbol-unexport

symbol-unintern

package-unuse

package-use

package-symbols

package-external-symbols

package-all-symbols

symbol-intern

package-name

package-shadowing-symbols

package-use-list

package-used-by-list

package?

current-package

package-error?

package-error-package

superpackage

subpackages

