Bvectors are a data structure that can be seen as bit vectors (element values 1 and 0)
or boolean vectors (element values #t and #f).  Most procedures in this SRFI are blind
to the difference, but ones that return a single value come in two flavors.  The semantics
come from SRFIs 133 and 151.

Constructors:  `make-bvector, bvector-unfold, bvector-unfold-right, bvector-copy,
bvector-append, bvector-append-subvectors`

Predicates:  `bvector?, bvector-empty?, bvector=`

Accessors: `bvector-ref/bit, bvector-ref/bool, bvector-len, bvector-take, bvector-take-right,
bvector-drop, bvector-drop-right`

Mutators: `bvector-set!, bvector-swap!, bvector-fill!, bvector-copy!`

Mapping and folding:  `bvector-map, bvector-for-each`

Conversion:  `bvector->list/bit, bvector->vector/bit, bvector->list/bool, bvector->vector/bool,
vector->bvector, list->bvector, bvector->integer, integer->bvector`

Generators: `make-bvector-generator/bit, make-bvector-generator/bool`

Logical operations:  `bvector-not, bvector-and, bvector-ior, bvector-xor,
bvector-nand, bvector-nor, bvector-andc1, bvector-andc2,
bvector-orc1, bvector-orc2, bvector-shift, bvector-count,
bvector-integer-length, bvector-if, bvector-first-set-bit`

Field operations:  `bvector-field, bvector-any?, bvector-every?, bvector-clear,
bvector-field-set!, bvector-replace, bvector-replace-same, bvector-rotate`

Output: `write-bvector/bit, write-bvector/bool`

