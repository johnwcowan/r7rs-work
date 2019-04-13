See [the 7th draft of SRFI 160](https://htmlpreview.github.io/?https://raw.githubusercontent.com/scheme-requests-for-implementation/srfi-160/fdaad69c4a96bd6ec601dcc7a7ce64598f7c2f16/srfi-160.html)

Bvectors can be viewed as bit vectors (possible values 1 and 0)
or as boolean vectors (possible values `#t` and `#f`.
Where an element is passed as an argument
either directly to a bvector procedure or indirectly via a
procedure argument, any of the four
values is legal; where an element is returned by a procedure,
it is split into `/bit` and `/bool` variants.

### Vector operations
```
make-bvector
bvector
bvector-unfold
bvector-unfold-right
bvector-copy
bvector-append
bvector-concatenate
bvector-append-subvectors
bvector?
bvector-empty?
bvector=
bvector-ref/bit
bvector-ref/bool
bvector-length
bvector-take
bvector-take-right
bvector-drop
bvector-drop-right
bvector-for-each/bit
bvector-for-each/bool
bvector-set!
bvector-swap!
bvector-fill!
bvector-copy!
bvector->list/bit
bvector->list/bool
list->bvector
bvector->vector
vector->bvector
make-bvector-generator/bit
make-bvector-generator/bool
```
## Bit operations
```
bvector-drop-while
bvector-drop-while-right
bvector-not
bvector-and
bvector-ior
bvector-xor
bvector-nand
bvector-nor
bvector-andc1
bvector-andc2
bvector-orc1
bvector-orc2
bvector-shift
bvector-count
bvector-integer-length
bvector-if
bvector-first-set-bit
bvector->integer
integer->bvector
bvector-display/bit
bvector-display/bool
bvector-field
bvector-field-any?
bvector-field-every?
bvector-field-clear
bvector-field-set
bvector-field-replace
bvector-field-replace-same
bvector-field-rotate
bvector-field-reverse
bvector-field-flip
```
