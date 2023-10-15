These are the datatypes of [ISO/IEC 11404 — General-Purpose Datatypes](https://standards.iso.org/ittf/PubliclyAvailableStandards/c039479_ISO_IEC_11404_2007(E).zip), an international standard for datatypes.  The scope of the standard is as follows:

> This International Standard specifies the nomenclature and shared semantics for a collection of datatypes commonly occurring in programming languages and software interfaces, referred to as the General-Purpose Datatypes (GPD). It specifies both primitive datatypes, in the sense of being defined ab initio without reference to other datatypes, and non-primitive datatypes, in the sense of being wholly or partly defined in terms of other datatypes. The specification of datatypes in this International Standard is “general-purpose” in the sense that the datatypes specified are classes of datatype of which the actual datatypes used in programming languages and other entities requiring the concept “datatype” are particular instances. These datatypes are general in nature; thus, they serve a wide variety of information processing applications.

## Primitive datatypes

 * booleans `(boolean=? and or not)`
 * states (like enums but unordered) `(enum=?)`
 * enums `(enum=? enum<= enum-next)`
 * characters `(char=?)`
 * ordinals (positive integers) `(= <= 1+)`
 * date-and-times (broken-out) `(date=? date<=? date-difference date-round date-extend)`
 * integers `(= <= nonnegative? negate + *)`
 * rationals `(= negate? <= nonnegative?  + *)`
 * scaled rationals (denominator is a power of k for some k) `(= <= negate + round * /)`
 * reals (inexact numbers) `(= <= negate + * 1/)`
 * complex numbers (inexact numbers)  `(= negate + * 1/ sqrt)` 
 * void (singleton object) `(eqv?)`

## Subtypes and extended types

 * range (limits by upper and lower bounds)
 * select (limits by specifying members)
 * size (limits by maximum size/length)
 * explicit (limits by subtyping)
 * extended (extends by supertyping)

## Generated datatypes

 * choice (discriminated union) `(choice-equal? choice-tag)`
 * pointer `(pointer=? pointer-ref)`
 * procedure `(eqv? apply)`
 * 

## Aggregate datatypes

 * records `(equal? *-ref *-set!)`
 * classes (records with procedure fields, overridden types of fields) `(equal? *-ref *-set!)`
 * sets `(set-contains set<=? set=? set-difference set-union set-intersection set-empty? set)`
 * bags `(bag-contains bag<=? bag=? bag-difference bag-union bag-intersection bag-empty? set)`
 * sequences (lists) `(null? car cdr equal? null? append)`
 * arrays (multidimensional) `(array= array-ref array-set!)`
 * tables (bags of records, all of the same type) `(relation->bag bag->relation relation->tuple-list relation-empty? relation=? table relation-remove relaiton-insert relation-project/select relation->something)`

## Defined datatypes

 * natural numbers aka cardinals (non-negative integers) `(= <= nonnegative? + *)`
 * characters `(char=?)`
 * modulo numbers (integers with different arithmetic) `(= <= + * negate)`
 * bits (0 or 1) `(= <= + *)
 * bit strings (bit vectors)  `(null? car cdr equal? null? append)`
 * character strings `(null? car cdr equal? null? append)`
 * time intervale `(time-interval=? negate +)`
 * octets `(= <=)`
 * octet strings `(null? car cdr equal? null? append)`
 * private objects (blobs, opaque objects with fixed size) (no operations)
 * (registered) object identifier ([OIDs](oid-info.com/#oid), u16vectors) `(oid=? oid->integer)`

## Defined generators

 * stacks `(top pop push)`
 * trees `(tree-empty? tree=? empty-tree tree-head tree-tail)`
 * optionals (Maybes) `(maybe? . underlying-operations-list)`
