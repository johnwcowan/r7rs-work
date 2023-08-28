These are the datatypes of [ISO/IEC 11404 — General-Purpose Datatypes](https://standards.iso.org/ittf/PubliclyAvailableStandards/c039479_ISO_IEC_11404_2007(E).zip), an international standard for datatypes.  The scope of the standard is as follows:

> This International Standard specifies the nomenclature and shared semantics for a collection of datatypes commonly occurring in programming languages and software interfaces, referred to as the General-Purpose Datatypes (GPD). It specifies both primitive datatypes, in the sense of being defined ab initio without reference to other datatypes, and non-primitive datatypes, in the sense of being wholly or partly defined in terms of other datatypes. The specification of datatypes in this International Standard is “general-purpose” in the sense that the datatypes specified are classes of datatype of which the actual datatypes used in programming languages and other entities requiring the concept “datatype” are particular instances. These datatypes are general in nature; thus, they serve a wide variety of information processing applications.

## Primitive datatypes

 * booleans
 * states (like enums but unordered)
 * enums
 * characters
 * ordinals (positive integers)
 * dates (broken-out)
 * times (broken-out)
 * integers
 * rationals
 * scaled rationals (denominator is a power of k for some k)
 * reals (inexact numbers)
 * complex numbers (inexact numbers)
 * void (singleton object)

## Aggregate datatypes

 * records
 * classes (records with procedure fields, overridden types of fields)
 * sets
 * bags
 * sequences (lists)
 * arrays (multidimensional)
 * tables (bags of records, all of the same type)

## Defined datatypes

 * natural numbers aka cardinals (non-negative integers)
 * modulo numbers (integers with different arithmetic)
 * bits (0 or 1)
 * bit strings (bit vectors)
 * character strings
 * octets
 * octet strings
 * private objects (blobs, opaque objects with fixed size)
 * (registered) object identifier ([OIDs](oid-info.com/#oid), u16vectors)

## Defined generators

 * stacks
 * trees
 * optionals (Maybes)
