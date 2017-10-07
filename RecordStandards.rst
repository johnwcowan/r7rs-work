Here's a list of the features provided by different kinds of standard and proposed record systems.  ''The specific syntax of syntax forms and the exact calling conventions of procedures is ignored here.''
  
= Glossary: =

 * A ''record constructor'' is a procedure that constructs a record of a specified type
 * A ''record accessor'' is a procedure that gets the value of a particular element of a record
 * A ''record mutator'' is a procedure that sets the value of a particular element of a record
 * A ''record predicate'' is a procedure that returns `#t` or `#f` depending on whether its argument is an instance of a given record type

 * An ''RTD'' (record-type-descriptor) is a run-time object representing a record type.

== Features: ==

 * [[SyntacticRecords|''Syntactic'']] means record types are specified by one or more macros that bind identifiers to record types, constructors, accessors, mutators, and predicates.
 * [[ProceduralRecords|''Procedural'']] means that record types are created by a procedure which returns an RTD, and that by applying other procedures to the RTD, constructors, accessors, mutators, and predicates are returned

 * [[GenerativeRecords|''Generative'']] record types are unique in each definition
 * [[NonGenerativeRecords|''Non-generative'']] record types have a uid (a symbol) and are identical if they share the same symbol

 * A [[NaturalRecordConstructor|''natural constructor'']] is a procedure whose arguments correspond to the fields of the record in the order they were specified when creating the RTD
 * A [[FilteringRecordConstructor|''filtering constructor'']] is a procedure whose arguments correspond to some of the fields of the record in an order which may or may not be the specification order.
 * A [[GeneralizedRecordConstructor|''generalized constructor'']] is one which applies an arbitrary procedure, called the protocol procedure, that transforms its arguments in any way desired and then calls a natural constructor.
 * A [[LabelRecordConstructor|''label constructor'']] is one which maps field labels to values independent of order.

= Standards =

== (srfi 9) is syntactic.  It provides: ==
 * Distinct types
 * Opaque RTD bindings with no interface
 * A single filtering constructor
 * Mutable fields
 * Immutable fields
 * Generative records
 * Top-level definitions only

== (srfi 57) is syntactic.  It extends (srfi 9) as follows: ==
 * Multiple interface inheritance
 * A label constructor
 * Record composition
 * Functional record update
 * In-place record update

== (srfi 99 records syntactic) is syntactic.  It extends (srfi 9) as follows: ==
 * Record definitions appear where other definitions appear
 * Single inheritance
 * A default natural constructor when the constructor parameters are unspecified
 * Auto-generated non-hygienic names

== (rnrs records syntactic 6) is syntactic.  It extends (srfi 99 records syntactic) as follows: ==
 * Generalized constructor
 * Sealed RTDs from which other RTDs cannot inherit (optional in SRFI-99)
 * Opaque RTDs that create records that don't satisfy RECORD? (optional in SRFI-99)
 * Non-generative RTDs (optional in SRFI-99)

== (srfi 99 records procedural) is procedural.  It extends (srfi 99 records syntactic) as follows: ==
 * RTDs have names (symbols)
 * Procedure to create an RTD
 * Procedure to create natural and filtering constructors from an RTD
 * Procedure to create predicates from an RTD
 * Procedure to create accessors and mutators from an RTD and a field name

== (srfi 99 records inspection) is procedural.  It provides the following: ==
 * Predicate that returns #t on any record
 * Procedure to extract RTD from a record
 * Procedures to access name, parent, field names, inherited field names from an RTD

== (rnrs records procedural 6) is procedural.  It extends (rnrs records syntactic 6) as follows: ==
 * Procedure to create an RTD
 * Procedure to create a constructor-descriptor (which can then create a constructor) for natural and generalized constructors from an RTD
 * Procedure to create predicates from an RTD
 * Procedure to create accessors and mutators from an RTD and a field index (doesn't work for inherited fields)

== (rnrs records inspection 6) is procedural.  It extends (srfi 99 records inspection) as follows: ==
 * Procedures to extract sealedness, opaqueness, and nongenerative id from an RTD; these are optional in SRFI-99

== RecordsArcfide is syntactic.  It extends SRFI 9 as follows: ==
 * Single inheritance (with multiple inheritance as possible future extension)

== AggregatesMedernach is procedural.  It provides: ==
 * Distinct types
 * Natural constructor
 * Optional inheritance
 * Mutable fields
 * Generative records
 * An accessor that applies a procedure to the fields in the record

== UserAggregatesRush is procedural.  It provides: ==
 * Distinct types
 * Subtype/supertype predicates
 * Universal constructors/predicate/accessors/mutators that require a RTD argument
 * Natural constructors that specify the number of fields
 * Mutable fields
 * No inheritance, only subtyping
 * Generative records

== UniqueTypesSnellPym is procedural.  It provides: ==
 * Distinct types
 * No RTD objects; type constructor returns procedures directly
 * A single filtering constructor
 * Immutable fields
 * Generative and non-generative records
 * Single inheritance (optional proposal)
 * Mutable fields (optional proposal)

= Issues and Discussions =

  * EfficiencyConcerns
  * InterfaceComparison

Eventually there should be a companion page for native record systems provided by individual Schemes.
