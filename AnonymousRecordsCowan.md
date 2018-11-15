These are anonymous record types created procedurally that can interoperate with either
SRFI 99 or R6RS procedural layers.  They can only create base records, not derived
records, so there is no problem with inheritance.  The types don't have names
and neither do the slots (hence "anonymous"), though the implementation needs
to create names to talk to the underlying system.

The constructor just allocates a record instance; any initialization has to
be done by the setters.

There is a restriction that the synctactic layer of the underlying record system
must bind the record name to a run-time object, not just a compile-time one.
I think this is always true in practice.

## Procedures

make-record-type slotcount -> rtd

make-record-predicate rtd -> proc

make-record-constructor rtd -> proc

make-record-getter rtd index -> proc

make-record-setter rtd index -> proc or #f

record-type? obj -> boolean

record-type-of obj -> rtd or #f

record-instance? obj -> boolean

## SRFI 99

(make-rtd name fieldspecs)

(make-rtd name fieldspecs parent)

(rtd? obj)

(rtd-constructor rtd)

(rtd-constructor rtd fieldspecs)

(rtd-predicate rtd)

(rtd-accessor rtd field)

(rtd-mutator rtd field)

(record? obj)

(record-rtd record)

## R6RS

(make-record-type-descriptor name parent uid sealed? opaque? fields)

(record-type-descriptor? obj)

(make-record-constructor-descriptor rtd parent-constructor-descriptor protocol)

(record-predicate rtd) 

(record-accessor rtd k)

(record-mutator rtd k) ;raises &assertion

(record? obj)

(record-rtd record)

## Chibi

(this is an example only)

(register-simple-type <name-string> <parent> <field-names>)  
 => <type>    ; parent may be #f, field-names should be a list of symbols

(make-type-predicate <opcode-name-string> <type>)  
 => <opcode>  ; takes 1 arg, returns #t iff that arg is of the type

(make-constructor <constructor-name-string> <type>)  
 => <opcode>  ; takes 0 args, returns a newly allocated instance of type

(make-getter <getter-name-string> <type> <field-index>)  
 => <opcode>  ; takes 1 args, retrieves the field located at the index

(make-setter <setter-name-string> <type> <field-index>)  
 => <opcode>  ; takes 2 args, sets the field located at the index

(type-slot-offset <type> <field-name>)  
 => <index>   ; returns the index of the field with the given name