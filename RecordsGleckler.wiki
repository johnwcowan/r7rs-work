I propose a combination of a simple procedural abstraction for defining new types at the most fundamental level with a more convenient syntax for everyday use.  I don't see a reason that we can't have both in WG1.

I propose [wiki:RecordsCowan] plus [wiki:RecordsArcfide].

Types created using [wiki:RecordsArcfide] are implemented by [wiki:RecordsCowan].

Types created using [wiki:RecordsCowan] may inherit from types created using [wiki:RecordsArcfide].  (Since [wiki:RecordsArcfide] doesn't support inheritance, it's not possible, without extension, to go the other way around.)