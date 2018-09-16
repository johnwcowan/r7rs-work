```
#!comment
page in progress
```

All information taken from Christaian Quiennic's, __Lisp In Small Pieces__, 1994 edition.

# Universal Global Environment

|What|How|
|----|---|
|Reference|x|
|Value|x|
|Modification|`(set! x ...)`|
|Extension|no|
|Definition|no, `define == set!`|

# Frozen Global Environment

|What|How|
|----|---|
|Reference|x|
|Value|`x`}, but `x` must exist|
|Modification|`(set! x ...)`}, but `x` must exist|
|Extension|`define`} (only one time)|
|Definition|no|

# Automatically Extendable Global Environment

|What|How|
|----|---|
|Reference|x|
|Value|x|
|Modification|`(set! x ...)`|
|Extension|`(set! x ...)`|
|Definition|no, `define == set!`|

# Hyperstatic Global Environment

|What|How|
|----|---|
|Reference|`x`, but `x` must exist|
|Value|`x`, but `x` must exist|
|Modification|`(set! x ...)`, but `x` must exist|
|Extension|`define`|
|Definition|no|

