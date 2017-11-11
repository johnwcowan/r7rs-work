```
#!comment
page in progress
```

All information taken from Christaian Quiennic's, __Lisp In Small Pieces__, 1994 edition.

# Universal Global Environment

|Reference|x|
|Value|x|
|Modification|{{{(set! x ...)}}}|
|Extension|no|
|Definition|no, {{{define == set!}}}|

# Frozen Global Environment

|Reference|x|
|Value|{{{x}}}, but {{{x}}} must exist|
|Modification|{{{(set! x ...)}}}, but {{{x}}} must exist|
|Extension|{{{define}}} (only one time)|
|Definition|no|

# Automatically Extendable Global Environment

|Reference|x|
|Value|x|
|Modification|{{{(set! x ...)}}}|
|Extension|{{{(set! x ...)}}}|
|Definition|no, {{{define == set!}}}|

# Hyperstatic Global Environment

|Reference|{{{x}}}, but {{{x}}} must exist|
|Value|{{{x}}}, but {{{x}}} must exist|
|Modification|{{{(set! x ...)}}}, but {{{x}}} must exist|
|Extension|{{{define}}}|
|Definition|no|

