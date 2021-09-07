[SettingsListsCowan](SettingsListsCowan.md) is nice, but would be even better with some syntax for using settings lists in
one's own procedures, perhaps:

`(let-settings (`*keywords*` `*settings-list*`) . `*body*`)`

where *keywords* is a list of symbol names to extract from the *settings-list*,
which will be bound to the named values in the current environment. The
`open-input-process` procedures from [ProcessPortsCowan](ProcessPortsCowan.md) might then be defined thus
for the case where their *filename* argument is a list:

```
   (define (open-[[input|output|input/output]]-process filename)
     (let-settings ((path arguments environment
                     stdin-redirection stdout-redirection) filename)
       ...
       (execvp path arguments environment) ; or whatever
       ...))
```

(It could be written in terms of a `call-with-settings` procedure, of course.)

## Issues

How does one tell if a key is not present?  Bind to `#f`?  Bind to a default specified as a third value?  Ideally, each key should have its own default, but that may be too heavyweight.  --JC
