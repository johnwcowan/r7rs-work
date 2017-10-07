To work with mixed binary/textual I/O, the following R6RS-compatible
procedures are proposed:

  * (read-blob <length> [<input-port>])
  * (read-blob! <blob> <length> [<input-port>])
  * (write-blob <blob> [<output-port>])
  * (write-partial-blob <blob> <from> <to> [<output-port>])
  * (utf8->string <blob>)
  * (string->utf8 <string>)

It's an error to pass invalid bytes to `utf8->string`.  A plausible recovery strategy is to convert each invalid byte into a U+FFFD character.

`Write-partial-blob` would be merged into `write-blob` if the same is done for blob copying.
