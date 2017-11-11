## System command procedures

`(shell-command `*command*`)`

Invokes the system shell to execute *command*, which must be a string. This procedure returns the exit status of the shell in the form that the C library’s system routine returns.

`(system-command `*command*` `*arg0*` `*arg* ...`)`

Invokes *command* directly, passing *arg0* as the zeroth argument and *args* as the additional arguments.  All arguments must be strings.  This procedure returns the exit status of *command*.

`(system-command-with-environment `*command*` `*environment*` `*arg0*` `*arg* ...`)`

Same as `system-command`, but passes *environment* (which must be an a-list mapping strings to strings) as the environment.
