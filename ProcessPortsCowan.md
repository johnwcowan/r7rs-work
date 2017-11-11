## Process Ports

The procedures in this proposal start a new operating system process and returns a port connected to either the standard input (for `open-output-process`) or the standard output (for `open-input-process`) of the new process, or both (for `open-input/output-process`).

This library depends on [SettingsListsCowan](SettingsListsCowan.md).

## Procedures

`(open-input-process `*filename*`)`

`(open-output-process `*filename*`)`

`(open-input/output-process `*filename*`)`

All of these are equivalent to the corresponding R5RS functions on file ports.  If *filename* is a string, it is a shell command suitable for passing to `shell-command`.  If it is a list, it is a settings list as described in [SettingsListsCowan](SettingsListsCowan.md).  Implementations MUST support the following keys:

`path`::

> The system command to be executed.

`arguments`::

> A list of strings to be passed as the arguments of the command.  If absent or `()`, no arguments are passed.

`environment`::

> An alist, mapping strings to strings, representing the environment variables to be passed to the new process and their values.  If the same environment variable appears more than once, all occurrences except the first are ignored, as is usual when processing alists.  If this key is absent, the current environment is passed.  If this key is `()`, an empty environment is passed.

`stdin-redirection`::

> A string representing the file to open and pass to the new process as its standard input, or a port (it is an error if the port has no OS-level underpinnings).  If this key is absent, no redirection is done.  It is an error to use this key in a call to `open-output-process` or `open-input/output-process`.

`stdout-redirection`::

> A string representing the file to open and pass to the new process as its standard output, or a port (it is an error if the port has no OS-level underpinnings).  If this key is absent, no redirection is done.  It is an error to use this key in a call to `open-input-process` or `open-input/output-process`.

>
