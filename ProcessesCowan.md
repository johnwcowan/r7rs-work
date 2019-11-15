Very preliminary version of a high-level process management library.
It's based somewhat on the Python 3 subprocess module.

## Issues

Issue 1: The question of assigning
an external encoding, newline encoding, and encoding error convention
for textual pipe ports remains open until [FilesAdvancedCowan](FilesAdvancedCowan.md)
is a little more settled.

Issue 3: It's not clear whether the control-terminal procedures
`open-control-tty`, `terminal-process-group`, and `set-terminal-process-group`
belong in this SRFI or in a terminal SRFI.

## Constructors

`(make-pipe)`

Makes a pipe and returns two values, both binary ports.
The first value is the read end of the pipe, the second value is the write end.
The caller may use the pipe internally as a queue (provided it does not get full),
pass one end to a subprocess and use the other to communicate with it,
or pass both ends to different processes.
In the latter two cases, the caller should close the port after passing it.

`(make-textual-pipe)`

The same as `make-pipe`, except that the ports are textual.  The encoding is
implementation-dependent.  Note that whether a pipe passed to a child
process is binary or textual in the child is determined solely by the child.

`(make-process `*setup cmd . args*`)`

Starts a child process which executes the program specified by the string *cmd*,
passing it the string arguments *args*.
The plist *setup* specifies how the newly created process is set up
and connected to the parent process that creates it and possibly to other processes.
All file descriptors not mentioned in *setup* are closed in the new process.
It returns a process-object (see below) from which various results can be extracted.
If the process cannot be created for any reason,
an error satisfying `process-exception?` is signaled.

Note: if more than one file descriptor is connected to a pipe,
precautions must be taken to avoid deadlock.
If the implementation does not make use of asynchronous I/O under the covers,
then the use of SRFI 170 `select` to decide which pipe is ready
to be read or written is advisable.

## The setup plist

A plist is a list whose elements alternate between keys and values.
Quasiquotation is a convenient way to construct a plist with fixed
keys and variable values.
The setup plist allows keys to be either symbols or exact integers.
Here is an explanation of each standard key.  Keys not listed here are ignored,
unless the implementation attributes a meaning to them.

`stdin`  
`stdout`  
`stderr`

Specifies a port to be connected to the standard input/output/error of the
child process.  It is an error to associate an output port with
the key `stdin` or an input port with the keys `stdout` and `stderr`.
The values are interpreted as follows (any other value is an error):

  * If the value is a file, socket, pipe, or other port that contains a
    file descriptor, that file descriptor is duplicated onto child port 0/1/2.
    It is an error if the value is any other kind of port such as a string
    or bytevector port.
   
  * If the value is `#t` or the key is omitted, the port in the child process
    is the same as the port ih the parent process.
    
  * If the value is `#f`, the port in the child process is closed.
  
  * If the value is the symbol `null`, the port in the child process is connected to
    the null device (`/dev/null` in Posix, `NUL` in Windows),
    so that an attempt to read from the port produces an immediate end of file,
    and an attempt to write to the port is ignored.
    
*exact non-negative integer*

The same as `stdin`, `stdout`, and `stderr`,
but specifies the file descriptor to be used in the child process.
File descriptors other than 0, 1, and 2 that do not appear as keys in the setup plist
are closed in the child process.

`stdout+stderr`

The same as `stdout`, but binds both the standard output and the standard error in the child process
to the same port.
It is an error to provide either `stdout` or `stderr` if this key is present in the setup plist.
This corresponds to `|&` in the C shell and `2>&1` in Posix shells.

`open-fds`

The value is a list of file descriptors not to be closed in the child process.
All file descriptors not mentioned in the setup plist are closed.

`path`

This key has a boolean value specifying whether to search the environment variable `PATH`
to find the command specified by *cmd*.
However, if *cmd* contains a slash, the path is not searched.
The default value is `#f`.

`arg0`

Specifies argument 0 to be passed to the child process.  The default value is the same
as the *cmd* argument.

`env`

Specifies an alist that maps strings to strings, which becomes the initial environment of the
child process.  If omitted, the child process has the same environment as the parent.

`group`

If the value is `#f` or the key is omitted,
the child process belongs to the same process group and session as the parent process.
If the value is an exact integer, it specifies the process group id
to which the child process will belong.
If the value is the symbol `new` it belongs to the same session but in a new process group.
If the value is the symbol `new-session`, it belongs to a new session and a
new process group; in Windows this implies a new console.

`wait`

If the value is `#f` or the key is omitted, `make-process` returns as soon as the child
process is created.  If the value is `#t`, `make-process` returns when the child terminates.

## Synthetic process objects

`(pid->proc `*pid*`)`

Creates a synthetic process object wrapping an arbitrary process id.
Because the process is not necessarily a child of the curren process,
s process object accessors may return `#f` unexpectedly
or read from a file such as `/proc/<pid>/status`.
It is always possible to send signals to a synthetic process object.

## Process object predicates

`(process? `*obj*`)`  
`(synthetic-process? `*obj*`)`

Returns `#t` if *obj* is a (synthetic) process object and `#f` otherwise.

## Process object accessors

The following procedures extract values from
the process object returned by `make-process`.
If the answer is not yet known, calling these procedure triggers an attempt to find out,
but in no case does the caller wait for any process to complete.

`(process-child-id `*process*`)`

Returns the process id of the child process as an exact integer.

`(process-child-group `*process*`)`

Returns the process group id of the child process as an exact integer.

`(process-child-session `*process*`)`

Returns the session id of the child process as an exact integer.

(`process-terminated? `*process*`)`

Returns `#t` if the process has terminated either normally or on a signal, and `#f` otherwise.

`(process-stopped? `*process*`)`

Returns `#t` if the process has stopped on a signal, and `#f` otherwise.

`(process-exit-code `*process*`)`

Returns the exit code as an exact integer if the process has terminated normally, or #f if not.

`(process-stop-signal `*process*`)`

Returns the signal number as a symbol if the process has stopped on a signal, or #f if not.

`(process-terminate-signal `*process*`)`

Returns the signal number as an exact integer if the process has terminated on a signal, or #f if not.

## Process termination procedures

Any attempt to wait for a process that is not a child of the calling process
signals an error satisfying `process-exception?`.
If the optional *stopped?* argument is present and true,
the call will return for a stopped process as well as a terminated one.

`(process-wait `*process* [*stopped?*]`)`

Waits for the specified child process to terminate.  Returns the process object.

`(process-wait-any `[*stopped?*]`)`

Waits for any child process to terminate.  Returns the process object of the child.

`(process-wait-group `*process* [*stopped?*]`)`

Waits for any of the child processes in the process group
specified by *process* to terminate.
Returns the process object of the terminated process.

## Sending signals to processes.

These procedures may be called on any process object,
synthetic or not.

`(process-terminate `*process*`)`

Sends the SIGTERM signal to the process.
Returns an unspecified value.

`(process-send-signal `*process signal*`)`

Sends the signal *signal* to the *process*.
Returns an unspecified value.

`(process-send-group-signal `*process-or-pgid signal*`)`

Sends the signal *signal* to all the processes in the process
group specified by the exact integer process group id or the
process object.

## Fork and exec

These procedures are not portable to Windows (they will raise errors satisfying `process-exception?`)
and should be avoided when possible.

`(process-fork)`

Forks the current process.  Returns a process object in the parent process
and `#f` in the child object.

`(process-fork `*thunk*`)`

Forks the current process and returns a process object in the parent process.  The child
process immediately invokes *thunk* and exits using the value that *thunk* returns.

`(process-exec `*setup cmd . args*`)`

In the current process, replaces the currently executing program with *cmd*, passing *args* to it.
All keys in *setup* except `closed-fds`, `path`, `arg0`, and `env` are ignored.
This procedure never returns (but may throw an exception).

## Exceptions

If any of the operations required by the procedures in this SRFI return an error code,
an error satisfying `process-exception?` is signaled.

`(process-exception? `*obj*`)`

Returns `#t` if *obj* is a process exception and `#f` otherwise.

`(process-exception-errno` *process-exception*`)`

Returns a Posix error number corresponding to the exception, or `#f` if there is none.

`(process-exception-message `*process-exception*`)`

Returns a string associated with *process-exception* indicating the nature of the problem.