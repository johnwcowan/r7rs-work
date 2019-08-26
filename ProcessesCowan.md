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

Issue 5: We need some provision for avoiding deadlock when the child's standard
output and standard error are both newly created pipes.

## The make-process procedure

`(make-process `*setup cmd . args*`)`

Starts a child process which executes the program specified by the string *cmd*,
passing it the string arguments *args*.
The plist *setup* specifies how the newly created process is set up
and connected to the parent process that creates it and possibly to other processes.
All file descriptors not mentioned in *setup* are closed in the new process.
It returns a process-object (see below) from which various results can be extracted.

## The setup plist

A plist is a list whose elements alternate between keys and values.
Quasiquotation is a convenient way to construct a plist with fixed
keys and variable values.
The setup plist allows keys to be either symbols or exact integers.
Here is an explanation of each key.  Keys not listed here are ignored,
unless the implementation attributes a meaning to them.

`stdin`  
`stdout`  
`stderr`

Specifies a port to be connected to the standard input/output/error of the
child process.  It is an error to associate an output port with
the key `stdin` or an input port with the keys `stdout` and `stderr`.
The values are interpreted as follows.

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
    and an attempt to write to the port fails.
    
  * If the value is the symbol `binary-pipe` or `textual-pipe`, a pipe is created. 
    If the key is `stdin`,
    the output side of the pipe is connected to the child process;
    if the key is `stdout`,
    the input side of the pipe is connected to the child process.
    The other side of the pipe can be obtained in the parent process
    from the process object returned by `make-process`.
    
*exact integer*

The same as `stdin`, `stdout`, and `stderr`,
but specifies the file descriptor to be used in the child process.
File descriptors other than 0, 1, and 2 that do not appear as keys in the setup plist
are closed in the child process.

If the argument is `binary-pipe` or `textual-pipe`, and the integer is negative, the write
side of the pipe is connected; otherwise the read side of the pipe is connected.  In all other
cases the sign of the integer is ignored.

`stdout+stderr`

The same as `stdout`, but binds the standard output and the standard error in the child process
to the same port.
In particular, if a pipe is created, the same pipe is used for both ports.
It is an error to provide either `stdout` or `stderr` if this key is present in the setup plist.
This corresponds to `|&` in the C shell and `2>&1` in Posix shells.

`closed-fds`

The value is a list of file descriptors to be closed.  By default, all file descriptors
not mentioned in the plist are closed.

`buffer`  
`char-buffer`

These keys are used to define the size of the binary/character-conversion buffer
used for any pipes that are created by this call to `make-process`. 

If the symbol is `block` or the key is omitted, the pipe is buffered
using an implementation-specified buffer size.
If the symbol is `none`, there is no buffering.
If the symbol is `line`, the pipe is line-buffered.
An exact integer specifies the size of the buffer.
See [FilesAdvancedCowan](FilesAdvancedCowan.md) for details.

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
If `mode` in an exact integer, it specifies the process group id
to which the child process will belong.
If it is the symbol `new` it belongs to the same session but in a new process group.
If it is the symbol `new-session`, it belongs to a new session and a
new process group; in Windows this implies a new console.

`wait`

If the value is `#f` or the key is omitted, `make-process` returns as soon as the child
process is created.  If the value is `#t`, `make-process` returns when the child terminates.

## Process object accessors

The following procedures extract values from
the process object returned by `make-process`.

`(process-child-id `*process*`)`

Returns the process id of the child process as an exact integer.

`(process-child-group `*process*`)`

Returns the process group id of the child process as an exact integer.

`(process-child-session `*process*`)`

Returns the session id of the child process as an exact integer.

`(process-stdin `*process*`)`

Returns the write end of the pipe connected to the child's standard input as an output port.
Whether it is a binary or textual port depends on how the pipe was created.
Returns `#f` if the child's standard input was not specified as `binary-pipe` or `textual-pipe`.

`(process-stdout `*process*`)`  
`(process-stderr `*process*`)`

Returns the read end of the pipe connected to the child's standard output/error as an input port.
Whether it is a binary or textual port depends on how the pipe was created.
Returns `#f` if the child's standard output/error was not specified as `binary-pipe` or `textual-pipe`.

`(process-fd `*process fd*`)`

Returns the appropriate end of the pipe connected to the child's file descriptor *fd*.
Whether it is an input or an output port or a binary or textual port depends on how the pipe was created.
Returns `#f` if the child's *fd* was not specified as  `binary-pipe` or `textual-pipe`.


## Process termination procedures

(`process-terminated? `*process*`)`

Returns `#t` if the process has terminated either normally or on a signal, and `#f` otherwise.

`(process-stopped? `*process*`)`

Returns `#t` if the process has stopped on a signal, and `#f` otherwise.

`(process-exit-code `*process*`)`

Returns the exit code as an exact integer if the process has terminated normally, or #f if not.

`(process-stop-signal `*process*`)`

Returns the signal number as an exact integer if the process has stopped on a signal, or #f if not.

`(process-terminate-signal `*process*`)`

Returns the signal number as an exact integer if the process has terminated on a signal, or #f if not.

`(process-wait `*process-or-pid*`)`

Waits for the specified child process to terminate.  Returns the process object.

`(process-wait-any)`

Waits for any child process to terminate.  Returns the process object.

`(process-wait-group `*process-or-pgid*`)`

Waits for any of the processes in the process group
specified by the process object or the exact integer process group id to terminate.
Returns the process object of the terminated process.

`(process-terminate `*process*`)`

Sends the SIGTERM signal to the child process.
Returns an unspecified value.

`(process-send-signal `*process-or-pid signal*`)`

Sends the signal *signal* to the child process, which may be
a process object or an exact integer process id.
Returns an unspecified value.

`(process-send-group-signal `*process-or-pgid signal*`)`

Sends the signal *signal* to all the processes in the process
group specified by the exact integer process group id or the
process object.

## Fork and exec

These procedures are not portable to Windows (they will raise errors) and should be avoided when possible.

`(process-fork)`

Forks the current process.  Returns a process object in the parent process
and `#f` in the child object.

`(process-fork `*thunk*`)`

Forks the current process and returns a process object in the parent process.  The child
process immediately invokes *thunk* and exits using the value that *thunk* returns.

`(process-exec `*setup cmd . args*`)`

Executes *cmd* in the current process, passing *args* to it.  All keys
in *setup* except `closed-fds`, `path`, `arg0`, and `env` are ignored.
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