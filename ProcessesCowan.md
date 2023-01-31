A pre-SRFI for a high-level process management library.
It's based to some degree on the Python 3 subprocess module.

## Issues

Issue 3: Consider including these procedures:

* (open-control-tty tty-name [flags])
* (become-session-leader)
* (tty-process-group fd/port/fname)
* (set-tty-process-group fd/port/fname pgrp)
* (control-tty-file-name)

Issue 4: Should the section "Fork and exec" be removed?

## Constructors

`(make-pipe)`

Makes a Posix pipe and returns two values, both binary ports.
The first value is the read end of the pipe, the second value is the write end.
The caller may use the pipe internally as a queue (provided it does not get full),
pass one end to a subprocess and use the other to communicate with it,
or pass both ends to different processes.
In the latter two cases, the caller should close the port after passing it.

`(make-textual-pipe)`

The same as `make-pipe`, except that the ports are textual.
The encoding is implementation-dependent.
If a specific encoding is required, use `make-pipe` and then
the operations of [SRFI 181](https://srfi.schemers.org/srfi-181/srfi-181.html)
to convert the binary ports to textual ports.

Note that whether a pipe passed to a child
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
then the use of `select` to decide which pipe is ready
to be read or written is advisable.

`(with-process-runner `*proc*`)`

Creates a process-runner, an opaque object with which newly created processes
can be registered.  The procedure *proc* is invoked on the process runner.
When *proc* returns, `with-process-runner` waits for all registered processes
to terminate and then returns a list of the processes in arbitrary order.

A process is registered by specifying a `runner` key in the setup dictionary
whose value is the process-runner.  Unless the `group` key specifies otherwise,
all registered processes run in the same newly created process group.

## The setup dictionary

This dictionary (whose implementation is not yet defined)
maps either symbols or exact integers to Scheme values.

`stdin`  
`stdout`  
`stderr`

Specifies a port to be connected to the standard input/output/error of the
child process.  It is an error to associate an output port with
the key `stdin` or an input port with the keys `stdout` and `stderr`.
The values are interpreted as follows (any other value is an error):

  * If the value is a file, socket, pipe, or other port that contains a
    file descriptor, that fFle descriptor is duplicated onto child port 0/1/2.
    However, if the port is a buffered output port, it is flushed
    as if by `flush-output-port` before it is duplicated, so that the port and
    the file descriptor are synchronized.
    It is an error if the value is any other kind of port such as a string
    or bytevector port.
   
  * If the value is `#t` or the key is omitted, the port in the child process
    is the same as the port in the parent process.
    
  * If the value is `#f`, the port in the child process is closed.
  
  * If the value is the symbol `null`, the port in the child process is connected to
    the null device (`/dev/null` in Posix, `NUL` in Windows),
    so that an attempt to read from the port produces an immediate end of file,
    and an attempt to write to the port is ignored.
    
*exact non-negative integer*

The same as `stdin`, `stdout`, and `stderr`,
but specifies the file descriptor to be used in the child process.
File descriptors other than 0, 1, and 2 that do not appear as keys in the setup dictionary
are closed in the child process.

`stdout+stderr`

The same as `stdout`, but binds both the standard output and the standard error
in the child process to the same port.
It is an error to provide either `stdout` or `stderr`
if this key is present in the setup plist.
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
If the same key appears more than once in the alist, the first value is used,
just as with `assoc`.

`current-directory`

Specifies the current (working) directory of the newly created process as a string.

`umask`

Specifies the Posix umask of the newly created process as an exact integer.

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

`runner`

Specifies a process-runner object that controls the execution of this process,
as explained above.

## Synthetic process objects

`(pid->process `*pid*`)`

Creates a synthetic process object wrapping an arbitrary process id.
Because the process is not necessarily a child of the current process,
process object accessors may return `#f` unexpectedly
or read from a file such as `/proc/<pid>/status`.
It is always possible to send signals to a synthetic process object.

`(current-process)`

Creates a synthetic process object wrapping the process id of the current process.

## Path search

`(path-search `*filename [dirpath]*`)`

The string *dirpath*
(whose default is the value of the environment variable `PATH`)
is split by a directory separator character
into a sequence of directory names.
Each directory name has a path separator character
appended to it followed by *filename* to form a sequence of pathnames.
The first such pathname that names an existing file
(whether it is executable or not) is returned by `path-search`;
if there are no such pathnames, `#f` is returned.
(The pathname may be modified in a system-dependent way,
as by adding an `.exe` suffix.)

On Posix systems, the directory separator character is `:`
and the path separator character is `/`.
On Windows, the directory separator character is `;`
and the path separator character is `\`.

Note that the above behavior is not necessarily exactly the same
as the behavior of the `path` key applied to `make-process`.

## Process object predicates

`(process? `*obj*`)`  
`(synthetic-process? `*obj*`)`

Returns `#t` if *obj* is a (synthetic) process object and `#f` otherwise.

## Process object accessors

The following procedures extract values from a process object.
If the answer is not yet known, these procedures return `#f`
rather than waiting for the process to complete.

These should all work correctly on children of the calling process.
They can be performed on other processes
by groveling in the `/proc` file system;
if that is unavailable (as on MacOS), `#f` can always be returned.

`(process-id `*process*`)`

Returns the process id of the process as an exact integer.

`(process-group `*process*`)`

Returns the process group id of the process as an exact integer.

`(process-session `*process*`)`

Returns the session id of the process as an exact integer.

`(process-exit-code `*process*`)`

Returns the exit code as an exact integer if the process
has terminated normally, or `#f` if not.

`(process-stop-signal `*process*`)`

Returns the signal number as a symbol if the process has stopped on a signal,
or `#f` if not.  The exact set of symbols is implementation-dependent, but
they are upper-case and begin with `SIG`.

`(process-terminate-signal `*process*`)`

Returns the signal number as a symbol if the process has terminated on a signal,
or `#f` if not.  The exact set of symbols is implementation-dependent, but
they are upper-case and begin with `SIG`.

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

These procedures are not portable to the Win32 API (they will raise errors satisfying
`process-exception?`) and should be avoided when not necessary.
However, they add a great deal of power and flexibility to the creation of process graphs.
If the *narrow?* argument is false or absent, all threads present in the parent process
are also present in the child.  If it is true, only a subset of threads that include
the calling thread is present in the child process.  Which threads exist depends on the
mapping of Scheme threads to Posix threads.

`(process-fork `[*narrow?*]`)`

Forks the current process.  Returns a process object in the parent process
and `#f` in the child object.  Both processes execute the continuation
of the call to `process-fork`.

`(process-fork `*thunk* [*narrow?*]`)`

Forks the current process and returns a process object in the parent process,
which then executes the continuation of the call to `process-fork`.  The child
process immediately invokes *thunk* and exits using the value that *thunk* returns.

`(process-exec `*setup cmd . args*`)`

In the current process, replaces the currently executing program with *cmd*,
passing *args* to it.  All threads except the current thread are terminated.
All keys in *setup* except `open-fds`, `path`, `arg0`, `env`, `current-directory`
and `umask` are ignored.  This procedure never returns (but may throw an exception).

## Exceptions

If any of the operations required by the procedures in this SRFI return an error code,
an error satisfying `process-exception?` is signaled.

`(process-exception? `*obj*`)`

Returns `#t` if *obj* is a process exception and `#f` otherwise.

`(process-exception-errno` *process-exception*`)`

Returns a Posix error number corresponding to the exception, or `#f` if there is none.

`(process-exception-message `*process-exception*`)`

Returns a string associated with *process-exception* indicating the nature of the problem.
