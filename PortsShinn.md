We specify the two ports types - binary and character - as in
[PortsCowan](PortsCowan.md), but make them distinct.  The following procedures are
provided as in the draft, but whether or not they are disjoint is
implementation-dependent:

* (character-port? <obj>)
* (binary-port? <obj>)

```
Rationale: Many existing languages, including some of the most popular
such as C/C++ and Java, enforce a strict distinction between binary
and textual ports.  Allowing programs to freely shift between binary
and textual operations is error-prone and difficult to implement
efficiently, especially if buffered encoding conversions are desired.
```

All ports described in the R5RS are character by default.  New binary
ports may be opened on files with the following procedures:

* (open-binary-input-file <path>)
* (open-binary-output-file <path>)

```
Rationale: The current draft follows Gambit's convention of using
property lists to specify additional file options, such as binary or
textual encoding.  This allows for extensibility, but we do not use
any of this flexibility in WG1, and it is unclear if this is the best
way to specify file options.  It also doesn't address the fact that we
need separate procedures for creating new ports anyway, such as for
TCP/IP ports, and our APIs should be consistent and reflect this.
```


The following operations can be performed on binary ports as in the
current draft:

* (read-u8 [<input-port>])
* (peek-u8 [<input-port>])
* (u8-ready? [<input-port>])
* (write-u8 <octet> [<output-port>])

The following utility is also provided for the common idiom of opening
a port from some arbitrary source, performing some work on it, then
closing the port:

* (call-with-port <port> <proc>)

If we decide not to have `close-port`, which this procedure implicitly depends on, then this proposal will instead provide two procedures:

* (call-with-input-port <port> <proc>)
* (call-with-output-port <port> <proc>)


```
Note that `current-in/output-port` are parameters, and can be
parameterized as such, obviating the need for `with-in/output-port`
shortcuts.
```

