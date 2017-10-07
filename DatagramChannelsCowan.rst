== Datagram channels ==

Datagram channels are a mild abstraction of UDP sockets.  They are a disjoint type.

See NetworkEndpointsCowan for network endpoints, an abstraction of a host or address plus a port number.  Several of the following procedures accept or return endpoints.

== Procedures ==

`(make-datagram-channel `[''endpoint'']`)`

Returns a new datagram channel suitable for sending and receiving datagrams.  ''Endpoint'' specifies the local endpoint of the channel; if it is omitted, the endpoint will specify any local address and a port specified by the operating system, the equivalent of `(make-network-endpoint #t 0)`. In Posix terms this is `socket()` immediately followed by `bind()`.

`(make-output-only-datagram-channel)`

Returns a newly allocated datagram channel only suitable for sending datagrams.  It is not bound to any port.  In Posix terms this is `socket()`.

`(datagram-channel? `''obj''`)`

Returns `#t` if ''obj'' is a datagram channel and `#f` otherwise.

`(datagram-channel-local-endpoint`''channel''`)`

Returns the local endpoint or `#f` if there is none.  The value returned may not be the same as the value passed to `make-datagram-channel` (in particular, the port must not be 0), but it must be acceptable to another invocation of `make-datagram-channel`.

`(datagram-channel-send-to `''channel''` `''endpoint''` `''bytevector''` `[[''start''|[''end'']]]`)`

Send the portion of ''bytevector'' defined by ''start'' (inclusive) and ''end'' (exclusive) to ''endpoint'' using ''channel''.  If ''end'' is omitted, it is the length of ''bytevector'' plus one; if ''start'' is omitted, it is 0.  Returns an undefined value.  In Posix terms this is `send()`.

`(datagram-channel-receive-from `''channel''` `''bytevector''` `[[''start''|[''end'']]]`)`

Receives a datagram from ''channel'' into the portion of ''bytevector'' defined by ''start'' (inclusive) and ''end'' (exclusive).  If ''end'' is omitted, it is the length of ''bytevector'' plus one; if ''start'' is omitted, it is 0.  Returns two values: the sending endpoint and the number of bytes received, which may be greater than, equal to, or less than ''end - start''.  If it is greater, excess bytes in the datagram are discarded; if it is less, the remaining bytes of the bytevector are unchanged. It is an error to invoke this procedure on an output-only datagram channel.  In Posix terms this is `recvfrom()`.

`(datagram-channel-connect! `''channel''` `''endpoint''`)`

Connects ''channel'' to a remote ''endpoint''.  This endpoint may be on the local host, but it is an error for the port to be 0.  Datagrams can be sent to this endpoint using `datagram-channel-send`, but it is still possible to send datagrams to other endpoints using `datagram-channel-send-to`.  When connected, a datagram channel will ignore any arriving datagrams that do not come from the specified remote endpoint.  Returns an undefined value.  In Posix terms this is `connect()`.

`(datagram-channel-disconnect! `''channel''`)`

Disconnects ''channel''.  Returns an undefined value.  In Posix terms this is `connect()` with an argument whose address family is `AF_UNSPEC`.

`(datagram-channel-remote-endpoint `''channel''`)`

Returns the remote endpoint to which ''channel'' is connected, or `#f` if it is not connected.  The value returned need not be the same as the value passed to `datagram-channel-connect!`, but must be acceptable to another invocation of `datagram-channel-connect!`.

`(datagram-channel-send `''channel''` `''bytevector''` `[[''start''|[''end'']]]`)`

Send the portion of ''bytevector'' defined by ''start'' (inclusive) and ''end'' (exclusive) to the remote endpoint of ''channel''.  If ''end'' is omitted, it is the length of ''bytevector'' plus one; if ''start'' is omitted, it is 0.  It is an error if ''channel'' is not connected.  Returns an undefined value.  In Posix terms this is `send()`.

`(datagram-channel-close `''channel''`)`

Close the underlying UDP port and abandon the channel.

== Example: TFTP ==

TFTP is a simple UDP-based protocol documented in [[http://tools.ietf.org/rfc/rfc1350.txt|RFC 1350]].  For our purposes, all that matters is that the client sends the first datagram and the server sends exactly one response to each datagram (assuming no loss of packets in transmission).

Here is skeleton code for a read-only server:

{{{
(define server-chan (make-datagram-channel (make-network-endpoint 69)))

(define buffer (make-bytevector 512)

(define (tftp-server)
  (let file-loop ()
    (let-values (((endpoint size)
                 (datagram-channel-receive-from tftp-chan buffer))
      (define file (open-input-file (extract-filename buffer 0 size)))
      (define chan (make-datagram-channel))
      (datagram-channel-connect! chan endpoint)
      (let block-loop ((seq 1))
        (define raw-block-length (read-bytevector! file buffer))
        (define block-length
          (if (eof-object? raw-block-length) 0 raw-block-length))
        (datagram-channel-send chan (box-tftp-data buffer 0 block-length seq))
        (let-values (((endpoint acksize)
                     (datagram-channel-receive-from chan buffer)))
          (validate-ack! buffer 0 acksize)
          (if (= size 512) (block-loop (+ 1 seq)))
          (datagram-channel-close chan)
          (close-input-port file)
          (file-loop)))))
}}}

And here is skeleton code for a corresponding client:

{{{
(define (tftp-client hostname filename)
  (define chan (make-datagram-channel))
  (define file (open-output-file filename))
  (define buffer (make-bytevector 512))
  (datagram-channel-connect! chan (make-network-endpoint host 69))
  (datagram-channel-send chan (format-read-request file))

  (let block-loop ((seq 1))
    (let-values (((endpoint size)
                 (datagram-channel-receive-from chan buffer)))
      (write-bytevector file (unbox-tftp-data buffer 0 size))
      (if (= size 512) (block-loop (+ seq 1)))))
  (datagram-channel-close chan)
  (close-output-port file))
}}}

== Issues ==

These names are very verbose, but I couldn't think of a better term than `datagram-channel`, which is borrowed from `java.nio.channels.DatagramChannel`.  This API is more powerful than Java's, though.