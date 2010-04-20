I didn't anticipate this being a WG1 issue, but recent interest in binary I/O on the mailing list has compelled me to write up this proposal, which I originally intended as an implementation proposal for Chicken Scheme. As I said, I don't think this should be in WG1, but it may offer inspiration for whatever aspects of BinaryData we *do* put in WG1.

See ticket #50 for relevant work and decisions in this area.

So I offer it up.

= The Problem =

Although you can do anything with just pairs, people like having vectors for efficiency; not just simple "Oh look it goes faster" efficiency, but they have a fundamentally better big-O access characterstic for indexed operations, than the traditional linked lists of pairs. Many would agree that this justifies their inclusion in WG1, rather than being a mere "implementation performance hack".

And so this logically leads to SRFI-4 homogenous vectors, which again don't let you store any data you couldn't store as an ordinary vector, or even as a linked list of pairs; but does it faster for special cases of homogenous numeric types. This is a feature that languages like C offer natively, and it's heartwarming to see that such things *can* be done in the supposedly efficiency-hostile highly dynamic world of Scheme; even if they weren't useful in their own right, they strike a blow at certain annoying types of argument for using C.

However, one area where SRFI-4 homogenous vectors come in very handy is in interfacing to the world outside of Scheme. This includes foreign-function interfaces, which often expose a C-like memory model that natively has homogenous numeric vectors, so we need a Scheme interface to them as part of that; but it also, more pedestrianly, includes binary I/O to filesystems, networks, and the like.

"Binary I/O" is a vague and overloaded term, but in practice, it generally means communication in terms of a series of eight-bit bytes; that's the fundamental primitive model of POSIX file-descriptor I/O, at least.

This leads one to assume that an SRFI-4 u8vector or s8vector is a logical model for what one reads and writes through a binary I/O port. But which? Unsigned or signed? The 'byte' model suggests unsigned.

But hang on - we often communicate multibyte numbers via binary I/O using some endianness convention or other. Or we communicate things like bit streams (mainly in the data compression world), padded to integer byte lengths. It's rash to assume that we're actually communicating in eight-bit chunks at all, even though our I/O interface gives us the byte as the unit of I/O granularity. We might well communicate a stream of floats over an I/O port.

= The Solution =

This lead me to deduce that a "vague sea of bytes" type is needed *in addition* to the SRFI-4 homogenous vectors. One that states nothing about the interpretation of the bytes within it, but which provides a central rallying point for interpretations to be added.

"Blob" is an acceptable name for such a thing, but perhaps already overloaded. I shy away from calling it a "vector" as that implies structure that is not there. Perhaps "bytestring" is a better name, as it implies a basic unit, but with the expectation that larger-scale structures are made of those units, rather than them being independent and orthogonal.

Binary I/O should read and write bytestrings. Bytestrings provide only a single operation - they can tell you how many bytes are in them.

However, all the SRFI-4 vector types should be instantiable against slices of a bytestring. In effect, you can say "Let me treat the N bytes starting at offset M of bytestring S as a {u8vector|s8vector|u16vector|...} with endianness E" These SRFI-4 vectors should share their storage with the bytestring, so if you set! elements of them, the bytestring itself changes. Thereby, providing a bidirectional way to map between bytestrings and actual defined interpretations of values represented in those bytes.

Advanced stuff like text<->binary codecs can work in this model, too. Create a decoder object and keep feeding it references to byte regions in bytestrings on the one end, and pull out character strings on the other end. Or vice versa with an encoder.

It might be useful to provide some low-level tools to copy regions of bytes around within bytestrings, to help with joining headers onto messages and the like, a common requirement in protocols. Also, being able to create a bytestring that shares the storage with a substring of another bytestring would be good, as it would allow one to write recursive functions over bytestrings easily.

C FFIs would naturally map `void *` to/from bytestrings (if it can find a `size_t` length from somewhere...)

It would be possible to efficiently avoid issues with unaligned loads/stores on platforms afflicted with those, by having the procedure that generates an SRFI-4 vector interpretation of a region of a bytestring check the alignment of the base of the vector at creation time, and return either a fast version that does simple loads and stores for aligned subvectors, or returns a slow version that shuffles bytes to extract unaligned elements for unaligned subvectors.

As for a printed representation of bytestrings? I'd say either go with a u8vector interpretation of the bytestring (eg, `#bs(195 12 0 255 ...)`), or base64 for compact representation of what is, after all, likely to be highly human unreadable binary data (eg, `#bs"YXNkZmFzZGYK"`); writing is as a sequence of u8s is potentially misleading, as it implies a structure that may or may not be there.