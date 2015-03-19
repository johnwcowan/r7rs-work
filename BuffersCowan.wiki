== Edit Buffers ==

This is a very preliminary description of edit buffers, basically mutable variable-length strings with some pointers into them.  Temporarily, these functions are defined in terms of their [[http://www.gnu.org/software/emacs/manual/html_node/elisp/Buffers.html#Buffers|Emacs Lisp]] equivalents, with the following exceptions:

 * There is no notion of the current buffer, so most procedures have the buffer as the first argument.
 * Buffers do not have intrinsic names, so ''buffer-or-name'' arguments accept only buffers.
 * Point and markers are zero-based, as is usual in Scheme.  Point is an advancing marker and is acceptable to the marker API.
 * Ranges are min-inclusive and max-exclusive, also as is usual in Scheme.
 * The start and end arguments are optional everywhere, as in R7RS-small.

== Procedures ==

This list is ''very'' tentative, and may grow to include SRFI 13, or shrink to exclude unnecessary cruft, or both.

=== Whole buffers ===

(buffer? obj) == (bufferp object)

(buffer-modified? buffer) == (buffer-modified-p)

(buffer-modified! buffer) == (restore-buffer-modified-p t)

(buffer-unmodified! buffer) == (set-buffer-modified-p nil)

(buffer-length buffer) == (buffer-size buffer)

=== Constructors ===

(make-buffer [size-hint]) == simple constructor, no name provided

(buffer->string buffer) == (buffer-string)

=== Point ===

(buffer-point buffer) == (point)

(buffer-point-increment) == (forward-char integer)

(buffer-skip-forward buffer string) == (skip-chars-forward string)

(buffer-skip-backward buffer string) == (skip-chars-forward string)

(buffer-point-at-start? buffer) == (bobp)

(buffer-point-at-end? buffer) == (eobp)

=== Markers ===

If the ''advancing?'' argument to `make-marker` is true, then if an insertion is done at or before the marker, the marker will automatically be repositioned to point to the end of the insertion.

(buffer-marker? obj) == (markerp obj)

(make-marker buffer marker-or-integer advancing?) == (copy-marker marker-or-integer insertion-type)

(marker-position marker) == (marker-position marker)

(marker-buffer marker) == (marker-buffer marker)

(marker-advancing? marker) == (marker-insertion-type marker)

(marker-set-position! marker position) == (set-marker marker position)

=== Accessors ===

(buffer-ref buffer [position]) == (char-after [position]), returns #f on failure

(buffer-set! buffer [position]) == no exact equivalent, does what you expect

(buffer-substring buffer start end) == (buffer-substring start end)

(buffer->string buffer) == (buffer-string)

Depending on #310, possibly consolidate the last two.

=== Comparison ===

buffer-comparator == No Emacs Lisp equivalent, not sure if ordering is appropriate.

=== Basic mutators ===

(buffer-insert! buffer . strings) == (buffer-insert . strings)

(buffer-insert-substring! to-buffer from-buffer [[start|[end]]]) == (insert-buffer-substring ...)

(buffer-erase! buffer) == (erase-buffer)

(buffer-delete! buffer start end) == (delete-region start end)

(buffer-extract! buffer start end) == (delete-and-extract-region start end)

(buffer-delete-characters! buffer count) == (delete-char count)

