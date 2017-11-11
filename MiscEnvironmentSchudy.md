[says: These are proposals I excluded from SRFI 112 because they are primarily for discrimination or computation rather than logging or display.  Some will probably be moved to other proposals.](JC)

## C memory model

* `word-size` (or `word-width` or `pointer-`*): reports the size of a Scheme pointer

* `cpu-word-size` (or `cpu-word-width`): reports the machine's largest available pointer size (which is often different, e.g. a 32-bit Scheme on a 64-bit machine)

* `fixnum-width`, `least-fixnum`, `greatest-fixnum` determine fixnum range.

* Also, per-object overhead matters (especially if you use a lot of pairs or boxed floats). So does the x2 multiplier for a simple copying GC.

* [adds:  `short-size`, `int-size`, `long-size` to determine the size of C shorts, ints, and longs in bytes; need some others too for `ptrdiff_t` and so on.](JC)

## Other

* `username`: mostly covered by `(get-environment-variable "USER")`, so
maybe a separate function is not necessary.

* `user-home-directory` (or just `user-home`): ~. On single-user systems, this should be the usual place for user files, e.g. the root of the main drive on Mac OS Classic. Already sort of covered by `(get-environment-variable "HOME")`.

* `settings-directory` (or `user-settings-directory`): where programs should save their user-specific settings. On Unix, this is the same as user-home-directory; on Mac OS X and Windows, it's a separate subdirectory. Should be provided in addition to user-home-directory for portability.

* `current-directory` (or `working-directory`): sometimes convenient for pathname resolution. On systems that don't have this concept, return the root or other default directory.

* `locale`: already covered by SRFI-29, and by `(get-environment-variable "LANG"`).

* `number-of-cpus` (or `cpu-count`): to determine how many threads to use.  This really belongs in a multithreading SRFI, but SRFI-21 doesn't have it.  [says: This is useful for parallelism rather than concurrency, where you may want more threads than CPUs, so probably belongs in a futures SRFI.](JC)

* `cpu-clock`, `cpu-frequency` (or `cpu-clock-`*): to estimate available power, e.g. to decide whether to turn on expensive optional features. Returns a frequency in hertz (or MHz?).

* `physical-memory`: how much memory does the machine have? Returns an exact integer in bytes, not Scheme pointers, because that's the conventional unit and because large data is often not composed of pointers. This might not fit in a fixnum, so maybe it should use a larger unit than bytes.

* `cache-memory` (or `cache-size`): how large is the cache, if any? This is often more important for performance than physical-memory. When there's more than one level of cache, it should return the one with the largest effect (L2, on present-day machines).

* `memory-used`: how much (virtual) memory is the Scheme implementation currently using? (Including garbage: this is about the process, not the GC.)

* `memory-limit`: how much (virtual) memory can the Scheme implementation use? This is a bit hard to implement, because it depends on word size, available swap, and quotas (e.g. on Unix, the soft limit from `getrlimit(RLIMIT_AS)`).

* `disk-free` (or `disk-available`): how much disk space can the program use?  This is important to avoid filling up disks with ever-growing data like logs or caches. With one argument, which must be a pathname, return the available space for that path's drive; with zero arguments, return the space for some default drive. Should ideally take the user quota into account, if any. The name should maybe not include "disk", since that's sometimes inaccurate, but I can't think of an alternative that's as easy to understand ("drive" isn't, and isn't much more accurate).

* `stack-limit`: approximately how deeply can we nest non-tail calls without overflowing the stack? This is often an issue for functional programs. I'm not sure if the unit should be bytes or calls; the latter is more useful but unreliable, since stack-frame sizes may vary a lot. Returns `#f` if there's no limit but available memory. Ideally this should return the remaining depth, so recursive functions can bail or change strategies when they get close to the limit.
