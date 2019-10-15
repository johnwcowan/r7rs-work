**This has nothing to do with R7RS-large, at least not yet.**

This is FAT-12 as described by informal sources and checked against
[ECMA-107](http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-107.pdf).
Note that there are no subdirectories.
All 2-byte numeric values are stored little-endian.

## FAT boot sector (4096 bytes)

0-2: `eb 3c 90`  
3-10: "FAT-ARCH"  
11-12: 512 (bytes per sector)  
13: 8 (sectors per cluster, so a cluster is a 4K disk block)  
14-15: 1 (number of sectors before the FAT)  
16: 1 (number of FATs, normally 2 but we save space with just 1)  
17-18: 224 (number of directory entries)  
19-20: max 4084 (number of clusters)  
21: `f8` (hard disk)  
22-23: 9 (sectors per FAT)  
24-25: 16 (arbitrary sectors per track)  
26-27: 1 (arbitrary number of heads)  
28-29: 0 (number of sectors before this sector)  
30-509: all 0  
510-511: `55 aa`

## File Allocation Table

12-bit FAT entries, each corresponding to a cluster, point to the next cluster in the file,
or `000` for free cluster, `ff7` for bad cluster, and `fff` for no more clusters.

9 sectors * 512 bytes = 4608 bytes.

3 FAT entries are stored in 2 bytes as follows:  `uv`, `wx`, `yz` are stored `xuv`, `yzw`.
That allows 6912 entries, but the max clusters is 4084, so the rest are filled
out with `ff7`.  Entry 0 is `1f8`, entry 1 is `fff`.

## Root directory

14 sectors * 512 bytes = 7168 bytes.

Each directory entry is 32 bytes, so 224 entries are available.
File names and extensions can contain ASCII upper case letters, digits, or `_`.

All unused entries are all-zero.  Deleted entries (probably not
supported) have `e5` in position 0.

0-7: File name padded with trailing spaces  
8-10: File extension padded with trailing spaces  
11: 1 (read only)  
12-21: all 0  
22-23: 0 (date, ignored)  
24-25: 0 (time, ignored)  
26-27: starting cluster, or `000` for an empty file  
28-31: file size in bytes

## File data

The rest of the file system contains the clusters of the external data.
The first cluster is always numbered 2, even though there are 24 sectors = 3 clusters before it.
The clusters of a file can appear anywhere in the file data area.

## API for writing

Note: This implementation guarantees that the clusters of all files appear in increasing order
within the file data area.

`(fat-create `*filename*`)` -> fat-object

Create the file and allocate the empty in-memory FAT and root directory.
Write out a boot sector onto the output port.
Write out the in-memory FAT and root directory.
The fat-object that is returned refers to these.
It also refers to the output port and to a list of open fat-file-objects
(initially empty).

`(fat-create-file `*fat-object filename*`)` -> fat-file-object

Update the *fat-object* by inserting a new root directory entry.  Allocate a one-block buffer.
The fat-file-object that is returned refers to the fat-object,
the directory entry, the buffer, and a zero index into the buffer.
Add it to the list of open fat-file-objects in the fat-object.

`(fat-write `*fat-file-object bytevector*`)` -> undefined

Copy the bytes of *bytevector* into the buffer of *fat-file-object*
at the index, and then update the index.
If the buffer fills, write it to the fat-object's output port,
update the in-memory FAT and root directory, reset the buffer index.

`(fat-complete-file `*fat-file-object*`)`

Flush the buffer even if it isn't full, write it out, and update the directory.
Remove *fat-file-object* from the list in the fat-object.

`(fat-complete `*fat-object*`)`

Complete all uncompleted files.  Rewind the output port.
Write out a boot sector containing the number of sectors actually in use.
Write out the in-memory FAT and root directory.

## API for reading

TBD
