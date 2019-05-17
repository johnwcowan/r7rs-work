**This has nothing to do with R7RS-large, at least not yet.**

All numeric values are stored little-endian.

FAT boot sector (512 bytes):

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

FAT proper:

12-bit FAT entries, each corresponding to a cluster, point to the next cluster,
or `000` for free cluster, `ff7` for bad cluster, and `fff` for no next cluster.

9 sectors * 512 bytes = 4608 bytes.

3 FAT entries are stored in 2 bytes as follows:  `uv`, `wx`, `yz` are stored `xuv`, `yzw`.
That allows 6912 entries, but the max clusters is 4084, so the rest are filled
out with `ff7`.  Entry 0 is `1f8`, entry 1 is `fff`.

Root directory:

14 sectors * 512 bytes = 7168 bytes.

Each directory entry is 32 bytes, so 224 entries are available.
File names and extensions can contain ASCII upper case letters, digits,
`-`, and `_`.  The characters `!#$%&(),` and space (but not leading
or trailing spaces) are also permitted but discouraged.

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
