## Interfaces

WG2 voted to provide a Posix package, but rejected a "full Posix" package that would provide all 81
headers, 1191 interfaces, and 51 data types from POSIX.1-2008.  What is proposed here is based
directly on the Posix bindings for Lua, which are modern and thorough
without being insanely comprehensive.  They have been reduced to those
available directly or indirectly in `scsh`.
Interfaces for networking, ptys, resource limits,
message queues, and time (plus various lesser-used interfaces) have been removed as well.
(28 headers, 159 interfaces, and 11 data types).

All the headers:  dirent, errno, fcntl, glob, grp, libgen,
poll, pwd, signal, stdio, stdlib, sys/msg, sys/resource, sys/socket,
sys/stat, sys/time, sys/times, sys/utsname, sys/wait,
termio, unistd, utime.

## Detailed functions by header

```

posix-dirent: opendir (\[path="."])
posix-dirent: readdir
posix-dirent: closedir
posix-fcntl: fcntl (fd, cmd\[, arg=0])
posix-fcntl: open (path, oflags\[, mode=511])
posix-glob: glob (\[pat="\*"], flags)
posix-grp: getgrgid (gid)
posix-grp: getgrnam (name)
posix-poll: poll (fds[, timeout=-1])
posix-pwd: getpwnam (name)
posix-pwd: getpwuid (uid)
posix-signal: kill (pid, opt)
posix-signal: killpg (pgrp[, sig=`SIGTERM`])
posix-stdio: ctermid ()
??? posix-stdio: fdopen (fd, mode)
posix-stdio: fileno (file)
posix-stdio: rename (oldpath, newpath)
posix-stdlib: abort ()
posix-stdlib: realpath (path)
posix-sys-stat: S_ISBLK (mode)
posix-sys-stat: S_ISCHR (mode)
posix-sys-stat: S_ISDIR (mode)
posix-sys-stat: S_ISFIFO (mode)
posix-sys-stat: S_ISLNK (mode)
posix-sys-stat: S_ISREG (mode)
posix-sys-stat: S_ISSOCK (mode)
posix-sys-stat: chmod (path, mode)
posix-sys-stat: fstat (fd)
posix-sys-stat: lstat (path)
posix-sys-stat: mkdir (path\[, mode=511])
posix-sys-stat: mkfifo (path\[, mode=511])
posix-sys-stat: stat (path)
posix-sys-stat: umask (\[mode])
posix-sys-times: times ()
posix-sys-utsname: uname ()
posix-sys-wait: wait (\[pid=-1\[, options]])
posix-termio: tcdrain (fd)
posix-termio: tcflow (fd, action)
posix-termio: tcflush (fd, action)
posix-termio: tcgetattr (fd)
posix-termio: tcsendbreak (fd, duration)
posix-termio: tcsetattr (fd, actions, a)
posix-termio: ccs
posix-termio: termios
posix-unistd: access (path[, mode="f"])
posix-unistd: alarm (seconds)
posix-unistd: chdir (path)
posix-unistd: chown (path, uid, gid)
posix-unistd: close (fd)
posix-unistd: dup (fd)
posix-unistd: dup2 (fd, newfd)
posix-unistd: exec (path, argt)
posix-unistd: execp (path, argt)
posix-unistd: fork ()
posix-unistd: fsync (fd)
posix-unistd: ftruncate (fd, length)
posix-unistd: getcwd ()
posix-unistd: getegid ()
posix-unistd: geteuid ()
posix-unistd: getgid ()
posix-unistd: getgroups ()
posix-unistd: gethostid ()
posix-unistd: getpgrp ()
posix-unistd: getpid ()
posix-unistd: getppid ()
posix-unistd: getuid ()
posix-unistd: isatty (fd)
posix-unistd: lchown (path, uid, gid)
posix-unistd: link (target, link[, soft=false])
posix-unistd: lseek (fd, offset, whence)
posix-unistd: nice (inc)
posix-unistd: pipe ()
posix-unistd: read (fd, count)
posix-unistd: readlink (path)
posix-unistd: rmdir (path)
posix-unistd; setuid (uid)
posix-unistd; seteuid (uid)
posix-unistd; setgid (uid)
posix-unistd; setegid (uid)
posix-unistd; setpgid (pid, pgid)
posix-unistd; setsid ()
posix-unistd: sync ()
posix-unistd: tcgetpgrp (fd)
posix-unistd: tcsetpgrp (fd, pgid)
posix-unistd: truncate (path, length)
posix-unistd: ttyname ([fd=0])
posix-unistd: unlink (path)
posix-unistd: write (fd, buf)
posix-utime: utime (path[, mtime=now[, atime=now]])
```

## List of record types:
```

posix-grp: PosixGroup
posix-poll: PosixPoll
posix-pwd: PosixPasswd
posix-sys-stat: PosixStat
posix-sys-times: PosixTms
posix-time: PosixTimespec
posix-time: PosixTm
```