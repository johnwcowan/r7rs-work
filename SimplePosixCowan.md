## Interfaces

WG2 voted to provide a Posix package, but rejected a "full Posix" package that would provide all 1191 interfaces from POSIX.1-2008.  What is proposed here is a subset of 209 interfaces that are provided primitively (that is, as a system call) in at least one of FreeBSD, NetBSD, OpenBSD, Linux, and Solaris.  The links are to the OpenGroup page describing the interface.

```

fork => pid
kill(pid, sig) => void
sigaction(sig, sigact) => sigact
sigprocmask(how, sigset) => sigset
sigpending(sigset) => void
sigsuspend(sigset) => void
alarm(seconds) => seconds-left
pause() => void
sleep(seconds) => seconds-left
getgroups(ngroups) => grouplist
getlogin() => string
getpgrp() => pgrp
setsid() => sid
setpgid(pid) => pgid
uname() => utsname
time() => time
times(tms) => time
ctermid() => string
sysconf(name) => value
chdir(path) => void
getcwd() => path
open(path, flags, mode) => fd
creat(path, flags, mode) => fd
umask(cmask) => cmask
link(old, new) => void
mkdir(path, mode) => void
mkfifo(path, mode) => void
rmdir(path) => void
rename(old, new) => void
chmod(path, mode) => void
chown(path, owner, group) => void
utime(path) => timbuf
pipe() => readfd, writefd
close(fd) => void
read(fd, buf, start, end) => bytes-read
write(fd, buf, start, end) => bytes-written
fcntl(fd, command, argin) => argout
fseek(fd, offset, whence) => position
tcgetpgrp(fd) => pgid
tcsetpgrp(fd, pgid) => void
const(name) => value
posixio(new) => old
fileno(port) => fd
fdopen(fd, port, access) => void
fseek(port, offset, whence) => void
ftell(port) => offset
localtime(seconds) => atime

execv/execve/execvp(path, file, argv, env)
wait/waitpid(pid, options) => stat, retpid
wifexited/wexitstatus/wifsignaled/wtermsig/wifstopped/wstopsig
sigemptyset/sigfillset/sigaddset/sigdelset/sigismember(sigset, signo) => various
getpid/getppid() => pid/ppid
getuid/geteuid/getgid/getegid() => uid/gid
setuid/setgid(uid/gid) => void
getenv/setenv/clearenv(name, value, new, overwrite) => void
ttyname(fd) => string, isatty(fd) => bool
opendir/readdir/rewinddir/closedir(dirname/dirid) => dirid/dirent
stat/fstad(path/fd) => stat
fdup(fd) => fd, fdup2(oldfd, newfd) => fd
cfgetospeed/cfgetispeed/cfsetospeed/cfsetispeed(termios, speed) => speed
tcgetattr(fd) => termios, tcsetattr(fd. optacts, termios) => void
tcsendbreak/tcdrain/tcflush/tcflow(fd, duration/queue/action) => void
getgrgid/getgrnam(gid/name) => group
getpwuid/getpwnam(gid/name) => passwd


records: sigset, sigaction, utsname, tms, dirent, stat, utimbuf, flock, termios, group, passwd
```