# Strace parser

Tool for parsing strace output so that file access is easy to follow
even if file descriptors are duplicated and closed in unpredictable
manner.

Specially designed for serial traffic monitoring on the host system
without special hardware.

## To watch file access

```sh
strace -xx -ttt -s99999 -eopen,read,write,ioctl,dup,du2,dup3,connect -o TRACEFILE CMD
runhaskell Main.hs <TRACEFILE
```

There is also `MainParser.hs` which does just strace parsing. Easies debugging
