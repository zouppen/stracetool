# Strace parser

## To capture trace

strace -xx -ttt -s99999 -eread,write,ioctl,dup,du2,dup3 -o TRACEFILE CMD
