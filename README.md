# Strace parser

## To capture trace

strace -xx -ttt -s99999 -eopen,read,write,ioctl,dup,du2,dup3,connect -o TRACEFILE CMD
