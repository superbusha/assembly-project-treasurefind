mount c: c:\
c:
cd tasm
cd bin
tasm /zi base.asm
tlink /v base.obj
cycles = max
base
