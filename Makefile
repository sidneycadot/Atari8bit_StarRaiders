
.PHONY : test copy default clean

default : starraiders.car check

check : starraiders.bin
	md5sum -c MD5SUM

starraiders.car : cartridge-header.bin starraiders.bin
	cat $^ > $@

starraiders.bin : starraiders.o
	ld65 -o starraiders.bin -C starraiders.lnk starraiders.o

starraiders.lst starraiders.o : starraiders.asm
	ca65 -l starraiders.lst starraiders.asm -o starraiders.o

clean :
	$(RM) *.o *~ *.map *.lst starraiders.bin starraiders.car
