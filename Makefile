all: helloworld.nes

helloworld.o: src/helloworld.asm
	ca65 src/helloworld.asm

reset.o: src/reset.asm
	ca65 src/reset.asm

helloworld.nes: helloworld.o reset.o
	ld65 src/*.o -C nes.cfg -o helloworld.nes