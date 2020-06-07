all: SpaceGame.nes

main.o: src/main.asm
	ca65 src/main.asm

reset.o: src/reset.asm
	ca65 src/reset.asm

SpaceGame.nes: main.o reset.o
	ld65 src/*.o -C nes.cfg -o SpaceGame.nes

clean:
	rm src/*.o