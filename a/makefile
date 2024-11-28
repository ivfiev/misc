program: program.o lib.o
	gcc -o ./build/program ./build/lib.o ./build/program.o -no-pie
program.o: program.asm
	nasm -f elf64 -g -F dwarf program.asm -l ./build/program.lst -o ./build/program.o
lib.o: lib.asm
	nasm -f elf64 -g -F dwarf lib.asm -l ./build/lib.lst -o ./build/lib.o

clean:
	rm -rf ./build

rebuild:
	make clean && make
