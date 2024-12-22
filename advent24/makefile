ADVENT_LIB = -Wl,-rpath,./advent/build -L./advent/build -ladvent
CFLAGS = -O3 -I./advent
HFLAGS = -O3 -outputdir /dev/shm/ghc -o /dev/shm/ghc/main

day22: ./day22/main.hs
	ghc $(HFLAGS) ./day22/main.hs
	/dev/shm/ghc/main "$(shell pwd)/day22/input"

%: libadvent.so ./%/*.c
	mkdir -p ./$*/build
	gcc $(CFLAGS) -o ./$*/build/$* ./$*/*.c $(ADVENT_LIB) -lm
	./$*/build/$* ./$*/input

libadvent.so: ./advent/*.c
	mkdir -p ./advent/build
	gcc $(CFLAGS) -fPIC -shared -o ./advent/build/libadvent.so ./advent/*.c
