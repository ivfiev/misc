ADVENT_LIB = -Wl,-rpath,./advent/build -L./advent/build -ladvent
CFLAGS = -O3 -I./advent
HFLAGS = -O2 -outputdir /dev/shm/ghc -o /dev/shm/ghc/main

day25: ./day25/main.hs
	mkdir -p /dev/shm/ghc
	ghc $(HFLAGS) ./day25/main.hs
	/dev/shm/ghc/main "$(shell pwd)/day25/input"

day24: ./day24/main.hs
	mkdir -p /dev/shm/ghc
	ghc $(HFLAGS) ./day24/main.hs
	/dev/shm/ghc/main "$(shell pwd)/day24/input"

day23: ./day23/main.hs
	mkdir -p /dev/shm/ghc
	ghc $(HFLAGS) ./day23/main.hs
	/dev/shm/ghc/main "$(shell pwd)/day23/input"

day22: ./day22/main.hs
	mkdir -p /dev/shm/ghc
	ghc $(HFLAGS) ./day22/main.hs
	/dev/shm/ghc/main "$(shell pwd)/day22/input"

%: libadvent.so ./%/*.c
	mkdir -p ./$*/build
	gcc $(CFLAGS) -o ./$*/build/$* ./$*/*.c $(ADVENT_LIB) -lm
	./$*/build/$* ./$*/input

libadvent.so: ./advent/*.c
	mkdir -p ./advent/build
	gcc $(CFLAGS) -fPIC -shared -o ./advent/build/libadvent.so ./advent/*.c
