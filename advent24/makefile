ADVENT_LIB = -Wl,-rpath,./advent/build -L./advent/build -ladvent
CFLAGS = -O3 -I./advent

%: libadvent.so ./%/*.c
	mkdir -p ./$*/build
	gcc $(CFLAGS) -o ./$*/build/$* ./$*/*.c $(ADVENT_LIB)
	./$*/build/$* ./$*/input

libadvent.so: ./advent/*.c
	mkdir -p ./advent/build
	gcc $(CFLAGS) -fPIC -shared -o ./advent/build/libadvent.so ./advent/*.c
