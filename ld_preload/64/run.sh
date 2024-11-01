g++ -fPIC -shared -o ./bin/libhack.so hack.cxx
g++ -fPIC -shared -o ./bin/liblib.so lib.cxx
g++ -o ./bin/main main.cxx -L./bin -llib
LD_LIBRARY_PATH=./bin LD_PRELOAD=./bin/libhack.so ./bin/main