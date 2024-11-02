g++ -m32 -fPIC -shared -o ./bin/libhack.so hack.cxx
g++ -m32 -o ./bin/main main.cxx
# LD_PRELOAD=./bin/libhack.so ./bin/main
./bin/main