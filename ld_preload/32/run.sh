g++ -O3 -m32 -fPIC -shared -o ./bin/libhack.so hack.cxx
g++ -O0 -m32 -o ./bin/main main.cxx
# LD_PRELOAD=./bin/libhack.so ./bin/main
./bin/main

# sudo gdb -quiet --command inject.gdb --pid $(pgrep main)
# sudo gdb -quiet --command unject.gdb --pid $(pgrep main)