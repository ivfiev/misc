g++ -O3 -m32 -fPIC -shared -o ./bin/libh3.so ./src/*
sudo gdb -quiet --command ./gdb/inject.gdb --pid $(pgrep h3sod)