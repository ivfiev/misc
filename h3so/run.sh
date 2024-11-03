g++ -O3 -m32 -fPIC -shared -o ./bin/libh3.so main.cxx
sudo gdb -quiet --command inject.gdb --pid $(pgrep h3sod)