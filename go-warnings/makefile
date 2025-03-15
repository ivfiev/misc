
run: build
	go build -o ./test/bin/A ./test/A.go && go build -o ./test/bin/B ./test/B.go && ./bin/tracer ./test/bin/A

build: ./include/* ./src/*
	mkdir -p bin
	gcc -O3 -o ./bin/tracer -I./include ./src/*
