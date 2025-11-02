
build:
	mkdir -p bin
	cabal build --builddir=/tmp/ssdead
	fd -t f "ssdead$$" /tmp/ssdead | xargs -I{} mv "{}" ./bin/

run: build
	./bin/ssdead "$(ARGS)"

compress: build
	upx --best ./bin/ssdead

release: build compress
