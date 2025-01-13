RUN_BUILD_SHM=cabal build --builddir=/dev/shm/busy-cabal-build
RUN_BIN_SHM=/dev/shm/busy-cabal-build/build/x86_64-linux/ghc-9.4.8/busy-0.1.0.0/x/busy/build/busy/busy

run:
	$(RUN_BUILD_SHM)
	$(RUN_BIN_SHM)