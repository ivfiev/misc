RUN_BUILD_SHM=cabal build --builddir=/dev/shm/rugfold-cabal-build
RUN_BIN_SHM=/dev/shm/rugfold-cabal-build/build/x86_64-linux/ghc-9.4.8/rugfold-0.1.0.0/x/rugfold/build/rugfold/rugfold
RUN_TEST_SHM=/dev/shm/rugfold-cabal-build/build/x86_64-linux/ghc-9.4.8/rugfold-0.1.0.0/x/rugfold-test/build/rugfold-test/rugfold-test

run-local:
	$(RUN_BUILD_SHM)
	$(RUN_BIN_SHM)

run-local-threaded:
	$(RUN_BUILD_SHM)
	$(RUN_BIN_SHM) +RTS -N24 -s

run-local-test:
	$(RUN_BUILD_SHM)
	$(RUN_BIN_SHM) &
	sleep 0.5
	$(RUN_TEST_SHM)
	pkill rugfold