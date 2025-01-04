RUN_BUILD_SHM=cabal build --builddir=/dev/shm/rugfold-cabal-build
RUN_BIN_SHM=/dev/shm/rugfold-cabal-build/build/x86_64-linux/ghc-9.4.8/rugfold-0.1.0.0/x/rugfold/build/rugfold/rugfold
RUN_TEST_SHM=/dev/shm/rugfold-cabal-build/build/x86_64-linux/ghc-9.4.8/rugfold-0.1.0.0/x/rugfold-test/build/rugfold-test/rugfold-test

run-local:
	$(RUN_BUILD_SHM)
	$(RUN_BIN_SHM) 8989

run-local-threaded:
	$(RUN_BUILD_SHM)
	$(RUN_BIN_SHM) 8989 +RTS -N24 -s

run-local-test:
	$(RUN_BUILD_SHM)
	$(RUN_TEST_SHM) $(RUN_BIN_SHM)
	pkill rugfold