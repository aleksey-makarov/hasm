https://www.youtube.com/watch?v=OV2hi8b5t48 -- NixOS/Nix - Cross Compilation via pkgsCross

qemu-aarch64 -g 11111 ./tests/test_bss.gcc
aarch64-unknown-linux-gnu-gdb ./tests/test_bss.gcc

qemu-aarch64 -g 11111 ./tests/out/testBss.gcc
aarch64-unknown-linux-gnu-gdb ./tests/out/testBss.gcc

rm ./tests/out/*.o.dump.golden
rm ./tests/out/testBss.o.dump.golden
cabal new-test --test-show-details=direct
cabal new-clean

hobjdump -f ./tests/out/testBss.gcc ./tests/out/testBss.gcc.dump
