let

  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gccCross = pkgs.pkgsCross.aarch64-multiplatform.pkgsStatic.buildPackages.gcc;
  gdbCross = pkgs.pkgsCross.aarch64-multiplatform.buildPackages.gdb;

  haskellPackages924 = pkgs.haskell.packages.ghc924;

  overrides = self : super : {
    exception-context-th = self.callCabal2nix "exception-context-th" sources.exception-context-th {};
    melf                 = self.callCabal2nix "melf"                 sources.melf                 {};
  };

  source-overrides = {
    singletons-base = "3.1";
    singletons-th = "3.1";
  };

in
  haskellPackages924.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (
        with haskellPackages924;
        [ cabal-install
          cabal2nix
          hpack
          niv
          pkgs.qemu
          gccCross
          gdbCross
        ]);
    inherit source-overrides;
    inherit overrides;
  }
