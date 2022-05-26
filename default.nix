let

  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  gccCross = pkgs.pkgsCross.aarch64-multiplatform.pkgsStatic.buildPackages.gcc;
  gdbCross = pkgs.pkgsCross.aarch64-multiplatform.buildPackages.gdb;

  melf                 = pkgs.haskellPackages.callCabal2nix "melf"                 sources.melf                 {};
  exception-context-th = pkgs.haskellPackages.callCabal2nix "exception-context-th" sources.exception-context-th {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = _ : _ : {
      inherit exception-context-th;
      inherit melf;
    };
  };

in
  haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with haskellPackages;
        [ cabal-install
          cabal2nix
          hpack
          niv
          pkgs.qemu
          gccCross
          gdbCross
        ]);
  }
