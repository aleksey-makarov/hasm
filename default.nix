let

  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gccCross = pkgs.pkgsCross.aarch64-multiplatform.pkgsStatic.buildPackages.gcc;
  gdbCross = pkgs.pkgsCross.aarch64-multiplatform.buildPackages.gdb;

  overrides = self : super : {
    exception-context-th = self.callCabal2nix "exception-context-th" sources.exception-context-th {};
    melf                 = self.callCabal2nix "melf"                 sources.melf                 {};
  };

in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (
        with pkgs.haskellPackages;
        [ cabal-install
          cabal2nix
          hpack
          niv
          pkgs.qemu
          gccCross
          gdbCross
        ]);
    inherit overrides;
  }
