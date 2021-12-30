let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  melf = pkgs.haskellPackages.callCabal2nix "melf" sources.melf {};
  exception-context-th = pkgs.haskellPackages.callCabal2nix "exception-context-th" sources.exception-context-th {};
  overrides = _ : _ : {
    inherit exception-context-th;
    inherit melf;
  };
  haskellPackages = pkgs.haskellPackages.override {
    inherit overrides;
  };
in
  haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          cabal2nix
          hpack
          niv
          packdeps
        ]);
  }
