{ ghc ? "ghc922" }:

let 
  pkgs = import ./default.nix {
    inherit ghc;
  };
in pkgs.unlifted-int.env
