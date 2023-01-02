{ ghc ? "ghc942" }:

let 
  pkgs = import ./default.nix {
    inherit ghc;
  };
in pkgs.prim-int.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [
    pkgs.haskell-language-server
  ];
})
