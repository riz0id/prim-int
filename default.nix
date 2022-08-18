{ ghc ? "ghc902" }:

let
  nixpkgs = import nix/nixpkgs.nix { };

  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${ghc}" = super.haskell.packages."${ghc}".override (old: {
          overrides = let
            sources = self.haskell.lib.packageSourceOverrides {
              prim-bool   = import nix/pkgs/prim-bool.nix { };
              prim-compat = import nix/pkgs/prim-compat.nix { };
              prim-int    = ./.;
            };

            default = old.overrides or (_: _: { });

          in self.lib.fold self.lib.composeExtensions default [ sources ];
        });
      };
    };
  };

  pkgs = import nixpkgs {
    overlays = [ overlay ];
  };

in {
  inherit (pkgs.haskell.packages."${ghc}") prim-int;
  inherit (pkgs) cabal-install clang haskell-language-server llvm;
}
