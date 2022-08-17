{ ghc ? "ghc922" }:

let
  nixpkgs = import nix/nixpkgs.nix { };

  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${ghc}" = super.haskell.packages."${ghc}".override (old: {
          overrides = let
            sources = self.haskell.lib.packageSourceOverrides {
              unlifted-bool = import nix/pkgs/unlifted-bool.nix { };
              prim-int = ./.;
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
  inherit (pkgs) cabal-install clang llvm;
}
