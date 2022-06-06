{ compiler ? "ghc921" }:

let
  nixpkgs = builtins.fetchTarball {
    # nixpkgs release 21.11
    # url: <https://github.com/NixOS/nixpkgs/releases/tag/21.11>
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz";
    sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  };

  config = { };

  overlay = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
          overrides = let
            packageSources =
              let
                unlifted-bool = builtins.fetchTarball {
                  # unlifted-bool-0.1.1.0
                  url    = "https://github.com/riz0id/unlifted-bool/releases/download/0.1.1.0/unlifted-bool-0.1.1.0.tar.gz";
                  sha256 = "0jpgcaj1k7wp9zqn0dqsj1k7gfw456mq9vmdq5nxfzhpmr4mpqvm";
                };
              in
                pkgsNew.haskell.lib.packageSourceOverrides {
                  "unlifted-bool" = unlifted-bool;
                  "unlifted-int"  = ./.;
                };

            manualOverrides = haskellPackagesNew: haskellPackagesOld: { };

            default = old.overrides or (_: _: { });

          in pkgsNew.lib.fold pkgsNew.lib.composeExtensions default [
            packageSources
            manualOverrides
          ];
        });
      };
    };
  };

  pkgs = import nixpkgs {
    inherit config;
    overlays = [ overlay ];
  };

in {
  inherit (pkgs.haskell.packages."${compiler}") unlifted-int;

  unlifted-int-shell = (pkgs.haskell.packages."${compiler}".unlifted-int).env;
}
