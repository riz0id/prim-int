args:

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions (map (f: f args) [  
      (import exts/prim-bool.nix)
      (import exts/prim-compat.nix)
      (import exts/prim-int.nix)
      (import exts/tasty-hedgehog.nix)
    ]) pkgs pkgs;
}