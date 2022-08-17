{ fetchTarball ? builtins.fetchTarball }:

let 
  pkg = "prim-bool";
  rev = "1.0.0";
in fetchTarball {
  url    = "https://github.com/riz0id/${pkg}/releases/download/${rev}/${pkg}.tar.gz";
  sha256 = "1gc37gm56ishra5hpgqn5kpqmawxzq5amfhrvczds2m0pw8sk4cj";
}