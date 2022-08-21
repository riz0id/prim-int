{ fetchTarball ? builtins.fetchTarball }:

let 
  pkg = "prim-bool";
  rev = "1.0.1";
in fetchTarball {
  url    = "https://github.com/riz0id/${pkg}/releases/download/${rev}/${pkg}-${rev}.tar.gz";
  sha256 = "0iayb1i2r80ysrz4gyyijgfxjagmn6s6d20lh4psr5g3ky1j09yq";
}