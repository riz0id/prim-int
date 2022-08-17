{ fetchTarball ? builtins.fetchTarball }:

let 
  pkg = "prim-bool";
  rev = "1.0.1";
in fetchTarball {
  url    = "https://github.com/riz0id/${pkg}/releases/download/${rev}/${pkg}.tar.gz";
  sha256 = "0xbxf29h5ych51dha0r58k6lwa3j42bwjwvcwq7chxmwm2yg9s6x";
}