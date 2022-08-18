{ fetchTarball ? builtins.fetchTarball }:

let 
  pkg = "prim-compat";
  rev = "1.0.0";
in fetchTarball {
  url    = "https://github.com/riz0id/${pkg}/releases/download/${rev}/${pkg}.tar.gz";
  sha256 = "0danzlaygha36h1g7ghc8mv49lwkcxf41vbnaya9bspvhr54ywp6";
}