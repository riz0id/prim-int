{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-bool";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bool";
    sha256 = "09m7gmlj0qfim7rnw8yc5k2jl94caxpyvgx5a00pvzmixz219plf";
    rev = "0fa052c8e9eb22d0279b285469915ba54c9034b4";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base ghc-prim prim-compat template-haskell
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-bool";
  description = "Unboxed booleans";
  license = lib.licenses.isc;
  mainProgram = "example";
}
