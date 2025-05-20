{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, tasty
, tasty-hedgehog
}:
mkDerivation {
  pname = "prim-compat";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-compat";
    sha256 = "0vjwi6s9cdlfqwhj8b3lhv07lkrhqvbciyrddmz1v4688q154sky";
    rev = "5a2d836b3c4f0d271f993293b78cb4074c1c13eb";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-compat";
  description = "Lightweight ghc-prim wrappers for backwards compatibility";
  license = lib.licenses.isc;
}
