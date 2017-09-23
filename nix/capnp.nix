{ mkDerivation, array, base, binary, bytes, bytestring, cereal
, deepseq, directory, exceptions, ghc-prim, heredoc, HUnit, mtl
, primitive, process, QuickCheck, quota, reinterpret-cast
, resourcet, stdenv, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
, transformers
}:
mkDerivation {
  pname = "capnp";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytes bytestring exceptions ghc-prim mtl primitive quota
    template-haskell text transformers
  ];
  executableHaskellDepends = [
    array base binary bytes bytestring cereal exceptions ghc-prim mtl
    primitive quota transformers
  ];
  testHaskellDepends = [
    base binary bytestring deepseq directory exceptions ghc-prim
    heredoc HUnit mtl primitive process QuickCheck quota
    reinterpret-cast resourcet template-haskell test-framework
    test-framework-hunit test-framework-quickcheck2 transformers
  ];
  homepage = "https://github.com/zenhack/haskell-capnp";
  description = "Cap'n Proto for Haskell";
  license = stdenv.lib.licenses.mit;
}
