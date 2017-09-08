{ mkDerivation, array, base, binary, bytes, bytestring, cereal
, deepseq, directory, exceptions, heredoc, HUnit, mtl, primitive
, process, QuickCheck, quota, reinterpret-cast, resourcet, stdenv
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, transformers, vector
}:
mkDerivation {
  pname = "capnp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytes bytestring exceptions mtl primitive quota
    template-haskell text transformers vector
  ];
  executableHaskellDepends = [
    array base binary bytes bytestring cereal exceptions mtl primitive
    quota transformers vector
  ];
  testHaskellDepends = [
    base binary bytestring deepseq directory exceptions heredoc HUnit
    mtl primitive process QuickCheck quota reinterpret-cast resourcet
    template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 transformers vector
  ];
  homepage = "https://github.com/zenhack/haskell-capnp";
  description = "Cap'n Proto for Haskell";
  license = stdenv.lib.licenses.mit;
}
