{ mkDerivation, array, base, binary, bytes, bytestring, cereal
, containers, deepseq, directory, dlist, exceptions, filepath
, heredoc, HUnit, mtl, primitive, process, process-extras
, QuickCheck, quota, reinterpret-cast, resourcet, stdenv
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, transformers, utf8-string
, vector
}:
mkDerivation {
  pname = "capnp";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytes bytestring exceptions mtl primitive quota
    template-haskell text transformers vector
  ];
  executableHaskellDepends = [
    array base binary bytes bytestring cereal containers directory
    dlist exceptions filepath mtl primitive quota template-haskell
    transformers utf8-string vector
  ];
  testHaskellDepends = [
    base binary bytestring containers deepseq directory exceptions
    heredoc HUnit mtl primitive process process-extras QuickCheck quota
    reinterpret-cast resourcet template-haskell test-framework
    test-framework-hunit test-framework-quickcheck2 transformers vector
  ];
  homepage = "https://github.com/zenhack/haskell-capnp";
  description = "Cap'n Proto for Haskell";
  license = stdenv.lib.licenses.mit;
}
