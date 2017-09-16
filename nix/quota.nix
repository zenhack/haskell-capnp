{ mkDerivation, base, exceptions, fetchgit, mtl, QuickCheck, stdenv
, test-framework, test-framework-quickcheck2, transformers
}:
mkDerivation {
  pname = "quota";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/zenhack/haskell-quota.git";
    sha256 = "1xaj4vfa67m674z1h18g7jafpp36c10jjf3jcwvdr0b2a0p6i06c";
    rev = "3059085facf116857313a88ffc0debefbf86aafa";
  };
  libraryHaskellDepends = [ base exceptions mtl transformers ];
  testHaskellDepends = [
    base exceptions mtl QuickCheck test-framework
    test-framework-quickcheck2 transformers
  ];
  homepage = "https://github.com/zenhack/haskell-quota";
  description = "Monad for tracking quota limits";
  license = stdenv.lib.licenses.asl20;
}
