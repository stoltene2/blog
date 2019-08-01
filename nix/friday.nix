{ mkDerivation, base, containers, convertible, deepseq, primitive
, QuickCheck, ratio-int, stdenv, test-framework
, test-framework-quickcheck2, transformers, vector, fetchgit
}:
mkDerivation {
  pname = "friday";
  version = "0.2.3.1";
  src = fetchgit {
    url = "git@github.com:stoltene2/friday.git";
    rev = "810ad3f87df93e8c883f5c4a2266df5aa8452826";
    sha256 = "1v5x9jynm3h1zf501ff8rk8ah8gpc9p7n9j2pr3ral25y1y6hvyb";
  };
  libraryHaskellDepends = [
    base containers convertible deepseq primitive ratio-int
    transformers vector
  ];
  testHaskellDepends = [
    base QuickCheck test-framework test-framework-quickcheck2 vector
  ];
  homepage = "https://github.com/RaphaelJ/friday";
  description = "A functional image processing library for Haskell";
  license = stdenv.lib.licenses.lgpl3;
}
