{ mkDerivation, base, containers, convertible, deepseq, primitive
, QuickCheck, ratio-int, stdenv, test-framework
, test-framework-quickcheck2, transformers, vector, fetchgit
}:
mkDerivation {
  pname = "friday";
  version = "0.2.3.1";
  src = fetchgit {
    url = "https://github.com/stoltene2/friday.git";
    rev = "df581739cb161dceb78189b9b3099467c3890350";
    sha256 = "1m0lsip85s93ah5axpn0i1l6y667g3brg8xhvhsip0kcnl0a3j6r";
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
