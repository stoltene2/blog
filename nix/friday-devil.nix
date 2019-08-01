{ mkDerivation, base, bytestring, convertible, deepseq, fetchgit, friday
, libdevil, stdenv, transformers, vector
}:
mkDerivation {
  pname = "friday-devil";
  version = "0.1.1.1";
  src = fetchgit {
    url = "git@github.com:stoltene2/friday-devil.git";
    rev = "73181586307a4db93302bfeb9af9ded615e60a07";
    # date = "2016-10-17T10:37:44-07:00";
    sha256 = "00l3nqcsr0sswilmkpldf6wirs9hng6cj4r3q9j7j7scnrskzgmk";
  };
  libraryHaskellDepends = [
    base bytestring convertible deepseq friday transformers vector
  ];
  librarySystemDepends = [ libdevil ];
  homepage = "https://github.com/RaphaelJ/friday-devil";
  description = "Uses the DevIL C library to read and write images from and to files and memory buffers";
  license = stdenv.lib.licenses.lgpl3;
}
