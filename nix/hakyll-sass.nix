# I added my own clone of this because the published version wasn't
# bumped and the maintainer has been unresponsive to the community.

{ mkDerivation, aeson-pretty, base, data-default-class, fetchgit
, filepath, hakyll, hsass, stdenv
}:
mkDerivation {
  pname = "hakyll-sass";
  version = "0.2.4";
  src = fetchgit {
    url = "https://github.com/meoblast001/hakyll-sass.git";
    sha256 = "0g34bdj4s670ljj9sjqqh2ghpcy9l9hsgc65kcr7d3430m1k7pgx";
    rev = "b9c08afc9d81abc5bc57a9adc6bd7b70d562ea46";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson-pretty base data-default-class filepath hakyll hsass
  ];
  homepage = "https://github.com/meoblast001/hakyll-sass";
  description = "Hakyll SASS compiler over hsass";
  license = stdenv.lib.licenses.mit;
}
