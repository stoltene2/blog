{ mkDerivation, aeson, base, binary, bytestring, containers
, filepath, hakyll, stdenv
, unordered-containers
}:

let
  pkgs = import <nixpkgs> { };
  friday = pkgs.haskellPackages.callPackage nix/friday.nix {};
  friday-devil = pkgs.haskellPackages.callPackage nix/friday-devil.nix {};
  hakyll-sass = pkgs.haskellPackages.callPackage nix/hakyll-sass.nix {};
in
mkDerivation {
  pname = "personal-blog";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary bytestring containers filepath friday
    friday-devil hakyll hakyll-sass unordered-containers
    # There is a runtime dependency on this package
    pkgs.pngquant
    pkgs.awscli
  ];
  homepage = "www.stolten.net";
  description = "My Personal Blog";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
