{ mkDerivation, atomic-write, attoparsec, base, bytestring
, containers, data-prometheus, hspec, pretty-simple, process
, stdenv, text
}:
mkDerivation {
  pname = "machine-check";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    atomic-write attoparsec base bytestring containers data-prometheus
    pretty-simple process text
  ];
  executableHaskellDepends = [ base bytestring pretty-simple ];
  testHaskellDepends = [ attoparsec base hspec text ];
  homepage = "https://github.com/sorki/machine-check";
  description = "Linux system checks";
  license = stdenv.lib.licenses.bsd3;
}
