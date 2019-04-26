{ mkDerivation, atomic-write, attoparsec, base, bytestring
, config-ini, containers, data-prometheus, dns, hspec, iproute
, pretty-simple, process, stdenv, text
}:
mkDerivation {
  pname = "machine-check";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    atomic-write attoparsec base bytestring config-ini containers
    data-prometheus dns iproute pretty-simple process text
  ];
  executableHaskellDepends = [ base bytestring pretty-simple ];
  testHaskellDepends = [ attoparsec base hspec text ];
  homepage = "https://github.com/vpsfreecz/machine-check";
  description = "Linux system checks";
  license = stdenv.lib.licenses.bsd3;
}
