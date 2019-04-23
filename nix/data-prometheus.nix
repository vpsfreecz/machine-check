{ mkDerivation, attoparsec, base, bytestring, containers, fetchgit
, hspec, lens, raw-strings-qq, stdenv, transformers, wreq
}:
mkDerivation {
  pname = "data-prometheus";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/vpsfreecz/data-prometheus";
    sha256 = "1yi0p2p5rynwrzlq5n8s71bly9xjpa3lcwdsc64k794l43lg1gbb";
    rev = "423e3262b7a0be2ba016f972aa1437aac729b0a1";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers transformers wreq
  ];
  executableHaskellDepends = [
    attoparsec base bytestring lens wreq
  ];
  testHaskellDepends = [
    attoparsec base containers hspec raw-strings-qq
  ];
  homepage = "https://github.com/vpsfreecz/data-prometheus";
  description = "Prometheus metrics data types and parser";
  license = stdenv.lib.licenses.bsd3;
}
