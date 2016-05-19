{ mkDerivation, base, binary, bitcoin-tx, bitcoin-types, bytestring
, cryptohash, hexstring, hspec, largeword, lens, stdenv
}:
mkDerivation {
  pname = "bitcoin-block";
  version = "0.13.1";
  src = ./.;
  buildDepends = [
    base binary bitcoin-tx bitcoin-types bytestring cryptohash
    hexstring largeword microlens microlens-th
  ];
  testDepends = [
    base bitcoin-tx bitcoin-types bytestring hexstring hspec
  ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Utility functions for manipulating bitcoin blocks";
  license = stdenv.lib.licenses.mit;
}
