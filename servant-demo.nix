{ mkDerivation, aeson, base, containers, hpack, rio, scotty
, servant, servant-client, servant-server, stdenv, stm, tasty
, tasty-wai, uuid, wreq
}:
mkDerivation {
  pname = "servant-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers rio scotty servant servant-client
    servant-server stm tasty tasty-wai uuid wreq
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base containers rio scotty servant servant-client
    servant-server stm tasty tasty-wai uuid wreq
  ];
  testHaskellDepends = [
    aeson base containers rio scotty servant servant-client
    servant-server stm tasty tasty-wai uuid wreq
  ];
  prePatch = "hpack";
  homepage = "https://github.com/chkl/servant-demo#readme";
  license = stdenv.lib.licenses.bsd3;
}
