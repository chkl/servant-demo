{ mkDerivation, aeson, base, containers, hpack, lens, rio, scotty
, servant, servant-client, servant-server, stdenv, stm, tasty
, tasty-wai, text, uuid, wai, warp, wreq, gdp
}:
mkDerivation {
  pname = "servant-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers lens rio scotty servant servant-client
    servant-server stm tasty tasty-wai text uuid wai warp wreq gdp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base containers lens rio scotty servant servant-client
    servant-server stm tasty tasty-wai text uuid wai warp wreq gdp
  ];
  testHaskellDepends = [
    aeson base containers lens rio scotty servant servant-client
    servant-server stm tasty tasty-wai text uuid wai warp wreq gdp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/chkl/servant-demo#readme";
  license = stdenv.lib.licenses.bsd3;
}
