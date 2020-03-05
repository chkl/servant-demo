{ mkDerivation, base, hpack, servant, servant-client
, servant-server, stdenv
}:
mkDerivation {
  pname = "servant-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base servant servant-client servant-server
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base servant servant-client servant-server
  ];
  testHaskellDepends = [
    base servant servant-client servant-server
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/servant-demo#readme";
  license = stdenv.lib.licenses.bsd3;
}
