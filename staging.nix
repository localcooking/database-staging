{ mkDerivation, base, opaleye, operations, lib }:
mkDerivation {
  pname = "staging";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base opaleye operations ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
