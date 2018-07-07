{ stdenv, fetchgit }:
let
  juliaPkg = import ./version.nix;
in
  stdenv.mkDerivation {
    name    = juliaPkg.name;
    version = juliaPkg.version;
    src     = fetchgit juliaPkg.src;
  
    dontBuild    = true;
    installPhase = ''
      mkdir $out
      cp -r * $out/
    '';
    setupHook = ../julia-common/setup.sh;
  }
  
