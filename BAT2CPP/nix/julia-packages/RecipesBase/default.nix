{ buildJuliaPackage }:
let
  juliaPkg = import ./version.nix;
in
  buildJuliaPackage juliaPkg
