let
  jlBuilder = import ./nix/julia-packages/julia-common/buildJulia.nix;
  config = {
    packageOverrides = pkgs: rec {
      buildJuliaPackage =
        jlBuilder {stdenv = pkgs.stdenv; fetchgit = pkgs.fetchgit; juliaPackages = pkgs.juliaPackages;};
      juliaPackages = {
        # Private ones
        BAT                 = pkgs.callPackage ./nix/julia-packages/BAT {};
        MultiThreadingTools = pkgs.callPackage ./nix/julia-packages/MultiThreadingTools {};
        # Normal julia packages
        Clustering       = pkgs.callPackage ./nix/julia-packages/Clustering {};
        Colors           = pkgs.callPackage ./nix/julia-packages/Colors {};
        Compat           = pkgs.callPackage ./nix/julia-packages/Compat {};
        Distributions    = pkgs.callPackage ./nix/julia-packages/Distributions {};
        DoubleDouble     = pkgs.callPackage ./nix/julia-packages/DoubleDouble {};
        ElasticArrays    = pkgs.callPackage ./nix/julia-packages/ElasticArrays {};
        FunctionWrappers = pkgs.callPackage ./nix/julia-packages/FunctionWrappers {};
        IntervalSets     = pkgs.callPackage ./nix/julia-packages/IntervalSets {};
        Parameters       = pkgs.callPackage ./nix/julia-packages/Parameters {};
        PDMats           = pkgs.callPackage ./nix/julia-packages/PDMats {};
        RandomNumbers    = pkgs.callPackage ./nix/julia-packages/RandomNumbers {};
        RecipesBase      = pkgs.callPackage ./nix/julia-packages/RecipesBase {};
#        StatsBase        = pkgs.callPackage ./nix/julia-packages/StatsBase {};
        StatsFuns        = pkgs.callPackage ./nix/julia-packages/StatsFuns {};
        Rmath        = pkgs.callPackage ./nix/julia-packages/Rmath {};
        BinaryProvider        = pkgs.callPackage ./nix/julia-packages/BinaryProvider {};
        BinDeps        = pkgs.callPackage ./nix/julia-packages/BinDeps {};
        SHA        = pkgs.callPackage ./nix/julia-packages/SHA {};
        URIParser        = pkgs.callPackage ./nix/julia-packages/SHA {};
        SpecialFunctions        = pkgs.callPackage ./nix/julia-packages/SpecialFunctions {};


        };
      };
    };
  pkgs = import <nixpkgs> { config = config;};
in
  pkgs.stdenv.mkDerivation {
    name = "bat2-shell";
    buildInputs = [ pkgs.stdenv
                    pkgs.julia
                    #
#                    pkgs.juliaPackages.BAT
#                    pkgs.juliaPackages.MultiThreadingTools
                    # Julia packages
                    #pkgs.juliaPackages.Clustering
                    #pkgs.juliaPackages.Colors
                    #pkgs.juliaPackages.Compat
#                    pkgs.juliaPackages.Distributions
                    #pkgs.juliaPackages.DoubleDouble
                    # pkgs.juliaPackages.ElasticArrays
                    # pkgs.juliaPackages.FunctionWrappers
                    # pkgs.juliaPackages.IntervalSets
                    # pkgs.juliaPackages.Parameters
                    # pkgs.juliaPackages.PDMats
                    # pkgs.juliaPackages.RandomNumbers
                    # pkgs.juliaPackages.RecipesBase
                    # pkgs.juliaPackages.StatsBase
                  ];
  }
