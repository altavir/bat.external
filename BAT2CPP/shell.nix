let
  config = {
    packageOverrides = pkgs: rec {
      # Private ones
      jl_BAT                 = pkgs.callPackage ./nix/julia-packages/BAT {};
      jl_MultiThreadingTools = pkgs.callPackage ./nix/julia-packages/MultiThreadingTools {};
      # Normal julia packages
      jl_Clustering       = pkgs.callPackage ./nix/julia-packages/Clustering {};
      jl_Colors           = pkgs.callPackage ./nix/julia-packages/Colors {};
      jl_Compat           = pkgs.callPackage ./nix/julia-packages/Compat {};
      jl_Distributions    = pkgs.callPackage ./nix/julia-packages/Distributions {};
      jl_DoubleDouble     = pkgs.callPackage ./nix/julia-packages/DoubleDouble {};
      jl_ElasticArrays    = pkgs.callPackage ./nix/julia-packages/ElasticArrays {};
      jl_FunctionWrappers = pkgs.callPackage ./nix/julia-packages/FunctionWrappers {};
      jl_IntervalSets     = pkgs.callPackage ./nix/julia-packages/IntervalSets {};
      jl_Parameters       = pkgs.callPackage ./nix/julia-packages/Parameters {};
      jl_PDMats           = pkgs.callPackage ./nix/julia-packages/PDMats {};
      jl_RandomNumbers    = pkgs.callPackage ./nix/julia-packages/RandomNumbers {};
      jl_RecipesBase      = pkgs.callPackage ./nix/julia-packages/RecipesBase {};
      jl_StatsBase        = pkgs.callPackage ./nix/julia-packages/StatsBase {};
      };
    };
  pkgs = import <nixpkgs> { config = config;};
in
  pkgs.stdenv.mkDerivation {
    name = "bat2-shell";
    buildInputs = [ pkgs.stdenv
                    pkgs.julia
                    #
                    pkgs.jl_BAT
                    pkgs.jl_MultiThreadingTools
                    # Julia packages
                    pkgs.jl_Clustering
                    pkgs.jl_Colors
                    pkgs.jl_Compat
                    pkgs.jl_Distributions
                    pkgs.jl_DoubleDouble
                    pkgs.jl_ElasticArrays
                    pkgs.jl_FunctionWrappers
                    pkgs.jl_IntervalSets
                    pkgs.jl_Parameters
                    pkgs.jl_PDMats
                    pkgs.jl_RandomNumbers
                    pkgs.jl_RecipesBase
                    pkgs.jl_StatsBase
                  ];
  }
