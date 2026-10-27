{
  description = "Harness core, AF_UNIX server, and Brick terminal client";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.nixpkgs-2605.follows = "haskellNix/nixpkgs-2605";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, nixpkgs-2605, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: _prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
              };
          })
        ];
        pkgs = import (if system == "x86_64-darwin" then nixpkgs-2605 else nixpkgs)
          { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in {
        # Named explicitly rather than `flake // { ... }` so that eachSystem
        # can see the output names without evaluating the project (IFD) for
        # every supported system.
        inherit (flake) apps checks devShells ciJobs hydraJobs;
        legacyPackages = pkgs;

        packages = flake.packages // { default = flake.packages."server:exe:server"; };
      });

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
