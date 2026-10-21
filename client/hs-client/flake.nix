{
  description = "A haskell.nix project";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  # nixpkgs unstable (26.11) dropped x86_64-darwin, and `eachSystem` below
  # evaluates *every* supported system to collect its output names — so one
  # unimportable system breaks `nix develop` on all of them.  Keep the last
  # pin that supports it and use it for that system only.
  inputs.nixpkgs-2605.follows = "haskellNix/nixpkgs-2605";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, nixpkgs-2605, flake-utils, haskellNix }:
    let
      # `nix flake check` and `nix develop` fully evaluate *every* system in
      # `eachSystem`.  haskell.nix resolves each project's plan via
      # import-from-derivation (`cabal.project.drv`), which carries a hard
      # platform requirement — so on an x86_64 box, evaluating the aarch64 /
      # darwin jobs fails with "Required system: 'aarch64-linux'".  A pure
      # flake can't detect the host system, so list only the target(s) you
      # actually build on here.  (Add other systems when working on those
      # hosts, e.g. via Hydra/CI on the matching machines.)
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: _prev: {
            # This overlay adds our project to pkgs
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                # uncomment with your current system for `nix flake show` to work:
                #evalSystem = "x86_64-linux";
              };
          })
        ];
        pkgs = import (if system == "x86_64-darwin" then nixpkgs-2605 else nixpkgs)
          { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;

        # `nix build .` builds the client executable; the per-component
        # attributes (e.g. `nix build .#client:exe:client`) come from `flake`.
        packages = flake.packages // { default = flake.packages."client:exe:client"; };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
