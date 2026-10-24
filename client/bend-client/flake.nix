{
  description = "Harness Bend 2 terminal client";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        # Compiler and Base effect signatures are pinned together.
        bend = pkgs.stdenv.mkDerivation {
          pname = "bend";
          version = "2.0.39";
          src = pkgs.fetchurl {
            url = "https://github.com/bendlang/bend/releases/download/v2.0.39/bend-2.0.39-linux-x64.tar.gz";
            sha256 = "6cf462fd8f70b094a4950ebb709a9c4ffd8f1d1189d9cedf1fd63cd5d041c7ab";
          };
          nativeBuildInputs = [ pkgs.autoPatchelfHook pkgs.makeWrapper ];
          dontBuild = true;
          dontStrip = true;
          installPhase = ''
            mkdir -p $out/libexec/bend $out/bin
            cp -r . $out/libexec/bend
            makeWrapper $out/libexec/bend/bin/bend $out/bin/bend
          '';
        };
        client = pkgs.stdenv.mkDerivation {
          pname = "bend-client";
          version = "0.1.0";
          src = pkgs.lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              pkgs.lib.cleanSourceFilter path type
              && !(builtins.elem (builtins.baseNameOf path) [ "build" "__pycache__" ]);
          };
          nativeBuildInputs = [ bend pkgs.clang ];
          buildPhase = ''
            CC=clang bash build.sh
          '';
          installPhase = ''install -Dm755 build/bend-client $out/bin/bend-client'';
          meta.mainProgram = "bend-client";
        };
      in {
        packages.default = client;
        packages.bend = bend;
        apps.default = {
          type = "app";
          program = "${client}/bin/bend-client";
          meta.description = "Harness Bend terminal client";
        };
        checks.default = client;
        devShells.default = pkgs.mkShell {
          packages = [ bend pkgs.clang ];
          CC = "clang";
        };
      });
}
