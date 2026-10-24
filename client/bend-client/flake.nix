{
  description = "Harness Bend 2 terminal client";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        # 0.18's strict parser rejects raw non-ASCII JSON; 0.19 fixes that.
        jsonC = pkgs.json_c.overrideAttrs (_: {
          version = "0.19";
          src = pkgs.fetchurl {
            url = "https://s3.amazonaws.com/json-c_releases/releases/json-c-0.19.tar.gz";
            sha256 = "37ad0249902e301bd9052bf712e511fcc6acff4ecaad4b5900aad9ce564e26de";
          };
        });
        # The foreign-effect ABI is tied to this compiler release.
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
          nativeBuildInputs = [ bend pkgs.clang pkgs.pkg-config pkgs.python3 ];
          buildInputs = [ jsonC ];
          buildPhase = ''
            CC=clang bash build.sh
          '';
          doCheck = true;
          checkPhase = ''python3 tests/test_socket.py'';
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
          packages = [ bend pkgs.clang pkgs.pkg-config pkgs.python3 ];
          buildInputs = [ jsonC ];
          CC = "clang";
        };
      });
}
