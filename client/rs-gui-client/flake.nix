{
  description = "Harness Rust + egui desktop client";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    # Match the systems currently supported by the other client flakes.
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        runtimeLibraries = with pkgs; [
          libGL libxkbcommon wayland
          libx11 libxcursor libxi libxrandr
        ];
        client = pkgs.callPackage ./nix/package.nix { inherit runtimeLibraries; };
        softwareClient = pkgs.writeShellScriptBin "rs-gui-client-software" ''
          export LIBGL_ALWAYS_SOFTWARE=1
          export __EGL_VENDOR_LIBRARY_FILENAMES=${pkgs.mesa}/share/glvnd/egl_vendor.d/50_mesa.json
          exec ${client}/bin/rs-gui-client "$@"
        '';
      in {
        packages.default = client;
        packages.rs-gui-client = client;
        packages.software = softwareClient;
        apps.default = {
          type = "app";
          program = "${client}/bin/rs-gui-client";
          meta.description = "Harness desktop client";
        };
        apps.software = {
          type = "app";
          program = "${softwareClient}/bin/rs-gui-client-software";
          meta.description = "Harness desktop client with Mesa software rendering";
        };
        checks.default = client;
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [ cargo rustc rustfmt clippy pkg-config ];
          buildInputs = runtimeLibraries;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath runtimeLibraries;
        };
      });
}
