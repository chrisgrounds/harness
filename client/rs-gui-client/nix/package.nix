{ lib, rustPlatform, pkg-config, makeWrapper, runtimeLibraries }:

rustPlatform.buildRustPackage {
  pname = "rs-gui-client";
  version = "0.1.0";
  src = lib.cleanSourceWith {
    src = ../.;
    filter = path: type:
      lib.cleanSourceFilter path type && baseNameOf path != "target";
  };
  cargoLock.lockFile = ../Cargo.lock;

  nativeBuildInputs = [ pkg-config makeWrapper ];
  buildInputs = runtimeLibraries;

  # winit/glutin load the display and OpenGL libraries at runtime.
  postFixup = ''
    wrapProgram $out/bin/rs-gui-client \
      --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath runtimeLibraries}
  '';

  meta = {
    description = "Native desktop client for the Harness Unix socket server";
    mainProgram = "rs-gui-client";
    platforms = lib.platforms.linux;
    license = lib.licenses.bsd3;
  };
}
