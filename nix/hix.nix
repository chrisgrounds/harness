{pkgs, ...}: {
  compiler-nix-name = "ghc96";

  shell.tools.cabal = "latest";
  shell.buildInputs = [ pkgs.ormolu ];
}
