let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {

  packages = with pkgs; [
    # Build toolchain
    ocamlPackages.ocaml
    ocamlPackages.dune_3
    ocamlPackages.findlib # provides ocamlfind

    # Development toolchain
    ocamlPackages.utop
    ocamlPackages.ocamlformat
    ocamlPackages.ocaml-lsp
  ];
  shellHook = ''
    echo "Welcome to Advent of Code !"
  '';
}
