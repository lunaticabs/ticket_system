{ inputs, ... }@flakeContext:
{ system }:
let
  pkgs = inputs.nixpkgs.legacyPackages."${system}";
in
pkgs.mkShell
{
  packages = [
    pkgs.ocamlPackages.findlib
    pkgs.ocamlPackages.ocaml-lsp
    pkgs.ocamlPackages.utop
    pkgs.ocamlPackages.ocamlformat
    pkgs.dune_3
    pkgs.ocaml
    pkgs.ocamlPackages.yojson
  ];
}
