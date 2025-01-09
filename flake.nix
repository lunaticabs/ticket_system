{
  description = "ocaml shell for ticket system";
  inputs = {
    nixpkgs.url = "flake:nixpkgs/nixpkgs-unstable";
  };
  outputs = inputs:
    let
      flakeContext = {
        inherit inputs;
      };
    in
    {
      devShells = {
        x86_64-linux = {
          default = import ./devShells/default.nix flakeContext { system = "x86_64-linux"; };
        };
      };
    };
}
