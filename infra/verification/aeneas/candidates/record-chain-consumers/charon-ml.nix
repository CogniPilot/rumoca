# The upstream charon-ml library with the record-chain consumer patch: the
# generated PeExtended variant (from generate-asts) and the fold helpers. Built
# with the Aeneas flake's OCaml package set so it links into Aeneas without
# duplicate packages.
{ aeneas, system }:
let
  upstream = aeneas.inputs.charon;
  pkgs = aeneas.inputs.nixpkgs.legacyPackages.${system};
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_2;
  patch = ./charon-ml.patch;
in
(upstream.packages.${system}.charon-ml.override { inherit ocamlPackages; }).overrideAttrs (old: {
  patches = (old.patches or [ ]) ++ [ patch ];
})
