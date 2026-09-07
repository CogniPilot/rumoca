{ aeneas, system }:
let
  patch = ./keep-function-roots.patch;
  charon = import ../charon/package.nix { inherit aeneas system; };
in
(aeneas.packages.${system}.aeneas.override { inherit charon; }).overrideAttrs (old: {
  patches = (old.patches or [ ]) ++ [ patch ];
  AENEAS_VERSION = "${aeneas.rev}-rumoca-${builtins.hashFile "sha256" patch}";
})
