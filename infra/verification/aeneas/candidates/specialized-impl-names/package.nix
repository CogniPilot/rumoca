let
  base = import ../../../charon/candidates/specialized-trait-normalization/aeneas-replay.nix;
  implNamesPatch = ./complete-trait-arguments.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ implNamesPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-impl-names-${builtins.hashFile "sha256" implNamesPatch}";
})
