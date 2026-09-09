let
  base = import ../implicit-alias/package.nix;
  loopPatch = ./shared-continuations.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ loopPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-loop-return-order-${builtins.hashFile "sha256" loopPatch}";
})
