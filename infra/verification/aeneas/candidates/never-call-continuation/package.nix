let
  base = import ../f64-type-binding/package.nix;
  loopPatch = ../loop-return-order/shared-continuations.patch;
  neverPatch = ./never-call-continuation.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ loopPatch neverPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-loop-${builtins.hashFile "sha256" loopPatch}-never-${builtins.hashFile "sha256" neverPatch}";
})
