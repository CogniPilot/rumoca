let
  base = import ../method-constraints/package.nix;
  dropPatch = ./drop-control.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ dropPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-drop-control-${builtins.hashFile "sha256" dropPatch}";
})
