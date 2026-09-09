let
  base = import ../expect-failed-model/package.nix;
  namingPatch = ./unique-parent-fields.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ namingPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-parent-clause-names-${builtins.hashFile "sha256" namingPatch}";
})
