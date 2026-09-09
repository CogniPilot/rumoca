let
  base = import ../method-constraints/package.nix;
  specializationPatch = ./specialized-names.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ specializationPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-specialized-names-${builtins.hashFile "sha256" specializationPatch}";
})
