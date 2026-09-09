let
  base = import ../vec-box-boundary/package.nix;
  registryPatch = ./binding-registry.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ registryPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-box-slice-boundary-${builtins.hashFile "sha256" registryPatch}";
})
