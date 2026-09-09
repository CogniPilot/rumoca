let
  base = import ../parent-clause-names/package.nix;
  registryPatch = ./binding-registry.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ registryPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-vec-box-boundary-${builtins.hashFile "sha256" registryPatch}";
})
