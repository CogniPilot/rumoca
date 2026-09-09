let
  base = import ../never-call-continuation/package.nix;
  registryPatch = ./binding-registry.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ registryPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-expect-failed-${builtins.hashFile "sha256" registryPatch}";
})
