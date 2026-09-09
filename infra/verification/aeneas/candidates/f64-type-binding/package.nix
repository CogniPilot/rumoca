let
  base = import ../implicit-alias/package.nix;
  floatPatch = ./float-dependencies-r2.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ floatPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-float-dependencies-${builtins.hashFile "sha256" floatPatch}";
})
