let
  base = import ../specialized-impl-names/package.nix;
  presentationPatch = ./prepared-name-components.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ presentationPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-name-presentation-${builtins.hashFile "sha256" presentationPatch}";
})
