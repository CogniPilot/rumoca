let
  base = import ../drop-control/package.nix;
  identityPatch = ./qualified-constructors.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ identityPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-builtin-identity-${builtins.hashFile "sha256" identityPatch}";
})
