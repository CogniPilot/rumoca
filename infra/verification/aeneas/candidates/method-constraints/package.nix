let
  base = import ../static-regions/lifecycle.nix;
  methodPatch = ./remove-method-guess.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ methodPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-no-method-guess-${builtins.hashFile "sha256" methodPatch}";
})
