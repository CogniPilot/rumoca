let
  base = import ../specialized-name-presentation/package.nix;
  aliasPatch = ./alias-inference.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ aliasPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-alias-inference-${builtins.hashFile "sha256" aliasPatch}";
})
