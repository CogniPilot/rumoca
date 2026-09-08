{ aeneas, system }:
let
  base = import ./constructor.nix { inherit aeneas system; };
  transport = ./transport.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ transport ];
  CHARON_GIT_COMMIT = "${old.CHARON_GIT_COMMIT}-transport-${builtins.hashFile "sha256" transport}";
})
