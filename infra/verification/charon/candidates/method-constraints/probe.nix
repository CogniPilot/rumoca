{ aeneas, system }:
let
  base = import ../capture-arguments/package.nix { inherit aeneas system; };
  probe = ./signature-probe.patch;
in
base.unwrapped.overrideAttrs (old: {
  patches = old.patches ++ [ probe ];
  CHARON_GIT_COMMIT = "${old.CHARON_GIT_COMMIT}-signature-probe-${builtins.hashFile "sha256" probe}";
})
