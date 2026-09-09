{ aeneas, system }:
let
  base = import ../associated-demand-retention/package.nix { inherit aeneas system; };
  repair = ./artifact-capture.patch;
in
base.overrideAttrs (old: {
  postPatch = (old.postPatch or "") + ''
    patch --batch --fuzz=0 -p1 < ${repair}
  '';
  CHARON_GIT_COMMIT = "${old.CHARON_GIT_COMMIT}-artifact-capture-${builtins.hashFile "sha256" repair}";
})
