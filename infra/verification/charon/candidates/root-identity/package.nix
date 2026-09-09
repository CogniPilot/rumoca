{ aeneas, system }:
let
  base = import ../associated-demand-retention/package.nix { inherit aeneas system; };
  repair = ./root-identity.patch;
in
base.overrideAttrs (old: {
  postPatch = (old.postPatch or "") + ''
    patch --batch --fuzz=0 -p1 < ${repair}
  '';
  CHARON_GIT_COMMIT = "${old.CHARON_GIT_COMMIT}-root-identity-${builtins.hashFile "sha256" repair}";
})
