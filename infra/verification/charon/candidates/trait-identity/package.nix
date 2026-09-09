{ aeneas, system }:
let
  base = import ../root-identity/package.nix { inherit aeneas system; };
  repair = ./trait-identity.patch;
in
base.overrideAttrs (old: {
  postPatch = (old.postPatch or "") + ''
    patch --batch --fuzz=0 -p1 < ${repair}
  '';
  CHARON_GIT_COMMIT = "${old.CHARON_GIT_COMMIT}-trait-identity-${builtins.hashFile "sha256" repair}";
})
