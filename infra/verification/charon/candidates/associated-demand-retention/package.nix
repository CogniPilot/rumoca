{ aeneas, system }:
let
  base = import ../specialized-trait-normalization/package.nix { inherit aeneas system; };
  repair = ./growth-edge-retention.patch;
in
base.overrideAttrs (old: {
  # The predecessor installs its private helper in postPatch; patch both owners after that.
  postPatch = (old.postPatch or "") + ''
    patch --batch --fuzz=0 -p1 < ${repair}
    cp ${./cyclic_retention.rs} tests/cyclic_retention.rs
  '';
  CHARON_GIT_COMMIT = "${old.CHARON_GIT_COMMIT}-growth-retention-${builtins.hashFile "sha256" repair}";
})
