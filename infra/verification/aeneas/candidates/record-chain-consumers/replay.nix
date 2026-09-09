# The consumer patches on the M0 replay chain with its existing Charon binary,
# so the consumers can be measured before the trait-identity Charon has a store
# path. Charon bytes for the measurements come from the trait-identity packet's
# frozen cargo build, recorded in run-evidence.json.
let
  system = builtins.currentSystem;
  aeneas = builtins.getFlake "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef";
  base = import ../loop-return-order/package.nix;
  charon-ml = import ./charon-ml.nix { inherit aeneas system; };
  aeneasPatch = ./aeneas.patch;
in
(base.override { inherit charon-ml; }).overrideAttrs (old: {
  patches = old.patches ++ [ aeneasPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-record-chain-replay-${builtins.hashFile "sha256" aeneasPatch}-ml-${builtins.hashFile "sha256" ./charon-ml.patch}";
})
