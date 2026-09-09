# Bisection recipe: the M0 replay chain with only the charon-ml patch, no Aeneas
# patch, to attribute a consumer regression to one of the two patches.
let
  system = builtins.currentSystem;
  aeneas = builtins.getFlake "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef";
  base = import ../loop-return-order/package.nix;
  charon-ml = import ./charon-ml.nix { inherit aeneas system; };
in
(base.override { inherit charon-ml; }).overrideAttrs (old: {
  AENEAS_VERSION = "${old.AENEAS_VERSION}-record-chain-ml-only-${builtins.hashFile "sha256" ./charon-ml.patch}";
})
