# Aeneas and charon-ml consumers of instantiation-record chains, on the M0 replay
# chain (specialized-impl-names) and the trait-identity Charon. The Charon package
# has no store path until the ui expectations lane lands; use replay.nix to build
# the consumers against the chain's existing Charon binary.
let
  system = builtins.currentSystem;
  aeneas = builtins.getFlake "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef";
  base = import ../loop-return-order/package.nix;
  charon = import ../../../charon/candidates/trait-identity/package.nix { inherit aeneas system; };
  charon-ml = import ./charon-ml.nix { inherit aeneas system; };
  aeneasPatch = ./aeneas.patch;
in
(base.override { inherit charon charon-ml; }).overrideAttrs (old: {
  patches = old.patches ++ [ aeneasPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-record-chain-${builtins.hashFile "sha256" aeneasPatch}-ml-${builtins.hashFile "sha256" ./charon-ml.patch}";
})
