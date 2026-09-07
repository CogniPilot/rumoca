let
  system = builtins.currentSystem;
  aeneas = builtins.getFlake "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef";
  base = import ../../package.nix { inherit aeneas system; };
  removal = ./remove-signature-guess.patch;
in
base.overrideAttrs (old: {
  patches = (old.patches or [ ]) ++ [ removal ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-no-signature-guess-${builtins.hashFile "sha256" removal}";
})
