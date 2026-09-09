# Build only the patched charon-ml library (compiles and format-checks the
# consumer patch without Aeneas).
let
  system = builtins.currentSystem;
  aeneas = builtins.getFlake "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef";
in
import ./charon-ml.nix { inherit aeneas system; }
