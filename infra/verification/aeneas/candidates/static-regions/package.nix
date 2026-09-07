let
  system = builtins.currentSystem;
  aeneas = builtins.getFlake "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef";
  adopted = import ../../package.nix { inherit aeneas system; };
  candidatePatch = ./static-regions.patch;
  rootPatch = ./contextual-keep-function-roots.patch;
  candidateSource = aeneas.inputs.nixpkgs.legacyPackages.${system}.applyPatches {
    name = "aeneas-static-regions-candidate-source";
    src = adopted.src;
    patches = [ candidatePatch ];
  };
in
adopted.overrideAttrs (old: {
  src = candidateSource;
  patches = [ rootPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-candidate-static-regions-${builtins.hashFile "sha256" candidatePatch}-roots-${builtins.hashFile "sha256" rootPatch}";
})
