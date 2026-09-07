let
  system = builtins.currentSystem;
  aeneas = builtins.getFlake "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef";
  base = import ../../package.nix { inherit aeneas system; };
  charon = import ../../../charon/candidates/capture-arguments/package.nix { inherit aeneas system; };
  staticPatch = ./static-regions.patch;
  lifecyclePatch = ./static-lifecycle.patch;
  rootPatch = ./contextual-keep-function-roots.patch;
  signaturePatch = ../capture-arguments/remove-signature-guess.patch;
  source = aeneas.inputs.nixpkgs.legacyPackages.${system}.applyPatches {
    name = "aeneas-static-lifecycle-source";
    src = base.src;
    patches = [ staticPatch lifecyclePatch ];
  };
in
(base.override { inherit charon; }).overrideAttrs (old: {
  src = source;
  patches = [ rootPatch signaturePatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-static-${builtins.hashFile "sha256" staticPatch}-lifecycle-${builtins.hashFile "sha256" lifecyclePatch}-roots-${builtins.hashFile "sha256" rootPatch}-no-signature-guess-${builtins.hashFile "sha256" signaturePatch}";
})
