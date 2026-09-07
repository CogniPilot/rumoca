{ aeneas, system }:
let
  rootPatch = ./keep-function-roots.patch;
  aliasPatch = ./preserve-shared-aliases.patch;
  patchedSource = aeneas.inputs.nixpkgs.legacyPackages.${system}.applyPatches {
    name = "aeneas-shared-alias-source";
    src = aeneas.outPath;
    patches = [ aliasPatch ];
  };
  charon = import ../charon/package.nix { inherit aeneas system; };
in
(aeneas.packages.${system}.aeneas.override { inherit charon; }).overrideAttrs (old: {
  src = "${patchedSource}/src";
  patches = (old.patches or [ ]) ++ [ rootPatch ];
  AENEAS_VERSION = "${aeneas.rev}-rumoca-${builtins.hashFile "sha256" rootPatch}-alias-${builtins.hashFile "sha256" aliasPatch}";
})
