{ aeneas, system }:
let
  base = import ../capture-arguments/package.nix { inherit aeneas system; };
  constructor = ./constructor.patch;
  facts = builtins.path {
    path = ../closure-regions;
    name = "closure-region-facts-source";
    filter = path: type:
      let
        relative = builtins.substring
          (builtins.stringLength (toString ../closure-regions) + 1)
          (builtins.stringLength path)
          path;
      in
      relative == "Cargo.toml" || relative == "src" || builtins.match "src/.*" relative != null;
  };
in
base.unwrapped.overrideAttrs (old: {
  patches = old.patches ++ [ constructor ];
  postPatch = (old.postPatch or "") + ''
    cp -r ${facts} closure-region-facts
    chmod -R u+w closure-region-facts
  '';
  CHARON_GIT_COMMIT = "${old.CHARON_GIT_COMMIT}-constructor-${builtins.hashFile "sha256" constructor}-facts-${builtins.hashString "sha256" (toString facts)}";
})
