let
  base = import ../../../aeneas/candidates/builtin-identity/package.nix;
  specializationPatch = ../../../aeneas/candidates/iterator-library-boundary/specialized-names.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ specializationPatch ];
  AENEAS_VERSION = "${old.AENEAS_VERSION}-specialized-names-${builtins.hashFile "sha256" specializationPatch}";
})
