{ aeneas, system }:
let
  base = import ../method-constraints/transport.nix { inherit aeneas system; };
  explicitSelf = ./explicit-self.patch;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ explicitSelf ];
  postPatch = (old.postPatch or "") + ''
    cp ${./explicit_substitution.rs} tests/explicit_substitution.rs
    cp ${./optional-borrow-try.rs} tests/ui/monomorphization/optional-borrow-try.rs
    cp ${./optional-borrow-try.out} tests/ui/monomorphization/optional-borrow-try.out
  '';
  CHARON_GIT_COMMIT = "${old.CHARON_GIT_COMMIT}-explicit-self-${builtins.hashFile "sha256" explicitSelf}";
})
