{ aeneas, system }:
let
  base = import ../explicit-substitution/package.nix { inherit aeneas system; };
  demands = ./finite-demands.patch;
  cycles = ./cyclic_demands.rs;
in
base.overrideAttrs (old: {
  patches = old.patches ++ [ demands ];
  postPatch = (old.postPatch or "") + ''
    mkdir -p src/transform/normalize/expand_associated_types tests/fixtures
    cp ${cycles} src/transform/normalize/expand_associated_types/cyclic_demands.rs
    cp ${./specialized_traits.rs} tests/specialized_traits.rs
    cp ${../explicit-substitution/trait-self-source.rs} tests/fixtures/specialized_traits.rs
    cp ${./cyclic-assoc-source.rs} tests/fixtures/cyclic-assoc-source.rs
    cp ${./cyclic-noninverse-source.rs} tests/fixtures/cyclic-noninverse-source.rs
    cp ${./cyclic-distinct-associated-source.rs} tests/fixtures/cyclic-distinct-associated-source.rs
  '';
  CHARON_GIT_COMMIT = "${old.CHARON_GIT_COMMIT}-finite-demands-${builtins.hashFile "sha256" demands}-${builtins.hashFile "sha256" cycles}";
})
