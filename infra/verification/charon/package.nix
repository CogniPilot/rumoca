{ aeneas, system }:
let
  upstream = aeneas.inputs.charon;
  patch = ./reconstruct-box-borrows.patch;
  original = upstream.packages.${system}.charon-unwrapped;
  patched = original.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ patch ];
    CHARON_GIT_COMMIT = "${upstream.rev}-rumoca-${builtins.hashFile "sha256" patch}";
    CARGO_BUILD_JOBS = "4";
    RUST_TEST_THREADS = "4";
    RAYON_NUM_THREADS = "4";
    checkPhase = old.checkPhase + ''
      CHARON_TOOLCHAIN_IS_IN_PATH=1 IN_CI=1 cargo clippy --profile release --locked --all-targets -- -D warnings
    '';
  });
in
upstream.packages.${system}.charon.overrideAttrs (old: {
  buildCommand = builtins.replaceStrings [ "${original}" ] [ "${patched}" ] old.buildCommand;
  # Upstream passthru checks refer to the unpatched source, not this package.
  passthru = { };
})
