let
  system = builtins.currentSystem;
  aeneas = builtins.getFlake "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef";
  upstream = aeneas.inputs.charon;
  original = upstream.packages.${system}.charon-unwrapped;
  captureArguments = ./capture-arguments.patch;
  boxPatch = ../../reconstruct-box-borrows.patch;
  patched = original.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ boxPatch captureArguments ];
    CHARON_GIT_COMMIT = "${upstream.rev}-capture-arguments-${builtins.hashFile "sha256" captureArguments}-box-${builtins.hashFile "sha256" boxPatch}";
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
  passthru = { };
})
