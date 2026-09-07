{ aeneas, system }:
let
  upstream = aeneas.inputs.charon;
  pkgs = upstream.inputs.nixpkgs.legacyPackages.${system};
  inherit (pkgs) lib;
  rustToolchain = upstream.packages.${system}.rustToolchain;
  miriSysroots = upstream.packages.${system}.charon-full-mir-sysroots;
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
pkgs.runCommand "charon" {
  nativeBuildInputs = [ pkgs.makeWrapper ] ++ lib.optionals pkgs.stdenv.isDarwin [ pkgs.cctools ];
  passthru.unwrapped = patched;
} (''
  cp -r ${patched} $out
  chmod -R u+w $out
  wrapProgram $out/bin/charon \
    --set CHARON_TOOLCHAIN_IS_IN_PATH 1 \
    --set CHARON_MIRI_SYSROOTS "${miriSysroots}" \
    --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [ rustToolchain ]}" \
    --prefix PATH : "${lib.makeBinPath [ rustToolchain ]}"
'' + lib.optionalString pkgs.stdenv.isDarwin ''
  install_name_tool -add_rpath "${rustToolchain}/lib" "$out/bin/charon-driver"
'')
