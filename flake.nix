{
  description = "rumoca — a Modelica compiler (crane + fenix reproducible build)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, crane, fenix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Pin the EXACT toolchain from rust-toolchain.toml (nightly-2026-02-27 +
        # rust-src + wasm32) via fenix, so the Nix build and CI use the same
        # rustc the developers pin. The sha256 is discovered on first build.
        rustToolchain = fenix.packages.${system}.fromToolchainFile {
          file = ./rust-toolchain.toml;
          sha256 = "sha256-5twI9QsrPl0ryOZ4POGYAivSeI08jgmWnv0wVvzbjcE=";
        };

        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
        ciJulia = pkgs.julia_111;
        ciPython = pkgs.python312.withPackages (ps: [
          ps.ipython
          ps.numpy
          ps.pandas
          ps.pip
          ps.sympy
          ps.virtualenv
        ]);

        # Native libs the workspace links against. libudev (systemd) for the
        # gamepad/input crates; clang/libclang for any bindgen-using dep.
        # crane's cleanCargoSource keeps only *.rs + Cargo.*, but several build
        # scripts embed non-Rust assets (jinja/toml templates under
        # crates/*/src/templates, XSD schemas, .mo models). Keep the whole
        # crates/ tree (plus the root manifests + .cargo config), scoped so we
        # never pull in packages/ node_modules or examples/.venv.
        src = pkgs.lib.cleanSourceWith {
          src = ./.;
          filter = path: _type:
            let rel = pkgs.lib.removePrefix (toString ./. + "/") (toString path);
            in pkgs.lib.hasPrefix "crates" rel
            # examples/ holds the .mo models that crate examples `include_str!`
            # (e.g. quadrotor_sil); keep it but drop the Python venv / node deps.
            || (pkgs.lib.hasPrefix "examples" rel
                && !(pkgs.lib.hasInfix ".venv" rel)
                && !(pkgs.lib.hasInfix "node_modules" rel))
            || pkgs.lib.hasPrefix ".cargo" rel
            || rel == "Cargo.toml"
            || rel == "Cargo.lock"
            || rel == "rust-toolchain.toml";
          name = "rumoca-src";
        };

        commonArgs = {
          inherit src;
          strictDeps = true;

          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.udev ];

          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          # The wasm binding crates are cdylibs for wasm32 and the python
          # binding needs a Python interpreter; the reproducible native build
          # targets only the CLI binaries. (CI's wasm/python jobs stay separate.)
          cargoExtraArgs = "--bin rumoca --bin rumoca-lsp";
        };

        # Build the third-party dependency closure once; every package/check
        # below reuses it so a code change never recompiles dependencies.
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        rumoca = craneLib.buildPackage (commonArgs // {
          inherit cargoArtifacts;
          doCheck = false; # tests run in CI, not in the package build
          # A dependency (gamepad/input) links libudev; autoPatchelfHook bakes
          # the store RPATH into the binaries so they run outside a nix shell.
          # libgcc_s (compiler runtime) is pulled from stdenv's cc lib.
          nativeBuildInputs = commonArgs.nativeBuildInputs ++ [ pkgs.autoPatchelfHook ];
          buildInputs = commonArgs.buildInputs ++ [ pkgs.stdenv.cc.cc.lib ];
        });

        # Release-mode artifacts for the MSL parity gate, built as one Cargo
        # graph so the shard / merge / ModelicaTest consumers restore them via
        # Cachix instead of recompiling + re-LTO'ing the workspace. A single
        # derivation keeps rumoca-worker, rumoca-sim-worker, rumoca-msl-tools,
        # and the libtest harness in one target directory; separate derivations
        # rebuild the same workspace crates and made rumoca-worker a serial
        # extra build.
        msl-artifacts = craneLib.mkCargoDerivation (commonArgs // {
          inherit cargoArtifacts;
          pname = "rumoca-msl-artifacts";
          buildPhaseCargoCommand = ''
            cargo build --release \
              -p rumoca-worker \
              -p rumoca-test-msl \
              --features rumoca-test-msl/msl-full-test \
              --bin rumoca-worker \
              --bin rumoca-sim-worker \
              --bin rumoca-msl-tools \
              --test msl_tests
          '';
          nativeBuildInputs = commonArgs.nativeBuildInputs ++ [ pkgs.autoPatchelfHook ];
          buildInputs = commonArgs.buildInputs ++ [ pkgs.stdenv.cc.cc.lib ];
          installPhaseCommand = ''
            mkdir -p $out/bin
            bin=$(find target/release/deps -maxdepth 1 -type f \
              -name 'msl_tests-*' ! -name '*.d' -perm -u+x | head -1)
            test -n "$bin" || { echo "msl_tests test binary not found"; exit 1; }
            cp "$bin" $out/bin/msl_tests
            cp target/release/rumoca-worker $out/bin/rumoca-worker
            cp target/release/rumoca-sim-worker $out/bin/rumoca-sim-worker
            cp target/release/rumoca-msl-tools $out/bin/rumoca-msl-tools
          '';
        });
        templateRuntimeShell = extraPackages: craneLib.devShell {
          inputsFrom = [ rumoca ];
          packages = extraPackages;
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.gfortran.cc.lib
            pkgs.stdenv.cc.cc.lib
            pkgs.zlib
          ];
        };
        # xtask itself is NOT built here: after the light-xtask split it carries no
        # compiler deps and compiles per-job in seconds, so build-once buys nothing.
        # The MSL merge and ModelicaTest jobs run reporting through the
        # compiler-linked `rumoca-msl-tools` bin, so the MSL artifact bundle
        # includes it and those jobs invoke the prebuilt binary instead of
        # recompiling the stack.
      in {
        packages = {
          default = rumoca;
          rumoca = rumoca;
          # rumoca already builds `--bin rumoca-lsp`; alias so the LSP gate can
          # `nix build .#rumoca-lsp` and read result/bin/rumoca-lsp.
          rumoca-lsp = rumoca;
          msl-artifacts = msl-artifacts;
        };

        checks = {
          inherit rumoca;
          clippy = craneLib.cargoClippy (commonArgs // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- -D warnings";
          });
          fmt = craneLib.cargoFmt { src = ./.; };
        };

        devShells.default = craneLib.devShell {
          inputsFrom = [ rumoca ];
          packages = pkgs.lib.optionals pkgs.stdenv.isLinux [
            ciJulia
          ] ++ [
            ciPython
            pkgs.binaryen
            pkgs.maturin
            pkgs.mdbook
            pkgs.nodejs_22
            pkgs.wasm-pack
          ];
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.gfortran.cc.lib
            pkgs.stdenv.cc.cc.lib
            pkgs.zlib
          ];
        };
        devShells.ci-template-core = templateRuntimeShell [];
        devShells.ci-template-python = templateRuntimeShell [ ciPython ];
        devShells.ci-template-julia = templateRuntimeShell (
          pkgs.lib.optionals pkgs.stdenv.isLinux [ ciJulia ]
        );
      });
}
