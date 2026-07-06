{
  description = "rumoca — a Modelica compiler (crane + fenix reproducible build)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
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
          buildInputs = [ pkgs.udev ];

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

        # Release-mode artifacts for the MSL parity gate, built ONCE here so the
        # shard / merge / ModelicaTest jobs consume them via Cachix instead of
        # each re-compiling + re-LTO'ing the workspace (LTO is a link-time cost no
        # per-crate cache can avoid — only build-once can). Both reuse
        # cargoArtifacts so only the workspace crates compile, deps are cached.
        rumoca-sim-worker = craneLib.buildPackage (commonArgs // {
          inherit cargoArtifacts;
          pname = "rumoca-sim-worker";
          doCheck = false;
          cargoExtraArgs = "-p rumoca-test-msl --bin rumoca-sim-worker";
          nativeBuildInputs = commonArgs.nativeBuildInputs ++ [ pkgs.autoPatchelfHook ];
          buildInputs = commonArgs.buildInputs ++ [ pkgs.stdenv.cc.cc.lib ];
        });

        # The MSL harness is a libtest binary (msl-full-test feature); build it
        # with `--no-run` and install the hashed test binary under a stable name.
        # It also needs the per-model compile worker when run outside Cargo.
        msl-test-binary = craneLib.mkCargoDerivation (commonArgs // {
          inherit cargoArtifacts;
          pname = "rumoca-msl-test-binary";
          buildPhaseCargoCommand = ''
            cargo test --release --no-run \
              -p rumoca-test-msl --features msl-full-test --test msl_tests
            cargo build --release -p rumoca-worker --bin rumoca-worker
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
          '';
        });
        # xtask itself is NOT built here: after the light-xtask split it carries no
        # compiler deps and compiles per-job in seconds, so build-once buys nothing.
        # The MSL merge/ModelicaTest jobs DO run the `repo msl` reporting subcommands
        # (compatibility-report, modelica-test-catalog); those live in the
        # compiler-linked `rumoca-msl-tools` bin, so build it once here and have
        # those jobs invoke the prebuilt binary instead of recompiling the stack.
        rumoca-msl-tools = craneLib.buildPackage (commonArgs // {
          inherit cargoArtifacts;
          pname = "rumoca-msl-tools";
          doCheck = false;
          cargoExtraArgs = "-p rumoca-test-msl --bin rumoca-msl-tools";
          nativeBuildInputs = commonArgs.nativeBuildInputs ++ [ pkgs.autoPatchelfHook ];
          buildInputs = commonArgs.buildInputs ++ [ pkgs.stdenv.cc.cc.lib ];
        });
      in {
        packages = {
          default = rumoca;
          rumoca = rumoca;
          # rumoca already builds `--bin rumoca-lsp`; alias so the LSP gate can
          # `nix build .#rumoca-lsp` and read result/bin/rumoca-lsp.
          rumoca-lsp = rumoca;
          inherit rumoca-sim-worker msl-test-binary rumoca-msl-tools;
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
          packages = [ pkgs.nodejs_22 pkgs.python3 ];
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
        };
      });
}
