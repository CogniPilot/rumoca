{
  description = "rumoca — a Modelica compiler (crane + fenix reproducible build)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    aeneas.url = "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef";
    openmodelica.url = "git+https://github.com/jgoppert/OpenModelica?submodules=1&rev=a96aa1a682c463b0fd2d285b486c09a8b7fe496d";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      crane,
      aeneas,
      openmodelica,
      fenix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfreePredicate =
            package:
            builtins.elem (nixpkgs.lib.getName package) [
              "cuda_cccl"
              "cuda_cudart"
              "cuda_nvcc"
            ];
        };
        rumocaVersion = (builtins.fromTOML (builtins.readFile ./Cargo.toml)).workspace.package.version;

        # Pin the EXACT toolchain from rust-toolchain.toml (nightly-2026-02-27 +
        # rust-src + wasm32) via fenix, so the Nix build and CI use the same
        # rustc the developers pin. The sha256 is discovered on first build.
        rustToolchain = fenix.packages.${system}.fromToolchainFile {
          file = ./rust-toolchain.toml;
          sha256 = "sha256-5twI9QsrPl0ryOZ4POGYAivSeI08jgmWnv0wVvzbjcE=";
        };

        # Kani is deliberately isolated from the ordinary development
        # toolchain.  Its compiler must exactly match the release bundle, while
        # rumoca's default shell remains pinned by rust-toolchain.toml.
        kaniVersion = "0.67.0";
        kaniSupported = system == "x86_64-linux";
        kaniRustToolchain = fenix.packages.${system}.fromToolchainFile {
          file = ./rust-toolchain-kani.toml;
          sha256 = "sha256-P39FCgpfDT04989+ZTNEdM/k/AE869JKSB4qjatYTSs=";
        };
        kaniCli = pkgs.rustPlatform.buildRustPackage {
          pname = "kani-verifier";
          version = kaniVersion;
          src = pkgs.fetchCrate {
            pname = "kani-verifier";
            version = kaniVersion;
            hash = "sha256-m0khwmHJAiEtICN/f2IE70A2/0JNKwaL3so429YtdOY=";
          };
          cargoHash = "sha256-KAFLA97yi74riDkBO3EJ9Uv6SdVQrJ1wLNJ68Jf9yWk=";
        };
        kaniHome = pkgs.stdenv.mkDerivation {
          pname = "kani-home";
          version = kaniVersion;
          src = pkgs.fetchurl {
            url = "https://github.com/model-checking/kani/releases/download/kani-${kaniVersion}/kani-${kaniVersion}-x86_64-unknown-linux-gnu.tar.gz";
            hash = "sha256-O196/TtRYD7nINt7wbxP5GtaT1022q2ZOcS0xli1GsA=";
          };
          nativeBuildInputs = [ pkgs.autoPatchelfHook ];
          buildInputs = [
            kaniRustToolchain
            pkgs.stdenv.cc.cc.lib
            pkgs.zlib
          ];
          installPhase = ''
            runHook preInstall
            mkdir -p "$out/kani-${kaniVersion}"
            cp -R . "$out/kani-${kaniVersion}/"
            ln -s ${kaniRustToolchain} "$out/kani-${kaniVersion}/toolchain"
            runHook postInstall
          '';
        };
        kani = pkgs.symlinkJoin {
          name = "kani-${kaniVersion}";
          meta = {
            mainProgram = "kani";
            platforms = [ "x86_64-linux" ];
          };
          paths = [
            kaniCli
            kaniHome
            kaniRustToolchain
          ];
          nativeBuildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            for proxy in kani cargo-kani; do
              wrapProgram "$out/bin/$proxy" \
                --set KANI_HOME "$out" \
                --prefix PATH : "${kaniRustToolchain}/bin"
            done
          '';
        };

        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
        ciJulia = pkgs.julia_111;
        ciPythonPackages = ps: [
          ps.casadi
          ps.ipython
          (ps.jax.overridePythonAttrs (_: {
            doCheck = false;
          }))
          ps.numpy
          ps.pandas
          ps.pip
          ps.sympy
          ps.virtualenv
        ];
        ciPython = pkgs.python312.withPackages ciPythonPackages;
        # The FMI conformance checks require exactly FMPy 0.3.30 while
        # nixpkgs tracks 0.3.27, so the pinned wheel is fetched by digest and
        # the conformance toolchain resolves from the flake rather than being
        # provisioned into the working tree at run time. Only the wheel's core
        # requirements are propagated; the GUI extra is not used here.
        fmpyPinned = pkgs.python312Packages.buildPythonPackage rec {
          pname = "fmpy";
          version = "0.3.30";
          format = "wheel";
          src = pkgs.fetchPypi {
            pname = "fmpy";
            inherit version format;
            dist = "py3";
            python = "py3";
            hash = "sha256-wuLk+yt42vy1yXc/iUb84p8C9dDyxhaZwknPD+g5H5M=";
          };
          # FMPy ships no CMake project of its own: it invokes the `cmake`
          # program to build FMU sources. Supply the real toolchain instead of
          # the PyPI wrapper wheel, and keep CMake out of the build phases.
          dontUseCmakeConfigure = true;
          dontCheckRuntimeDeps = true;
          propagatedBuildInputs = (
            with pkgs.python312Packages;
            [
              attrs
              jinja2
              lark
              lxml
              msgpack
              nbformat
              numpy
            ]
          ) ++ [ pkgs.cmake ];
          doCheck = false;
          pythonImportsCheck = [ "fmpy" ];
        };
        fmiPython = pkgs.python312.withPackages (ps: ciPythonPackages ps ++ [ fmpyPinned ]);
        openModelicaCli = openmodelica.packages.${system}.default;
        mlirCpuTools = pkgs.symlinkJoin {
          name = "rumoca-mlir-cpu-tools-18";
          paths = [
            pkgs.llvmPackages_18.clang
            pkgs.llvmPackages_18.llvm
            pkgs.llvmPackages_18.mlir
          ];
          postBuild = ''
            ln -s "$out/bin/clang" "$out/bin/clang-18"
            ln -s "$out/bin/llc" "$out/bin/llc-18"
            ln -s "$out/bin/mlir-opt" "$out/bin/mlir-opt-18"
            ln -s "$out/bin/mlir-translate" "$out/bin/mlir-translate-18"
          '';
        };

        # Native libs the workspace links against. libudev (systemd) for the
        # gamepad/input crates; clang/libclang for any bindgen-using dep.
        # crane's cleanCargoSource keeps only *.rs + Cargo.*, but several build
        # scripts embed non-Rust assets (jinja/toml templates under
        # crates/*/src/templates, XSD schemas, .mo models). Keep the whole
        # crates/ tree (plus the root manifests + .cargo config), scoped so we
        # never pull in packages/ node_modules or examples/.venv.
        src = pkgs.lib.cleanSourceWith {
          src = ./.;
          filter =
            path: _type:
            let
              rel = pkgs.lib.removePrefix (toString ./. + "/") (toString path);
            in
            pkgs.lib.hasPrefix "crates" rel
            # examples/ holds the .mo models that crate examples `include_str!`
            # (e.g. quadrotor_sil); keep it but drop the Python venv / node deps.
            || (
              pkgs.lib.hasPrefix "examples" rel
              && !(pkgs.lib.hasInfix ".venv" rel)
              && !(pkgs.lib.hasInfix "node_modules" rel)
            )
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
        mlirCpuTestArgs = builtins.concatStringsSep " " [
          "--package rumoca-exec-mlir"
          "--features required-mlir-cpu"
          "--lib"
          "--test suite_exec_mlir"
        ];
        mlirCpuTests =
          assert pkgs.lib.hasInfix ".#checks.x86_64-linux.mlir-cpu" (
            builtins.readFile ./.github/workflows/ci.yml
          );
          craneLib.cargoTest (
            commonArgs
            // {
              inherit cargoArtifacts;
              pname = "rumoca-mlir-cpu-tests";
              cargoExtraArgs = mlirCpuTestArgs;
              nativeBuildInputs = commonArgs.nativeBuildInputs ++ [ mlirCpuTools ];
              preCheck = ''
                for tool in clang-18 llc-18 mlir-opt-18 mlir-translate-18; do
                  command -v "$tool"
                  "$tool" --version | grep -Eq 'version 18(\.|$)|MLIR 18(\.|$)'
                done
              '';
            }
          );

        rumoca = craneLib.buildPackage (
          commonArgs
          // {
            inherit cargoArtifacts;
            doCheck = false; # tests run in CI, not in the package build
            # A dependency (gamepad/input) links libudev; autoPatchelfHook bakes
            # the store RPATH into the binaries so they run outside a nix shell.
            # libgcc_s (compiler runtime) is pulled from stdenv's cc lib.
            nativeBuildInputs = commonArgs.nativeBuildInputs ++ [ pkgs.autoPatchelfHook ];
            buildInputs = commonArgs.buildInputs ++ [ pkgs.stdenv.cc.cc.lib ];
          }
        );

        # Store-native Python binding used by cross-repository integration
        # builds. This replaces the workspace's mutable venv/wheel-by-mtime
        # staging while retaining Rumoca's pinned fenix toolchain.
        rumocaPython = pkgs.python312Packages.buildPythonPackage {
          pname = "rumoca";
          version = rumocaVersion;
          pyproject = true;
          inherit src;
          sourceRoot = "rumoca-src/crates/rumoca-bind-python";
          cargoRoot = "../..";
          cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
            inherit src;
            cargoRoot = ".";
            name = "rumoca-${rumocaVersion}-cargo-vendor";
            hash = "sha256-6IrOS6ihidQlejnWKFSWUN84NKamLRGgkrGEFY4q9hI=";
          };
          nativeBuildInputs = [
            rustToolchain
            pkgs.pkg-config
            pkgs.rustPlatform.cargoSetupHook
            pkgs.rustPlatform.maturinBuildHook
          ];
          # Parser build scripts refresh generated files inside the Cargo
          # workspace. Nix sources are read-only, so make this derivation's
          # private build copy writable without mutating the checkout.
          postPatch = ''
            chmod -R u+w ../..
          '';
          buildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.udev ];
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          CARGO_TARGET_DIR = "target";
          doCheck = false;
          pythonImportsCheck = [ "rumoca" ];
        };

        rumocaPythonEnv = pkgs.python312.withPackages (_: [ rumocaPython ]);

        # Release-mode artifacts for the MSL parity gate, built as one Cargo
        # graph so the shard / merge / ModelicaTest / pinned-library consumers
        # restore them from the CI producer instead of recompiling + re-LTO'ing
        # the workspace. A single
        # derivation keeps rumoca-worker, rumoca-msl-tools,
        # the focused profile runner, and the libtest harness in one target
        # directory; separate derivations
        # rebuild the same workspace crates and made rumoca-worker a serial
        # extra build.
        msl-artifacts = craneLib.mkCargoDerivation (
          commonArgs
          // {
            inherit cargoArtifacts;
            pname = "rumoca-msl-artifacts";
            buildPhaseCargoCommand = ''
              cargo build --release \
                -p rumoca-worker \
                -p rumoca-test-msl \
                --features rumoca-test-msl/msl-full-test,rumoca-test-msl/msl-profile-bin \
                --bin rumoca-worker \
                --bin rumoca-msl-tools \
                --bin rumoca-msl-profile \
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
              cp target/release/rumoca-msl-tools $out/bin/rumoca-msl-tools
              cp target/release/rumoca-msl-profile $out/bin/rumoca-msl-profile
            '';
          }
        );
        commonDevShellArgs = {
          buildInputs = commonArgs.buildInputs;
          shellHook = ''
            export PATH="''${CARGO_HOME:-$HOME/.cargo}/bin:$PATH"
          '';
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (
            [
              pkgs.stdenv.cc.cc.lib
              pkgs.zlib
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.udev ]
          );
        };
        commonDevPackages = [
          pkgs.jq
          pkgs.pkg-config
        ];
        mkDevShell =
          extraPackages:
          craneLib.devShell (commonDevShellArgs // { packages = commonDevPackages ++ extraPackages; });
        templateRuntimeShell =
          extraPackages:
          craneLib.devShell (
            commonDevShellArgs
            // {
              packages = commonDevPackages ++ extraPackages;
              LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (
                [
                  pkgs.gfortran.cc.lib
                  pkgs.stdenv.cc.cc.lib
                  pkgs.zlib
                ]
                ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.udev ]
              );
            }
          );
        wasmShell = mkDevShell [
          pkgs.binaryen
          pkgs.nodejs_22
          pkgs.wasm-pack
        ];
        pythonShell = templateRuntimeShell [
          ciPython
          pkgs.maturin
        ];
        embeddedBenchmarkShell = templateRuntimeShell [
          ciPython
          pkgs.gcc-arm-embedded
          pkgs.qemu
          pkgs.util-linux
        ];
        juliaShell = templateRuntimeShell (pkgs.lib.optionals pkgs.stdenv.isLinux [ ciJulia ]);
        modelicaShell = templateRuntimeShell (pkgs.lib.optionals pkgs.stdenv.isLinux [ openModelicaCli ]);
        # The external FMI conformance toolchain. Shared so `.#full` is a
        # superset of `.#fmi` by construction rather than by coincidence.
        fmiRuntimeTools = [
          fmiPython
          pkgs.cmake
          pkgs.curl
          pkgs.jre_headless
          pkgs.libxml2
          pkgs.nodejs_22
          pkgs.unzip
        ];
        fmiShell = templateRuntimeShell (
          fmiRuntimeTools
          ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ openModelicaCli ]
        );
        docsShell = mkDevShell [
          pkgs.binaryen
          pkgs.mdbook
          pkgs.nodejs_22
          pkgs.wasm-pack
        ];
        fullShell = templateRuntimeShell (
          pkgs.lib.optionals pkgs.stdenv.isLinux [
            ciJulia
            openModelicaCli
          ]
          ++ fmiRuntimeTools
          ++ [
            pkgs.binaryen
            pkgs.cargo-expand
            pkgs.cargo-llvm-cov
            pkgs.cargo-nextest
            pkgs.hyperfine
            pkgs.maturin
            pkgs.mdbook
            pkgs.ripgrep
            pkgs.wasm-pack
          ]
        );
        # xtask itself is NOT built here: after the light-xtask split it carries no
        # compiler deps and compiles per-job in seconds, so build-once buys nothing.
        # The MSL merge and ModelicaTest jobs run reporting through the
        # compiler-linked `rumoca-msl-tools` bin, so the MSL artifact bundle
        # includes it and those jobs invoke the prebuilt binary instead of
        # recompiling the stack.
      in
      {
        packages = {
          default = rumoca;
          rumoca = rumoca;
          # rumoca already builds `--bin rumoca-lsp`; alias so the LSP gate can
          # `nix build .#rumoca-lsp` and read result/bin/rumoca-lsp.
          rumoca-lsp = rumoca;
          rumoca-python = rumocaPython;
          rumoca-python-env = rumocaPythonEnv;
          msl-artifacts = msl-artifacts;
        }
        // pkgs.lib.optionalAttrs kaniSupported {
          kani = kani;
          kani-cli = kaniCli;
          kani-home = kaniHome;
        }
        // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
          openmodelica-cli = openModelicaCli;
        };

        checks = {
          inherit rumoca;
          clippy = craneLib.cargoClippy (
            commonArgs
            // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets -- -D warnings";
            }
          );
          fmt = craneLib.cargoFmt { src = ./.; };
        }
        // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
          mlir-cpu = mlirCpuTests;
          openmodelica-cli = openModelicaCli;
        };

        # Keep the default shell source-independent: it supplies only the
        # pinned Rust/native build environment. Optional runtimes are exposed
        # through named shells, and the packaged compiler remains an explicit
        # `nix build .#rumoca` operation.
        devShells.default = mkDevShell [ ];
        devShells.full = fullShell;
        devShells.wasm = wasmShell;
        devShells.python = pythonShell;
        devShells.embedded-benchmark = embeddedBenchmarkShell;
        devShells.julia = juliaShell;
        devShells.modelica = modelicaShell;
        devShells.fmi = fmiShell;
        devShells.docs = docsShell;
        devShells.${if system == "x86_64-linux" then "lean-pilot" else null} = mkDevShell [
          (import ./infra/verification/aeneas/package.nix { inherit aeneas system; })
          pkgs.elan
        ];
        # Python wheel packaging must depend only on the build and smoke-test
        # toolchain.  In particular, it must not inherit optional template
        # runtimes such as JAX, whose platform support is narrower than the
        # wheel matrix (currently excluding x86_64-darwin).
        devShells.ci-python-wheel = mkDevShell [
          pkgs.maturin
          pkgs.python312
        ];
        # WASM packaging needs the workspace build inputs plus the JavaScript
        # and optimization tools. Keep the interactive shell's Rumoca, OMC,
        # Julia, Python, and documentation closures out of this CI boundary.
        devShells.ci-wasm = wasmShell;
        devShells.${if kaniSupported then "kani" else null} = pkgs.mkShell (
          commonDevShellArgs
          // {
            packages = [
              pkgs.pkg-config
              kani
            ];
            KANI_HOME = kaniHome;
          }
        );
        devShells.ci-template-core = templateRuntimeShell [ ];
        devShells.ci-template-cuda = templateRuntimeShell (
          pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.cudaPackages.cuda_cudart
            pkgs.cudaPackages.cuda_nvcc
          ]
        );
        devShells.ci-template-fmi = fmiShell;
        devShells.ci-template-modelica = modelicaShell;
        devShells.ci-template-wasm = templateRuntimeShell [ pkgs.wasm-tools ];
        devShells.ci-template-python = templateRuntimeShell [ ciPython ];
        devShells.ci-template-julia = juliaShell;
      }
    );
}
