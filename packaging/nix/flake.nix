{
  description = "Rust development environment";

  inputs = {
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # we are currently using 25.05 version of rustup
    # for faster dev env setup times
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";

    # project root (not a flake!)
    src = {
      url = "path:..";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, src, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Read rust-toolchain.toml from the *project root*
        overrides =
          builtins.fromTOML (builtins.readFile (src + "/rust-toolchain.toml"));

        # Nix-only native libs that should be added to -L for *native* builds.
        # If you don't have any yet, leave this list empty.
        nativeLibs = [
          # e.g. pkgs.libvmi
        ];

        nativeLibFlags =
          builtins.concatStringsSep " " (map (a: "-L ${a}/lib") nativeLibs);
      in {
        devShells.default = pkgs.mkShell rec {
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = with pkgs; [
            clang
            llvmPackages.bintools
            rustup
            nodejs_22
            # plus any runtime libs, e.g. libvmi, etc.
          ];

          RUSTC_VERSION = overrides.toolchain.channel;

          # https://github.com/rust-lang/rust-bindgen#environment-variables
          LIBCLANG_PATH =
            pkgs.lib.makeLibraryPath [ pkgs.llvmPackages_latest.libclang.lib ];

          shellHook = ''
            export PATH=$PATH:''${CARGO_HOME:-~/.cargo}/bin
            export PATH=$PATH:''${RUSTUP_HOME:-~/.rustup}/toolchains/$RUSTC_VERSION-x86_64-unknown-linux-gnu/bin/

            # detect host triple (assumes rustc is available)
            host_target="$(${"RUSTC:-rustc"} -Vv | sed -n 's/^host: //p')"

            case "$host_target" in
              x86_64-unknown-linux-gnu)
                export CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUSTFLAGS="''${CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUSTFLAGS} ${nativeLibFlags}"
                ;;
              aarch64-unknown-linux-gnu)
                export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_RUSTFLAGS="''${CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_RUSTFLAGS} ${nativeLibFlags}"
                ;;
            esac
          '';

          # NOTE: no global RUSTFLAGS here; .cargo/config.toml remains in control
          # for wasm32-unknown-unknown (atomics, bulk-memory, etc.)

          LD_LIBRARY_PATH =
            pkgs.lib.makeLibraryPath (buildInputs ++ nativeBuildInputs);

          # Add glibc, clang, glib, and other headers to bindgen search path
          BINDGEN_EXTRA_CLANG_ARGS =
            # Includes normal include path
            (builtins.map (a: ''-I"${a}/include"'') [
              # add dev libraries here (e.g. pkgs.libvmi.dev)
              pkgs.glibc.dev
            ])
            # Includes with special directory paths
            ++ [
              ''
                -I"${pkgs.llvmPackages_latest.libclang.lib}/lib/clang/${pkgs.llvmPackages_latest.libclang.version}/include"''
              ''-I"${pkgs.glib.dev}/include/glib-2.0"''
              "-I${pkgs.glib.out}/lib/glib-2.0/include/"
            ];
        };
      });
}
