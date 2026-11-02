#!/usr/bin/env bash
# Reuse the release package already built by this job; no Cargo build here.
set -euo pipefail
out="$PWD/target/humanoid-comparison"
deps="$RUNNER_TEMP/humanoid-comparison-deps"
mkdir -p "$deps"
date +%s > "$RUNNER_TEMP/humanoid-comparison-start"
curl --fail --location --retry 2 --connect-timeout 10 --max-time 90 \
  https://github.com/google-deepmind/mujoco/releases/download/3.8.1/mujoco-3.8.1-linux-x86_64.tar.gz \
  -o "$deps/mujoco.tar.gz"
echo "ed9bd454c40e9e9913c70a2f83ab059defa09b05a27ef55ca93317457212b4a7  $deps/mujoco.tar.gz" | sha256sum --check
tar -xzf "$deps/mujoco.tar.gz" -C "$deps"
curl --fail --location --retry 2 --connect-timeout 10 --max-time 90 \
  https://github.com/modelica/ModelicaStandardLibrary/releases/download/v4.1.0/ModelicaStandardLibrary_v4.1.0.zip \
  -o "$deps/msl.zip"
echo "5409cfa17797c52d866dc9d2675badc1378b4bbf88066e60899a489cb1cb8a2d  $deps/msl.zip" | sha256sum --check
unzip -q "$deps/msl.zip" -d "$deps/msl"
ulimit -s 1048576
test "$(git rev-parse HEAD)" = "$CI_SOURCE_SHA"
git diff --exit-code HEAD --
flake="git+file://$PWD?rev=$CI_SOURCE_SHA"
rumoca_package=$(nix path-info "$flake#rumoca")
rumoca_drv=$(nix path-info --derivation "$flake#rumoca")
nix derivation show "$rumoca_drv" > "$deps/rumoca-derivation.json"
nix flake metadata --json "$flake" > "$deps/rumoca-flake.json"
python3 examples/benchmarks/humanoid/run.py \
  --rumoca "$rumoca_package/bin/rumoca" --msl "$deps/msl" \
  --nix-derivation "$deps/rumoca-derivation.json" --nix-metadata "$deps/rumoca-flake.json" \
  --mujoco "$deps/mujoco-3.8.1" --output "$out"
