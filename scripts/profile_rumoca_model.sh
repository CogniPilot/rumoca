#!/usr/bin/env bash
set -euo pipefail

# Profiles a Rumoca model compile/check path and emits:
# 1) flamegraph SVG (for humans)
# 2) perf script + folded stacks (for AI/automation)
#
# Why this script uses perf+inferno directly:
# - More robust than cargo flamegraph in some environments where addr2line
#   post-processing can spam/fail.
# - AI-friendly outputs come from `perf script` and folded stacks.

usage() {
  cat <<'EOF'
Usage:
  profile_rumoca_model.sh [options]

Options:
  --workspace-root <path>   Workspace root override (default: auto-detect from repo layout)
  --model <qualified.name>  Model to profile
                            (default: WindPowerPlants.Components.GenericVariableSpeedGeneratorElectrical)
  --windpower-root <path>   WindPower source-root directory
                            (default: <workspace>/.tmp/windpower/WindPowerPlants-2.0.0)
  --msl-root <path>         MSL source-root directory
                            (default: auto-detect under packages/rumoca/target/msl/ModelicaStandardLibrary-*)
  --mode <mode>             profiling mode: flamegraph | ai-data | all (default: all)
  --freq <n>                perf sampling frequency (default: 99)
  --sudo                    run perf/flamegraph with sudo (default: true)
  --no-sudo                 run perf/flamegraph without sudo
  --help                    show this help

Outputs:
  <workspace>/packages/rumoca/target/profile/<timestamp>/
    - flamegraph.svg
    - perf.data
    - perf.script.txt
    - perf.folded.txt
    - run.meta.txt
EOF
}

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
RUMOCA_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"
WORKSPACE_ROOT="$(cd -- "$RUMOCA_DIR/../.." && pwd)"
MODEL_NAME="WindPowerPlants.Components.GenericVariableSpeedGeneratorElectrical"
MODE="all"
FREQ="99"
USE_SUDO="1"
WINDPOWER_ROOT=""
MSL_ROOT=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --workspace-root)
      WORKSPACE_ROOT="${2:?missing value for --workspace-root}"
      shift 2
      ;;
    --model)
      MODEL_NAME="${2:?missing value for --model}"
      shift 2
      ;;
    --windpower-root)
      WINDPOWER_ROOT="${2:?missing value for --windpower-root}"
      shift 2
      ;;
    --msl-root)
      MSL_ROOT="${2:?missing value for --msl-root}"
      shift 2
      ;;
    --mode)
      MODE="${2:?missing value for --mode}"
      shift 2
      ;;
    --freq)
      FREQ="${2:?missing value for --freq}"
      shift 2
      ;;
    --sudo)
      USE_SUDO="1"
      shift
      ;;
    --no-sudo)
      USE_SUDO="0"
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

if [[ "$MODE" != "flamegraph" && "$MODE" != "ai-data" && "$MODE" != "all" ]]; then
  echo "Invalid --mode: $MODE (expected: flamegraph | ai-data | all)" >&2
  exit 2
fi

if [[ -z "$WINDPOWER_ROOT" ]]; then
  WINDPOWER_ROOT="$WORKSPACE_ROOT/.tmp/windpower/WindPowerPlants-2.0.0"
fi
if [[ -z "$MSL_ROOT" ]]; then
  MSL_ROOT="$(find "$WORKSPACE_ROOT/packages/rumoca/target/msl" -maxdepth 1 -type d -name 'ModelicaStandardLibrary-*' | head -n 1 || true)"
fi

if [[ ! -d "$RUMOCA_DIR" ]]; then
  echo "Rumoca directory not found: $RUMOCA_DIR" >&2
  exit 1
fi
if [[ ! -d "$WINDPOWER_ROOT" ]]; then
  echo "WindPower root not found: $WINDPOWER_ROOT" >&2
  exit 1
fi
if [[ -z "$MSL_ROOT" || ! -d "$MSL_ROOT" ]]; then
  echo "MSL root not found/detected: $MSL_ROOT" >&2
  exit 1
fi
if ! command -v perf >/dev/null 2>&1; then
  echo "perf is not available on PATH." >&2
  exit 1
fi
if ! command -v cargo >/dev/null 2>&1; then
  echo "cargo is not available on PATH." >&2
  exit 1
fi

TS="$(date -u +%Y%m%dT%H%M%SZ)"
OUT_DIR="$WORKSPACE_ROOT/packages/rumoca/target/profile/$TS"
mkdir -p "$OUT_DIR"

CHECK_FILE="$WINDPOWER_ROOT/WindPowerPlants/package.mo"
if [[ ! -f "$CHECK_FILE" ]]; then
  echo "Modelica package file not found: $CHECK_FILE" >&2
  exit 1
fi

SUDO_CMD=""
if [[ "$USE_SUDO" == "1" ]]; then
  SUDO_CMD="sudo"
fi

{
  echo "timestamp_utc=$TS"
  echo "workspace_root=$WORKSPACE_ROOT"
  echo "rumoca_dir=$RUMOCA_DIR"
  echo "check_file=$CHECK_FILE"
  echo "model_name=$MODEL_NAME"
  echo "msl_root=$MSL_ROOT"
  echo "windpower_root=$WINDPOWER_ROOT"
  echo "mode=$MODE"
  echo "freq=$FREQ"
  echo "sudo=$USE_SUDO"
} > "$OUT_DIR/run.meta.txt"

echo "Output dir: $OUT_DIR"
echo "Model: $MODEL_NAME"

cd "$RUMOCA_DIR"

echo "Recording perf data..."
$SUDO_CMD perf record -F "$FREQ" -g --call-graph fp -o "$OUT_DIR/perf.data" -- \
  target/release/rumoca check "$CHECK_FILE" \
  --model "$MODEL_NAME" \
  --source-root "$MSL_ROOT" \
  --source-root "$WINDPOWER_ROOT"

echo "Exporting perf script..."
$SUDO_CMD perf script -i "$OUT_DIR/perf.data" --no-inline > "$OUT_DIR/perf.script.txt"

if command -v inferno-collapse-perf >/dev/null 2>&1; then
  echo "Exporting folded stacks via inferno-collapse-perf..."
  inferno-collapse-perf < "$OUT_DIR/perf.script.txt" > "$OUT_DIR/perf.folded.txt"
elif command -v stackcollapse-perf.pl >/dev/null 2>&1; then
  echo "Exporting folded stacks via stackcollapse-perf.pl..."
  stackcollapse-perf.pl "$OUT_DIR/perf.script.txt" > "$OUT_DIR/perf.folded.txt"
else
  echo "No stack collapse tool found. Install inferno: cargo install inferno" >&2
fi

if [[ "$MODE" == "flamegraph" || "$MODE" == "all" ]]; then
  if command -v inferno-flamegraph >/dev/null 2>&1; then
    echo "Generating flamegraph.svg via inferno-flamegraph..."
    inferno-flamegraph < "$OUT_DIR/perf.folded.txt" > "$OUT_DIR/flamegraph.svg"
  elif command -v flamegraph.pl >/dev/null 2>&1; then
    echo "Generating flamegraph.svg via flamegraph.pl..."
    flamegraph.pl "$OUT_DIR/perf.folded.txt" > "$OUT_DIR/flamegraph.svg"
  else
    echo "No flamegraph renderer found. Install inferno: cargo install inferno" >&2
  fi
fi

echo "Done."
echo "Artifacts:"
ls -lh "$OUT_DIR"
