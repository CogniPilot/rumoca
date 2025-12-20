#!/usr/bin/env bash
# Build and install the rumoca Python package via maturin.
#
# Usage:
#   ./tools/build-python.sh           # Development install (editable)
#   ./tools/build-python.sh --release # Release build and install wheel
#
# Requirements:
#   - maturin (pip install maturin)
#   - Rust toolchain

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
PYTHON_DIR="$ROOT_DIR/bindings/python"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

info() { echo -e "${GREEN}[INFO]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }
step() { echo -e "${BLUE}[STEP]${NC} $1"; }

# Parse arguments
RELEASE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --release)
            RELEASE="1"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [--release]"
            echo ""
            echo "Build and install the rumoca Python package."
            echo ""
            echo "Options:"
            echo "  --release   Build optimized release wheel (default: development install)"
            echo ""
            echo "Examples:"
            echo "  $0             # Fast development install"
            echo "  $0 --release   # Optimized release build"
            exit 0
            ;;
        *)
            error "Unknown argument: $1"
            ;;
    esac
done

# Check for maturin
if ! command -v maturin &> /dev/null; then
    error "maturin not found. Install with: pip install maturin"
fi

cd "$PYTHON_DIR"

if [[ -n "$RELEASE" ]]; then
    step "Building release wheel..."
    maturin build --release

    step "Installing wheel..."
    WHEEL=$(ls -t "$ROOT_DIR/target/wheels/"*.whl 2>/dev/null | head -1)
    if [[ -z "$WHEEL" ]]; then
        error "No wheel found in target/wheels/"
    fi
    pip install --force-reinstall "$WHEEL"

    info "Installed release wheel: $(basename "$WHEEL")"
else
    step "Building and installing in development mode..."
    maturin develop

    info "Development install complete"
fi

# Show installed version
echo ""
info "Installed rumoca version:"
python -c "import rumoca; print(f'  {rumoca.__version__}')" 2>/dev/null || warn "Could not verify installation"
