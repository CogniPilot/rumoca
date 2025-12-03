#!/bin/bash
# Build and install the rumoca Python package for local development.
#
# Usage:
#   ./tools/python-dev.sh           # Build and install in development mode
#   ./tools/python-dev.sh --release # Build with release optimizations
#   ./tools/python-dev.sh --test    # Build and run tests
#   ./tools/python-dev.sh --clean   # Clean build artifacts

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
PYTHON_DIR="$ROOT_DIR/python"

cd "$ROOT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Check for maturin
check_maturin() {
    if ! command -v maturin &> /dev/null; then
        warn "maturin not found. Installing..."
        pip install maturin
    fi
}

# Clean build artifacts
clean() {
    info "Cleaning build artifacts..."
    rm -rf "$PYTHON_DIR/dist"
    rm -rf "$PYTHON_DIR/build"
    rm -rf "$PYTHON_DIR"/*.egg-info
    rm -rf "$PYTHON_DIR/rumoca/*.so"
    rm -rf "$PYTHON_DIR/rumoca/*.pyd"
    rm -rf "$ROOT_DIR/target/wheels"
    info "Clean complete."
}

# Build and install in development mode
build_dev() {
    local release_flag=""
    if [[ "$1" == "--release" ]]; then
        release_flag="--release"
        info "Building in release mode..."
    else
        info "Building in debug mode..."
    fi

    check_maturin
    cd "$PYTHON_DIR"
    maturin develop $release_flag

    info "Development build complete!"
    echo ""
    info "Testing import..."
    python -c "import rumoca; print(f'  Native bindings: {rumoca.NATIVE_AVAILABLE}')"
}

# Build wheels
build_wheels() {
    info "Building wheels..."
    check_maturin
    cd "$PYTHON_DIR"
    maturin build --release

    info "Wheels built in: $PYTHON_DIR/target/wheels/"
    ls -la "$ROOT_DIR/target/wheels/"
}

# Run tests
run_tests() {
    info "Running tests..."
    cd "$PYTHON_DIR"

    # Install test dependencies
    pip install pytest pytest-cov -q

    # Run tests
    python -m pytest tests/ -v
}

# Show help
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Build and install the rumoca Python package for local development."
    echo ""
    echo "Options:"
    echo "  (none)      Build and install in development mode (debug)"
    echo "  --release   Build with release optimizations"
    echo "  --test      Build and run tests"
    echo "  --wheels    Build wheel packages"
    echo "  --clean     Clean build artifacts"
    echo "  --help      Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                  # Quick dev build"
    echo "  $0 --release        # Optimized build"
    echo "  $0 --release --test # Optimized build + tests"
}

# Parse arguments
RELEASE=""
RUN_TESTS=""
BUILD_WHEELS=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --release)
            RELEASE="--release"
            shift
            ;;
        --test)
            RUN_TESTS="1"
            shift
            ;;
        --wheels)
            BUILD_WHEELS="1"
            shift
            ;;
        --clean)
            clean
            exit 0
            ;;
        --help|-h)
            show_help
            exit 0
            ;;
        *)
            error "Unknown option: $1"
            ;;
    esac
done

# Execute
if [[ -n "$BUILD_WHEELS" ]]; then
    build_wheels
else
    build_dev "$RELEASE"
fi

if [[ -n "$RUN_TESTS" ]]; then
    run_tests
fi

info "Done!"
