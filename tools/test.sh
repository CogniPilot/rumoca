#!/usr/bin/env bash
# Run all tests and checks for rumoca.
#
# Usage:
#   ./tools/test.sh           # Run all checks
#   ./tools/test.sh --quick   # Skip slow tests
#   ./tools/test.sh --fix     # Auto-fix formatting issues
#
# This script runs:
#   1. cargo fmt --check (or --fix to auto-format)
#   2. cargo clippy --all-targets
#   3. cargo test
#   4. rumoca-fmt --check (Modelica formatting)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

info() { echo -e "${GREEN}[INFO]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; }
step() { echo -e "${BLUE}[STEP]${NC} $1"; }

# Parse arguments
QUICK=""
FIX=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --quick|-q)
            QUICK="1"
            shift
            ;;
        --fix|-f)
            FIX="1"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Run all tests and checks for rumoca."
            echo ""
            echo "Options:"
            echo "  --quick, -q   Skip slow tests"
            echo "  --fix, -f     Auto-fix formatting issues"
            echo "  --help, -h    Show this help"
            echo ""
            echo "Examples:"
            echo "  $0             # Run all checks"
            echo "  $0 --quick    # Skip slow tests"
            echo "  $0 --fix      # Auto-fix formatting"
            exit 0
            ;;
        *)
            error "Unknown argument: $1"
            exit 1
            ;;
    esac
done

cd "$ROOT_DIR"

FAILED=0

# Step 1: Rust formatting
step "Checking Rust formatting (cargo fmt)..."
if [[ -n "$FIX" ]]; then
    cargo fmt
    info "Rust code formatted"
else
    if cargo fmt --check; then
        info "Rust formatting OK"
    else
        error "Rust formatting check failed. Run with --fix to auto-format."
        FAILED=1
    fi
fi

# Step 2: Clippy
step "Running clippy..."
if cargo clippy --all-targets -- -D warnings; then
    info "Clippy OK"
else
    error "Clippy found issues"
    FAILED=1
fi

# Step 3: Tests
step "Running tests (cargo test)..."
if [[ -n "$QUICK" ]]; then
    if cargo test --lib; then
        info "Library tests OK"
    else
        error "Tests failed"
        FAILED=1
    fi
else
    if cargo test; then
        info "All tests OK"
    else
        error "Tests failed"
        FAILED=1
    fi
fi

# Step 4: Modelica formatting
step "Checking Modelica formatting (rumoca-fmt)..."
if [[ -n "$FIX" ]]; then
    if cargo run --bin rumoca-fmt --quiet 2>/dev/null; then
        info "Modelica code formatted"
    else
        warn "rumoca-fmt formatting completed (some files may have been modified)"
    fi
else
    if cargo run --bin rumoca-fmt --quiet -- --check 2>/dev/null; then
        info "Modelica formatting OK"
    else
        warn "Modelica formatting check failed (non-blocking)"
    fi
fi

# Summary
echo ""
if [[ $FAILED -eq 0 ]]; then
    info "All checks passed!"
    exit 0
else
    error "Some checks failed"
    exit 1
fi
