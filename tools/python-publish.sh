#!/bin/bash
# Publish the rumoca Python package to PyPI.
#
# Usage:
#   ./tools/python-publish.sh              # Publish to PyPI
#   ./tools/python-publish.sh --test       # Publish to TestPyPI first
#   ./tools/python-publish.sh --dry-run    # Build but don't upload
#
# Prerequisites:
#   - PyPI API token set as PYPI_TOKEN environment variable
#   - Or ~/.pypirc configured with credentials
#   - maturin installed (will be installed if missing)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
PYTHON_DIR="$ROOT_DIR/python"

cd "$ROOT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
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

step() {
    echo -e "${BLUE}[STEP]${NC} $1"
}

# Check for maturin
check_maturin() {
    if ! command -v maturin &> /dev/null; then
        warn "maturin not found. Installing..."
        pip install maturin
    fi
}

# Get current version from pyproject.toml
get_version() {
    grep -E '^version = ' "$PYTHON_DIR/pyproject.toml" | head -1 | sed 's/version = "\(.*\)"/\1/'
}

# Check if version already exists on PyPI
check_version_exists() {
    local version="$1"
    local package="rumoca"
    local url="https://pypi.org/pypi/$package/$version/json"

    if curl -s -o /dev/null -w "%{http_code}" "$url" | grep -q "200"; then
        return 0  # exists
    else
        return 1  # doesn't exist
    fi
}

# Build wheels for current platform
build_local() {
    step "Building wheels for current platform..."
    check_maturin
    cd "$PYTHON_DIR"
    maturin build --release
    info "Wheels built in: $ROOT_DIR/target/wheels/"
}

# Publish to PyPI
publish_pypi() {
    local repository="$1"
    local repo_url=""
    local repo_name="PyPI"

    if [[ "$repository" == "testpypi" ]]; then
        repo_url="--repository-url https://test.pypi.org/legacy/"
        repo_name="TestPyPI"
    fi

    step "Publishing to $repo_name..."

    cd "$PYTHON_DIR"

    # Check for token
    if [[ -n "$PYPI_TOKEN" ]]; then
        info "Using PYPI_TOKEN environment variable"
        maturin publish --skip-existing $repo_url --username __token__ --password "$PYPI_TOKEN"
    elif [[ -n "$MATURIN_PYPI_TOKEN" ]]; then
        info "Using MATURIN_PYPI_TOKEN environment variable"
        maturin publish --skip-existing $repo_url
    elif [[ -f ~/.pypirc ]]; then
        info "Using ~/.pypirc credentials"
        maturin publish --skip-existing $repo_url
    else
        error "No PyPI credentials found. Set PYPI_TOKEN or configure ~/.pypirc"
    fi
}

# Show help
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Publish the rumoca Python package to PyPI."
    echo ""
    echo "Options:"
    echo "  (none)      Build and publish to PyPI"
    echo "  --test      Publish to TestPyPI instead"
    echo "  --dry-run   Build wheels but don't upload"
    echo "  --help      Show this help message"
    echo ""
    echo "Environment variables:"
    echo "  PYPI_TOKEN           PyPI API token for authentication"
    echo "  MATURIN_PYPI_TOKEN   Alternative token variable (maturin native)"
    echo ""
    echo "Notes:"
    echo "  - This script builds wheels for the current platform only"
    echo "  - For multi-platform wheels, use the GitHub Actions workflow"
    echo "  - The CI workflow at .github/workflows/python.yml builds for:"
    echo "    Linux (x86_64, aarch64), macOS (x86_64, aarch64), Windows (x86_64)"
    echo ""
    echo "Examples:"
    echo "  PYPI_TOKEN=pypi-xxx $0           # Publish to PyPI"
    echo "  PYPI_TOKEN=pypi-xxx $0 --test    # Test on TestPyPI first"
    echo "  $0 --dry-run                      # Just build, don't upload"
}

# Main
DRY_RUN=""
TEST_PYPI=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN="1"
            shift
            ;;
        --test)
            TEST_PYPI="1"
            shift
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

# Get and display version
VERSION=$(get_version)
info "Package version: $VERSION"

# Check if version already exists (only for real PyPI, not TestPyPI)
if [[ -z "$TEST_PYPI" ]] && [[ -z "$DRY_RUN" ]]; then
    if check_version_exists "$VERSION"; then
        warn "Version $VERSION already exists on PyPI!"
        echo ""
        echo "To publish a new version:"
        echo "  1. Update version in python/pyproject.toml"
        echo "  2. Update version in Cargo.toml (should match)"
        echo "  3. Run this script again"
        echo ""
        read -p "Continue anyway? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi
fi

# Build
build_local

# Show what was built
echo ""
info "Built wheels:"
ls -la "$ROOT_DIR/target/wheels/"

if [[ -n "$DRY_RUN" ]]; then
    info "Dry run complete. Wheels built but not uploaded."
    exit 0
fi

# Publish
echo ""
if [[ -n "$TEST_PYPI" ]]; then
    publish_pypi "testpypi"
    echo ""
    info "Published to TestPyPI!"
    info "Install with: pip install -i https://test.pypi.org/simple/ rumoca==$VERSION"
else
    publish_pypi "pypi"
    echo ""
    info "Published to PyPI!"
    info "Install with: pip install rumoca==$VERSION"
fi
