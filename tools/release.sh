#!/bin/bash
# Create a new release for rumoca.
#
# Usage:
#   ./tools/release.sh 0.7.4        # Create release v0.7.4
#   ./tools/release.sh 0.7.4 --dry-run  # Show what would happen
#
# This script:
#   1. Updates version in Cargo.toml, pyproject.toml, and package.json
#   2. Commits the version bump
#   3. Creates and pushes a git tag
#   4. The CI then handles publishing to crates.io, PyPI, and VS Code Marketplace

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
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }
step() { echo -e "${BLUE}[STEP]${NC} $1"; }

# Parse arguments
VERSION=""
DRY_RUN=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN="1"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 VERSION [--dry-run]"
            echo ""
            echo "Create a new release for rumoca."
            echo ""
            echo "Arguments:"
            echo "  VERSION     Version number (e.g., 0.7.4)"
            echo "  --dry-run   Show what would happen without making changes"
            echo ""
            echo "Example:"
            echo "  $0 0.7.4"
            exit 0
            ;;
        *)
            if [[ -z "$VERSION" ]]; then
                VERSION="$1"
            else
                error "Unknown argument: $1"
            fi
            shift
            ;;
    esac
done

if [[ -z "$VERSION" ]]; then
    error "Version required. Usage: $0 VERSION [--dry-run]"
fi

# Validate version format
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    error "Invalid version format: $VERSION (expected: X.Y.Z)"
fi

cd "$ROOT_DIR"

# Check for clean working directory
if [[ -n "$(git status --porcelain)" ]]; then
    warn "Working directory has uncommitted changes:"
    git status --short
    echo ""
    read -p "Continue anyway? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Get current versions
CARGO_VERSION=$(grep -E '^version = ' Cargo.toml | head -1 | sed 's/version = "\(.*\)"/\1/')
PYTHON_VERSION=$(grep -E '^version = ' bindings/python/pyproject.toml | head -1 | sed 's/version = "\(.*\)"/\1/')
PYTHON_RUNTIME_VERSION=$(grep -E '^__version__' bindings/python/rumoca/version.py | sed 's/__version__ = "\(.*\)"/\1/')
VSCODE_VERSION=$(grep -E '"version"' editors/vscode/package.json | head -1 | sed 's/.*"version": "\(.*\)".*/\1/')

info "Current versions:"
echo "  Cargo.toml:       $CARGO_VERSION"
echo "  pyproject.toml:   $PYTHON_VERSION"
echo "  version.py:       $PYTHON_RUNTIME_VERSION"
echo "  package.json:     $VSCODE_VERSION"
echo ""
info "New version: $VERSION"
echo ""

if [[ -n "$DRY_RUN" ]]; then
    info "Dry run mode - no changes will be made"
    echo ""
fi

# Update Cargo.toml
step "Updating Cargo.toml..."
if [[ -z "$DRY_RUN" ]]; then
    sed -i "s/^version = \"$CARGO_VERSION\"/version = \"$VERSION\"/" Cargo.toml
fi

# Update pyproject.toml
step "Updating bindings/python/pyproject.toml..."
if [[ -z "$DRY_RUN" ]]; then
    sed -i "s/^version = \"$PYTHON_VERSION\"/version = \"$VERSION\"/" bindings/python/pyproject.toml
fi

# Update version.py (runtime version)
step "Updating bindings/python/rumoca/version.py..."
if [[ -z "$DRY_RUN" ]]; then
    sed -i "s/^__version__ = \"$PYTHON_RUNTIME_VERSION\"/__version__ = \"$VERSION\"/" bindings/python/rumoca/version.py
fi

# Update package.json
step "Updating editors/vscode/package.json..."
if [[ -z "$DRY_RUN" ]]; then
    sed -i "s/\"version\": \"$VSCODE_VERSION\"/\"version\": \"$VERSION\"/" editors/vscode/package.json
fi

# Update Cargo.lock
step "Updating Cargo.lock..."
if [[ -z "$DRY_RUN" ]]; then
    cargo check --quiet 2>/dev/null || true
fi

# Show changes
if [[ -z "$DRY_RUN" ]]; then
    echo ""
    info "Changes made:"
    git diff --stat
    echo ""
fi

# Commit
step "Committing version bump..."
if [[ -z "$DRY_RUN" ]]; then
    git add Cargo.toml Cargo.lock bindings/python/pyproject.toml bindings/python/rumoca/version.py editors/vscode/package.json
    git commit -m "Release v$VERSION"
fi

# Create tag
step "Creating tag v$VERSION..."
if [[ -z "$DRY_RUN" ]]; then
    git tag "v$VERSION"
fi

# Push
echo ""
info "Ready to push. This will trigger the release workflow."
echo ""
echo "Commands to run:"
echo "  git push origin main"
echo "  git push origin v$VERSION"
echo ""

if [[ -z "$DRY_RUN" ]]; then
    read -p "Push now? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        git push origin main
        git push origin "v$VERSION"
        echo ""
        info "Release v$VERSION pushed!"
        info "Monitor the workflow at: https://github.com/cognipilot/rumoca/actions"
    else
        info "Skipped push. Run the commands above when ready."
    fi
else
    info "Dry run complete. No changes were made."
fi
