#!/usr/bin/env bash
# Test the VSCode extension locally.
#
# Usage:
#   ./tools/test-extension.sh           # Build, package, and install extension
#   ./tools/test-extension.sh --dev     # Launch VSCode in extension development mode
#   ./tools/test-extension.sh --build   # Just build (don't install)
#   ./tools/test-extension.sh --system  # Use system rumoca-lsp (skip cargo build)
#
# This script:
#   1. Builds rumoca-lsp in release mode
#   2. Compiles the VSCode extension TypeScript
#   3. Packages as .vsix
#   4. Installs the extension in VSCode

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
VSCODE_DIR="$ROOT_DIR/editors/vscode"

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
DEV_MODE=""
BUILD_ONLY=""
USE_SYSTEM=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --dev|-d)
            DEV_MODE="1"
            shift
            ;;
        --build|-b)
            BUILD_ONLY="1"
            shift
            ;;
        --system|-s)
            USE_SYSTEM="1"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Test the VSCode extension locally."
            echo ""
            echo "Options:"
            echo "  --dev, -d      Launch VSCode in extension development mode"
            echo "  --build, -b    Just build (don't install)"
            echo "  --system, -s   Use system rumoca-lsp (skip cargo build)"
            echo "  --help, -h     Show this help"
            echo ""
            echo "Examples:"
            echo "  $0              # Build, package, and install extension"
            echo "  $0 --dev        # Launch VSCode dev mode (F5 debugging)"
            echo "  $0 --build      # Just build .vsix without installing"
            echo "  $0 --system     # Skip LSP build, use installed version"
            echo ""
            echo "After installing, configure VSCode to use your local build:"
            echo "  1. Set rumoca.useSystemServer: true"
            echo "  2. Or set rumoca.serverPath to: $ROOT_DIR/target/release/rumoca-lsp"
            exit 0
            ;;
        *)
            error "Unknown argument: $1"
            exit 1
            ;;
    esac
done

cd "$ROOT_DIR"

# Step 1: Build rumoca-lsp
if [[ -z "$USE_SYSTEM" ]]; then
    step "Building rumoca-lsp (release)..."
    cargo build --release --bin rumoca-lsp
    info "Built: $ROOT_DIR/target/release/rumoca-lsp"

    # Copy binary to extension's bin directory so it gets bundled
    step "Copying rumoca-lsp to extension bin directory..."
    mkdir -p "$VSCODE_DIR/bin"
    cp "$ROOT_DIR/target/release/rumoca-lsp" "$VSCODE_DIR/bin/"
    info "Copied to: $VSCODE_DIR/bin/rumoca-lsp"
else
    info "Skipping cargo build (using system rumoca-lsp)"
fi

# Step 2: Install npm dependencies
step "Installing npm dependencies..."
cd "$VSCODE_DIR"
if [[ ! -d "node_modules" ]]; then
    npm install
else
    info "node_modules exists, skipping npm install"
fi

# Step 3: Compile TypeScript
step "Compiling TypeScript..."
npm run esbuild

# Development mode - launch VSCode
if [[ -n "$DEV_MODE" ]]; then
    step "Launching VSCode in extension development mode..."
    echo ""
    info "VSCode will open with the extension loaded."
    info "Press F5 in VSCode to start debugging, or Ctrl+Shift+P -> 'Developer: Reload Window'"
    echo ""
    info "To use your local rumoca-lsp build, add to .vscode/settings.json:"
    echo "  {"
    echo "    \"rumoca.serverPath\": \"$ROOT_DIR/target/release/rumoca-lsp\""
    echo "  }"
    echo ""
    code --extensionDevelopmentPath="$VSCODE_DIR" .
    exit 0
fi

# Step 4: Package extension
step "Packaging extension..."
npm run package
VSIX_FILE=$(ls -t rumoca-modelica-*.vsix 2>/dev/null | head -1)

if [[ -z "$VSIX_FILE" ]]; then
    error "Failed to create .vsix file"
    exit 1
fi

info "Created: $VSCODE_DIR/$VSIX_FILE"

# Build only mode - stop here
if [[ -n "$BUILD_ONLY" ]]; then
    echo ""
    info "Build complete. To install manually:"
    echo "  code --install-extension $VSCODE_DIR/$VSIX_FILE"
    exit 0
fi

# Step 5: Install extension
step "Installing extension in VSCode..."
code --install-extension "$VSIX_FILE" --force

echo ""
info "Extension installed successfully with bundled rumoca-lsp!"
info "Reload VSCode window (Ctrl+Shift+P -> 'Developer: Reload Window') to activate."
