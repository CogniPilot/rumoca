#!/usr/bin/env bash
set -euo pipefail

REPO="${RUMOCA_INSTALL_REPO:-climamind/rumoca}"
BIN_DIR="${RUMOCA_INSTALL_BIN_DIR:-$HOME/.local/bin}"
VERSION="${RUMOCA_INSTALL_VERSION:-latest}"
WITH_LSP="${RUMOCA_INSTALL_WITH_LSP:-0}"

usage() {
    cat <<'EOF'
Install rumoca from GitHub Releases.

Usage:
  install.sh [--version <vX.Y.Z|X.Y.Z|latest>] [--repo <owner/repo>] [--bin-dir <path>] [--with-lsp]

Environment overrides:
  RUMOCA_INSTALL_REPO
  RUMOCA_INSTALL_BIN_DIR
  RUMOCA_INSTALL_VERSION
  RUMOCA_INSTALL_WITH_LSP (1 to install rumoca-lsp too)
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
    --version)
        if [[ $# -lt 2 ]]; then
            echo "missing value for --version" >&2
            usage
            exit 1
        fi
        VERSION="$2"
        shift 2
        continue
        ;;
    --repo)
        if [[ $# -lt 2 ]]; then
            echo "missing value for --repo" >&2
            usage
            exit 1
        fi
        REPO="$2"
        shift 2
        continue
        ;;
    --bin-dir)
        if [[ $# -lt 2 ]]; then
            echo "missing value for --bin-dir" >&2
            usage
            exit 1
        fi
        BIN_DIR="$2"
        shift 2
        continue
        ;;
    --with-lsp)
        WITH_LSP=1
        shift
        continue
        ;;
    -h | --help)
        usage
        exit 0
        ;;
    *)
        echo "unknown argument: $1" >&2
        usage
        exit 1
        ;;
    esac
done

if [[ -z "$VERSION" || -z "$REPO" || -z "$BIN_DIR" ]]; then
    echo "version/repo/bin-dir must be non-empty" >&2
    exit 1
fi

detect_platform() {
    local os arch
    case "$(uname -s)" in
    Linux)
        os="linux"
        ;;
    Darwin)
        os="macos"
        ;;
    *)
        echo "unsupported OS: $(uname -s). Use install.ps1 on Windows." >&2
        exit 1
        ;;
    esac

    case "$(uname -m)" in
    x86_64 | amd64)
        arch="x86_64"
        ;;
    aarch64 | arm64)
        arch="aarch64"
        ;;
    *)
        echo "unsupported architecture: $(uname -m)" >&2
        exit 1
        ;;
    esac

    printf '%s-%s' "$os" "$arch"
}

resolve_tag() {
    if [[ "$VERSION" == "latest" ]]; then
        local api tag
        api="https://api.github.com/repos/${REPO}/releases/latest"
        tag="$(curl --proto '=https' --tlsv1.2 -fsSL "$api" | sed -nE 's/.*"tag_name"[[:space:]]*:[[:space:]]*"([^"]+)".*/\1/p' | head -n1)"
        if [[ -z "$tag" ]]; then
            echo "failed to resolve latest release tag from ${api}" >&2
            exit 1
        fi
        printf '%s' "$tag"
        return
    fi

    if [[ "$VERSION" == v* ]]; then
        printf '%s' "$VERSION"
    else
        printf 'v%s' "$VERSION"
    fi
}

install_asset() {
    local tag="$1"
    local asset="$2"
    local target="$3"
    local url tmp
    url="https://github.com/${REPO}/releases/download/${tag}/${asset}"
    tmp="$(mktemp)"
    trap 'rm -f "$tmp"' RETURN
    echo "Downloading ${url}"
    curl --proto '=https' --tlsv1.2 -fLsS "$url" -o "$tmp"
    install -m 0755 "$tmp" "$target"
    rm -f "$tmp"
    trap - RETURN
}

platform="$(detect_platform)"
tag="$(resolve_tag)"
rumoca_asset="rumoca-${platform}"
lsp_asset="rumoca-lsp-${platform}"

mkdir -p "$BIN_DIR"
install_asset "$tag" "$rumoca_asset" "$BIN_DIR/rumoca"
if [[ "$WITH_LSP" == "1" ]]; then
    install_asset "$tag" "$lsp_asset" "$BIN_DIR/rumoca-lsp"
fi

echo "Installed rumoca to ${BIN_DIR}/rumoca"
if [[ "$WITH_LSP" == "1" ]]; then
    echo "Installed rumoca-lsp to ${BIN_DIR}/rumoca-lsp"
fi

if [[ ":$PATH:" != *":$BIN_DIR:"* ]]; then
    echo "Add this to your shell profile:"
    echo "  export PATH=\"${BIN_DIR}:\$PATH\""
fi

"$BIN_DIR/rumoca" --version || true
