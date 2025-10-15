#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# Detect ROS Python site-packages, generate overlay path, and register
# a Poetry+ROS Jupyter kernel so notebooks can import rclpy, etc.
# ---------------------------------------------------------------------------

set -e

# Skip if we're already inside the Poetry env
if [[ "$VIRTUAL_ENV" == *"/tests/notebooks/python/.venv" ]]; then
  echo "🐍 Poetry environment already active."
else
  echo "⚠️ Not inside Poetry env; continuing anyway."
fi

# ---------------------------------------------------------------------------
# 0. Resolve paths robustly (works regardless of VS Code cwd)
# ---------------------------------------------------------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
VSCODE_DIR="${WORKSPACE_DIR}/.vscode"
POETRY_DIR="${WORKSPACE_DIR}/tests/notebooks/python"
OUTPUT_FILE="${VSCODE_DIR}/ros_path"

echo "📂 Workspace: $WORKSPACE_DIR"
echo "📂 Poetry dir: $POETRY_DIR"

# ---------------------------------------------------------------------------
# 1. Detect ROS distro and Python version
# ---------------------------------------------------------------------------
if [ -z "$ROS_DISTRO" ]; then
  if [ -d /opt/ros ]; then
    ROS_DISTRO=$(ls /opt/ros | head -n 1)
  else
    echo "❌ No /opt/ros directory found. Is ROS installed?"
    exit 1
  fi
fi

PYVER=$(python3 -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')")
ROS_PATH="/opt/ros/${ROS_DISTRO}/lib/python${PYVER}/site-packages"

# Optional: also include local colcon workspace overlay if it exists
LOCAL_WS="${HOME}/ros2_ws/install"
if [ -d "${LOCAL_WS}" ]; then
  LOCAL_PATH=$(find "${LOCAL_WS}" -type d -path "*/lib/python${PYVER}/site-packages" | head -n 1 || true)
else
  LOCAL_PATH=""
fi

mkdir -p "$VSCODE_DIR"
{
  echo "$ROS_PATH"
  if [ -n "$LOCAL_PATH" ]; then
    echo "$LOCAL_PATH"
  fi
} > "$OUTPUT_FILE"

echo "✅ Detected ROS distro: $ROS_DISTRO"
echo "✅ ROS Python path:     $ROS_PATH"
[ -n "$LOCAL_PATH" ] && echo "✅ Local overlay path:  $LOCAL_PATH"
echo "✅ Wrote overlay path to $OUTPUT_FILE"

# ---------------------------------------------------------------------------
# 2. Register Poetry+ROS Jupyter kernel
# ---------------------------------------------------------------------------
if command -v poetry >/dev/null 2>&1; then
  if [ -d "$POETRY_DIR" ]; then
    echo "🔧 Registering Poetry+ROS Jupyter kernel..."
    (
      cd "$POETRY_DIR"
      source "/opt/ros/${ROS_DISTRO}/setup.bash"

      # Ensure ipykernel is installed inside Poetry env
      if ! poetry run python -c "import ipykernel" >/dev/null 2>&1; then
        echo "📦 Installing ipykernel inside Poetry env..."
        poetry run pip install ipykernel
      fi

      poetry run python -m ipykernel install \
        --user \
        --name="rumoca" \
        --display-name "Python (rumoca)"
    )
    echo "✅ Registered Jupyter kernel: Python (rumoca)"
  else
    echo "⚠️  Poetry directory not found: $POETRY_DIR"
  fi
else
  echo "⚠️  Poetry not found in PATH. Skipping kernel registration."
fi
