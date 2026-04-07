#!/bin/sh
#
# Hammerspoon
#
# Installs third-party spoons needed by this config.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SPOONS_DIR="$SCRIPT_DIR/../hammerspoon.symlink/Spoons"
URLDISPATCHER_DIR="$SPOONS_DIR/URLDispatcher.spoon"
URLDISPATCHER_ZIP_URL="https://github.com/Hammerspoon/Spoons/raw/master/Spoons/URLDispatcher.spoon.zip"

mkdir -p "$SPOONS_DIR"

if [ -d "$URLDISPATCHER_DIR" ]; then
  echo "  URLDispatcher.spoon already installed."
  exit 0
fi

if ! command -v curl >/dev/null 2>&1; then
  echo "  curl not found, skipping URLDispatcher.spoon install."
  exit 0
fi

if ! command -v unzip >/dev/null 2>&1; then
  echo "  unzip not found, skipping URLDispatcher.spoon install."
  exit 0
fi

TMP_ZIP="$(mktemp "/tmp/URLDispatcher.spoon.XXXXXX.zip")"
trap 'rm -f "$TMP_ZIP"' EXIT INT TERM

echo "  Downloading URLDispatcher.spoon."
curl -fsSL "$URLDISPATCHER_ZIP_URL" -o "$TMP_ZIP"
unzip -oq "$TMP_ZIP" -d "$SPOONS_DIR"

exit 0
