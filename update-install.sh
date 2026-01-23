#!/bin/bash
# Build and update the system-wide Klar installation

set -e

BUILT_BINARY="./zig-out/bin/klar"
INSTALL_PATH="/usr/local/bin/klar"

echo "Building Klar..."
./build.sh

echo "Updating $INSTALL_PATH..."
sudo cp "$BUILT_BINARY" "$INSTALL_PATH"
echo "Done. Installed version:"
klar --version 2>/dev/null || echo "(version check not available)"
