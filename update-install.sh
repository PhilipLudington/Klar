#!/bin/bash
# Build and update the system-wide Klar installation

set -e

BUILT_BINARY="./zig-out/bin/klar"
INSTALL_BIN="/usr/local/bin/klar"
INSTALL_LIB="/usr/local/lib/klar"

echo "Building Klar..."
./run-build.sh

echo "Updating $INSTALL_BIN..."
sudo cp "$BUILT_BINARY" "$INSTALL_BIN"

echo "Installing stdlib to $INSTALL_LIB/stdlib/..."
sudo mkdir -p "$INSTALL_LIB/stdlib"
sudo rsync -a --delete stdlib/ "$INSTALL_LIB/stdlib/"

echo "Done. Installed version:"
klar --version 2>/dev/null || echo "(version check not available)"
