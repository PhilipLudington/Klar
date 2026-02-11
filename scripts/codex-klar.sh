#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"

codex \
  -C "$repo_root" \
  -a on-request \
  -s workspace-write \
  --add-dir /opt/homebrew/Cellar/zig/0.15.2/lib/zig \
  --add-dir /Users/mrphil/.cache/zig \
  "$@"
