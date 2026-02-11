#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"

args=("$@")

# Convenience: if first arg looks like a session id and no explicit command is
# provided, convert it to `resume <session-id>`.
#
# Examples:
#   ./scripts/codex-klar.sh 019c4dff-e39f-7532-a3db-ed43801d19e4
#   ./scripts/codex-klar.sh --resume 019c4dff-e39f-7532-a3db-ed43801d19e4
if [[ ${#args[@]} -ge 1 && "${args[0]}" == "--last" ]]; then
  args=("resume" "--last" "${args[@]:1}")
elif [[ ${#args[@]} -ge 2 && "${args[0]}" == "--resume" && "${args[1]}" == "--last" ]]; then
  args=("resume" "--last" "${args[@]:2}")
elif [[ ${#args[@]} -ge 2 && "${args[0]}" == "--resume" ]]; then
  args=("resume" "${args[1]}" "${args[@]:2}")
elif [[ ${#args[@]} -ge 1 && "${args[0]}" =~ ^[0-9a-fA-F-]{8,}$ ]]; then
  args=("resume" "${args[0]}" "${args[@]:1}")
fi

codex \
  -C "$repo_root" \
  -a on-request \
  -s workspace-write \
  --add-dir /opt/homebrew/Cellar/zig/0.15.2/lib/zig \
  --add-dir /Users/mrphil/.cache/zig \
  "${args[@]}"
