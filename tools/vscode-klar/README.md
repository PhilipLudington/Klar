# Klar VS Code Extension

This extension provides:

- Syntax highlighting for `.kl` files
- LSP features via `klar lsp` (diagnostics, completion, hover, definition)

## Development

1. `cd tools/vscode-klar`
2. `npm install`
3. `npm run compile`
4. Press `F5` in VS Code to launch an Extension Development Host

## Configuration

- `klar.lsp.path`: path to the `klar` executable (default: `klar`)
- `klar.lsp.args`: extra args appended after `klar lsp`
