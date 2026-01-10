# LSP Integration

Baboon provides a Language Server Protocol (LSP) implementation for editor integration. The LSP server provides real-time diagnostics, hover information, go-to-definition, completion, and document symbols for `.baboon` files.

## Running the LSP Server

### STDIO Mode (Default)

Standard mode for editor integration:

```bash
baboon --model-dir ./src/models :lsp
```

The server communicates via stdin/stdout using the LSP JSON-RPC protocol.

### TCP Mode

For debugging or testing with external tools:

```bash
baboon --model-dir ./src/models :lsp --port 5000
```

The server listens on the specified port and accepts a single client connection. Output appears on stderr:

```
Baboon LSP server listening on port 5000
Client connected
```

## Verifying the LSP Server

### Quick Verification with Explorer Mode

The simplest way to verify the compiler and model loading works is to use explorer mode, which shares the same compilation pipeline:

```bash
# Build native executable
mdl :build

# Test with the built-in test models
baboon --model-dir baboon-compiler/src/test/resources/baboon :explore
```

If explorer mode successfully loads and displays types, the LSP server will work with the same models.

### Testing TCP Mode

1. Start the server:
   ```bash
   baboon --model-dir baboon-compiler/src/test/resources/baboon :lsp --port 5000
   ```

2. Connect with netcat or a similar tool:
   ```bash
   nc localhost 5000
   ```

3. Send an LSP initialize request (JSON-RPC format):
   ```json
   Content-Length: 123

   {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
   ```

The server should respond with its capabilities.

## Supported LSP Features

### Diagnostics

The server publishes diagnostics on document open, change, and save. Diagnostics include:
- Parse errors (syntax issues)
- Type errors (undefined types, type mismatches)
- Validation errors (evolution issues, missing roots)

### Hover

Hover over type references to see:
- Type definition summary
- Source location
- Field structure for data types

### Go to Definition

Jump to the definition of:
- Type references in field declarations
- Parent types in `+`/`-`/`^` operations
- Types in collection parameters (`lst[T]`, `opt[T]`, etc.)

### Completion

Auto-completion is triggered by:
- `.` - for namespace-qualified types
- `:` - for annotations (`:derived[...]`)
- `[` - for collection type parameters

Completions include:
- Type names from current domain
- Built-in types (`str`, `uid`, `i32`, etc.)
- Keywords and annotations

### Document Symbols

Shows outline of the current document:
- Namespaces
- Data types
- ADTs and their branches
- Enums
- Contracts
- Foreign types

## Editor Integration

### VS Code

Install the [Baboon VS Code extension](https://marketplace.visualstudio.com/items?itemName=SeptimalMind.baboon-vscode).

The extension can be configured via standard VS Code settings (`settings.json`):

```json
{
  "baboon.serverPath": "baboon",
  "baboon.modelDirs": ["${workspaceFolder}/models"],
  "baboon.serverOptions": ["--debug"]
}
```

#### Settings Reference

- `baboon.serverPath`: Path to the `baboon` executable. Defaults to `baboon` (assumes it's in your PATH).
- `baboon.modelDirs`: List of directories containing `.baboon` files. If empty, the workspace root is used.
- `baboon.serverOptions`: Extra arguments to pass to the compiler before the `:lsp` command.
- `baboon.serverArgsOverride`: Replace all arguments passed to the server. If this is set, `baboon.modelDirs` and `baboon.serverOptions` are ignored.

For development, you can point `baboon.serverPath` to your local build:
`"baboon.serverPath": "${workspaceFolder}/baboon-compiler/.jvm/target/graalvm-native-image/baboon"`

### Neovim (nvim-lspconfig)

Add to your LSP configuration:

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.baboon then
  configs.baboon = {
    default_config = {
      cmd = { 'baboon', '--model-dir', '.', ':lsp' },
      filetypes = { 'baboon' },
      root_dir = lspconfig.util.root_pattern('.git', 'model'),
    },
  }
end

lspconfig.baboon.setup({})
```

### Emacs (lsp-mode)

```elisp
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(baboon-mode . "baboon"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("baboon" "--model-dir" "." ":lsp"))
    :major-modes '(baboon-mode)
    :server-id 'baboon-lsp)))
```

## Workspace Configuration

The LSP server uses workspace folders to discover `.baboon` files. When initialized, it:

1. Scans all workspace folders for `.baboon` files
2. Compiles the domain model
3. Publishes initial diagnostics

On document changes, it recompiles and updates diagnostics incrementally.

## Troubleshooting

### Server doesn't start

1. Verify the native executable is built: `mdl :build`
2. Check that `baboon` is in your PATH or use an absolute path
3. Test with explorer mode first to verify model loading works

### No diagnostics appearing

1. Ensure the file has `.baboon` extension
2. Check that `--model-dir` includes the file's directory
3. Verify the file is in an open workspace folder

### TCP mode connection issues

1. Check no firewall blocks the port
2. Ensure no other process uses the same port
3. The server accepts only one client; restart for new connections

## Architecture

The LSP server uses the following components:

- `BaboonLanguageServer` - Main LSP protocol handler
- `BaboonTextDocumentService` - Handles document operations (open, change, save)
- `BaboonWorkspaceService` - Handles workspace operations
- `WorkspaceState` - Maintains compilation state and model cache
- `DocumentState` - Tracks open document contents

Feature providers:
- `DiagnosticsProvider` - Converts compilation issues to LSP diagnostics
- `HoverProvider` - Generates hover information
- `DefinitionProvider` - Resolves go-to-definition locations
- `CompletionProvider` - Generates completion items
- `DocumentSymbolProvider` - Extracts document outline
