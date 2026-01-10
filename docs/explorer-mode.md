# Explorer Mode

Baboon's interactive explorer provides a REPL-style shell for browsing and debugging domain models. It's useful for understanding type structures, testing UEBA encoding/decoding, and exploring evolution history.

## Launching Explorer Mode

```bash
baboon --model-dir ./src/models :explore
```

You can specify multiple model directories or individual files:

```bash
baboon --model-dir ./schemas --model ./extra/types.baboon :explore
```

## Quick Start

Run explorer on the test models:

```bash
# Build the compiler first
mdl :build

# Launch explorer
baboon --model-dir baboon-compiler/src/test/resources/baboon :explore
```

On startup, the explorer:
1. Displays the number of loaded domains
2. Auto-selects the domain if only one exists
3. Shows a list of available domains if multiple exist

The prompt shows current context:
```
baboon acme.checkout v:2.0.0 #
```

## Commands Reference

### Navigation

| Command | Description |
|---------|-------------|
| `domains` | List all loaded domain packages |
| `versions` | List versions in the current domain |
| `switch <domain> [version]` | Switch to a domain and optionally a specific version |

Example:
```
baboon (no domain) # domains
  acme.checkout (3 version(s))
  acme.billing (2 version(s))

baboon (no domain) # switch acme.checkout 2.0.0
Switched to acme.checkout v:2.0.0

baboon acme.checkout v:2.0.0 #
```

### Type Exploration

| Command | Description |
|---------|-------------|
| `types [-a] [-r] [filter]` | List types. `-a` searches all domains/versions, `-r` enables regex filtering |
| `show <type>` | Print type structure in baboon syntax |

Example:
```
baboon acme.checkout v:2.0.0 # types Order
Types matching 'Order':
  data Order
  data OrderLine
  adt OrderEvent

baboon acme.checkout v:2.0.0 # types -r .*Event$
Types matching '.*Event$':
  adt OrderEvent
  adt PaymentEvent

baboon acme.checkout v:2.0.0 # show Order
root data Order: derived[json], derived[ueba] {
  id: uid
  lines: lst[OrderLine]
  total: f64
}
```

### Codec Testing

| Command | Description |
|---------|-------------|
| `example <type>` | Generate a random instance with JSON and UEBA hex output |
| `decode <type> <ueba-hex>` | Decode UEBA hex back to JSON with byte offset map |

Example:
```
baboon acme.checkout v:2.0.0 # example OrderLine
JSON:
{
  "sku": "abc123",
  "qty": 42
}

UEBA (hex):
0a06616263313233002a

baboon acme.checkout v:2.0.0 # decode OrderLine 0a06616263313233002a
Decoded JSON:
{
  "sku": "abc123",
  "qty": 42
}

Byte offset map:
[0x00-0x07] sku: "abc123"
[0x08-0x09] qty: 42
```

### Dependency Analysis

| Command | Description |
|---------|-------------|
| `deps <type>` | Show types that depend on this type (reverse dependencies) |
| `depsof <type>` | Show types this type depends on (forward dependencies) |
| `deptree <type>` | Show full dependency tree |

Example:
```
baboon acme.checkout v:2.0.0 # depsof Order
Types that Order depends on:
  OrderLine
  uid (builtin)

baboon acme.checkout v:2.0.0 # deps OrderLine
Types that depend on OrderLine:
  Order
  OrderDraft
```

### Evolution Analysis

| Command | Description |
|---------|-------------|
| `evo [-v] <type>` | Show evolution history for a type. `-v` enables verbose output |

Example:
```
baboon acme.checkout v:2.0.0 # evo PaymentMethod
Evolution history for PaymentMethod:
  1.0.0 → 2.0.0: Added branch BankTransfer (auto-convertible)

baboon acme.checkout v:2.0.0 # evo -v Order
Evolution history for Order:
  1.0.0:
    data Order { id: uid, items: lst[Item] }

  2.0.0:
    data Order { id: uid, lines: lst[OrderLine], total: f64 }

  Changes 1.0.0 → 2.0.0:
    - Renamed field: items → lines
    - Renamed type: Item → OrderLine
    - Added field: total (requires manual conversion)
```

### Other Commands

| Command | Description |
|---------|-------------|
| `help` | Show available commands |
| `exit` / `quit` | Exit the explorer |

## Tab Completion

The explorer supports tab completion for:
- Command names
- Type names (based on current domain/version context)
- Command flags (`-a`, `-r`, `-v`)

Press Tab to auto-complete or display available options.

## Use Cases

### Debugging UEBA Codecs

When debugging serialization issues, use `example` to generate test data and `decode` to verify round-trip encoding:

```
baboon acme v:1.0.0 # example ComplexType
# Copy the UEBA hex from output

baboon acme v:1.0.0 # decode ComplexType <paste-hex-here>
# Verify the decoded JSON matches
```

### Understanding Type Dependencies

Before refactoring a type, check what depends on it:

```
baboon acme v:1.0.0 # deps UserId
# Shows all types that would be affected by changes to UserId
```

### Planning Schema Evolution

Use `evo` to understand how types changed across versions and what conversions are needed:

```
baboon acme v:3.0.0 # evo -v Customer
# Shows full evolution history with detailed changes
```
