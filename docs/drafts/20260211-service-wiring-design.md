# Service Wiring Code Generation - Cross-Language Design

## Overview

Service wiring bridges generated Baboon service interfaces to transport layers (HTTP, gRPC, message queues, etc.). The wiring:

1. Accepts a method identifier and encoded payload (JSON string or UEBA bytes)
2. Dispatches to the correct method
3. Decodes the input using the appropriate codec
4. Invokes the service implementation
5. Encodes the output
6. Returns the result

## Architecture

### Two Modes

The wiring operates in one of two modes, determined by the `ServiceResultConfig` pragmas:

**noErrors mode** (`noErrors=true`, C# default): Methods return plain values. Exceptions propagate naturally. The wiring function returns the encoded response directly (`string` for JSON, `byte[]` for UEBA). Transport-level errors (no matching method) throw `BaboonWiringException`.

**errors mode** (`noErrors=false`): Methods return a container type (e.g., `Either<Err, Out>`). The wiring composes operations through a generated `IBaboonServiceRt` typeclass that abstracts over the container. Transport-level errors are values (e.g., `Either<BaboonWiringError, string>`).

### Runtime Types (shared, language-agnostic)

```
BaboonMethodId(serviceName: string, methodName: string)

BaboonWiringError =
  | NoMatchingMethod(method: BaboonMethodId)
  | DecoderFailed(method: BaboonMethodId, exception: Exception)
  | EncoderFailed(method: BaboonMethodId, exception: Exception)
  | CallFailed(method: BaboonMethodId, domainError: object?)

BaboonWiringException(error: BaboonWiringError)  // for noErrors mode
```

### Generated Types (per-domain, only in errors mode)

`IBaboonServiceRt` is generated based on the configured container type. The container's type pattern (e.g., `<$error, $success>`) determines the interface shape. A default implementation is only generated for built-in `Either`.

```
interface IBaboonServiceRt {
    Container<L, R> Pure<L, R>(R value)
    Container<L, R> Fail<L, R>(L error)
    Container<C, B> LeftMap<A, B, C>(Container<A, B> value, Func<A, C> f)
    Container<A, C> FlatMap<A, B, C>(Container<A, B> value, Func<B, Container<A, C>> f)
}
```

Where `Container<X, Y>` is produced by substituting `$error`/`$success` in the configured pattern.

### Generated Wiring (per-service)

Each service gets a static wiring class with `InvokeJson` and `InvokeUeba` methods. The dispatch is a switch/match on `method.methodName`.

## C# Implementation (Reference)

### Files

| File | Type | Purpose |
|------|------|---------|
| `BaboonServiceWiring.cs` | Runtime resource | `BaboonMethodId`, `BaboonWiringError`, `BaboonWiringException` |
| `CSServiceWiringTranslator.scala` | Compiler | Generates wiring per service + `IBaboonServiceRt` per domain |
| `CSTypes.scala` | Compiler | Type references for runtime types |
| `CSDefnTranslator.scala` | Compiler | Integration point - produces `.Wiring` output files |
| `CSBaboonTranslator.scala` | Compiler | Includes runtime file, generates ServiceRt |
| `BaboonModule.scala` | Compiler | DI wiring |

### Key Design Decisions

1. **Wiring class lives in parent namespace**: For service `I1` in `Testpkg.Pkg0`, the interface is `Testpkg.Pkg0.I1.I1` but the wiring class is `Testpkg.Pkg0.I1Wiring`. Reference the interface as `I1.I1` from the wiring.

2. **UEBA variable naming**: Input stream uses `ims`/`br`, output stream uses `oms`/`bw` to avoid redeclaration in the same scope.

3. **Per-method error handling**: Even when the service is in errors mode, individual methods without an `err` type use try-catch + `rt.Pure`/`rt.Fail` instead of `rt.LeftMap`.

4. **Context parameter**: When `ServiceContextConfig` specifies a context, it's threaded through as the first parameter of all wiring methods and forwarded to service calls.

## Scala Implementation Notes

### Type System Advantages

Scala has proper HKTs, so `IBaboonServiceRt` can be a proper typeclass:

```scala
trait BaboonServiceRt[F[_, _]] {
  def pure[L, R](value: R): F[L, R]
  def fail[L, R](error: L): F[L, R]
  def leftMap[A, B, C](value: F[A, B])(f: A => C): F[C, B]
  def flatMap[A, B, C](value: F[A, B])(f: B => F[A, C]): F[A, C]
}
```

This can be a runtime type rather than generated, since Scala supports `F[_, _]` directly.

### noErrors Mode

Methods return values directly. Use `try`/`catch` for error handling, throw `BaboonWiringException`.

### errors Mode

Use the typeclass in implicit scope. The wiring method signature would be:

```scala
def invokeJson[F[_, _]: BaboonServiceRt](
  method: BaboonMethodId, data: String, impl: I1, ctx: BaboonCodecContext
): F[BaboonWiringError, String]
```

### File Structure

- Runtime: `BaboonServiceWiring.scala` - all types including `BaboonServiceRt[F[_, _]]`
- Generated: `{Service}Wiring.scala` per service
- No separate ServiceRt generation needed (HKT support)

## Rust Implementation Notes

### Type System

Rust has generics but not HKTs. Use an enum-based `Result`-like approach:

```rust
pub enum BaboonWiringError {
    NoMatchingMethod(BaboonMethodId),
    DecoderFailed(BaboonMethodId, Box<dyn std::error::Error>),
    EncoderFailed(BaboonMethodId, Box<dyn std::error::Error>),
    CallFailed(BaboonMethodId, Box<dyn std::any::Any>),
}
```

### noErrors Mode

Return `Result<Vec<u8>, BaboonWiringError>` or panic. Given Rust's convention, `Result` is idiomatic even in noErrors mode. Consider using `Result` always.

### errors Mode

Rust doesn't have HKTs. Options:
1. **Always use `Result`**: Since Rust's `Result` is the de facto error handling type, the wiring could always return `Result<T, BaboonWiringError>`. The `?` operator makes composition natural.
2. **Trait-based**: Define a trait similar to C#'s `IBaboonServiceRt` with associated types.

Recommendation: Use `Result<T, BaboonWiringError>` in all cases. Rust users expect `Result`.

### File Structure

- Runtime: `baboon_wiring.rs` module in `baboon_runtime`
- Generated: methods on the service module or a separate `wiring` submodule

## TypeScript Implementation Notes

### Type System

TypeScript lacks HKTs but has union types and discriminated unions.

```typescript
type BaboonWiringError =
  | { tag: 'NoMatchingMethod'; method: BaboonMethodId }
  | { tag: 'DecoderFailed'; method: BaboonMethodId; error: Error }
  | { tag: 'EncoderFailed'; method: BaboonMethodId; error: Error }
  | { tag: 'CallFailed'; method: BaboonMethodId; domainError: unknown }
```

### noErrors Mode

Functions return the encoded value directly. Throw `BaboonWiringError` on transport errors. This is idiomatic for sync TypeScript.

### errors Mode

Options:
1. **Return `{ok: T} | {err: BaboonWiringError}`**: Explicit result type, no exceptions.
2. **Higher-kinded emulation via fp-ts `Either`**: Possible but heavyweight dependency.

Recommendation: Generate a simple discriminated union result type. TypeScript's pattern matching (via `if`/`switch` on `tag`) makes this ergonomic.

### Async Consideration

TypeScript services may be async. The wiring should support `Promise<T>` return types. Consider generating both sync and async variants, or making the wiring always async (since `await syncValue` is a no-op).

### File Structure

- Runtime: `baboon_wiring.ts` with types
- Generated: `{service}_wiring.ts` per service, exported as functions

## Python Implementation Notes

### Type System

Python uses type hints. No HKT support.

```python
@dataclass
class BaboonMethodId:
    service_name: str
    method_name: str

class BaboonWiringError:
    ...  # dataclass variants
```

### Approach

Python naturally uses exceptions. The noErrors mode is the most Pythonic approach. For errors mode, return a `Result`-like type or use the `returns` library pattern.

Recommendation: Support noErrors mode first. Errors mode can use a simple `Either`/`Result` dataclass if pragmas configure it.

### File Structure

- Runtime: `baboon_wiring.py` module
- Generated: `{service}_wiring.py` per service

## Implementation Priority

1. **C#** - Done
2. **Scala** - Natural fit due to HKT support, most complete type system
3. **TypeScript** - Function-based codecs pattern already established, straightforward extension
4. **Rust** - `Result` type makes errors mode natural
5. **Python** - Simplest, noErrors mode only initially

## Testing Strategy

For each language:
1. Define test services with and without error types (reuse `I1` and `I2` from `pkg03.baboon`)
2. Create mock implementations
3. Test JSON round-trip (encode input -> invoke -> decode output)
4. Test UEBA round-trip
5. Test no-matching-method error
6. Test malformed input handling
7. Test service exception propagation
