# Service Wiring (Transport Plumbing)

This document describes Baboon's service wiring system — the code generation layer that connects service interfaces to wire formats (JSON, UEBA). It covers the architecture, configuration resolution, generated artifacts, and how to implement wiring for a new backend.

## Overview

Given a Baboon service definition:

```baboon
root service I1 {
    def testCall (
        data in { }
        data out { i00: i32 }
        data err { msg: str }
    )
}
```

The compiler generates three artifacts:

1. **Service interface** — a trait/interface the user implements with business logic
2. **Wiring class** — a static dispatcher that marshals between wire format and domain types
3. **Runtime typeclass** (`IBaboonServiceRt`) — abstracts over the error container shape (only in errors mode)

The wiring class is the "plumbing" that decouples transport from business logic. The user never writes serialization code — they implement a typed interface and plug it into the generated dispatcher.

## Two Operating Modes

### No-Errors Mode (`noErrors = true`)

Methods return bare types. Transport errors surface as exceptions.

```
Service Interface:
    Out testCall(In arg)

Wiring:
    string InvokeJson(method, data, impl, ctx)
        → parse JSON → decode → call impl → encode → return JSON string
        → any failure throws BaboonWiringException
```

### Errors Mode (`noErrors = false`)

Methods return a container type (e.g. `Either<Err, Out>`). Transport errors are values, not exceptions.

```
Service Interface:
    Either<Err, Out> testCall(In arg)

Wiring:
    Either<BaboonWiringError, string> InvokeJson(method, data, impl, rt, ctx)
        → parse JSON → wrap in container via rt.Pure/rt.Fail
        → chain decode/call/encode via rt.FlatMap
        → domain errors mapped via rt.LeftMap
        → returns Either<BaboonWiringError, string>
```

## Configuration Resolution

Two independent resolvers control wiring behavior. Both follow the same priority chain:

```
CLI pragmas (--pragma key=value)  >  Domain pragmas  >  CLI config flags  >  Defaults
```

### Service Result Resolver

Controls error handling strategy.

| Pragma Suffix | CLI Flag | Description |
|---|---|---|
| `no-errors` | `--service-result-no-errors` | `true` = bare returns, `false` = container wrapping |
| `type` | `--service-result-type` | Container type name (e.g. `Baboon.Runtime.Shared.Either`) |
| `pattern` | `--service-result-pattern` | Template with `$error` and `$success` placeholders |
| `hkt` | — | Enable higher-kinded type parameter (Scala) |
| `hkt.name` | — | HKT parameter name (default: `F`) |
| `hkt.signature` | — | HKT bounds (default: `[+_, +_]`) |

Pragma keys are prefixed per language: `cs.service.result.no-errors`, `scala.service.result.type`, etc.

**Return type rendering** (`ResolvedServiceResult.renderReturnType`):

```
if noErrors OR no err type on method:
    return successType                       // e.g. "Out"
else:
    expand pattern with $error and $success  // e.g. "Either<Err, Out>"
    prepend resultType                       // e.g. "Baboon.Runtime.Shared.Either<Err, Out>"
```

### Service Context Resolver

Controls whether methods take a context parameter (e.g. for dependency injection).

| Mode | Effect |
|---|---|
| `none` | No context parameter |
| `abstract` | Context is a type parameter on the service interface: `interface I1<Ctx>` |
| `type` | Context is a concrete type: `void method(MyContext ctx, In arg)` |

Pragma keys: `{lang}.service.context`, `{lang}.service.context.type`, `{lang}.service.context.parameter.name`.

## Container Shape Abstraction

The wiring operates over an abstract container via four operations defined in a typeclass interface (`IBaboonServiceRt`). The interface shape is derived from the `pattern` template by substituting generic type parameters.

### How the Interface Is Generated

The pattern template `<$error, $success>` determines the container's generic signature. The generator substitutes abstract type variable names (L, R, A, B, C) for `$error` and `$success` to produce the interface methods:

```
Pattern: <$error, $success>    →  Container<L, R>
    Pure<L, R>(R value)        →  Container<L, R>
    Fail<L, R>(L error)        →  Container<L, R>
    LeftMap<A, B, C>           →  Container<A, B> → (A → C) → Container<C, B>
    FlatMap<A, B, C>           →  Container<A, B> → (B → Container<A, C>) → Container<A, C>
```

### Three Tested Container Shapes

#### 1. Standard Bifunctor: `Either<$error, $success>`

Error on the left, success on the right. This is the built-in default.

```csharp
interface IBaboonServiceRt {
    Either<L, R> Pure<L, R>(R value);
    Either<L, R> Fail<L, R>(L error);
    Either<C, B> LeftMap<A, B, C>(Either<A, B> value, Func<A, C> f);
    Either<A, C> FlatMap<A, B, C>(Either<A, B> value, Func<B, Either<A, C>> f);
}
```

Service interface: `Either<Err, Out> testCall(In arg)`
Wiring return: `Either<BaboonWiringError, string>`

When using the built-in `Either`, the generator also emits a default `BaboonServiceRtDefault` implementation.

#### 2. Reversed Bifunctor: `Result<$success, $error>`

Success first, error second. Demonstrates that the pattern substitution handles arbitrary parameter ordering.

```csharp
interface IBaboonServiceRt {
    Result<R, L> Pure<L, R>(R value);
    Result<R, L> Fail<L, R>(L error);
    Result<B, C> LeftMap<A, B, C>(Result<B, A> value, Func<A, C> f);
    Result<C, A> FlatMap<A, B, C>(Result<B, A> value, Func<B, Result<C, A>> f);
}
```

Service interface: `Result<Out, Err> testCall(In arg)`
Wiring return: `Result<string, BaboonWiringError>`

#### 3. Single-Parameter Container: `Outcome<$success>`

Only the success type appears in the container. The error type is erased (tracked as `object` at runtime). L/A/C type parameters become phantom — they're valid generic parameters but don't appear in the container type.

```csharp
interface IBaboonServiceRt {
    Outcome<R> Pure<L, R>(R value);
    Outcome<R> Fail<L, R>(L error);
    Outcome<B> LeftMap<A, B, C>(Outcome<B> value, Func<A, C> f);
    Outcome<C> FlatMap<A, B, C>(Outcome<B> value, Func<B, Outcome<C>> f);
}
```

Service interface: `Outcome<Out> testCall(In arg)`
Wiring return: `Outcome<string>`

The user-provided `OutcomeServiceRt` implementation casts errors through `object`:
```csharp
public Outcome<R> Fail<L, R>(L error) {
    return new Outcome<R>.Failure(error!);  // L stored as object
}
public Outcome<B> LeftMap<A, B, C>(Outcome<B> value, Func<A, C> f) {
    // cast error from object to A, apply f, store result as object
    var mapped = f((A)failure.Error);
    return new Outcome<B>.Failure(mapped!);
}
```

## Wiring Pipeline (Errors Mode)

Each service method follows a three-step pipeline composed via the Rt typeclass:

```
Step 1: Decode input
    try:
        input = rt.Pure(codec.Decode(data))
    catch:
        input = rt.Fail(DecoderFailed(method, ex))

Step 2: Call implementation + handle domain errors
    output = rt.FlatMap(input, v => {
        try:
            if method has err type:
                callResult = impl.method(v)             // returns Container<Err, Out>
                return rt.LeftMap(callResult,            // map domain Err → BaboonWiringError
                    err => CallFailed(method, err))
            else:
                return rt.Pure(impl.method(v))           // plain Out → wrap in container
        catch:
            return rt.Fail(CallFailed(method, ex))
    })

Step 3: Encode output
    return rt.FlatMap(output, v => {
        try:
            encoded = codec.Encode(v)
            return rt.Pure(encoded.ToString())
        catch:
            return rt.Fail(EncoderFailed(method, ex))
    })
```

The same pipeline is used for both JSON and UEBA transports. The only difference is the codec calls and the final return type (`string` vs `byte[]`).

## Runtime Error Types

The `BaboonServiceWiring.cs` runtime file defines shared types used by all wiring variants:

```csharp
record BaboonMethodId(string ServiceName, string MethodName);

abstract record BaboonWiringError {
    record NoMatchingMethod(BaboonMethodId Method);
    record DecoderFailed(BaboonMethodId Method, Exception Exception);
    record EncoderFailed(BaboonMethodId Method, Exception Exception);
    record CallFailed(BaboonMethodId Method, object? DomainError);
}

class BaboonWiringException : Exception {   // used only in noErrors mode
    BaboonWiringError Error { get; }
}
```

- `NoMatchingMethod`: The method name in `BaboonMethodId` doesn't match any known method
- `DecoderFailed`: Input wire data couldn't be parsed/decoded
- `EncoderFailed`: Output couldn't be encoded to wire format
- `CallFailed`: The service implementation returned a domain error or threw an exception

In no-errors mode, these are wrapped in `BaboonWiringException` and thrown. In errors mode, they are values inside the container's failure branch.

## Generated File Structure

For a domain with services, the generator produces:

```
{output}/
  {Pkg}/
    I1.cs                    # Service interface (namespace I1 { interface I1 { ... } })
    I1.Wiring.cs             # Static wiring class with InvokeJson/InvokeUeba
    I2.cs                    # Another service interface
    I2.Wiring.cs             # Another wiring class
    BaboonServiceRt.cs       # IBaboonServiceRt interface (errors mode only)
                             # + BaboonServiceRtDefault (if using built-in Either)
```

The wiring file and the Rt interface are only generated when the domain contains at least one service definition.

## Methods Without Error Types

When a service method has no `err` type (like `I2.noErrCall`), its return type is always the bare success type regardless of the errors mode setting. In the wiring, these methods skip the `LeftMap` step and go directly `Pure → FlatMap(call) → FlatMap(encode)`.

```baboon
root service I2 {
    def noErrCall (           // no err type
        data in { value: i32 }
        data out { result: str }
    )
}
```

Service interface (both modes): `Out noErrCall(In arg)`
Wiring (errors mode): still returns `Either<BaboonWiringError, string>` — transport errors can still occur even though the service method itself can't produce domain errors.

## Implementing Wiring for a New Backend

### Required Components

1. **Runtime file** with `BaboonMethodId`, `BaboonWiringError` variants, and `BaboonWiringException`
2. **`$LANGServiceWiringTranslator`** with:
   - `translate(defn)` — generates the wiring class for a service
   - `translateServiceRt(domain)` — generates the `IBaboonServiceRt` interface
3. Integration into `$LANGDefnTranslator` to call the wiring translator for service types

### Generator Structure

The wiring translator is instantiated per-domain with resolved configuration:

```scala
class $LANGServiceWiringTranslator(
    target: $LANGTarget,
    trans: $LANGTypeTranslator,
    codecs: Set[$LANGCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
) {
    private val resolved = ServiceResultResolver.resolve(domain, "$lang", ...)
    private val resolvedCtx = ServiceContextResolver.resolve(domain, "$lang", ...)
    private val hasJsonCodecs = codecs.exists(_.isInstanceOf[$LANGJsonCodecGenerator])
    private val hasUebaCodecs = codecs.exists(_.isInstanceOf[$LANGUEBACodecGenerator])

    // renderContainer: builds "ResultType<error, success>" from pattern + type
    private def renderContainer(error: String, success: String): String

    // Main entry points
    def translate(defn: DomainMember.User): Option[TextTree[$LANGValue]]
    def translateServiceRt(domain: Domain): Option[TextTree[$LANGValue]]
}
```

### Key Implementation Details

**`renderContainer`** performs pattern substitution:
```scala
def renderContainer(error: String, success: String): String = {
    val p = resolved.pattern.get
        .replace("$error", error)
        .replace("$success", success)
    s"${resolved.resultType.get}$p"
}
```

**`translateServiceRt`** builds the typeclass interface by substituting abstract type variables into the pattern:
- `Pure<L, R>` returns `container(L, R)`
- `Fail<L, R>` returns `container(L, R)`
- `LeftMap<A, B, C>` takes `container(A, B)` and `A → C`, returns `container(C, B)`
- `FlatMap<A, B, C>` takes `container(A, B)` and `B → container(A, C)`, returns `container(A, C)`

**Context parameter handling** uses three helper methods:
- `ctxParamDecl` — the parameter declaration string (e.g. `"Ctx ctx, "` or `""`)
- `ctxArgPass` — the argument forwarding string (e.g. `"ctx, "` or `""`)
- `genericParam` — the generic type parameter (e.g. `"<Ctx>"` or `""`)

**TextTree rendering for container types**: When a `TextTree[$LANGValue]` type reference needs to be used inside `renderContainer` (which takes plain strings), use `mapRender` to convert the tree to its rendered string form:
```scala
private def renderFq(tree: TextTree[$LANGValue]): String = tree.mapRender {
    case t: $LANGValue.$LANGType     => (t.module.parts :+ t.name).mkString(".")
    case t: $LANGValue.$LANGTypeName => t.name
}
// Then: renderContainer("BaboonWiringError", renderFq(inRef))
```

### Testing Strategy

Each container shape needs an **overlay test stub** applied on top of the base language stub:

```
test/$lang-stub/                      # Base stub with all types and codecs
test/$lang-stub-either-overlay/       # WiringTests.cs only (uses built-in Either)
test/$lang-stub-result-overlay/       # CustomContainers.cs + WiringTests.cs (reversed Result)
test/$lang-stub-outcome-overlay/      # CustomContainers.cs + WiringTests.cs (single-param Outcome)
```

Each overlay contains:
- **`CustomContainers` file** (for non-built-in shapes): Defines the container type and the `IBaboonServiceRt` implementation
- **`WiringTests` file**: Mock service implementations (success, failure, throwing) and tests covering JSON/UEBA success, domain error, no matching method, decoder failure, service throws, and no-err-type methods

Build script pattern (in `.mdl/defs/tests.md`):
```bash
# Copy base stub
rsync -a --exclude='Generated*' --exclude='bin' --exclude='obj' \
    ./test/$lang-stub/ "$TEST_DIR/$lang-stub/"
# Apply overlay
rsync -a ./test/$lang-stub-result-overlay/ "$TEST_DIR/$lang-stub/"
# Generate with shape-specific flags
$BABOON_BIN ... \
    --service-result-no-errors=false \
    --service-result-type="CustomContainers.Result" \
    --service-result-pattern="<\$success, \$error>"
```

Note: `$error` and `$success` must be escaped as `\$error` and `\$success` in bash scripts to prevent variable expansion.
