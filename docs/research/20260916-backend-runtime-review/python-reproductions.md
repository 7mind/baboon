# Python read-only fragment probes

Baseline `69bbb4c48a519d9a4148f370b76bde19af4bac6a`; working directory repository root. No files/import caches written (`-B`). `python3 -B -c 'import sys; print(sys.version); import pydantic; print(pydantic.__version__)'` printed Python `3.12.12` then `ModuleNotFoundError: No module named 'pydantic'`. Consequently these are extracted method-body probes, **not execution of the installed runtime or generated application**.

Exact executed command:

```sh
sed -n '213,300p;435,449p' baboon-compiler/src/main/resources/baboon-runtime/python/baboon_runtime_shared.py | python3 -B -c 'import sys,struct; from io import BytesIO; from typing import Generic,TypeVar; T=TypeVar("T"); exec("from __future__ import annotations\n"+sys.stdin.read());
for n in (127,128):
 try:
  stream=BytesIO(); LEDataOutputStream(stream).write_str("a"*n); print("write_str",n,"ok",len(stream.getvalue()))
 except Exception as e: print("write_str",n,type(e).__name__,str(e))
try: print(Lazy(lambda: 1).is_value_created)
except Exception as e: print("Lazy.is_value_created",type(e).__name__,str(e))
values=[10,20]; print("swap expression",{v for v in range(len(values))},"element expression",{v for v in values}); print("empty optional-to-set",type({}).__name__)'
```

Captured output (exit 0 because exceptions were deliberately reported):

```text
write_str 127 ok 128
write_str 128 error 'b' format requires -128 <= number <= 127
Lazy.is_value_created AttributeError 'Lazy' object has no attribute '_value_ref'
swap expression {0, 1} element expression {10, 20}
empty optional-to-set dict
```

The writer failure is for the expected reason: continuation bit sets the first length byte to 128, then signed `struct.pack("<b", ...)` rejects it. Production method bodies are unchanged; `from __future__ import annotations` avoids requiring unrelated annotation dependencies. Current generated DTO strings call `write_str` at `translator/python/PyUEBACodecGenerator.scala:361`; runtime metadata does at `baboon-runtime/python/baboon_any_opaque.py:128–132`. No generated end-to-end fixture was executed. Suggested next check: actual runtime strings at UTF-8 lengths 0,127,128,16383,16384, then cross-language byte comparison.

The lazy property references an absent `_value_ref`; initialization and `.value` use `_value`. `git grep -n 'is_value_created' -- '*.py' '*.scala'` returned only `baboon-compiler/src/main/resources/baboon-runtime/python/baboon_runtime_shared.py:447`. This establishes no tracked caller in these extensions, not absence of external public clients.

Collection expressions were manually instantiated from `PyConversionTranslator.scala:265,269–277`, compared with element iteration at `:341–352`; **the compiler was not run and exact supported-model reachability remains unconfirmed**. `FieldOp.SwapCollectionType` dispatch to this helper is present at `:208,225`. Expected semantics for an identity-element list→set conversion of `[10,20]` are `{10,20}`, and an absent optional→set should construct an empty set, not a dict. Reproduce using an accepted multi-version model before changing code.

`git grep -n 'decode_from_json_string' -- '*.py' '*.scala'` returned only facade line222. JSON text/value and generated metadata mismatches in the review remain source-based hypotheses, not reproduced defects. No benchmark or performance measurements were run.
