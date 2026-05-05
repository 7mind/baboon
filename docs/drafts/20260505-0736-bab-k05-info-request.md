# BAB-K05 — info request to the original reporter

**Context:** The user-supplied upstream defect report 2026-05-05 lists
`[BAB-K05]` ("Generated codec emits a redundant conversion-method call",
Kotlin backend, severity nit). The entry notes "1 site in a mid-sized
schema; exact site not narrowed". Triage of `KtJsonCodecGenerator.scala`
and `KtUEBACodecGenerator.scala` found multiple legitimate narrowing-
conversion sites that look like candidates but aren't bugs (real
`.jsonPrimitive.int.toByte()` / `.toShort()` narrowings from
`Int` to `Byte`/`Short`; `(header.toInt() or 1).toByte()` UEBA chains
where multi-step conversion may or may not be redundant depending on
the prior expression's type). Without knowing which site fired the
compiler warning, a guess could break a different code path.

`PR-31.3` is marked `[!]` blocked in `tasks.md` pending the response
below.

---

## Prompt to send to the reporter

> Hi — I'm looking at the **BAB-K05** entry in your Baboon defect report
> ("Generated codec emits a redundant conversion-method call"). The
> report notes "exact site not narrowed", which is what's blocking the
> fix: the Kotlin codec generators in Baboon have many legitimate
> narrowing-conversion sites (e.g. `.jsonPrimitive.int.toByte()`,
> `(header.toInt() or 1).toByte()`, various `.toInt()`/`.toLong()`/
> `.toShort()` chains across `KtJsonCodecGenerator.scala` and
> `KtUEBACodecGenerator.scala`), and without knowing which one tripped
> the warning, a guess could break a different code path.
>
> Could you provide:
>
> 1. **The compiler-warning text verbatim** — Kotlin's
>    `Redundant call of conversion method.` warnings include a file
>    path, line, column, and the offending expression. The exact line
>    typically looks like
>    `<path>/SomeCodec.kt:NN:CC: warning: Redundant call of conversion method '<method>'.`.
>    The full warning line plus a few lines of surrounding generated
>    source is ideal.
>
> 2. **The Baboon source type that triggered it** — the field's name,
>    its declared type (`i32`/`u8`/`i64`/`f64`/etc.), and the enclosing
>    data type / version. If you can spot which generated
>    `<TypeName>Codec.kt` file produced the warning, that's enough to
>    map back via the .baboon model.
>
> 3. **Whether it's JSON or UEBA codec** — the path under your
>    generated tree (`/json/` vs `/ueba/`, or whichever convention you
>    use) tells us which generator emitted it. Also helpful: was the
>    warning issued by the JSON codec generator
>    (`KtJsonCodecGenerator.scala`) or the UEBA codec generator
>    (`KtUEBACodecGenerator.scala`)?
>
> 4. **(Optional) A minimal `.baboon` reproducer** — if you can isolate
>    the type to a single declaration that reproduces the warning when
>    compiled by `kotlinc`, that's gold; we can lock it in as a
>    regression test. Not required if (1)–(3) pinpoint the site.
>
> The report classifies this as a nit (compiler warning, not an
> error), so we won't block on it indefinitely — but with the above we
> can apply a surgical fix to the right template instead of guessing.

---

## Resume path

When the reporter answers:

1. Map the file:line in their generated Kotlin output back to the
   emitting template in `KtJsonCodecGenerator.scala` /
   `KtUEBACodecGenerator.scala`. The codec generators stamp output as
   `<TypeName>Codec.kt`; cross-reference with the user's .baboon model
   to find the field/type.
2. Drop the redundant conversion in that template specifically. Verify
   no other type that exercises the same template path is broken
   (e.g. if the conversion was guarding a wider→narrower transition,
   removing it might leave a type error for a sibling case).
3. Add a regression: extend `m31-bab-k05/m31_bab_k05.baboon` (or reuse
   an existing fixture if it already triggers the path) with a struct
   matching the reporter's shape; run `kotlinc -Werror` against the
   generated output to lock the fix.
4. Flip `[PR-31.3]` from `[!]` blocked to `[x]` resolved in
   `tasks.md`; add a Completed entry referencing this info-request
   doc.
