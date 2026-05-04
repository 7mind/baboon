package io.septimalmind.baboon.parser.model

// A doc comment captured at parse time. The `raw` string contains the
// verbatim source bytes between (and including) the doc-comment delimiters
// -- for prefix docs, the full prefix-doc block; for postfix docs, the
// `//!` marker plus its trailing line content (NLC excluded).
//
// Cleanup of the raw body (delimiter stripping, common-prefix stripping,
// blank-line collapsing) is the typer's responsibility (PR-30.3); the
// parser preserves the raw text verbatim so the typer's cleanup is
// deterministic and centralised.
case class RawDocComment(raw: String, pos: InputPointer)

// Doc-comment slot on `RawNodeMeta`.
//
// Both `prefix` and `suffix` are `Option`, not `List`: per the M30 Q3 lock
// (`docs/spec/docstrings.md` §4) two prefix doc blocks back-to-back with
// no intervening declaration is a parser error
// (`ParserIssue.StackedDocComments`), and a postfix `//!` is single-line
// by construction (one `//!` per field-line, never stacked).
//
// Layered design (PR-30.2 ↔ PR-30.3). The parser captures docs uniformly
// at every `withMeta` site -- the capture is mechanically uniform and is
// not aware of which carriers are emission-bearing positions in the typed
// model. Spec §3.2 / §9 list the non-positions where the captured doc has
// no carrier in the typed model: individual enum values, ADT inheritance
// arms (`+ Ref`, `- Ref`, `^ Ref`), namespace openers, foreign per-language
// mapping entries, and plain (non-template) type aliases. The drop at
// those non-positions happens at PR-30.3's typer/translator stage, not at
// the parser. PR-30.3 is responsible for stripping `RawDocs` at non-
// position carriers as it lifts raw nodes into typed `Field` /
// `MethodDef` / `DomainMember.User`.
case class RawDocs(prefix: Option[RawDocComment], suffix: Option[RawDocComment])

object RawDocs {
  val empty: RawDocs = RawDocs(None, None)
}
