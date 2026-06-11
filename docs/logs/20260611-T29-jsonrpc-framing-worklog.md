# T29 — JsonRpcTransport byte-accurate LSP framing — worklog

## Defect (hypothesis, then confirmed)

`baboon-compiler/.jvm/src/main/scala/io/septimalmind/baboon/lsp/protocol/JsonRpcTransport.scala`
wrapped the `InputStream` in a UTF-8 `BufferedReader` (`:14`) and `readMessage`
allocated `new Array[Char](contentLength)` (`:39`), reading `contentLength`
**chars**. The LSP spec (and `writeMessage:54-57`) defines `Content-Length` as
the **byte** length of the UTF-8 body. For any multi-byte char the char count is
less than the byte count, so the reader stopped short, left trailing body bytes
in the buffer, and corrupted the framing of the next message. `parse(...).toOption`
+ `Try{...}.toOption.flatten` swallowed the cause, and the message loop treats
`None` as a disconnect — a silent exit 0.

## Environment note

sbt-git (jgit) throws `NoWorkTreeException: Bare Repository` when `sbt` runs in a
linked git worktree (`.git` is a file). Per CLAUDE.md, verification ran in a real
clone: `git clone --shared <repo>/.git /tmp/baboon-t29-clone` at `ab5ed902`, with
the three changed files copied in. The committed result lives on branch
`implement/T29` in the worktree.

## Fail-first RED (against the pre-fix, char-based transport)

The BLogger ctor param was added FIRST (so the new test compiles), but the
char-based `readMessage` body was left unchanged, then:

    sbt "baboonJVM/testOnly io.septimalmind.baboon.lsp.protocol.JsonRpcTransportTest"

Result (compiled cleanly — failure is for the FRAMING reason, not a compile error):

    [info] JsonRpcTransportTest:
    [info] JsonRpcTransport.readMessage
    [info] - should frame two consecutive messages where the first body carries multi-byte UTF-8 chars *** FAILED ***
    [info]   None was not equal to Some({
    [info]     "jsonrpc" : "2.0",
    [info]     "method" : "note",
    [info]     "params" : { "text" : "em—dash 🚀" }
    [info]   }) (JsonRpcTransportTest.scala:43)
    ...
    [info] - should continue to the next frame after a JSON parse failure *** FAILED ***
    [info]   None was not equal to Some(... "method":"recovered" ...) (JsonRpcTransportTest.scala:93)
    [info] JsonRpcTransport.writeMessage -> readMessage
    [info] - should round-trip multi-byte content *** FAILED ***
    [info]   None was not equal to Some(... "em—dash 🚀" ...) (JsonRpcTransportTest.scala:105)
    [info] Tests: succeeded 5, failed 3, canceled 0, ignored 0, pending 0
    [info] *** 3 TESTS FAILED ***

The core two-consecutive-messages multi-byte case (`:43`) reproduces the framing
defect: the first body holds an em-dash (U+2014, 3 bytes) and a rocket emoji
(U+1F680, 4 bytes); the char-counting read stops short, the second `readMessage`
reads a corrupted header and returns `None`.

## Fix (D15 items 1 + 3)

- Headers read as ASCII lines DIRECTLY off the `InputStream` (a
  `BufferedInputStream`, byte-oriented): accumulate bytes until CRLF (bare LF
  tolerated; a lone CR consumes a following LF if present), parse
  `Content-Length:` as a BYTE count, stop at the empty line. No char `Reader`.
- Body read as EXACTLY `contentLength` bytes (loop on short reads), decoded via
  `new String(bytes, StandardCharsets.UTF_8)`. Body bytes never pass through a
  char Reader.
- Observability via a new `BLogger` ctor param (threaded through `LspLauncher`
  stdio + TCP construction sites). `None` now means ONLY "connection ended":
  clean EOF before any header byte -> `None` quietly; mid-message EOF or missing
  Content-Length -> log cause, `None`; JSON parse failure after a correctly-framed
  read -> log cause and CONTINUE to the next frame (stream is positioned at the
  next header).
- `writeMessage` byte-framing preserved unchanged.

## GREEN (post-fix)

    sbt "+compile" "baboonJVM/testOnly io.septimalmind.baboon.lsp.protocol.JsonRpcTransportTest"

- `+compile` (JVM + Scala.js cross-build): [success] Total time: 57 s.
- Test suite: all 8 cases pass, including the two-consecutive-messages multi-byte
  case, JSON-parse-failure-then-continue, and writeMessage->readMessage multi-byte
  round-trip.

    [info] Tests: succeeded 8, failed 0, canceled 0, ignored 0, pending 0
    [info] All tests passed.
