# Bug report → prompts agent: autonomous flow files "fix-vs-wontfix" disposition questions and stops instead of fixing everything

**Reporter context:** user ran `/advance` (the command-of-commands that chains
`/investigate:advance` → `/plan:advance` → `/implement:advance` to quiescence).
**Severity:** high — causes premature, unwanted human-in-the-loop stops on work
the user expects done autonomously.

## TL;DR
The flow stopped a run as **MIXED** and parked 7 confirmed/known defects on a
single **"fix now vs wontfix?" disposition question** to the user. The user's
standing intent is: **"fix everything properly" is always the default — never
ask whether to fix a confirmed defect, and never stop because a fix changes a
versioned/external API or has wide blast radius.** The prompts currently *permit*
(and arguably *invite*) the disposition-question stop. They must forbid it.

## Observed behaviour (this run)
- A `/advance` run completed the target feature, but along the way found 7
  pre-existing / out-of-scope codegen defects (D1–D7), all with documented root
  causes.
- Instead of investigating → planning → implementing fixes for them, the run
  filed ONE `questions` item ("for each of D1–D7, fix now or wontfix?") and ended
  **MIXED**, requiring the user to answer before any fix proceeds.
- The orchestrator justified this as the *only sanctioned* pause
  (BLOCKED-ON-QUESTIONS), citing each defect's "candidate for separate fix or
  wontfix" reviewer note and a prior in-ledger precedent where an analogous
  defect (D8) had been put to the user as a question (and answered "fix now").

## Expected behaviour
- **Default disposition for any open/confirmed defect = FIX, properly, now.**
- Confirmed defects flow investigate → plan → implement **autonomously**. No
  "should I fix this?" gate.
- The run stops for the user **only** when an answer would change *what to build*
  or *how the system must behave* (a genuine requirements ambiguity), or when the
  work is literally *impossible to proceed on* without the user (missing repro,
  missing credentials/access). It must **never** stop on:
  - fix-vs-wontfix / whether-to-fix,
  - "this is out of scope / pre-existing,"
  - "this changes a versioned/external/public API" or "wide blast radius,"
  - magnitude / proportion / cost.

## Root cause — the exact prompt loopholes

1. **`investigate/advance.md` step 6 is too broad.** It says: *"NEEDS user input
   → file an open question and STOP"* for *"a decision only the user can make
   (ambiguous repro, missing access, **a decision only the user can make**…)."*
   "A decision only the user can make" is the loophole that was read to include
   "fix vs wontfix" and "this changes external API." This is the primary leak.

2. **`advance.md` forbids the *AskUserQuestion* form of a scope/blast-radius
   confirmation but not the *filed-question* form.** Its §Stop condition says a
   "scope / scale / blast-radius / is-this-OK confirmation is FORBIDDEN" and the
   "ONLY legitimate user-facing pause is BLOCKED-ON-QUESTIONS (an `open`
   `questions` item)." A sub-flow (or the orchestrator interpreting it) can
   therefore manufacture a disposition `questions` item and relabel the same
   forbidden confirmation as a "legitimate" BLOCKED-ON-QUESTIONS stop. The rule
   bans the *channel* (AskUserQuestion) but not the *content* (a
   disposition/confirmation question), so it is trivially routed around.

3. **No prompt states the default disposition is FIX.** Nothing says "an open or
   confirmed defect is fixed by default." Worse, the defect-filing guidance
   actively encourages the opposite framing: reviewers file defects as
   *"candidate for a separate fix or wontfix"* (see implement reviewer T42 /
   K13), which presents `wontfix` as a co-equal, flow-solicited option and seeds
   exactly the disposition question.

4. **The `wontfix` terminal status reads as a flow-solicitable outcome.** Across
   the defect lifecycle (`open → wip → {root-caused | inconclusive} → resolved |
   wontfix`), nothing marks `wontfix` as *user-initiated only*. The flow treats
   it as a disposition it may ask about.

## Proposed prompt changes (for the prompts agent)

**A. `advance.md` — add a standing default + close the filed-question loophole.**
- Add near §Stop condition: *"Default disposition for every `open`/`wip`/
  `root-caused`/`inconclusive` defect is FIX. The flow NEVER asks whether to fix
  a confirmed/known defect; it drives investigate → plan → implement. `wontfix`
  is a user-INITIATED decision only — the flow never solicits it."*
- Strengthen the no-pause rule: *"A `questions` item is a legitimate stop ONLY if
  its answer changes WHAT to build or HOW the system must behave (a genuine
  requirements ambiguity), or unblocks otherwise-impossible work (missing repro /
  access / credentials). A question that asks fix-vs-wontfix, whether-to-fix,
  scope, out-of-scope/pre-existing handling, or external-API/blast-radius
  disposition is the SAME forbidden confirmation as AskUserQuestion — do not file
  it; CONTINUE and fix."* (i.e., the ban must be on the *content*, not just the
  AskUserQuestion *channel*.)

**B. `investigate/advance.md` step 6 — narrow "a decision only the user can
make."** Replace the open-ended phrasing with explicit inclusions/exclusions:
- *Legitimate* step-6 questions: ambiguous/contradictory requirements that change
  the target behaviour; missing reproduction that cannot be produced from the
  repo; missing external access/credentials.
- *NOT* step-6 questions (CONTINUE instead): fix-vs-wontfix; "out of scope /
  pre-existing"; "changes a versioned/external/public API"; blast radius;
  magnitude. Add: *"A confirmed root cause is ALWAYS file-and-deferred to a fix;
  never park a confirmed defect on a disposition question."*

**C. Defect-filing guidance (implement reviewer T42 / K13, and anywhere defects
are created).** Drop the *"candidate for a separate fix or wontfix"* framing. File
out-of-scope/pre-existing findings as *"to be fixed in a separate task"* — a fix
intent, never a disposition the flow will later solicit.

**D. Defect lifecycle note.** Annotate `wontfix` in the status enum docs as
*user-initiated only; the autonomous flow never transitions a defect to `wontfix`
and never asks for it.*

## Interaction with the user's global `~/.claude/CLAUDE.md` (note for the agent)
The user's global guidelines bias toward caution: *"bias toward caution over
speed,"* *"Ask questions when instructions are unclear,"* and (harness) *"for
actions that are hard to reverse or outward-facing, confirm first."* These
**reinforced** the wrong stop here (a generated-API change is "outward-facing /
hard to reverse"). The autonomous-flow prompts should **explicitly override** the
cautious-confirm defaults *in the `/advance` context*: running `/advance` is
itself the authorization to fix everything without confirmation. Recommend adding
to `advance.md`: *"Running `/advance` overrides any 'confirm hard-to-reverse /
outward-facing changes' or 'ask when unclear' default for the disposition of
confirmed defects — proceed and fix."* (Alternatively the user could scope their
global "confirm outward-facing" rule to exclude flow-driven autonomous runs.)

## Acceptance for the fix (how to know the loophole is closed)
Given the same situation (N confirmed/known defects, each an external-API-changing
codegen fix, each previously tagged "candidate for fix or wontfix"), a fresh
`/advance` run must **fix all N** (investigate → plan → implement) and reach
**DRAINED**, filing **zero** disposition questions — stopping only if a defect
genuinely cannot be reproduced/proceeded-on from the repo, or a requirement is
ambiguous about *what the behaviour should be*.
