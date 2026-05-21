# Rumoca Development Instructions (AI + Human)

This document defines the operational workflow for development work in
`packages/rumoca`.

## Mandatory For AI Agents

For AI coding agents, this document is **not optional guidance**. It is a
mandatory execution policy.

- AI agents MUST follow this document for all Rumoca work.
- If any instruction from a user conflicts with this document, the agent must
  pause and explicitly surface the conflict instead of silently bypassing this
  policy.
- AI agents MUST treat the Root-Cause Proof Protocol and MLS/spec cross-checks
  as required preconditions for semantic fixes.

Use this together with:

- `AGENTS.md` for mandatory guardrails
- `spec/README.md` for active spec status
- `spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md` for MLS compliance mapping
- `spec/SPEC_0025_PR_REVIEW_PROCESS.md` for verification and review policy

## Core Rule: MLS + Spec First

For any semantic change, identify the governing Modelica Language Specification
(MLS) section and the governing Rumoca spec section before editing.

- Do not guess semantics from one model behavior.
- Do not merge behavior that cannot be justified by MLS/spec.
- If uncertain, stop and find the normative rule first.

## Upstream-First Fix Policy

When behavior is wrong, push fixes as far upstream in the compiler pipeline as
possible.

Preferred order:

1. Parse / resolve / typecheck / instantiate / flatten / ToDae root cause
2. Compile/session orchestration
3. Solver/runtime or template layer only when the issue is truly runtime-only

Do not add top-layer compatibility workarounds for broken lower-layer
invariants.

## Root-Cause Proof Protocol (Mandatory)

For **every** non-trivial bug (semantic, performance, correctness, or CI), do
not jump to a fix from surface symptoms. Treat first ideas as hypotheses.

Minimum required protocol before finalizing a fix:

1. Reproduce concretely
- Reproduce with one minimal, concrete case and record the exact failing phase.

2. Trace end-to-end
- Follow the value/decision through owning phases and identify the **first**
  step where behavior diverges from expected.

3. Prove expected behavior
- Cross-check against governing spec/MLS (or explicit project contract) and
  state why the observed behavior is wrong.

4. Test competing hypotheses
- List plausible alternatives and rule them out with evidence (logs, IR output,
  targeted tests, or debugger traces).

5. Fix at the earliest responsible layer
- Implement the correction at the first layer that introduced the error.
- Do not keep downstream compensating workarounds unless explicitly justified.

6. Verify and clean up
- Add regression coverage for the root cause.
- Remove temporary probes/debug code and any superseded symptom patches.
- Re-run focused checks for the touched phases.

Quality bar:
- If evidence is incomplete, continue investigation instead of finalizing a
  speculative patch.
- Prefer a slower, proven root-cause fix over a fast symptom fix.

## Enforcement Gates (Mandatory, Blocking)

For any non-trivial semantic/compiler change, AI agents MUST complete all gates
below before finalizing code changes. If any gate is missing, stop and continue
investigation; do not patch.

Gate 1: Normative rule anchor
- Cite the exact MLS and Rumoca spec section(s) that govern the behavior.
- If no governing rule is found, do not implement semantic changes yet.

Gate 2: First divergence proof
- Identify the first phase and transformation where actual behavior diverges
  from expected behavior.
- Include concrete evidence (IR snapshot, trace, debugger step, or targeted
  phase output).

Gate 3: Competing hypotheses elimination
- Provide at least two plausible hypotheses.
- Reject non-root hypotheses with concrete evidence.

Gate 4: Namespace/symbol identity proof
- Do not merge or substitute symbol spellings (including case variants,
  alias-vs-instance names, suffix matches) unless semantic identity is proven
  by compiler-owned data (scope resolution, alias map, symbol table, or
  declaration provenance).
- String similarity is never sufficient proof.

Gate 5: Earliest-owner fix
- Implement the fix in the earliest phase that introduced the divergence.
- If a later-phase fix is used, explicitly justify why earlier-phase ownership
  is infeasible.

Gate 6: Regression + negative control
- Add a positive regression test for the fixed root cause.
- Add a negative/ambiguity test proving disallowed behavior is still rejected.

Gate 7: Cleanup and strictness
- Remove temporary debug instrumentation before finalization.
- Do not weaken validators/checkers to make failing models pass.

## Required Evidence Block In Agent Updates

For non-trivial fixes, agent progress/final updates MUST include this exact
evidence block format:

1. Governing rule(s):
- MLS: ...
- Rumoca spec: ...

2. First divergence:
- Phase:
- Artifact/proof:

3. Rejected hypotheses:
- H1 ... (rejected because ...)
- H2 ... (rejected because ...)

4. Fix ownership:
- Earliest owning phase:
- Why this layer:

5. Verification:
- Positive regression:
- Negative/ambiguity regression:
- Focused runtime/compile check:

## Communication Clarity Rule (Mandatory)

For non-trivial compiler bugs, AI updates must include a plain-language
explanation in addition to technical details.

Required plain-language content:

1. What is broken
- One short sentence in everyday language.

2. Where it breaks
- The exact model + path/symbol + compiler phase.

3. What should happen
- One short sentence describing expected behavior.

4. Why it broke
- One short sentence naming the first bad transformation.

5. What changed
- One short sentence describing the fix and why it is safe.

Do not hide the core issue behind jargon-only explanations.
If technical terms are used, define them briefly.

## Concrete Example Rule (Mandatory)

For any non-trivial fix or bug explanation, include at least one concrete,
real example taken from the actual failure location.

Required example content:

1. Show the real code/snippet
- Use the exact model/code path where the bug was found.
- For Modelica bugs: include the exact Modelica snippet.
- For Rust bugs: include the exact Rust snippet.
- If exact code cannot be shown, include minimal pseudo-code that preserves the
  real structure and names.

2. Show expected vs actual in plain language
- "Actual:" one short line describing what Rumoca produced/did.
- "Expected:" one short line describing what should have happened.

3. Show phase-by-phase mapping
- One short line per relevant phase (for example resolve, instantiate, flatten,
  ToDae) describing what happened to the example in that phase.
- Explicitly identify the first phase where the example becomes wrong.

4. Tie fix directly to that example
- State exactly what changed and why it corrects that specific example.
- Avoid abstract explanations that are not anchored in the concrete example.

This rule is mandatory in addition to the Communication Clarity Rule.

## Symptom-Patch Guard (Mandatory)

Before merging a fix, explicitly answer:

1. "Did we change only a validator/checker fallback?"
- If yes, stop and continue investigation unless Gate 5 is explicitly justified.

2. "Did we identify and patch the first producer of the wrong symbol/value/path?"
- If no, stop and continue investigation.

3. "Can we point to one concrete before/after artifact proving the producer changed?"
- If no, fix is not complete.

For symbol-qualification bugs, validation-layer relaxations are temporary triage
only and must not be final unless an upstream fix is proven infeasible.

## Symbol Identity & Namespace Discipline (Mandatory)

Do not treat two names as equivalent because they "look related" (for example,
case-only differences, alias-vs-instance spellings, or shared suffixes).

Required rules:

1. Prove identity from compiler semantics, not string similarity
- Accept equivalence only when it is established by symbol tables, alias maps,
  lexical scope resolution, or explicit declaration/flattening provenance.

2. Preserve namespace intent
- Keep package/type aliases and instance/component namespaces distinct unless
  MLS/spec and compiler ownership explicitly unify them.

3. If mixed spellings appear, fix the first producer
- Trace where each spelling is introduced and correct the earliest phase that
  creates inconsistent qualification.
- Avoid downstream permissive fallback that silently merges unrelated symbols.

4. Add invariant checks/regressions
- Add focused tests that fail if namespace/casing inconsistencies reappear.
- Include at least one negative test proving ambiguous names are not merged.

## MSL-Backed Development Expectations

Compiler quality claims must be backed by Modelica Standard Library (MSL)
evidence.

- Keep failing models visible in validation scope; do not hide failures by
  excluding targets after the fact.
- Do not weaken tests to force pass rates.
- Classify failures explicitly:
  - valid Modelica rejected by Rumoca (compiler bug),
  - invalid/non-standard pattern correctly rejected,
  - ambiguous case requiring policy decision.

## Bug Triage Proof Requirements (Mandatory)

Before proposing a semantic fix, provide all of the following:

1. Exact model/library location
- library name + version
- fully-qualified model/class name
- file path and relevant lines/snippet

2. Exact MLS cross-check
- cite the precise MLS section and wording
- explicitly map source behavior to MLS requirement

3. Explicit verdict
- `Rumoca bug`, or
- `Non-standard library pattern`, or
- `Ambiguous / policy decision needed`

4. Fix policy from verdict
- `Rumoca bug`: fix upstream in compiler phases first
- `Non-standard pattern`: keep strict default; if needed, add explicit opt-in
  compatibility behavior with documentation

Additional mandatory triage note:
- Include a short `Rejected hypotheses` section for every non-trivial bug,
  listing plausible explanations you ruled out and the evidence used.

## Compatibility Policy

Default behavior stays strict and spec-aligned.

- Compatibility deviations must be explicit and opt-in.
- Document each deviation with:
  - why it is needed,
  - which libraries/models require it,
  - default setting (`off` unless explicitly approved).
- No silent fallback behavior in diagnostics or tooling.

## Verification Workflow

Run the smallest command set that covers the changed behavior, then run required
gates from the active specs.

Common local verification commands:

```bash
rum verify lint
rum verify workspace
rum verify quick
rum verify msl-parity
```

For compiler-semantics changes that require the full parity gate:

```bash
cargo test --release --package rumoca-test-msl --test msl_tests \
  balance_pipeline::balance_pipeline_core::test_msl_all -- --ignored
```

If MSL quality/baseline data changes, regenerate and promote it using the
workflow in `SPEC_0025_PR_REVIEW_PROCESS.md` and
`SPEC_0030_COVERAGE_TRIM_PROCESS.md`.

## Compile-Time Performance Triage

If MSL runs show many compile timeouts or very slow compiles:

- Do not only increase timeout limits.
- Reproduce with one concrete model first (for example via
  `modelica_compare_cli.mjs probe-compile --model ... --compile-debug`).
- Profile Rumoca compiler hot paths with Rust tooling (`cargo`-based
  profiling/bench workflows) to find the dominant phase/crate before changing
  compile strategy.

## Architecture Discipline

- Respect crate boundaries and phase ownership (`SPEC_0029`).
- Keep IR crates data-only.
- Preserve deterministic public collections where required.
- Prefer minimal, explicit changes that maintain existing invariants.

## Done Criteria

Work is done only when:

- MLS/spec grounding is explicit for semantic changes,
- architecture boundaries remain intact,
- tests prove behavior (not only code-path execution),
- required verification commands ran (or blocker is explicitly stated),
- any compatibility behavior is explicit, opt-in, and documented.
