# Rumoca Development Instructions (AI + Human)

This document defines the operational workflow for development work in
`packages/rumoca`.

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
