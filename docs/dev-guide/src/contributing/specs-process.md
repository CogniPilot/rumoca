# Where the Rules Live: Specs

Rumoca keeps **one source of truth per rule**: the specs in
[`spec/`](https://github.com/CogniPilot/rumoca/tree/main/spec).
[`AGENTS.md`](https://github.com/CogniPilot/rumoca/blob/main/AGENTS.md) is a
thin routing index from "what you are touching" to "which spec to read" —
it contains no rules itself, and neither does this book.

## The Map

| If you are touching… | Read |
|---|---|
| Compiler pipeline / any IR / any phase | [SPEC_0007](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0007_IR_PIPELINE.md) |
| Crate dependencies, foundation types, re-exports | [SPEC_0029](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0029_CRATE_BOUNDARIES.md) |
| Modelica semantics (anything MLS-affecting) | [SPEC_0022](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md) |
| Name lookup, scopes, `DefId` | [SPEC_0001](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0001_DEFID.md), [SPEC_0002](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0002_SCOPE_TREE.md) |
| Diagnostics, spans, error codes, tracing | [SPEC_0008](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0008_PHASE_ERRORS.md) |
| Tool config (`rumoca-tool-*`, env-var policy) | [SPEC_0018](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0018_TOOL_CONFIG.md) |
| Function length, nesting, file size, determinism | [SPEC_0021](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0021_CODE_COMPLEXITY.md) |
| Opening a PR | [SPEC_0025](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0025_PR_REVIEW_PROCESS.md) |
| Scope/philosophy questions ("should this live in the compiler?") | [SPEC_0031](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0031_COMPILER_PHILOSOPHY.md) |

Start at
[`spec/README.md`](https://github.com/CogniPilot/rumoca/blob/main/spec/README.md)
for the full index with statuses;
[SPEC_0000](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0000_SPEC_GUIDELINES.md)
explains how specs themselves work.

## Working With Specs

- Active specs (`ACCEPTED`/`REFERENCE`) are mandatory; archived ones are
  history.
- If a change you want violates a spec, **propose the spec change first**
  — architecture tests enforce important boundaries, and bypassing a test
  is never the move.
- If you cannot find the spec for what you are about to change, stop and
  ask: the rule either exists somewhere you have not looked, or it needs to
  be written before the code.
- Do not duplicate spec content into other documents (including this one);
  one source of truth per rule is what keeps the spec set trustworthy.

## House Norms Worth Knowing Early

These recur in review and each traces to a spec:

- Prefer the correct long-term fix over a short-term hack; fix root causes
  in the owning phase rather than weakening checks downstream.
- No `clippy` `allow` attributes; address the lint.
- No behavior-changing `RUMOCA_*` environment variables — knobs are
  documented CLI flags or config keys.
- Debugging is `tracing`-based, not `eprintln!` behind env vars.
- Deterministic collections and complexity limits per SPEC_0021.
