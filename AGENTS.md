# AGENTS.md

**This file is a routing index, not a guidance document.** All design rules
live in `spec/`. Use the tables below to figure out which spec to read for
the task you're working on, then go read that spec. If a rule isn't in a
spec, it's not a rule — propose a spec change first.

## Starting point

- [spec/README.md](spec/README.md) — index of all active specs and their
  status (`ACCEPTED`, `REFERENCE`, `DRAFT`).
- [spec/SPEC_0000_SPEC_GUIDELINES.md](spec/SPEC_0000_SPEC_GUIDELINES.md) —
  how specs themselves work; what the statuses mean; how to propose a new
  spec.
- [CONTRIBUTING.md](CONTRIBUTING.md) — local setup and `cargo xtask` CLI usage.
- [spec/SPEC_0032_DEVELOPMENT_PROCESS.md](spec/SPEC_0032_DEVELOPMENT_PROCESS.md) —
  operational workflow, triage proof requirements, upstream-first fix policy,
  and MSL-backed validation expectations.

## Task → specs

| If you are touching... | Read these specs |
|---|---|
| Compiler pipeline / any IR / any phase | [SPEC_0007](spec/SPEC_0007_IR_PIPELINE.md) — IR stage contracts, structural-transformation scope |
| Crate dependencies, foundation types, re-exports, single-source helpers | [SPEC_0029](spec/SPEC_0029_CRATE_BOUNDARIES.md) |
| Modelica semantics (any MLS-affecting change) | [SPEC_0022](spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md) (use its section index) |
| Name lookup, scopes, `DefId` | [SPEC_0001](spec/SPEC_0001_DEFID.md), [SPEC_0002](spec/SPEC_0002_SCOPE_TREE.md) |
| Diagnostics, spans, error codes, tracing | [SPEC_0008](spec/SPEC_0008_PHASE_ERRORS.md) |
| Tool config (`rumoca-tool-*`) | [SPEC_0018](spec/SPEC_0018_TOOL_CONFIG.md) |
| Function length, nesting, file size, deterministic collections, code-size policy | [SPEC_0021](spec/SPEC_0021_CODE_COMPLEXITY.md) |
| Development workflow, bug triage, root-cause proof, upstream-first fixes | [SPEC_0032](spec/SPEC_0032_DEVELOPMENT_PROCESS.md) |
| Opening a PR (workflow, metrics, verification commands, MSL gates, done criteria) | [SPEC_0025](spec/SPEC_0025_PR_REVIEW_PROCESS.md) |

## Rules of thumb

- Active specs (`ACCEPTED` / `REFERENCE`) are mandatory. Archived specs are
  historical context only.
- If you cannot find the spec for what you're about to change, stop and ask
  before coding — the rule either exists somewhere you haven't looked or it
  needs to be written.
- Do not duplicate spec content in `AGENTS.md`, code comments, or other
  pointer documents. One source of truth per rule.
