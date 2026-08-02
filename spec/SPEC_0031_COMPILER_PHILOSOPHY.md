# SPEC_0031: Compiler Scope and Philosophy

## Status
ACCEPTED

## Summary
Rumoca is a Modelica-to-symbolic-system compiler whose core scope ends at DAE
generation. Solvers, runtimes, UIs, and bindings are replaceable extensions
layered on top of a stable, solver-agnostic DAE contract.

## Origin

This spec preserves the intent originally written by **Thomas Meschede**
in two companion docs (commit `7aaf38a`, PR #143, "add philosophy and
architecture documents to docs."):

- `docs/philosophy.md` — scope, principles, non-goals.
- `docs/architecture_maintenance.md` — operational rules for maintaining the
  core/extension split.

The standalone docs were absorbed into the spec set during documentation
consolidation; this spec is the permanent home for that material. Where
the original docs prescribed concrete crate names (`rumoca-solver-ref`,
`rumoca-wasm-core`, `rumoca-wasm-full`) the workspace has since evolved
into a more granular crate set, but the underlying rules survive
unchanged — see [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) for current
crate layout.

## Specification

### Pipeline

```
Modelica → parse → resolve → flatten → DAE
                                      ├→ Solve IR → execution / animation
                                      └→ GALEC IR → eFMI code generation
```

The **core compiler scope ends at DAE generation.** Everything downstream is
a replaceable extension. See [SPEC_0007](SPEC_0007_IR_PIPELINE.md) for the
detailed IR-stage contracts.

### Core Principle

| Layer | Role |
|---|---|
| Modelica | Input language |
| DAE-IR | Stable, deterministic, solver-agnostic system representation |
| Solver | Pluggable backend |

Rumoca builds portable symbolic systems. The DAE is the **only contract**
between compiler and downstream execution.

### DAE Contract Properties

| Property | What it means |
|---|---|
| Complete | Captures the full mathematical system; no out-of-band side channels |
| Deterministic | Same input → same DAE-IR, bit-for-bit |
| Solver-agnostic | No solver-specific encoding choices |
| FMI-aligned | Variable partitions and equation form match MLS Appendix B + FMI conventions; suitable for direct FMU export |
| Internally configurable | Transformations (alias elim, index reduction, BLT) MAY run inside the compiler before DAE-IR is finalized |
| Externally stable | Once emitted, the DAE-IR shape does not change per consumer |

### System Boundaries

| Boundary | Rule | Why |
|---|---|---|
| Compiler ↔ Solver | DAE-IR is complete and solver-agnostic; solvers MUST NOT influence DAE structure | Multiple solvers must consume the same DAE; coupling kills portability |
| Compiler ↔ Runtime | Time stepping, integrators, events, animation belong to solver/runtime | Execution policy is not compile semantics |
| Compiler ↔ UI | UI is an external consumer for visualization and orchestration | UI must not influence what gets compiled |
| Compiler ↔ Codegen | Codegen is one-way out of compiler IR | No target-specific assumptions feed back into earlier stages |

### Extensions

WASM builds (with optional solvers), editor integrations (VS Code,
LSP, formatter, linter), bindings (Python, WASM), and example applications
are **extensions layered on top of the core.** Required properties:

- Consume the same DAE interface as any other extension.
- Remain replaceable — no extension is special.
- Introduce no reverse dependencies into the core.

See [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) for the workspace-level
enforcement.

### Non-Goals

Rumoca is explicitly NOT:

- a solver framework — it produces DAEs, it does not integrate them;
- a Modelica runtime — execution lives in solver/runtime crates;
- UI-driven — no UI concern shapes compilation;
- tied to a specific backend — diffsol, rk45, MLIR, CasADi, etc. are all
  equal consumers of the same DAE-IR.

### Hard Rule: Core Isolation

> The core must compile and function without any extension packages.

| Rule | Why |
|---|---|
| No reverse dependencies (core → extensions) | Extensions are optional; the core ships standalone |
| No feature leakage from extensions into the core | Compile-time scope is owned by the core, not by extensions |
| No optional shortcuts inside the core that exist to serve an extension | Avoids the "bundled solver drift" risk below |

### Reference Solver Rules

A reference solver ships alongside the compiler for end-to-end testing and
example workflows. To preserve the core/solver split:

| Rule | Why |
|---|---|
| Reference solver lives in a separate crate | Solver code stays below the DAE contract |
| Reference solver MUST NOT introduce assumptions into IR | IR shape must work for any consumer |
| Reference solver MUST remain replaceable | "Reference" is a label, not a privilege |
| Other solvers MAY use the same DAE-IR with no special-casing | Validates the agnostic-DAE property |

### WASM Build Rules

| Rule | Why |
|---|---|
| Core WASM build = parsing, semantics, DAE generation only | Provides DAE to any external solver/runtime |
| Full WASM build with bundled solver = extension, not core | Solver inclusion is a feature flag, not a default |
| WASM transitive dep graph MUST NOT pull in a solver backend by default | Enforced by `test_bind_wasm_default_graph_does_not_include_diffsol` in `architecture_hardening_test.rs` |

### Known Risk: Bundled-Solver Drift

A reference solver bundled with the compiler tempts contributors to
optimize the DAE for that solver, creating hidden coupling.

**Mitigation:**

- Test the compiler with multiple solver backends (diffsol, rk45) and a
  solver-free path.
- Keep concrete solver crates strictly below the DAE contract per
  [SPEC_0029 §12](SPEC_0029_CRATE_BOUNDARIES.md).
- Reject changes that shape DAE-IR around a single backend.

### Guiding Idea

> Rumoca extracts symbolic systems from Modelica models and makes them
> usable across different execution environments.

## Related Specs

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — IR pipeline and stage contracts.
- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — workspace-level boundary
  enforcement that operationalizes the principles above.
