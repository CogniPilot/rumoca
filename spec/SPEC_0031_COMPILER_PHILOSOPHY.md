# SPEC_0031: Compiler Scope and Philosophy

## Status
REFERENCE

## Summary
Rumoca is a Modelica-to-symbolic-system compiler. Its contract is layered, not
single: the DAE is the canonical semantic IR, Solve IR is the executable
contract every backend consumes, and FMI 3 Model Exchange is the one
solver-facing model interface. Solvers, runtimes, UIs, and bindings are
replaceable extensions above those contracts.

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
Modelica → parse → resolve → flatten → DAE → structural → Solve IR → FMI 3 ME / codegen → execution / animation
```

**Modelica semantics end at DAE generation.** Structural analysis and Solve
lowering are still compiler phases, but they add no language meaning — they
make the same system executable. Everything below the FMI 3 ME boundary
(integrators, hosts, viewers) is a replaceable extension. See
[SPEC_0007](SPEC_0007_IR_PIPELINE.md) for the detailed IR-stage contracts and
[SPEC_0038](SPEC_0038_UNIFIED_FMI_EXECUTION.md) for the execution boundary.

### Core Principle

| Layer | Role |
|---|---|
| Modelica | Input language |
| DAE-IR | Canonical semantic system representation: stable, deterministic, solver-agnostic |
| Solve IR (`rumoca-ir-solve`) | Executable contract: what evaluators, JIT/device adapters, and codegen targets consume |
| FMI 3 ME | The single solver-facing model interface (SPEC_0038) |
| Solver | Pluggable integrator behind that interface |

Rumoca builds portable symbolic systems. The DAE fixes what a model **means**;
Solve IR fixes what a consumer **runs**; FMI 3 ME fixes how a solver **calls**
it. No private fourth path around those three is permitted.

### Compilation Soundness Theorem

For every Modelica model `M`, a successful simulation-capable compilation to
an FMI Model Exchange component `A` is a claim that every conforming host and
solver execution of `A` refines the Modelica semantics of `M` within the
execution's declared numerical tolerance:

```text
compile(M) = A  =>  for every conforming host/solver H,
                     trace(H, A) refines semantics(M) within tolerance(H)
```

This is the compiler's governing soundness obligation. It has four inseparable
parts:

- Every finalized compiler and component aggregate is valid by construction;
  independently constructed roots, metadata, layouts, or provenance cannot be
  paired into a successful artifact.
- Every compiler phase and the FMI host/solver boundary preserves the semantic
  relation established by its predecessor. A concrete solver may own numerical
  method state, but it cannot introduce a solver-specific model contract.
- Unsupported or unproved semantics fail with a typed diagnostic at their
  first owning phase. Producing a plausible artifact and discovering the gap
  from a wrong trace is a soundness failure, not partial support.
- Differential and strict-high trace results are counterexample and regression
  evidence for the theorem. They do not replace construction or refinement
  proofs, and raw compilation or simulation completion is not affirmative
  semantic evidence.

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
| Compiler ↔ Solver | Solvers reach the model only through FMI 3 ME over a checked Solve kernel; they MUST NOT influence DAE or Solve structure | Every solver sees one model interface; coupling kills portability |
| Compiler ↔ Runtime | Time stepping, integrators, events, animation belong to solver/runtime | Execution policy is not compile semantics |
| Compiler ↔ UI | UI is an external consumer for visualization and orchestration | UI must not influence what gets compiled |
| Compiler ↔ Codegen | Codegen is one-way out of compiler IR | No target-specific assumptions feed back into earlier stages |

### Extensions

WASM builds (with optional solvers), editor integrations (VS Code,
LSP, formatter, linter), bindings (Python, WASM), and example applications
are **extensions layered on top of the core.** Required properties:

- Consume the same published contract as any other extension: Solve IR for
  backends and codegen, FMI 3 ME for anything that integrates the model.
- Remain replaceable — no extension is special.
- Introduce no reverse dependencies into the core.

See [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) for the workspace-level
enforcement.

### Non-Goals

Rumoca is explicitly NOT:

- a solver framework — it produces DAEs, it does not integrate them;
- a Modelica runtime — execution lives in solver/runtime crates;
- UI-driven — no UI concern shapes compilation;
- tied to a specific backend — MLIR, Cranelift, CasADi and the other execution
  backends are equal consumers of the same Solve IR, and integrators (diffsol,
  rk45) are equal hosts of the same FMI 3 ME contract.

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
| Reference solver lives in a separate crate | Solver code stays below the Solve IR / FMI 3 ME contracts |
| Reference solver MUST NOT introduce assumptions into IR | IR shape must work for any consumer |
| Reference solver MUST remain replaceable | "Reference" is a label, not a privilege |
| Other solvers MAY host the same FMI 3 ME contract with no special-casing | Validates the agnostic-contract property |

### WASM Build Rules

| Rule | Why |
|---|---|
| Core WASM build = parsing, semantics, DAE generation only | Provides DAE to any external solver/runtime |
| Full WASM build with bundled solver = extension, not core | Solver inclusion is a feature flag, not a default |
| WASM transitive dep graph MUST NOT pull in a solver backend by default | Enforced by `test_bind_wasm_default_graph_does_not_include_diffsol` in `architecture_hardening_test/main.rs` |

### Known Risk: Bundled-Solver Drift

A reference solver bundled with the compiler tempts contributors to
optimize the DAE for that solver, creating hidden coupling.

**Mitigation:**

- Test the compiler with multiple solver backends (diffsol, rk45) and a
  solver-free path.
- Keep concrete solver crates strictly below the Solve IR / FMI 3 ME contracts
  per [SPEC_0029 §12](SPEC_0029_CRATE_BOUNDARIES.md).
- Reject changes that shape DAE-IR or Solve IR around a single backend.

### Guiding Idea

> Rumoca extracts symbolic systems from Modelica models and makes them
> usable across different execution environments.

## Related Specs

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — IR pipeline and stage contracts.
- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — workspace-level boundary
  enforcement that operationalizes the principles above.
- [SPEC_0038](SPEC_0038_UNIFIED_FMI_EXECUTION.md) — FMI 3 ME as the sole
  solver-facing model interface.
