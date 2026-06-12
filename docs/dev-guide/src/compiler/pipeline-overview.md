# Pipeline Overview

A model moves through the compiler in a fixed sequence of phases, each owned
by a `rumoca-phase-*` crate, each producing or refining one of four
intermediate representations:

```text
Modelica source (.mo)
   │  parse                 rumoca-phase-parse
   ▼
  AST ───────────────────── rumoca-ir-ast
   │  resolve               rumoca-phase-resolve
   │  typecheck             rumoca-phase-typecheck
   │  instantiate           rumoca-phase-instantiate
   │  flatten               rumoca-phase-flatten
   ▼
  Flat ──────────────────── rumoca-ir-flat
   │  DAE lowering          rumoca-phase-dae
   ▼
  DAE ───────────────────── rumoca-ir-dae      ◄── the stable contract
   │  structural prep +     rumoca-phase-structural,
   │  solve lowering        rumoca-phase-solve
   ▼
  Solve ─────────────────── rumoca-ir-solve
   │
   ├── simulate             rumoca-sim, rumoca-solver-*
   └── generate code        rumoca-phase-codegen + targets
```

The stage contracts — what each IR may contain, what each phase may and may
not do — are normative in
[SPEC_0007](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0007_IR_PIPELINE.md).
The narrative below is the mental model.

## The Story of a Compile

1. **Parse** turns source text into an AST that faithfully represents the
   text — comments, spans, syntax structure — with no semantic knowledge.
2. **Resolve** builds the scope tree and assigns every definition a stable
   `DefId`. From this point on, compiler identity is `DefId`-keyed, never
   string-keyed.
3. **Typecheck** checks the resolved tree and evaluates structural
   parameters (the ones that determine array sizes and loop ranges).
4. **Instantiate** applies modifications and builds the instance tree for
   the requested model — `Tank tank1(area = 2.0)` becomes a concrete
   instance with `area` bound.
5. **Flatten** walks the instance tree into a single flat model: one list of
   variables with fully qualified names, one list of equations, `connect`
   sets expanded into equality and flow-sum equations. Arrays stay symbolic;
   `der`, `pre`, `sample` are still present as expressions.
6. **DAE lowering** eliminates Modelica-specific operators and produces the
   MLS Appendix B canonical form: pure functions over the variable vector,
   with events, relations, and clocks as explicit metadata. `reinit` becomes
   guarded update equations; `pre` becomes explicit `__pre__.*` slots;
   `assert`/`terminate` become event actions.
7. **Structural preparation** (on the way to simulation) matches equations
   to unknowns, sorts into block lower-triangular (BLT) order, tears
   algebraic loops, performs index reduction with dummy derivatives, and
   selects states.
8. **Solve lowering** converts the prepared system into a register-machine
   representation — scalar programs and tensor program nodes — that
   execution backends consume directly.

You can watch every step on a real model:

```bash
rumoca compile Model.mo --emit ast-mo     # or flat-mo, dae-mo, *-json
rumoca compile Model.mo --emit solve-json
rumoca compile Model.mo --inspect structure
rumoca compile Model.mo --target sympy -o /tmp/out -v   # phase timing lines
```

## Try It Here

This block runs the same pipeline in your browser; **Show DAE** displays the
lowered system (stage 6) for whatever you type:

```modelica,interactive
model Mixer "Two tanks exchanging fluid"
  parameter Real k = 0.4 "Exchange coefficient";
  Real h1(start = 1.0);
  Real h2(start = 0.0);
  Real q "Exchange flow";
equation
  q = k * (h1 - h2);
  der(h1) = -q;
  der(h2) = q;
  annotation(experiment(StopTime = 10.0));
end Mixer;
```

## Where the Boundaries Bite

Three boundary rules explain most review feedback on pipeline changes:

- **Codegen targets the lowest IR it needs — no lower.** A formatter reads
  AST; FMI export and symbolic backends read DAE; numeric kernels read
  Solve. Reaching down for convenience couples a backend to representation
  details it should not know.
- **The DAE is lean.** Mass matrices, Jacobians, BLT orderings, tearing
  choices — anything that is a *solver work product* — belongs in structural
  analysis results or Solve artifacts, never stored inside the DAE.
- **Phases fail loudly in their own stage.** If source information (a span,
  a name structure, a type) is lost at a phase boundary, the fix is to
  preserve it at that boundary — not to reconstruct it downstream by parsing
  strings.
