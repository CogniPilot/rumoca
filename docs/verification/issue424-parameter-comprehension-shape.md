# Issue424 parameter constructor shape

Actual BEFORE source: `e5388bd4bf2c08e4c20d29751bd3fedb95cb8fb6`.
Attempt `pr427-parameter-controls-after-e538-20261011`, gate
`parameter-dimensions-and-masks-7`, executed **4 passed / 3 failed / 0 ignored**.
All three added parameter/Boolean tests fail ToDae ED020 expression shape
mismatch; all four original literal-range/negative controls pass. The test
execution took 0.01 seconds; this was not a resource stop. Raw artifacts under
`$HOME/scratch/rumoca/slam-integration-cycle-20261010` retain their exact source:

| Artifact | SHA256 |
|---|---|
| `recipe.json` | `81a75b83e17cf11c528a4929920cd74c2d865759d4403477b714e3062202e1a3` |
| `parameter-dimensions-and-masks-7.log` | `26851b113f2c782f9f94278faadbd453158cf665463cfb9cf1c8a741dc78019b` |
| `parameter-dimensions-and-masks-7.guardian.json` | `a60a9d573a82e7ed687f9c656b4169db3398f2088b2681d23a1917bd202ebcaf` |

## First divergent owner

Flatten's `expand_array_comprehension_bindings` uses context-free
`infer_array_dimensions` to decide whether to retain the compact Flat binding.
Literal endpoints satisfy that check. Parameter endpoints such as `nx` and `nr`
do not, even though the surrounding Flatten context already knows their values.
The fallback `try_expand_index_ranges` does read that context and enumerates the
ranges, but `expand_comprehension` concatenates every inner result into one
vector (`result.extend(inner)`). A valid [2,3] or [3,4] constructor therefore
becomes a rank 1 expression. The precise canonical DAE shape check rejects that
incorrect Flat binding. DAE refusal is correct; the earlier replacement of the
source binding is the defect.

The published PR421 source `f84a4304ece1a101e2f9a28275b1ed76fefdf87e` has the
same context-free preservation call and concatenating fallback. This repair
does not duplicate a parameter-constructor owner fix there.

## Narrow repair

The preservation decision now calls existing EvalFlat
`infer_array_dimensions_full_with_functions` with Flatten's existing
`Context::param_eval_context(Some(variable_name))`. The same contextual owner
already infers binding dimensions in Flatten's array-dimension pass. It reads
known parameters, arrays and function shapes, retains exact lexical binder
provenance, and enumerates no tensor elements. No second evaluator, dimension
rule, binder lookup or canonical owner is introduced. Known declared axes still
remain unchanged; existing invalid-shape rejection is not weakened.

The unchanged 5 conforming cases in the 3 existing tests cover both axis orders,
parameter Real values, parameter Boolean masks and a normal Boolean binding.
Their shape/value expectations remain unchanged. Added owner assertions require
their Flat bindings to remain `ArrayComprehension`, so correct scalar values
alone cannot hide another eager-expansion fallback.

[MLS §4.4.2](https://specification.modelica.org/maint/3.6/class-predefined-types-and-declarations.html#static-semantics)
permits Integer parameter expressions as dimensions;
[MLS §10.4.1.2](https://specification.modelica.org/maint/3.6/arrays.html#constructor-with-several-iterators)
defines multiple iterators as nested constructors with the last textual iterator
outermost. SPEC_0007 Flat stage and SPEC_0032 §§1–2 require structured symbolic
array ownership; SPEC_0029/SPEC_0041 place Flat constant/scoped evaluation in
EvalFlat; SPEC_0033 §§2–6 require this first-owner repair and actual before/after
qualification. No accepted specification changes are needed.

## AFTER still unrun

Formatting and `git diff --check` pass. No Cargo, OMC, cache mutation or final
qualification was performed for this candidate. Root owns the guarded lane.
The smallest gate is unchanged:

```sh
cargo test --locked --offline --jobs 4 -p rumoca-compile --lib \
  session::tests::reported_bug_regressions::issue_424_ \
  -- --nocapture --test-threads=4
```

It must execute 7 tests, 0 ignored. The whole existing module selects 9 tests and
also preserves original 423/426 controls. Existing Flatten array-comprehension
and EvalFlat scoped dimension tests remain relevant focused controls. Original
invalid C3 stays unchanged and precisely rejected; no issue-closure claim is
made from this unqualified candidate.
