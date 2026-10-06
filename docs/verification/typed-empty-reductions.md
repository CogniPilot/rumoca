# Typed empty comprehension reductions

Governing contracts are MLS §10.3.4 / ARR-039 (reduction identities), MLS
§10.7 / ARR-036 (size requirements still apply at zero extent), SPEC_0032 §1
(empty structured domains), SPEC_0007 (Flat/DAE ownership), and SPEC_0033
§§2–4 (earliest divergence and unchanged strict checking).

The concrete counterexample on `dbe4908ad` is:

```modelica
model EmptyPrefix
  input Real a[6];
  output Real p[6];
equation
  for i in 1:6 loop
    p[i] = sum(a[k]*a[k] for k in 1:i-1);
  end for;
end EmptyPrefix;
```

At `i=1`, the domain is exactly `1:0`. The expected value is a Real zero.
The source passes the preceding phases, but ToDae reports ED019:
`an empty array needs an explicit checked element type`.

The first divergence is Flatten's comprehension expansion. The independent
producer regression observes `[]` instead of the complete comprehension
`{i for i in 1:0}`, including its body, binder, and source span. Expansion
has erased the only source from which canonical construction can derive the
element type. This is not an unknown runtime extent: the producer regression
uses literal bounds and no enclosing equation family. It is also not a
backend defect: the model fails before Solve and code generation. The empty
literal rejection is correct and remains unchanged.

Flatten now returns the original AST occurrence when its existing structural
expansion produces no elements. DAE plans that occurrence's exact domain,
lowers its body in the checked binder scope, derives its element type from
that body, and prefixes the proven domain extents to its shape. It checks the
body without evaluating it at a manufactured domain point. Nonempty
constructors follow the existing expansion path.

The regression suite checks native Real and Integer sum/product identities,
their canonical scalar types and argument dimensions, nested empty axes,
lexical binder shadowing, multiple rectangular binders, descending empty
ranges, and runtime coordinates that must not be read. Negative controls
retain rejection of incompatible body dimensions, nonnumeric body types,
and nested runtime extents. Nonempty controls check sum `7.5` and product `6`.
The frozen `crates/rumoca-compile/src/session/tests/fixtures/spd6_typed_empty.mo`
control restores the generic `i=1:6` pivot loop, including its first empty
sum, while retaining the six-by-six factorization and sixteen RHS. Its only
change from `spd6_dependent.mo` is separating this pivot loop from the existing
`i=2:6` factor-column loop.

## Verification

Before-fix focused compiler and producer regressions both fail with the
expected diagnostics. The first clean-cache build stopped with rustc's
`No space left on device` before test execution; its failure is infrastructure
evidence, not a semantic result. Repeating the baseline in an isolated shared
memory target produced the two semantic failures described above.

After-fix checks pass:

- 26 focused reduction tests: 18 Compile, one DAE, seven Flatten.
- Complete native suites for those packages: 1,397 pass, with three existing
  ignored doctests. This includes the exact Flatten producer regression.
- Final focused rerun after the test helper's Clippy nesting refactor and
  freezing the generic SPD6 source: the same 26 pass.
- Workspace `cargo fmt --all --check` and `git diff --check`.
- Strict all-target/all-feature Clippy for those packages with `-D warnings`.

```sh
cargo test -p rumoca-compile -p rumoca-phase-flatten -p rumoca-phase-dae reduction
cargo test -p rumoca-compile -p rumoca-phase-flatten -p rumoca-phase-dae
cargo fmt --all --check
cargo clippy -p rumoca-compile -p rumoca-phase-flatten -p rumoca-phase-dae \
  --all-targets --all-features -- -D warnings
```

The fixed-20 MSL/OMC canary delta, final workspace tests/lint/docs, MSL full
gate, ModelicaTest semantic gate, and pinned compatibility corpus remain for
the combined branch. They were not run in this isolated checkout because the
user requested integration on the existing upstream branch and the parallel
workspace gate held that source frozen. No cohort parity claim follows from
these focused tests.

All local Cargo commands use a task-owned target/TMPDIR, `nice -n 10`,
`CARGO_BUILD_JOBS=2 RUST_TEST_THREADS=2 RAYON_NUM_THREADS=2`,
`CARGO_INCREMENTAL=0`, and explicit parent timeouts. The user's resource
budget takes precedence over SPEC_0033's default four-worker cadence. The
task's shared memory allocation was monitored against an 8 GB stop limit,
with at least 16 GB of available host RAM retained. Final allocation was
5.51 GB; no unrelated cache or user artifact was removed.
