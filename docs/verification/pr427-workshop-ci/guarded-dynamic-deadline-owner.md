# Guarded dynamic deadline ownership

The original Booster diagnostic on the `379f3d56` production baseline reaches
`Events::dynamic_time_event` with expression 6928, an `Add` at source span
53455..53496. Its model-arm scope and active arm are both 11, with ancestors
9/7/11. The unchanged authored expression is Booster line 1292,
`time < launch_time + launch_burn_duration`. The first refusal is
`expect_discharged_model_arm`, before native execution. This is distinct from
the ordinary structured-root boundary diagnosed for QuadAcro.

The candidate changes the ToDAE event owner for a constructor-proven arm-owned
dynamic time relation. The actual already-lowered relation owns the existing
guarded scalar root, or the existing structured root when its issued domain is
present. A read-only construction query uses the same canonical guard projection
and live/closed/function authority check as relation publication. It neither
accepts an arbitrary Boolean mask nor inspects source spelling for activation.
The existing dynamic-deadline API and its unconditional scheduling path remain
strict. No DAE or Solve wire field/schema changes are needed.

The relation root retains the selected arm's arithmetic, existing lazy region,
strict/non-strict zero domain, relation buffer, complete guard/data dependency
proof, and checked replay. Structured domains retain their existing owner;
the separately reviewed ordinary structured-arm root repair is required for
that path. The candidate shares the existing domain/scalar root dispatch rather
than adding a second evaluator or time-event runtime. It adds no inactive
deadline sentinel and does not evaluate a scoped deadline unconditionally.

The governing boundaries are accepted SPEC0007's independent typed condition
system, SPEC0032 §1, and catalog SOLVE-C11. MLS 3.7 §8.5 permits a located
relation event; exactly scheduling the special `time >= discrete expression`
and `time < discrete expression` cases is a quality-of-implementation concern.
The buffered value and event crossing must still be correct. The published
[MLS 3.7 PDF](https://specification.modelica.org/maint/3.7/MLS.pdf), rather than a
development HTML version, is the semantic reference used for this distinction.
The existing `activation_relation_roots` warning still governs unconditional
`when` deadlines; this patch does not replace that optimized owner.

At `60b948dbb9d670ddafc5cbb468cecbe7021e3ad9`, the counted live/closed
query and clock-boundary gates each passed one test. The public runtime gate
failed compilation with E0599: the fixture called a nonexistent
`SimulationSession::initialize` method. All four runtime tests and the following
original scenario gate are **unrun**, with no runtime-semantic conclusion.
The test-only correction uses the initializing `SimulationSession::new` entry
and preserves models, options, curve oracles and engine receipts. Its four
public runtime controls passed at
`124cea3839c812ae55a3a0daaf2ad7837b51fc82` with zero ignored tests and an
accepted exact count. The test process took 0.03 seconds; the guarded command
took 209.252 seconds including compilation. Both interpreter curves and
observed Cranelift execution without refusal/declines satisfied the unchanged
independent integral oracles.
[Exact runtime AFTER bindings](guarded-deadline-124cea-after-receipt.json)
retain the recipe, command, source hashes, guard and count receipt. The
original scenario gate also passed one roster test (41.33 seconds test time,
231.450 seconds including compilation), compiling all sixteen unchanged
authored configurations with pinned libraries. This includes QuadAcro,
ReusableBoosterLanding and the embedded Booster controller. This is a source
compilation result, not browser execution or full-horizon simulation.
[Exact attempt bindings](guarded-deadline-60b948-compile-receipt.json) retain the
recipe, logs, count decisions and guard receipt.

The four public tests use independent piecewise
integrals over eleven samples on both the interpreter and observed Cranelift
path. They cover `>`/`>=`, `<`/`<=`, a discrete reset moving the threshold from
0.45 to 0.85, inactive undefined square-root arithmetic, and refusal when that
arithmetic is selected. The clock control requires the existing typed refusal
for a `Previous` read outside its issued schedule. The existing IR relation
test now verifies the construction guard query during its live arm, refuses
open/foreign-arm queries, and preserves closed JSON/binary replay.

Focused commands (runtime four passed; query and clock passed on the preceding
`60b948` source):

```sh
# Four public native/interpreter curve controls.
cargo test --locked --offline --jobs 4 -p rumoca --no-default-features \
  --features fmu-packaging --test suite_core \
  reported_bug_runtime::guarded_time_deadlines:: -- --nocapture
# One clock-boundary control.
cargo test --locked --offline --jobs 4 -p rumoca-phase-solve --lib \
  tests::clocks::guarded_time_relation_refuses_a_previous_coordinate_outside_its_clock \
  -- --exact --nocapture
# One existing live/closed relation and replay control, with the new query.
cargo test --locked --offline --jobs 4 -p rumoca-ir-dae --lib \
  tests::model_conditionals::arm_relation_binds_its_exact_guard_and_replay_requires_closed_authority \
  -- --exact --nocapture
```

Existing unconditional time-activation and dynamic rescheduling regressions
remain required controls. Original Booster/QuadAcro browser runtime execution is pending;
no workshop success, public bug closure, or runtime equivalence is claimed from
source review alone. If the strict crossing or reset controls falsify this
owner, the candidate must remain unintegrated and an explicit checked schedule
activation representation must be reconsidered.

Required follow-up controls not executed by this recipe:

```sh
# Existing unconditional time/when owners, including dynamic rescheduling.
cargo test --locked --offline --jobs 4 -p rumoca --no-default-features \
  --features fmu-packaging --test suite_core time_event_when_activation:: -- --nocapture
# Construction-owned event-dependent deadline classification.
cargo test --locked --offline --jobs 4 -p rumoca-phase-dae --lib \
  event_dependent_threshold_owns_a_dynamic_time_event -- --nocapture
```

The separately proposed exact-initialization equality test is unrun here.
Default-feature runtime qualification, browser/scenario execution and final
mandatory CI remain separate obligations.
