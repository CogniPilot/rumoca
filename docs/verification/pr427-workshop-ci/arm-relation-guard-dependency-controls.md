# Arm relation guard dependency controls

These test-only controls are based on `3fb67e6a50f22f2ed34cedfc5d40d77457859902`.
They have not been built or executed. They exercise the existing canonical
constructors, independently of source compiler model specializations.

The scalar relation boundary must reject a free domain captured only by its
issued activation guard (SPEC0007 stage contracts and accepted SPEC0032 §1).
The B.1c construction proof must include current-value reads in that guard;
reversing the two producer owners must refuse the exact dependency, while
the corrected order and its checked binary replay must succeed. The periodic
clock control has a constant primitive relation and reads `previous(counter)`
only in its guard, so the guard is the only possible clock dependency
(MLS §16.5/§16.8 and the existing condition-memory/clock contracts).

The generated test domain is issued by the domain constructor. It does not
fabricate a source declaration ID. Every expression retains registered source
provenance. There are no production edits, new dependencies, scalar tensor
expansions, or alternate validators.

Pending gates, with exact expected counts 2 and 1:

```sh
cargo test --locked --offline --jobs 4 -p rumoca-ir-dae --lib \
  tests::model_conditionals::guard_dependencies:: -- --nocapture
cargo test --locked --offline --jobs 4 -p rumoca-phase-solve --lib \
  tests::clocks::arm_relation_previous_guard_owns_its_condition_memory_clock \
  -- --exact --nocapture
```

No public-language failure or runtime after result is claimed by these controls.
