# Mixed-clock arm guard control

This test-only followup to `a8286477f320128f42353dd01a456de7cc627cfb`
has not been compiled or executed. The original one-clock control remains
semantically unchanged and shares its checked-constructor fixture with this
negative control.

The guard combines `previous(counterA)<5` and `previous(counterB)<5` with
`and`. The two counters have distinct construction-issued periodic clocks,
with exact periods 1/10 and 1/5. The primitive relation is still `1.0>0.0`;
both clock dependencies occur only in its issued model-arm guard. The
expected refusal is `LowerError::NonComputable` at the condition clock proof,
with reason `condition mixes relations from different clock partitions` and
a non-dummy source span. A later failure to resolve `previous` inside the
wrong schedule does not satisfy the test.

The ownership boundary follows SPEC0007's independent typed condition system
and its single-owner clocked partitions, the existing Solve condition-memory
contract, and MLS §16's clocked `previous` semantics. The test constructs
issued identities through the existing DAE API; it introduces no production
policy, wire layout, fabricated declaration ID, or alternate checker.

Pending focused gate, expected count **2**:

```sh
cargo test --locked --offline --jobs 4 -p rumoca-phase-solve --lib \
  tests::clocks::arm_relation_previous_guard_ -- --nocapture
```

The exact negative selector, expected count **1**, is
`tests::clocks::arm_relation_previous_guard_refuses_mixed_condition_memory_clocks`.
This is a construction-proof regression, not a reproduced public Modelica
failure or a runtime qualification receipt.
