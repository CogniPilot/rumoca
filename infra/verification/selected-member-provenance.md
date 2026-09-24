# Selected-member modifier provenance repair

Source `3cd5041e8` integrates `d95ff887f` on top of the ARR-026 fix. The complete
7143 run exposed three DC-drive compile losses; the first two had high traces.
See [the failed full audit](selected-member-full-7143b62d-audit.md).

In `CurrentControlledDCPM`, the modifier `wRef = wNominal` substitutes the
root-written value `driveData.motorData.wNominal`. Previously the stored value
inherited the written source's `dcpm` occurrence. The post-materialization proof
therefore could not find the selected `driveData` declaration and left two
member identities unresolved; Flatten correctly refused them with EF024.
The first bad write was `process_nested_modifications_recursive` in
`rumoca-phase-instantiate/src/mod_env.rs`.

MLS §7.2.4 and SPEC_0007/0029 require preserving the context of each expression.
`ModificationValue` and `InstanceData` now distinguish a substituted value's
occurrence from the written source occurrence. Direct modifier substitution and
record-field projection propagate each independently. Post-materialization
resolves each expression surface with its own occurrence proof. The repair does
not restore declaration-type guesses or weaken the Flatten identity check.

Independent review found two additional propagation sites. `98fce614f` preserves
value scope through array projection and reindexes it with family copies.
`be97aace9` separates source/value component lookup maps during both array
candidate planning and element replay. The regression uses distinct array roots
with opposing scalar shadows, and fails if either map is conflated. Final source
review approved both paths; no identity validator or numerical guard was weakened.

Worker evidence at `/tmp/rumoca-fourbar-ci-redeclare/target/`:

- `selected-member-proof.log`: original occurrence before/after evidence.
- `selected-member-reference-planner-red.log` and `selected-member-reference-replay-red.log`:
  independent red controls for array lookup contexts.
- `selected-member-reference-instantiate.log`: 262 instantiate tests pass.
- `selected-member-reference-arr.log`: all 67 array contracts pass.
- `selected-member-reference-clippy.log` and `selected-member-reference-fmt.log`: pass.
- All three controlled DC-drive compile diagnostics succeed; eval-ast's 109 tests
  passed in the preceding scope-propagation follow-up.

Main validation at clean `be97aace9213da677e123b0668e309fe93ac6e3a`:
[focused/canary audit](fourbar-be97aace-tier1-audit.md) confirms all 14 raw traces
and all compared scores unchanged from 513. Focus: five completions, three
compared/high, two exclusions, zero missing. Canary: nine compared/high, zero
exclusions/missing. Both partial gates pass.

The [complete 566-model audit](provenance-full-be97aace-audit.md) restores exactly
307 compiled, 211 raw completions, 192 compared/high, 19 exclusions and zero
missing. All 211 Rumoca and OMC traces are byte-identical to 513. This includes
the two formerly high controlled DC drives and the Thyristor completion lost
at 7143. The full gate still fails historical accounting/runtime floors and
four older certified-model timeouts; no baseline is promoted.
