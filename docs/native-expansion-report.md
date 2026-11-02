# Native emission report

`rumoca sim bench model.mo --expansion-report emission.json` retains a diagnostic
of native lowering alongside the normal benchmark output. The opt-in collector
adds preparation cost: instrumented preparation times must not be compared with
an uninstrumented benchmark. It does not choose a backend, change a graph or
alter numerical lowering.

The versioned JSON separates admission-probe and definition scopes, using the
existing emitter phase. Probes can be refused as part of the constructor's
admission decision; those known outcomes remain visible and are not runtime
failures or lost collection. Raw warning flags remain present in both phases.
The JSON reports allocated CLIF instructions before optimization,
logical transferred cells, the actual chosen path and constructor-issued loop
capacity bounds. Instructions exclude child scopes emitted in the same function;
children retain their parent entry. A bulk move can transfer many cells while
allocating little code. Capacity-weighted copied cells are static upper bounds,
not dynamic execution counts; early-exit folds can do less work. Overflow is
unknown, never a smaller count. Host-kernel body instructions are not fabricated.
Only the listed transfer sites are counted; this is not a census of every load
and store or a machine-code size measurement.

Typed entries name a diagnostic program snapshot and operation ordinal. The
canonical program, its call table and its table binding are retained. Fused
producers remain visible as deferred entries, and consumers list their source
operation ordinals. Flat-map bodies remain under their original Map source.
Row scopes retain their original operations, assignment rows or fold body,
including the exact retained native call table when it was compiled earlier.
These report-local indices are not persistent semantic identities. Source JSON
is a diagnostic view, not a new compiler wire reader; a row constant also records
its binary64 bits. Consult the bound source/CI revision for a complete input
closure, including nested row constants and external resources.

Collection covers native CLIF construction on the calling thread. Current
constructors emit synchronously; queued parallel jobs compile already-built
functions. Child threads do not inherit collection. Nested collections are
independent and restore their parent on return or unwind. An unclosed scope or failed definition, invalid instruction accounting or unattributed transfer marks the
report incomplete. An invocation error is retained separately. Empty or
incomplete collection fails an explicitly requested CLI report instead of
reading as a zero-expansion success.

The original floor of 16 and single-shape warning remain visible. A warning is
not a proof that code grows proportionally: use multiple shapes to distinguish
fixed loop overhead, bounded static unroll and actual source-sized emission.
No quality floor or numerical policy is changed by this diagnostic.

## Focused validation

```bash
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 \
  cargo test -p rumoca-exec-cranelift --lib expansion::tests -- --nocapture
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 \
  cargo test -p rumoca-exec-cranelift --test tensor_expansion -- --nocapture
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 \
  cargo test -p rumoca --lib expansion_report::tests -- --nocapture
```

The four original literal/Map/Fold/output-range witnesses are built at
16/17/64/128 elements, with contiguous and strided output variants and a bounded
static matrix control, an exact-order fused chain and a row call whose table was
compiled outside collection. They check numerical bits with collection enabled and
disabled and preserve raw source/counter inventories under
`target/tensor-expansion-fixtures`. Linux CI uploads that directory from the
normal workspace suite; no second build or duplicate test run is added.
The historical private known-open list is not a measured current baseline.
Current fixture results and their source binding must be reviewed before a new
known-open manifest or generic expansion ratchet is accepted.
