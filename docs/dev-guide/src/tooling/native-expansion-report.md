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
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 \
  cargo test -p rumoca --lib sim_bench::expansion_tests -- --nocapture
```

The four original literal/Map/Fold/output-range witnesses are built at
16/17/64/128 elements. Range controls use contiguous and strided outputs over a
fixed wide input, varying the projected output count on the existing tape path.
Separate controls cover a bounded static matrix, an exact-order fused chain
and a row call whose table was compiled outside collection. They compare
numerical bits with collection enabled and disabled and preserve raw inventories
under
`target/tensor-expansion-fixtures`. Linux CI uploads that directory from the
normal workspace suite; no second build or duplicate test run is added.
The fixture gate compares every unforced warning occurrence with
`crates/rumoca-exec-cranelift/tests/tensor_expansion/known-warnings.tsv`.
This measured inventory includes both admission probes and definitions, and
keeps the historical floor of 16. A new fixture, a new or duplicate warning,
missing collection, an incomplete report or unattributed copy fails the gate.
Removing a warning also requires removing its manifest entry, with the raw
before/after evidence reviewed. The raw report is written before comparison
so a failed gate retains its counterexample. Report-local entry/program indices
identify diagnostic occurrences; they are not persistent semantic identities.

The initial inventory comes from the successful #556 fixture run. At the
observed 17/64/128 widths, literal aggregate construction allocates 10 CLIF
instructions per element and strided row output allocates two. Contiguous row
output uses bounded code. Map capture packs are outside iteration; its code at
64 and 128 elements is unchanged. Fold updates at those widths copy no cells
in this fixture and have unchanged code size. The heuristic nevertheless warns
on some smaller Map/Fold cases, so those warnings remain in the inventory.
The external-call-table fixture retains its two warnings at width 64; one
shape does not establish its growth rate. The existing bounded static matrix
classification remains unchanged.

This is a regression tripwire over these fixtures, not a proof that arbitrary
models are expansion-free. It does not promote historical classifications,
change the emitter's warning predicate, or turn a raw warning into an
asymptotic or dynamic-cost claim.
