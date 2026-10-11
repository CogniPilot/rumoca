# Byte-preserving issued-fixture replacement

`replace_source_tables.py` consumes the actual issuer output; it does not
generate metadata, evaluate Modelica, or rewrite typed tables. Before any write,
it requires the exact six-destination roster, original source/variant strings,
matching artifact/source hashes, issuer Git revision and full tracked-source
seal, matching actual producer executable digest, current DAE/Solve schema
owners, one full 14,400-input owner, and mandatory source/generated binder
provenance. Python's JSON integers are exact; duplicate keys and nonfinite JSON
numbers are refused. The checks do not replace canonical checked replay.

Integrate this helper before issuance so the clean committed source seal
includes it. After actual issuance, while the source tree is idle and still
clean at the issuer pin, run under the root's coordinated lane:

```sh
python3 "$fixture_tree/crates/rumoca-exec-wasm/tests/suite_typed_calls/replace_source_tables.py" \
  "$fixture_tree" \
  "$HOME/scratch/rumoca/pr427-wasm-fixture-refresh-20261011/issued" \
  "$HOME/scratch/rumoca/pr427-wasm-fixture-refresh-20261011/historical"
```

The archive parent exists and `historical` must be new. An optional final
argument supplies an independently retained copy of the issued producer binary
if the shared target's executable has been retired; its observed digest must
still match. Without it, the helper verifies the original executable path
recorded by issuance. Do not rerun issuance from a source tree after replacement
without first committing that new source state and giving it a fresh receipt.

The archive retains all six old fixture bytes at their original relative paths,
the complete old strict reader, the issuer source seal/manifest, old/new SHA-256
records and the replacement receipt. All archives precede source changes.
Temporary files are atomically replaced without changing file modes. New fixture
files are byte-for-byte copies of `issued.json`; no numeric payload is parsed
and reserialized. The reader changes only its three literal pins:
`compilerRevision`, `compilerSourceSha256`, `producerBinarySha256`.
Original source equality, one-owner/full-input checks, canonical deserialization
and every arithmetic/fault/atomicity/recovery assertion remain unchanged.

The procedure reports **STAGED_UNQUALIFIED**, not execution success. Inspect the
diff and raw replacement receipt, then run:

```sh
cargo test --locked --offline -p rumoca-exec-wasm --test typed_calls
```

The actual source modules select six `source_tables` tests and one `tensor_source`
test, including all six issued wires, independent literal recurrence, canonical
evaluation and actual Wasmi execution. The full test target also preserves its
other numeric/control-flow/alias/bounds tests. Record actual counts and results;
do not infer them from successful generation. An interrupted staging operation
may leave a partial source diff; the complete prior bytes are retained in the
archive for exact restoration before another attempt.

All six positives currently share the same existing reader pin triple; there
is no separate Integer fixture revision. The four historical wire-refusal
fixtures remain untouched. Static payload inspection found no other committed
structured-binder JSON/snapshots beyond these ten files. The representative
Solve golden has no structured binders and already records schema 75. Current
DAE/Flat replay tests build their payloads from constructors; the required field
is serialized there rather than supplied by a fallback. Core's deliberate
missing-provenance JSON remains a negative test. No other stale literal schema
44/74 assertions were found in the inspected Rust, JS, TS, Python or snapshots.

No actual issuer artifacts exist on this helper candidate. Only Python syntax,
the existing pin match counts and diff checks have been inspected; no replacement,
Cargo build or executor run is claimed. Review/generation/fixture qualification
remain separate from the currently frozen source gates.
