# Ordinary structured arm relation controls

These three test-only controls are based on
`379f3d56ad90e1f6046275c3af752672cdc42706` and remain **unrun**.
They exercise the ordinary structured-root constructor, independently of
runtime quotient certificates and public guide model diagnostics.

The generated domain is construction-issued. Its binder appears only in the
model conditional's guard (`i>0`), while the primitive relation is `time>0`.
Ordinary root publication is tested both while its own arm is live (the
ToDAE producer path) and after the conditional closes. Both paths must retain
that exact issued guard and domain. Extents 2 and 1,000,000 must issue the same expression
count and one structured root, with no scalar roots or quotient owners.
JSON and binary replay must reproduce exact bytes and the same guard/domain
relationship; the binary reader must consume the entire input.

The negative controls require exact foreign-domain identity refusal, reject
publication from an unrelated active arm or a function boundary, and preserve
successful publication from the owned closed model context after those failed
attempts. A JSON root changed to an independently issued domain of identical
geometry must fail the binder proof during checked replay.

The boundaries follow accepted SPEC0007's independent typed condition owner,
SPEC0032 §1's compact structured event family, and the existing model-arm
construction and replay authority. The shared test helper already issues the
conditional; these controls add no alternate evaluator, checker, production
policy, source declaration identity, or serialization field. They do not yet
establish the cause of a public QuadAcro or Booster failure.

Pending gate, expected test count **3**:

```sh
cargo test --locked --offline --jobs 4 -p rumoca-ir-dae --lib \
  tests::model_conditionals::structured_relations:: -- --nocapture
```
