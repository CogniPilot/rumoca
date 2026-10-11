# Structured Real quotient qualification

The exact combined PR427 source `4b3e7c22fb478438715c11cae462e26091b28ad8` passed the checked numeric-memory certificate tests and the unchanged public structured quotient trace on October 11, 2026. The original model previously lost approximately `2.86775e-7` in `x[2]` at `t=0.7`; the existing analytic tolerance, model, eleven output times, and Interpreter/Auto policies remain unchanged.

| Gate | Passed/selected | Guardian elapsed | Peak process-group RSS |
|---|---:|---:|---:|
| quotient-ir-ten | 10/10 | 53.30 s | 1,799,156 KiB |
| quotient-public-three | 3/3 | 139.45 s | 2,736,520 KiB |

The ten IR cases include scalar and structured memory authority, both domain directions, current-wire replay, exact root ratio/order, private storage, event policy, and overlapping complete Map rejection. The three public cases exercise the original full trace, shifted initial quotients in ascending and descending domains, and honest generated-target refusal where structured quotient initialization is unsupported. Auto is an execution-policy check; this receipt does not separately establish which backend executed every prepared structured row.

Reproduce at the source above inside the normal full development environment:

```sh
cargo test --locked --offline --jobs 4 -p rumoca-ir-solve --lib quotient_memory:: -- --nocapture
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core structured_real_quotient -- --nocapture
```

Both gates completed with exit zero, without a guardian resource stop. The limits remained 600 seconds, 4 GiB process-group RSS, 8 GiB owned Cargo cache, an 8 GiB scratch free-space floor, and a 16 GiB host available-memory floor. Maximum cache usage was 6,773,415,936 bytes; minimum scratch free space was 156,092,235,776 bytes. The first attempt at `0e1560f8f7cbc29e80cffb24a7880ae89cab04d6` passed nine IR tests but failed to compile the public suite because a new lowering helper called a private core method. The final source uses the existing public checked domain extents; that first attempt supplies no public execution evidence.

The immutable evidence directory is `$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-structured-quotient-after-4b3e7-20261011`. Raw evidence digests:

| File | SHA-256 |
|---|---|
| `recipe.json` | `81ce96e45eec5ef524cb67c3040e04cd7e058a759950d24f41bc9b0aef6e9bd1` |
| `quotient-ir-ten.log` | `cfe6c5235d42335e2de7a9f2fac9dbc8a92d361d9ee678f3848258af23af121f` |
| `quotient-public-three.log` | `91a63eac85687f52fb9625dbda7371ab2ac55059360a7f9a771effb666f3ea12` |
| `quotient-ir-ten.guardian.json` | `c475edc35205e9652e86f2beb2d7466865c973421c0509a03065ee002c4ff712` |
| `quotient-public-three.guardian.json` | `a9e827f0c04a50695c08e69586a3716e127ca992dd4a1f216ede24a51f1514f8` |

The CLI sealed from the same source has SHA-256 `faf711401a2d4d2a7e5ef66d4b4739cbb05023bb02a96f01cd4d57b1921598de`; its separate receipt is under `$HOME/scratch/rumoca/workshop427-ci/evidence/current-4b3e-cli`. Solve schema 77 fixture issuance and the full current Wasmi suite remain separate required gates. This focused receipt makes no full CI, OMC agreement, PR421, or SLAM claim.
