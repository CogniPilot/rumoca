# Native typed Integer modulo and remainder

The unchanged Booster source calls `mod(pre(estimator_sample), gps_decimation)`
at line 1585. The direct native-table diagnostic at
`afd5bbe10763b7474a18a12ad7932ae0923692d7` identifies checked primal owner 46,
source span 66639..66686: native IntegerModulo emission is unsupported. This
fails before the original strict simulation gate. Power composition did not
remove that error. The temporary diagnostic probes are absent from this patch.

MLS [§3.7.2](https://specification.modelica.org/maint/3.6/operators-and-expressions.html#event-triggering-mathematical-functions)
defines modulo with the divisor's sign and remainder with the dividend's sign.
SPEC_0007's Solve execution boundary, SPEC_0040 SOLVE-C84 and SPEC_0029 §12
assign native execution of these already-checked operations to Cranelift;
SPEC_0033 §§2–6 requires the first-owner proof and focused qualification.
The canonical `rumoca-eval-solve::typed_program::number` already admits these
operators; the missing implementation is native `typed_program::integer`.

The repair reuses native Integer arithmetic/status/domain emission. It refuses
zero divisors before machine remainder, selects safe divisor 1 for divisor -1
(including the representable remainder of MIN/-1), and adjusts modulo only
for a nonzero opposite-sign remainder. Selecting rhs-or-zero before checked
addition avoids an overflowing unused candidate. Final declared-domain checks
remain shared. Inputs and outputs use raw Int64 cells; no f64 transport,
helper fallback, owner omission, checker change or wire cutover is introduced.

| Frozen source | Actual result |
|---|---|
| Tests-only `8825f68590cbf586929eeb364fba8281f7067d03` | 0 pass / 2 fail: both native operators unsupported |
| Production `3b6b7ce339426588788f5e9c59f94990cafd30de` | 2 focused tests pass; 141 complete Cranelift library tests pass, zero ignored |
| Same production source | All-target/all-feature Clippy and workspace formatting check pass |

Both tests reuse the existing checked table and raw-cell harness, compare with
the canonical typed evaluator and independently tabulate all sign pairs,
wide exact Integer values, MIN/-1, MIN/MIN, MAX/MIN, active zero faults and
negative-only domain refusals. All 4,117 regular tracked source files, clean
status and HEAD are identical before/after each frozen attempt. The unchanged
600 s, 4 GiB RSS-plus-swap, 8 GiB cache, 16 GiB host and 8 GiB scratch guards
and 4/4/4 concurrency apply; no resource stop, retry or enlarged cap occurred.

The adjacent JSON binds raw logs, guardians, source manifests, commands and the
sealed test ELF. Evidence lives under the HOME-relative scratch path recorded
there. Original Booster AFTER with the qualified derivative context/dependency
chain remains pending. These focused results do not claim final combined CI.
