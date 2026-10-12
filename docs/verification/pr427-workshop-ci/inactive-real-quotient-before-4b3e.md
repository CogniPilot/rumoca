# Inactive Real quotient fault before repair

Public sealed source `4b3e7c22fb478438715c11cae462e26091b28ad8` fails this model at initialization, although its undefined arm is inactive throughout the requested interval `0..0.1`:

```modelica
model InactiveDivision
 output Real y;
 Real x(start=0,fixed=true);
equation
 y=if time < 0.5 then 1 else mod(1/time,7.0);
 der(x)=y;
end InactiveDivision;
```

The actual failure is `EX001: root condition output produced non-finite value NaN at index 0`. The source if-expression requires `y=1` and `x=t` throughout this interval (MLS §3.3 conditional selection, §3.6.5 if-expressions, §8.5 event generation). The competing unguarded `mod(time,7.0)` and guarded `mod(1/(time+1),7.0)` controls both completed two time points; selecting `mod(1/time,7.0)` at time zero correctly retained an evaluation fault. Thus the positive model reaches execution, rather than only demonstrating a general unsupported quotient profile.

Earliest observed divergence is root-condition evaluation. DAE runtime-quotient construction currently creates an `Always` activation; Solve root lowering groups relations without consuming their root activation. Quotient scalar and Map memory producers also evaluate their raw ratio independently of source arm selection. An eventual repair must issue and replay the source activation at its canonical owner and preserve that same authority for roots, initialization and event numeric memory. This receipt does not establish a specific proposed representation or an AFTER result.

The sealed CLI SHA-256 is `faf711401a2d4d2a7e5ef66d4b4739cbb05023bb02a96f01cd4d57b1921598de`. Raw evidence is retained under `$HOME/scratch/rumoca/workshop427-ci/evidence/inactive-quotient-division-4b3e`. Inactive source SHA-256 is `b123e4790bf3f5a87b17ae05f26486f6f8cea9d3cb9bd43058d94922fcf0eb22`; its raw log SHA-256 is `9eb8f2aa806f52179b966d168b92a5336c5fd23e0a92a1cf456e4e7f8d365ed8`. Every child was independently guarded to 60 seconds and 4 GiB process-group RSS; all finished without a resource stop. No Cargo build or shared-cache access occurred.

Two ordinary public `suite_core` regressions preserve inactive and selected undefined-arm behavior. The inactive case runs both valid and undefined ratios through eleven independently expected `y=1, x=t` samples under Interpreter and Auto; Auto alone is not a forced-native proof. The selected case retains the non-finite fault. The tests remain unexecuted at this test-only source, and production is unchanged:

```sh
cargo test --locked --offline -p rumoca --test suite_core reported_bug_runtime::main_dae_integer::real_quotient_ -- --nocapture
```

This selector chooses exactly two tests. Normal `suite_core` CI selects both without a new runner or optional skip.
