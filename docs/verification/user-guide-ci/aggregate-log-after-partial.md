# Actual aggregate logarithm AFTER, partial

Source `51398d4ab2e8ce900ebe2326e53f564f1ef152da` measured **4 passed / 1 failed**, with three selected tests unrun when the serial recipe stopped. The temporary diagnostic was removed. This is a partial failing qualification, not an all-green AFTER result.

The ranked numeric tests (2) passed all vector/matrix Log/Log10 analytic and finite-difference comparisons and scalar-policy nonfinite assertions. The compact logarithm arithmetic-profile test (1) and existing scalar singular derivative test (1) passed. The existing broadcast quotient test failed at tests.rs:1370 because it expects the reciprocal's checked directional owner to be absent; earlier division support now supplies that owner. Its test-only repair is independently owned and pending. No validator, arithmetic policy, original model or numeric tolerance was weakened in this attempt.

The broadcast profile, original TestGlycol plus ViscosityProbe, and nested exponential interface gates were unrun. An independently frozen three-gate recipe on the exact same source will measure those remaining behaviors without superseding this failed attempt. Original Glycol causal closure is still pending.

[Portable partial receipt](aggregate-log-after-partial.json) retains each executed gate's actual summary, guardian and SHA256 bindings, raw recipe hash, terminal audit, and the pending remainder recipe hash. The counted runner released its lease; zero owned-target references were observed and 2276 denied reads were explicitly unobserved. The preceding [ranked BEFORE](aggregate-log-ranked-before-after.md) remains 0 passed / 2 failed, and the [original BEFORE](glycol-aggregate-log-before.md) remains 0 passed / 1 failed.
