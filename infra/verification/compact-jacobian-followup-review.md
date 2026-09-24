# Compact Jacobian followup review

Reviewed clean signed `c5064507720b2360f79fae3d2d4bd6989dcd0b8c` against `c0402208af178510d6dbc5a4c4280df7961a02eb`, read-only. **Disposition: prior blocker resolved; no new blocker in this delta.**

`scaled_newton_delta_impl` now rejects a compact source unless `system.structure` equals its full value-layout pattern. The check precedes RHS construction, kernel selection, sparse cache access, and dense LU/SVD fallback. Dense sources keep their prior behavior. The new adversarial test uses different same-size patterns at dimensions 2 and 17 to exercise dense and sparse kernel choices; it checks both direct and cached wrappers, missing structure, a valid source control, and no scaled dense allocation or LU entry on refusal. No new allocation or solver policy is introduced by the production change.

The worker reports eight focused scaling tests plus clippy/fmt passed. I ran no builds, tests, or profiles. Main owns integration and canonical Fourbar/cohort validation.
