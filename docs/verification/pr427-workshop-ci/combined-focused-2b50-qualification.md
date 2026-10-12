# Combined focused regression run

The frozen combined source `2b50ac92a51469c4b8183c0de57514167d3b59f3`
ran 548 selected tests: **546 passed, two failed, 9,747 were outside this
selection**. Test execution took 2.907 seconds; the complete resumed build and
run took 312.290 seconds. Every tracked source byte and symlink entry matched
before and after. The adjacent receipt binds commands and raw artifact hashes.

The initial 600-second compilation attempt timed out before running tests.
Only after that process was terminal did the same source resume with its owned
cache. This is a bounded build continuation, not a passing first-attempt claim.

The selection covers all IR-DAE and PhaseStructural tests, the original
algebraic-family numerical reference, structured selected indices, empty array
reductions, runtime index errors, dynamic inactive-gather reachability, late
WASM fault provenance, registry invariants, and the three original
conditional-derivative regressions. The passing numerical reference retains
exact bit comparisons; its corrected iterator traversal follows the source
constructor order. Registry selection uses `binary(registry_invariants)`.

The triangular constructor failure already produced typed `ED019` at the exact
index occurrence; the assertion expected obsolete diagnostic wording. The
runtime index failure already produced the exact index 3 / axis 1 / extent 2
error and source span through `ModelExchangeSession`; an earlier assertion
incorrectly expected the `SolveIr` wrapper. Commit `7d4772358` corrects those
expectations and strengthens the triangular error-code/span check. That commit
is **not qualified by this earlier run**: both corrected assertions require a
fresh run on the final combined candidate. No compiler validator or numerical
tolerance is weakened.

This record does not establish full workspace, browser, guide, MSL, quick/full,
remote CI, or final-candidate success.
