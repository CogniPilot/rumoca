# Exact projection context and completed-fold reuse

## Current verification status

The original context/completed-walk prototype passed focused tests but failed
full-model liveness as detailed below. Its production implementation has now
been replaced by an exact generic-summary dependency graph. **The revised graph
passes all 32 focused projection regressions, strict owner Clippy, and both
unchanged full Eigen6 native regressions.** The old prototype's results are kept
separately below; they are not used as evidence for this revision.

Each graph node retains the typed function/fold/carried/scalar/field identity,
the exact initial and update expression IDs, and an immutable snapshot of its
checked lexical parent environment. Snapshot filtering preserves all domain IDs,
point values, order and multiplicity. Every body is evaluated over its complete
checked domain; selected typed addresses and source read versions remain intact.
Carried edges enqueue nodes instead of recursively appending new occurrences of
the same fold's domain to the evaluation stack. Every node owns a fresh expression
visitation set, so a previous node cannot absorb its direct dependencies.

Only after every reachable node succeeds does a reverse-edge worklist propagate
parameter occurrences to a fixed point, completing all cyclic components before
publishing reusable closures. Input-dependent, failed and pending graphs cannot
publish entries. The actual-argument path keeps the original recursive walker.
The revised harness includes all 256 directed two-node graph/parameter cases
against independent exhaustive reachability, the original lexical-stack oracle,
and the original uncached small-fold oracle across four address offsets. The
last offset intentionally omits one input coordinate, guarding against replacing
selected addresses with all matrix coordinates. Lower/upper address failures
and distinct child-parent snapshots are retained. Full Eigen6 source/assertions remain
unchanged; its harness reports compile/preparation milestones.

Governing contracts are routed by `AGENTS.md`: the compact lexical domains in
[SPEC_0032](../../spec/SPEC_0032_RANGE_PRESERVING_TENSORS.md), checked construction
in [SPEC_0036](../../spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md) and its
[catalog](../../spec/SPEC_0043_CONSTRUCTION_CATALOG.md), reference proof discipline
in [SPEC_0037](../../spec/SPEC_0037_FORMALLY_VERIFIED_COMPILER.md), and the earliest
owner workflow in [SPEC_0033](../../spec/SPEC_0033_DEVELOPMENT_PROCESS.md). The change
is local to projection; no construction rules, validators or DAE wire data change.

The final gates used the source based on
`c4968a8275aefa85cf142ee34cdde1bc0513e956`, without another agent's compiler changes:

- `cargo test -p rumoca-eval-dae --lib projection::tests -- --nocapture`:
  32 passed, zero failed/ignored, 24 unrelated tests filtered. This includes the
  exact original-walker oracle, all 256 graph cases, 4,681 lexical stacks,
  sparse selected-address controls and typed external/index refusals. The final
  gate's peak summed process RSS was 437,344 KiB.
- `cargo clippy -p rumoca-eval-dae --all-targets -- -D warnings`: passed.
  The first revised attempt reported three excessive-nesting errors in the closure
  worklist. Its computation was moved unchanged into a private free helper,
  without lint allowances, and both focused tests and Clippy were rerun.
- `cargo test -p rumoca --no-default-features --test suite_core
  function_projection_eigen6 -- --nocapture`: both full-source tests passed,
  zero failed/ignored, 714 unrelated tests filtered. Compilation took 2m47s;
  the two tests completed in 26.71s. The complete build/test process group's
  peak summed RSS was 1,547,024 KiB, below its 8 GiB cap.
- Rustfmt 1.95 and `git diff --check`: passed. Minimal facade compilation retains
  the existing `target_manifest.rs:657` unused-mut warning; owner Clippy is strict.

The full Eigen6 gate checks the retained 32-sweep domain, both typed output calls,
all six eigenvalue and 36 eigenvector scalar dependencies against all 36 actual
matrix coordinates, and independent numerical invariants in persistent sessions
over correlated, singular/indefinite diagonal, zero and recovery inputs. Sorted
finite eigenvalues, `AV=VΛ` and `VᵀV=I` tolerances remain unchanged. The new executable
SHA-256 is `4fcf11f1fabda7354cd1f601516f7af39010f1cda5b985f7eb3d2b852ce3af30`.
This is native correctness/liveness evidence, not an actual-WASM or full-SLAM pass.

Final logs are `/tmp/slam-projection-graph-tests-final.log`,
`/tmp/slam-projection-graph-clippy-final.log` and
`/tmp/slam-projection-graph-eigen6.log`; each has an adjacent `-watch.log` and
`.handles` recording owned process groups and terminal status. All gates and
watchers exited zero. No process remains live. The private target finished at
2,480,988 KiB; no shared cache artifacts were moved or deleted.

The revised gates used a private disk target at
`/tmp/slam-projection-graph-target`, capped at 7.5 GiB including its `TMPDIR`.
The exact dependency-fingerprint closure of the previous minimal-feature umbrella
was copied read-only from the shared cache; the old failed executable was excluded.
All 4,667 copied files have matching source/destination SHA-256 hashes. The
copy manifest is `/tmp/slam-projection-private-cache-copy-manifest.json`, SHA-256
`bbe1ec59b347b2024a94d44d039f4475e79a137f4a91bced46ee60d3ad76d5d2`.
The selected closure has no missing fingerprint dependencies and initially occupied
about 2.25 GiB. Cargo rebuilt the changed graph source before executing it. The
cache copy is preparation evidence, separate from the test results. All commands
ran through `nix develop path:../slam_web#ci` with `CARGO_INCREMENTAL=0`,
`CARGO_BUILD_JOBS=2`, `RUST_TEST_THREADS=2`, `RAYON_NUM_THREADS=2`, cores 6 and 7,
nice 10, a 600-second maximum time limit and the owned-process-group RSS/cache
watchdog. The shared cache and other agents' artifacts remain unchanged.

## Original prototype evidence and failure

The unchanged `SymmetricEigen6.mo` fixture compiles, but the browser session
constructor exceeded a 90-second watchdog. A 15.074-second profile of that
constructor attributed 71.83% of samples inclusively to projection fold/expression
walking and 51.46% to `expression_domain_context`; recursive names were counted
once per CPU sample. The responsible owner is `rumoca-eval-dae::projection`, before
prepared assignment execution. This is separate from assignment preparation.

The optimization gives each exact lexical domain-point sequence a private identity
within one projection. It preserves domain IDs, point values, stack order and
multiplicity. Derived lookups are invalidated on every push/pop. An unchanged stack
no longer clones its point vectors for every expression visit. These identities
never enter the DAE or another projection query.

A successful completed fold may be reused only within its current generic function
summary capture, keyed by function/fold/carried value/field/scalar and the exact
lexical parent context. The actual-argument specialization path keeps its original
walk. Failed projections and active invocations cannot become completed entries.
A coarse active guard can suppress an edge at another lexical context; that makes
all active ancestor closures incomplete and therefore ineligible for reuse. Typed
source, read-version and runtime address operations are retained.

Focused evidence:

- 28 projection regressions pass, including original uncached fold-walk comparison.
- Every lexical stack of length zero through four over four distinct typed scopes
  and two values matches the original exhaustive lexical filtering. Siblings,
  shadowed display names, repeated IDs and reversed stack order remain distinct.
- An unchanged stack builds one context for 1,001 lookups.
- The positive fold fixture visits the same exact coordinates while reducing
  original fold walks from four to two, with two completed-fold reuse hits.
- The nested, shadowed, interdependent carried-value fixture exercises cross-context
  suppression, retains identical original incidence and walk counts, and caches no
  partial closure. A separate unrelated carry stays outside the projected result.
- Repeated failed index 8 into extent 7 remains an exact refusal; input-controlled
  call-site selectors remain distinct and index 4 into extent 3 remains a refusal.
- Strict `cargo clippy -p rumoca-eval-dae --all-targets -- -D warnings`,
  formatting and whitespace checks pass. Two overly nested test helpers were
  extracted without changing their assertions; the 28 regressions were rerun.

The full 32-sweep fixture is an exact copy of the application source, SHA-256
`1c68c6fa5b9e9c65136b13b4b9838e49a7ecb5ebb0457d93ac3b674e31d32edd`.
Its integration regression covers every scalar of both typed output calls, all 36
input matrix coordinates, and persistent numerical sessions over a correlated
matrix, a singular/indefinite diagonal matrix, zero input and recovery. Eigenpair
residuals, sorted eigenvalues and orthonormal vectors are checked independently.
The default-feature native facade build stopped before either test ran. Its first
attempt failed because the CI shell lacked the installed libudev pkg-config path;
that was corrected only for the next process. The second attempt reached the
facade but hit its 600-second compilation watchdog while unrelated MSL workers
contended with the reserved nice-10 cores. No numerical/session pass or failure
was observed.

The subsequent `cargo test -p rumoca --no-default-features --test suite_core
function_projection_eigen6 -- --nocapture` compiled the proper umbrella in
489 seconds. Both unchanged full-source tests then ran for approximately 111
seconds before the combined 600-second watchdog expired; neither completed.
The minimal facade emitted an existing `unused_mut` warning in
`target_manifest.rs:657`. This does not affect the strict owner Clippy result.

A separate 300-second gate ran that exact compiled binary, without recompilation
or altered assertions. Both tests remained unfinished at 143 seconds while their
shared process RSS grew to 39,516,828 KiB (about 37.7 GiB). Only that owned process
group was terminated to prevent further memory growth. The prototype's full
incidence and numerical tests therefore **never passed**. That prototype was not a
completed fix for Eigen6 liveness, and no performance improvement is claimed for it.
Future native full-fixture gates require both a time watchdog and an explicit
8 GiB process RSS watchdog.

Source inspection identifies a remaining combinatorial walk: the canonical
outer Jacobi domain has 960 checked `(sweep,p,q)` points, with a six-point child
`k` domain. Both folds carry the 36-scalar `work` and `vectors` values. Fold
boundaries bypass expression visitation, so a read of another carried scalar
recursively enumerates that fold again while the previous fold's points remain
on the stack. Repeated occurrences of the same domain ID produce distinct exact
contexts with significant order and multiplicity. Cross-context suppression
correctly prevents reuse of incomplete closures. Interning these contexts saves
individual clones but does not remove the exponential proof walk.

The `work` reads retain ten distinct checked definition ordinals
`11,25,33,34,35,37,41,42,43,51`; `vectors` retains `38,47,63,65`. Any subsequent
closure algorithm must preserve those source/read-version identities and every
actual typed address, rather than replacing them with matrix-size incidence.
This diagnosis led to the revised exact carried-dependency graph with completed
strongly connected closures and its separate exhaustive differential proof above.
All full-source assertions remain enabled and unchanged.

The combined branch's unchanged full Eigen6 actual-WASM source gate now passes
with compiler revision `943aa7d7b745eccc63ceb0f6a8f678f1542b60c7`. The compiler
WASM SHA-256 is
`a2310458a4723af3c43d1934b50d8ad7509e247f58465b2c1beb7e3b0eb5f135`.
The application record `slam_web/dev/modelica-fusion-eigen6-verification.json`
retains source SHA-256
`1c68c6fa5b9e9c65136b13b4b9838e49a7ecb5ebb0457d93ac3b674e31d32edd`.
All 42 outputs of the full 6x6, 32-sweep model pass over seven spectra, with finite
and ordered eigenvalues, eigenpair residual, orthonormality, full reconstruction
and planar nullspace assertions. The entire preparation/numerical gate takes
56.701 seconds, with 501,152 KiB peak aggregate RSS under 120-second/8-GiB limits
on cores 4/5; log `/tmp/slam-eigen6-943aa7d7b.log`.

This is actual WASM execution under Node, not complete browser graph acceptance.
Elapsed includes preparation and all calls; standalone kernel speed and isolated
before/after projection timing are still unmeasured. Production pin and runtime
integration remain unchanged.
SPEC_0033 Tier 1 fixed 20-model canary comparison and its durable delta remain
required on the integrated branch; these focused tests are not a canary or a cohort
parity claim.
