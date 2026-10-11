# Native projection preparation follows issued refresh demand

This source-only candidate is based on d65. The actual current Euler38 failure
and first-fault native preparation chain are recorded in
[euler425-d65-current-core-before.md](euler425-d65-current-core-before.md).
No repaired runtime or memory result is claimed here.

The governing contracts are SPEC_0043 §6a (constructor-issued refresh facts,
source-bound applications, unchanged initialization/sensitivity domains),
SPEC_0032 §§2/4 (derived scalar views and target kernel preparation),
SPEC_0029 (backend-neutral runtime ownership), and SPEC_0033 §§2/3 (actual
reproduction and earliest responsible consumer). Initialization remains the
simultaneous MLS §8.6 system. This change does not alter source-language behavior,
canonical Solve data, wire versions, structural patterns, or coloring.

The existing runtime returns immediately after a complete
`causal_solution_certified` refresh schedule, including its error return. It
never falls through to projection. Previously, construction nevertheless
prepared native applications for every eligible structural projection block.
Preparation now consumes the union of block identities actually reachable by:

- Every uncertified algebraic, derivative, root, event, and clock refresh plan.
- Every uncertified issued derivative/event-settled remainder, conservatively
  including the algebraic-after-derivative relation.
- Initialization's existing recorded settled-read cones. Per-row residual
  probes project these subsets directly even when the full refresh is causal.

Uncertified plans retain their complete simultaneous block inventory, including
stage-error fallbacks. No additional stage classifier, dependency evaluator,
coloring, or source reconstruction is introduced. Cone positions map through
the owning algebraic refresh's existing structural block indices; invalid
positions or structural indices refuse with typed errors. Empty demand never
calls the backend preparation callback. Required backend preparation errors
still propagate.

The initialization complete-view path (no recorded subset) executes the same
regular refresh, already included above. Initialization tangent and other
parameter sensitivity probes use `SolverYAndParameters`, whose prepared
SolverY application entry explicitly declines. State manifold projection has
its separate existing owner. Reduced-chart conditioning and implicit row reads
use row/JVP entries, not the complete prepared application. Alternate charts
retain the existing exact source/pattern/color reuse checks; no new alternate
preparation or cached values are introduced.

Source-derived focused gates, all UNRUN for this candidate:

```sh
cargo test --locked --offline --jobs 4 -p rumoca-solver --lib projection_demand_ -- --nocapture
cargo test --locked --offline --jobs 4 -p rumoca-solver --lib prepared_projection_jacobian:: -- --nocapture
```

The first selects four tests (causal zero-preparation, required failure,
initialization cone retention, and foreign cone refusal). The complete module
selects ten, including the six existing freshness, fault, source-owner, and
seed-domain controls. The test-only BEFORE commit selects the first two new
tests on the unchanged producer; expected failure is not an actual result.

Final issue qualification still requires both original Euler38 tests, including
the unchanged 2 GiB child address-space limit and actual native path. The outer
resource guardian remains 600 seconds, 4 GiB aggregate RSS, 8 GiB owned cache,
8 GiB scratch floor, and 16 GiB available-memory floor. A constructor success
alone does not prove the ten-step quiescent curve or close issue #425.
