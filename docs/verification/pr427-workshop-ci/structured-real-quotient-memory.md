# Certified compact Real quotient memory

Source candidate `d458700c3b621931c7cfa4d8fdf032a36c3c2568`, based on
`c01db27355d9be5836c605acf80ed84325b7168f`. This is a source freeze;
**Cargo qualification is UNRUN**. Formatting and `git diff --check` pass.

The unchanged public `structured_real_quotient_keeps_each_domain_point_boundary_and_trace`
uses `y[i]=mod(8*time+i,7); der(x)=y` for `i in 1:2`. Its actual B4 BEFORE
(`b4e38503723816d0529168c46ac1b027ca1ea73d`) fails at Interpreter
`x[2](0.7)=2.83499971322463`, independently expected `2.835`. The unchanged
bound is `7.67e-8`; the error is approximately `2.86775e-7`. Auto is not reached.
The retained guarded attempt is
`pr427-real-structured-quotient-trace-1-b4-20261011`; its raw log SHA-256 is
`eb1dc54864edd163e78c04fa34301fabd3764e4431722c096b38bd35ef727753`.
[The scalar-control record](real-quotient-integral-before-b4.md) preserves the
competing numerical hypotheses and actual before evidence.

The first differing owner is Solve quotient memory construction. The checked
DAE already retains the exact structured domain, quotient batch and root family.
The scalar repair buffers `ModelEvent` Real steps, but omits
`StructuredModelEvent`. Thus each RK trial recomputes Floor/Trunc across the
boundary instead of using the held discontinuous part required by
[MLS §8.5](https://specification.modelica.org/maint/3.7/equations.html#events-and-synchronization).

The repair reserves one private contiguous P span per issued domain and lowers
one Map over that domain. Source index registers remain symbolic; their affine
strides are not recorded as base-point constants. Readers use the same issued
DomainId and canonical ordinal mapping, with an exact scalar-ABI progression
proof. No per-point program, target list, or parallel domain owner is built by
Solve lowering. Function/noEvent evaluation and exact Integer transport retain
their existing owners.

The existing QuotientMemory role now also certifies a whole Map: private target
span, canonical complete target map, exclusive contiguous root range, matching
Floor/Trunc producer, exact live ratio at every existing generated root view,
root zero policy, unclocked FollowCurrent updates and integrator restart. Root
proof streams adjusted constants through the existing shared symbolic checker;
it allocates no pointwise LinearOp programs and uses no second evaluator.
Repeated constant-stride entries accumulate in the same order as the runtime.
Existing root dependency certificates still own algebraic refresh policy.

Initialization evaluates the **same prepared Map producer**, from the current
solved-state trial, and publishes all numeric results after evaluation. Existing
initialization fixed-point and algebraic refresh orchestration retain authority.
It uses reusable fallible runtime scratch, rather than a duplicate initialization
Map. The two-period shifted control starts with step two, so zero-filled hidden
memory cannot pass; both ascending and descending source domains retain the
same independent eleven-sample analytic traces.

Current generated Solve targets do not consume this initialization role. Their
common checked renderer refuses explicitly; it cannot publish a project with an
empty initializer. DAE/Flat inspection exports remain separate. This patch does
not claim structured quotient template/FMI support or native Map execution
parity: preparation uses the existing evaluator scalar adapter for event rows.

The representation cutover is Solve **76 → 77**. Old wire fails closed. The six
WASM positives must be reissued by the actual qualified current source producer;
no fixture metadata is relabeled. Governing active contracts are SPEC_0007
Stage 4 / SPEC_0040 SOLVE-C14 and SOLVE-C20, SPEC_0032 §§2–4,
SPEC_0029 §§3/3a, SPEC_0033 §§2–4, and MLS §§3.7.2, 8.5 and 8.6.
SPEC_0036/0037 remain DRAFT.

Focused qualification commands, using the ordinary CI-selected Cargo suites:

```sh
cargo test --locked --offline -p rumoca-ir-solve --lib quotient_memory:: -- --nocapture
# 10 tests: existing scalar certificate4 + structured certificate6
cargo test --locked --offline -p rumoca --test suite_core structured_real_quotient -- --nocapture
# 3 tests: unchanged original trace, shifted initial-step/direction control,
# and explicit generated-target refusal
```

The structured certificate tests cover current-wire replay, both directions,
wrong root order, duplicate stride tampering, forged initialization ownership,
reserved pre storage, two complete disjoint Map owners and overlapping current-wire tampering,
incomplete root range, observation refresh, foreign zero
policy and missing refresh metadata. Assertions and numerical tolerances of the
original trace are unchanged. After receipts, broader affected suites, new fixture
issuance and exact-head CI are pending ROOT qualification.

The shared `validate_structured_discrete_shape` constructor in ir-solve proves
scalar/structured and inter-structured target exclusivity, including ordinary
B.1c Maps. The numeric certificate does not add a second ownership checker.
A complete two-Map positive control and overlapping-current-wire negative
control exercise that existing authority. Constant-stride coefficients use an
explicit zero-start left fold in original metadata order, then dimension-order
`value += coordinate * coefficient`, matching both existing Map evaluator
plans. This avoids relying on the floating-point iterator Sum identity.
The producer functions live in the ordinary `events::quotient_memory` module;
`events.rs` stays below the active 2,000-line architecture ceiling.
