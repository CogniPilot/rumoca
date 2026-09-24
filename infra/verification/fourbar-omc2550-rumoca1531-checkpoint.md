# Fourbar acceleration/force checkpoint (read-only)

Sources: Rumoca clean `9d7a15a8ce7a3050ce16c840d960f576f14cea8f`, captured machine Solve `/tmp/rumoca-fourbar-be97-ir/worker/ir-solve.json` (same source equations before the Identity affinity repair); OMC retained generated C under `/tmp/rumoca-fluid/target/fluid-campaign/tier2-bulk-clear-7e67b9954/omc_sim_work/`, basename `Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1`. Rumoca's timed ME path executes prepared/JIT kernels, not a Fourbar C product.

**Exact representative chain.** OMC `_03lsy.c:1939-1962` emits `eqFunction_2351..2353` as causal assignments inside `residualFunc2550` (`:2338-2343`):

```
rev1.frame_a.f[1] = -R_rel.T[2,1]*frame_b.f[2]
                     -R_rel.T[3,1]*frame_b.f[3]
                     -R_rel.T[1,1]*frame_b.f[1]
```

Rumoca Solve's `implicit_rhs.nodes[0].ScalarPrograms.programs[514]` owns global outputs/rows 1521–1523. It loads `rev1.frame_b.f[1:3]` (Y1546–1548), `rev1.R_rel.T[1:3,1:3]` (Y1558–1566), and `rev1.frame_a.f[1:3]` (Y1525–1527); the source program also loads `rev1.R_rel.w[1:3]`, which pure-call owner102 does not use in this result. Owner102 performs `transpose(R_rel.T) * frame_a.f`; the program negates it and stores the three-component residual `frame_b.f - (-transpose(R_rel.T)*frame_a.f)`. All three rows are residual rows of block1531's primary tearing; they are not causal steps. This is a verified difference in selected execution: OMC computes these frame-a force components as assignments; Rumoca evaluates the original vector relation in the reduced closure. It is **not** proof that these three rows alone account for the 616-versus-20 partition or any measured cost.

The algebraic orientation change would require a construction-owned proof that the rotation matrix is orthogonal and that the inversion preserves the source equation/domain and failure semantics. A general producer opportunity, if that proof exists, is structural/Solve lowering of rotation-owned vector equalities into checked causal inverse assignments before tearing; codegen alone must not assume generic matrix orthogonality. OMC `_03lsy.c:8241-8252` registers system2550 with 20 unknowns and a symbolic Jacobian callback; `_12jac.c:13795-13801` initializes its 20×20 Jacobian. Rumoca block1531 has 616 coordinates, 16 tears, 600 causal steps, and its affine recovery coefficients are checked at runtime. These are different partitions; their individual cost cannot be inferred from dimensions.

**Change recommendation.** The source-group opportunity is the better first scoped compiler change: `/tmp/rumoca-fourbar-schedule-owner-ce1/position-rotation-source-group.json` binds nine consecutive block1035 assignment steps to one source program/PureCall owner74. OMC emits `cse33=cos(j5.phi)` once. Rumoca native `InputResults` already caches equal call inputs, so grouping can remove repeated packing, comparison, and copying, not nine trigonometric evaluations. Reuse the checked multi-output grouping discipline in `crates/rumoca-ir-solve/src/refresh.rs:1437`, while preserving each target's write and decline/failure order in `crates/rumoca-eval-solve/src/prepared/torn_sweep.rs:183`. That proof is narrower than rotation inversion and targets an observed hot schedule; it still requires a failure-order review before implementation.

No code, build, or capture was run for this checkpoint.
