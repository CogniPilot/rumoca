# Fourbar executed-code comparison at be97

The timed Rumoca backend is Cranelift-generated native kernels plus shared Rust
Model Exchange projection. No Rumoca-generated Fourbar C is executed in this
benchmark. OMC's retained executable uses generated C and its numerical runtime.
Comparing a Rumoca C-export template to this benchmark would compare different
products. The `c-ode` target does not supply this implicit Fourbar runtime.

## Fresh evidence

Rumoca capture: `/tmp/rumoca-fourbar-projection-repeat-perf-be97/`.
`capture-identity.json` binds clean source `be97aace9213da677e123b0668e309fe93ac6e3a`,
ELF, request, worker identity and interior simulation window216113–216118.
One run: exit0, sim_ok, 940 CPU-clock samples, zero lost samples. Its trace SHA
`7f0ebb08c690e4654500d632becfd2a34c53dde069ead992d3cfd3b15ee40315`
matches canonical be97 and the retained f781 trace. Instrumented simulation
6.365720149s is not a replacement for canonical timing.

Fresh self samples: memcmp7.77%, affine torn solve7.34%, memmove6.49%, checked
pivot6.38%; generated assignment schedules3/7/17/20 contribute5.43/4.26/3.09/2.87%.
These self entries are distinct. Stack-presence counts in the same940 samples:
affine projection370, scaled torn delta204, nonlinear torn projection155,
observable error norm365, reduced nonlinear Jacobian75. These inclusive counts
**overlap**, and incomplete unwinding can omit callers. They do not partition
runtime or establish invocation counts. Fresh libc copies/comparisons have not
all been assigned to particular source buffers.

[OMC capture and generated equations](omc-fourbar-runtime-math-and-capture.md):
298 whole-process CPU samples, zero lost; outputCSV is byte-identical to the
retained reference. It observes LAPACK `dgesv` in linear/nonlinear solve paths
and the generated analytic nonlinear Jacobian. The capture includes
initialization andCSV output, unlike the Rumoca interior Sim window. No direct
ratio between their sampled percentages is valid.

## Concrete algorithm differences

OMC's generated runtime equations separate a12-variable nonlinear position
closure from linear2/16/20 torque, velocity and acceleration systems. Both
compilers perform causal lowering and both call runtime numerical solvers.
Rumoca's prior616-coordinate affine owner uses16 tears and600 causal coordinates;
it does **not** perform a dense616-variable LU. Different grouping prevents a
one-to-one identification from dimensions. In particular OMC's observed two-
variable torque system is outside the previously identified Rumoca affineowner.

OMC executes a generated analytic nonlinear Jacobian callback. In Rumoca,
`runtime/projection/tearing.rs::reduced_jacobian` perturbs each tear variable,
repeats the full causal sweep and differences both residuals and recovered
coordinates. Replacing that with a construction-backed tangent schedule could
remove repeated perturbation work, but must preserve recovered-coordinate
checks, branch/guard behavior and final residual certificates. Its75 attributed
samples alone cannot explain or close the full performance gap.

A second candidate is compiling more of the fixed affine coefficient/recovery
schedule, currently executed by generic Rust loops. Existing sparsity and
tearing are already used. The earlier recovery-support micro-optimization did
not prove a wall-time benefit and remains removed. Neither candidate is yet a
validated optimization, and neither justifies weakening the observable error
norm or reusing numeric results across different semantic points.

## Equation mapping and follow-up

A diagnostic run of the same frozen be97 worker emitted exact25MB Solve JSON:
`/tmp/rumoca-fourbar-be97-ir/worker/ir-solve.json`. Only artifact flags and output
directory changed; simulation succeeded with the same trace SHA. Human-readable
Modelica export reported an unsupported function assertion; no `.mo` Solve
artifact was produced. Machine JSON supplies variable/equation ownership for
the mapping below. No performance claim is based on this artifact-producing run.

The subsequent `semantic-map.json` in the profile directory confirms owner1531's
rows/columns match the retained witness exactly. Representative current blocks:
1035:279coordinates/6position tears; 1217:362coordinates/6first-derivative tears;
1515:15coordinates/6torque tears; 1531:616coordinates/16acceleration-force tears.
Named-variable overlaps put OMC1946's two torque unknowns in1515, OMC2180's
joint speeds in1217, and OMC1921's joint angles in1035. These are descriptive
correspondences, not algebraic-equivalence proofs.

The velocity hypothesis was confirmed and repaired in the typed Identity
interaction rule; see [the proof](identity-affinity-proof.md). Block1217 is now
affine and block1035 remains nonlinear. The complete ce1 cohort preserves exact
211raw/192compared-high identities,19 exclusions,0missing; historical gate
failures remain disclosed in [the full audit](identity-affinity-full-ce1a85bc-audit.md).

The subsequent [ce1 diagnostic](schedule-owner-attribution-ce1a85bc.md) binds
all19 emitted schedule symbols to issued owners and the JIT map. Of939 interior
CPU samples,69 land in assignment schedules;64 belong to guarded position
block1035, split across algebraic and derivative refresh contexts. The
[independent integrity audit](schedule-owner-capture-integrity-ce1a85bc.md)
confirms a byte-identical canonical ce1 trace and zero lost samples. All probes
were removed. Lazy diagnostic writes can occur inside Sim, so this is
attribution evidence, not a speed comparison. Generic affine solving and scaling
remain substantial costs; neither a sparse-Jacobian switch nor an analytic
position Jacobian alone is demonstrated to close the speed gap.
