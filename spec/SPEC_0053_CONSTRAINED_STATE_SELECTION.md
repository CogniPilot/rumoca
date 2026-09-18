# SPEC_0053: Constrained State Selection

## Status
DRAFT

## Summary
Select independent integration coordinates from constrained DAE systems and
construct their value, derivative, and FMI mappings together.

## Specification

This proposal extends SPEC_0007 / STRUCT-T07. Source signature analysis and
coupled formal derivatives, candidate coordinate maps, and an initial static
coordinate-selection profile are implemented under STRUCT-T07. Dynamic basis
changes remain proposed. The existing implementation retains
lower-order constraints in `ContinuousSolveSystem::manifold_residual` and
`manifold_projection_plan` (`rumoca-ir-solve/src/model.rs`), while
`SolveRuntime` evaluates derivatives of every retained state coordinate.

### 1. Compiler ownership

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Derive independent coordinates and dependent differential coordinates from source-bound equations, derivative incidence, and initialization obligations before constructing executable derivative kernels | structural reduction | Avoid an ill-conditioned ambient ODE with unnecessary independent directions |
| Keep source tensors and equation families authoritative; coordinate selection is a checked aggregate map over their scalar views, never a replacement collection of scalar declarations | DAE and Solve construction | Preserve SPEC_0032 ownership and provenance |
| A construction witness binds each candidate set, its integration dimension, reconstruction equations, derivative equations, and coordinate maps to the same DAE | checked Solve construction | Matching alone does not establish numerical regularity |
| Honor `StateSelect` and `reinit` requirements by typed coordinate identity; a `StateSelect.always` coordinate that is structurally redundant (an algebraic or output coordinate with no independent integration slot: an alias of another state's derivative, an acceleration-level derivative sensor, or a constraint/function output) is demoted per MLS 3.6 §4.8.8, while an `always` set that is infeasible among peer primary differential state candidates fails explicitly | structural reduction | Preserve MLS state-selection semantics |
| Preserve every source equation, assertion, initialization condition, and visible variable when changing differential roles | structural reconstruction | Coordinate choice cannot change the source solution set |
| Prove dependency closure for the selected derivative outputs; dependent derivatives execute only when needed by that closure or an observation | Solve planning | Removing outputs must not discard needed equations or compute avoidable derivatives |
| Classify each holonomic manifold constraint as definitional (a conserved first integral: its lower-order form is implied by the ODE, so one differentiation reconstructs a matched state derivative) or redundant (a loop closure: over-determining at the position level, closed only by differentiating to acceleration, which introduces a multiplier), carry that classification on the prepared manifold, and retain the source coordinates when every manifold constraint is definitional but reduce to an independent basis when any constraint is redundant | structural reduction classifies; Solve state selection decides | A first integral has no globally injective reduced chart, so a fixed reduced basis folds when a coordinate passes through zero, while a retained loop closure leaves a redundant acceleration residual the solver cannot integrate cheaply; the per-constraint differentiation order separates the two where no whole-model predicate can |

The `StateSelect.always` demotion is likewise structural, read from the source
role of each requested coordinate. Only a genuine state variable owns an
independent integration slot, so it is the sole coordinate whose `always` request
forces an independent basis column; `select`/`choice` in
`rumoca-phase-solve/src/state_selection.rs` keep exactly those forced, and the
selection obligation in `FormalDerivativeSystem::construct_state_candidate`
requires exactly those back. An algebraic or output coordinate is determined by
the equation system, so an `always` request on it cannot make it an independent
state; it is demoted to a top-priority eligible column that is kept as a state
whenever the stage's constraint structure admits it and released to a dependent
coordinate otherwise. `Elementary.RollingWheelSetDriving` and
`RollingWheelSetPulling` request `always` on the generalized coordinates and on
the angular-velocity sensors `der_theta = der(theta)`: the sensors and the
platform positions are algebraic and demote, so the three independent rolling
constraints reduce the velocity level without an over-forced basis, while a
velocity constraint that couples two genuine peer state candidates (each a
differentiated state in its own right) still fails explicitly because neither
peer is demotable.

The manifold classification is structural, taken from `prepare_for_solve`'s differentiation proof rather than from a whole-model predicate. A constraint whose reconstruction closes after a single differentiation is definitional (for example a unit-quaternion norm `Q*Q = 1`, whose derivative `2*Q*der(Q) = 0` follows identically from the kinematic rate equations); one that closes only at acceleration level, differentiation `maximum_order` two, is a redundant loop closure that introduces a multiplier. The structural phase tags each retained manifold row and `PreparedDae::manifold_requires_reduction` reports whether any row is redundant, so state selection reads the decision without re-deriving structure. `Rotational3DEffects.GyroscopicEffects`, a tree of quaternion bodies, carries only definitional norms and retains its source basis; `Loops.Fourbar1` and the `Constraints` loops each close a kinematic loop at acceleration level and reduce.

### 1a. Differential structure analysis

`analyze_differential_structure(DaeView)` in
`rumoca-phase-structural/src/differential_structure.rs` returns a checked
`DifferentialStructure` with source coordinates, matching, equation and variable
orders, and formal dimension. Its implemented contract lives in
[SPEC_0007 / STRUCT-T07](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#3-structural-lowering-transformation-catalog-spec_0007-structural-lowering-scope).
`construct_formal_derivatives(&Dae)` returns an inspectable `FormalDerivativeSystem`
with source-bound value/derivative coordinates and complete differentiated
equation owners. It retains original initialization and attributes, introduces
no independent state basis, and is not a prepared numerical DAE. Its contract
also lives in STRUCT-T07. Its `construct_state_candidate` method accepts source-branded
coordinate proposals and returns an inspectable `FormalStateCandidate`, preserving
source owners and appending aggregate value/derivative maps. It checks formal
dimension, distinctness, bounds, required/forbidden source states, and complete
structural matching. Consuming preparation preserves that exact transformed root for ordinary Solve
lowering and FMI projection. Numerical initialization and reconstruction remain
required before integration; structural matching is no regularity certificate.

`FormalDerivativeView::stages` now exposes complete source-bound equation and
coordinate owners at each derivative order minus certified tensor offset.
These borrowed stages partition the formal system and retain compact tensor
domains. Their dimensions are structural freedoms, not numerical rank results;
numerical selection must still certify the relevant stage Jacobians.

`lower_formal_derivative_stages` in `rumoca-phase-solve` now compiles these
owners through shared typed expression lowering and directional AD. The borrowed
analysis product retains the formal root, complete tensor captures, equation
body ordering, compact domains, and call assertions. It evaluates residuals at
supplied points; it does not issue a regular basis or an executable prepared model.

### 2. Value and derivative agreement

The static implementation first seeds needed acyclic definitions through the
shared typed lowerer, including assignment coercions. Source states, fixed
values, `always`/`prefer` guesses, and explicit overrides retain their guesses.
Exact source-bound initial-value transfers also seed aliased state coordinates.
These transfers remain trial guesses, including when their expressions depend
on an initialization unknown. Corrections retain the full residual and Jacobian.
Within each source preference class, coordinates with stated initial values
precede unfixed guesses. It then settles lower differential stages with bounded
least-squares Newton steps. Those trial points
select coordinates only: they are not initialization results or overrides.
Mandatory states are excluded from dependent-column pivoting; declared states
and their formal derivatives precede newly introduced algebraic candidates.
Every selected lower stage retains all residuals and requires a full-rank
dependent basis. Highest-derivative reconstruction retains the ordinary numerical
kernel checks.
The selected aggregate projects original start expressions in coordinate order,
retaining parameter dependencies and scalar broadcasts; formal derivatives use
unfixed zero guesses. The selected checked DAE still solves the original
initialization problem before integration. Numerical rank at a trial point never
certifies global regularity.

Here `z` denotes independent coordinates and `d` dependent coordinates. A local
coordinate representation satisfies `g(d,z,p,t)=0` with nonsingular `g_d`.
Its tangent reconstruction solves `g_d * delta_d = -g_z * delta_z`; parameter
and time derivatives additionally include their corresponding terms. This
identity establishes the local chain rule, not numerical accuracy by itself.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Reconstruct dependent values with independent coordinates fixed, and certify all retained constraints plus recovered-coordinate accuracy at unchanged tolerances | checked numerical kernel | Small residuals can hide inaccurate coordinates |
| Derive the selected RHS and its AD from the same reconstructed system and numerical coordinate meaning | Solve construction and evaluation | Projecting seeds alone is not the derivative of an unprojected callback |
| Keep differentiated implicit blocks coupled when elimination would reintroduce unneeded dependent directions; certify the original equations | structural reduction and Solve | A reduced interface cannot merely conceal an unstable full-state computation |
| Choose regular coordinate maps using bounded deterministic numerical work under the construction-issued candidate set; retain rank and conditioning checks | checked numerical kernel | Exhaustive subset enumeration and model-name choices are not a production algorithm |
| Treat rank loss, inconsistent constraints, and failed reconstruction as typed failures; changing basis cannot change the model's integration dimension | runtime | A singular model is not a license to discard equations |
| Solve the original initialization problem before mapping its result to independent coordinates; add no new fixed initial values | initialization | MLS initial obligations survive state selection |

### 2a. Changing coordinate bases (proposed, not yet implemented)

The `CircleChart.mo` fixture has `der(q)=v`, `der(v)=lambda*q`, and
`q*q=1`. Its initialized solution is a full circular rotation. The current
static `q[2], der(q[2])` coordinates fold at a quarter turn: both signs of
`q[1]` reconstruct the same selected state. A completed three-second run
selects the wrong branch even though every algebraic residual is small.
`GyroscopicEffects` exhibits the same geometric limitation through two
selected position components. This is an open correctness counterexample,
not evidence that a different fixed coordinate preference is sufficient.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Retain construction-issued candidate identities, per-stage dimensions, source constraints, and value/derivative maps in an executable selection product before supporting basis changes | structural and Solve construction | Runtime cannot recreate semantic ownership from names or transient analysis kernels |
| Keep basis choice fixed during each continuous integration segment; test its regularity with the same reconstructed point and constraint Jacobian | FMI component | Changing coordinate meaning during a numerical stage invalidates the integrator state |
| Request a coordinate-change event while the current basis is still regular; failed trial reconstruction must not become an accepted step or a branch change | FMI component and numerical host | Detecting failure after a fold cannot preserve physical continuation |
| Choose a regular alternative with bounded numerical work over the issued candidates, honoring required and forbidden states | numerical selection | Neither subset enumeration nor model-specific coordinate lists are general algorithms |
| Transfer new state values from the last consistent full physical coordinate; re-establish all original constraints and tangent reconstruction at unchanged tolerances | FMI component | A chart change preserves the physical solution, not merely its residual norm |
| Bind selection state, coordinate maps, numerical caches, and rollback snapshots atomically to one active basis | FMI component | Mixed old/new maps can produce plausible but incorrect derivatives |
| Preserve initialization, observables, integration dimension, and standard FMI event/reset behavior through every transition | compiler and FMI component | Coordinate selection is a representation change |

### 3. FMI ownership

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Expose independent states through one checked FMI inventory, using generated state variables when the source-variable mapping can change | FMI projection | Dynamic mappings need stable external identities |
| Evaluate source-variable observations through the same reconstruction | FMI kernel | Output names retain their physical meaning |
| Request Event Mode when a completed step requires a new basis; report changed state values and nominals, invalidate caches, and reset numerical history through the common host | FMI component and host | Basis changes are standard FMI state transitions |
| Keep coordinate selection inside the component; numerical plugins consume ordinary FMI states and derivatives | all integrators | Preserve SPEC_0038's single solver boundary |
| Bind every advertised deployment profile to the same coordinate contract | linked and generated components | Packaging cannot change state semantics |

### 4. Required evidence

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Prove coordinate coverage, reconstruction ownership, unchanged initialization, and tensor preservation with positive and negative construction tests | compiler suites | A dimension count is insufficient |
| Exercise singular and changing bases, time/parameter dependence, `StateSelect`, and `reinit` | compiler and FMI suites | The reduced rotation fixture is not the general contract |
| Compare value and AD callbacks after perturbed reconstruction guesses, not only at exact constrained points | numerical tests | Constraint roundoff can reintroduce cancellation |
| Retain the analytical `RateCancellation.mo` cases and the saved RevoluteConstraint failure; require a normal-budget original-model OMC comparison | focused validation | A successful alternate trajectory cannot dismiss the original failure |
| Close the `CircleChart.mo` three-second branch counterexample and complete a full revolution against its analytical solution and OMC; preserve the captured GyroscopicEffects reconstruction failure | compiler and FMI suites | Short successful runs can hide incorrect branch continuation |
| Require Tier 1 and a complete Tier 2 sweep preserving previously high models before breadth resumes | SPEC_0033 verification | Local derivative evidence is not cohort coverage |

## Rationale

The reduced rotation fixture has six retained Rumoca state scalars and four
constraints. OMC selects two independent states. At an exact constrained point,
both independent tangent directions are accurate even near the Euler singularity.
An experiment reconstructing dependent values to the existing tolerance can
still trigger the old dependent-acceleration JVP refusal. Therefore a wrapper
around the existing full-state RHS is not sufficient evidence for this proposal.
The compiler must construct and validate the reduced differential system itself.

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md), [STRUCT-T07](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#3-structural-lowering-transformation-catalog-spec_0007-structural-lowering-scope)
- [SPEC_0032](SPEC_0032_RANGE_PRESERVING_TENSORS.md), [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md), [SPEC_0038](SPEC_0038_UNIFIED_FMI_EXECUTION.md)
- [MLS StateSelect](https://specification.modelica.org/maint/3.6/class-predefined-types-and-declarations.html#stateselect)
- [FMI 3.0.2](https://fmi-standard.org/docs/3.0.2/) — ModelStructure and Model Exchange completed-step/Event Mode rules
- [Tang et al., structural offsets by fixed-point iteration](https://arxiv.org/pdf/1406.4473), §2
- [McKenzie and Pryce, structural analysis and dummy derivatives](https://orca.cardiff.ac.uk/id/eprint/100978/), 2017
- [MultiBody evidence ledger](../docs/dev-guide/src/tooling/multibody-coverage.md)
