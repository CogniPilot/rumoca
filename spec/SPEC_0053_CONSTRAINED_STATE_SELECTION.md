# SPEC_0053: Constrained State Selection

## Status
PROPOSED

## Summary
Select independent integration coordinates from constrained DAE systems and
construct their value, derivative, and FMI mappings together.

## Specification

This proposal extends SPEC_0007 / STRUCT-T07. Source signature analysis and
coupled formal derivatives and candidate coordinate maps are implemented; executable independent
state selection remains pending.
It does not change the current acceptance profile. The existing implementation retains
lower-order constraints in `ContinuousSolveSystem::manifold_residual` and
`manifold_projection_plan` (`rumoca-ir-solve/src/model.rs`), while
`SolveRuntime` evaluates derivatives of every retained state coordinate.

### 1. Compiler ownership

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Derive independent coordinates and dependent differential coordinates from source-bound equations, derivative incidence, and initialization obligations before constructing executable derivative kernels | structural reduction | Avoid an ill-conditioned ambient ODE with unnecessary independent directions |
| Keep source tensors and equation families authoritative; coordinate selection is a checked aggregate map over their scalar views, never a replacement collection of scalar declarations | DAE and Solve construction | Preserve SPEC_0032 ownership and provenance |
| A construction witness binds each candidate set, its integration dimension, reconstruction equations, derivative equations, and coordinate maps to the same DAE | checked Solve construction | Matching alone does not establish numerical regularity |
| Honor `StateSelect` and `reinit` requirements by typed coordinate identity; an inconsistent requested basis fails explicitly | structural reduction | Preserve MLS state-selection semantics |
| Preserve every source equation, assertion, initialization condition, and visible variable when changing differential roles | structural reconstruction | Coordinate choice cannot change the source solution set |
| Prove dependency closure for the selected derivative outputs; dependent derivatives execute only when needed by that closure or an observation | Solve planning | Removing outputs must not discard needed equations or compute avoidable derivatives |

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
structural matching. It does not issue a numerical regularity certificate or an
executable state basis. The remaining basis-selection and runtime obligations
below are still proposed.

`FormalDerivativeView::stages` now exposes complete source-bound equation and
coordinate owners at each derivative order minus certified tensor offset.
These borrowed stages partition the formal system and retain compact tensor
domains. Their dimensions are structural freedoms, not numerical rank results;
numerical selection must still certify the relevant stage Jacobians.

### 2. Value and derivative agreement

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
