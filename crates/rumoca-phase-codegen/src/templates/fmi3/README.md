# `fmi3`

## Use case

Use this target for FMI 3.0.2 exchange with native array variables. The emitted
source-code FMU supports both Model Exchange and Co-Simulation over the same
checked component state.

## Contract

- Readiness 1: official schemas, an independent importer, ABI lifecycle tests,
  and numerical ME/CS trace parity validate the source FMU.
- Input: one checked FMI component aggregate linking tensor-shaped DAE variables
  to exact Solve storage runs.
- Output: flat `.fmu`, `modelDescription.xml`, `buildDescription.xml`, and C
  source declared by the package.
- FMI 3 value references address complete tensor variables with declared
  dimensions.
- ME and CS are lifecycle profiles of one kernel, not independent lowerings.

## Unsupported

Public variables keep their FMI value types: Real as Float64, Integer and
enumeration ordinals as Int32, Boolean as Boolean, and String parameters and
constants as String with their literal start text. A String whose declaration
gives no literal start is refused.

The current profile supports parameter-dependent error assertions, including
assertions inside pure array functions, and discrete equations and condition
memories determined by parameters alone. The checked profile retains the
predicates and original messages; invalid parameters fail initialization or the
next FMI evaluation after a legal parameter change. Discrete equations settle
with the parameter bindings in dependency order. Time-, state-, and
input-dependent assertions and discrete updates remain outside this profile.

Dependent parameters retain their Solve initialization assignments, evaluated
in dependency order, and export as calculated parameters. Changing an
independent tunable parameter recomputes these bindings before checking
assertions or reading outputs. An initialization residual runs the runtime's
settled initialization (bindings and the initialization projection alternate
until the bindings stop changing, each residual row evaluated after the
algebraic refresh) through the shared projection kernel at the FMI tolerance.
Initialization rows over declaration seeds, homotopy continuation, delay
histories, and retained state-manifold rows are refused.

The current profile rejects general events, clocks, runtime event history,
external calls/tables, and random operations. Algebraic systems run the shared
ME projection kernel of SPEC_0044 ME-PROJ-001. Non-finite refreshed values fail
the FMI call and a failed CS step rolls back its state. Unimplemented FMI
capabilities are absent from metadata and reject at the ABI.

The C sources are split into translation units that compile in parallel:
`model.c` (FMI surface, refresh, kernel), `rmc_assign.c` (exact assignments),
`rmc_rows.c` (residual rows), `rmc_jacobian.c` (forward Jacobians),
`rmc_isolators.c` (isolators and causal runs), and `rmc_functions.c` (pure
functions), sharing `model.h`.

## Verification

- `suite_template_runtime` validates official FMI 3.0.2 schemas, FMPy, FMI-VDM, source
  declarations, direct C ABI lifecycle, native-array access, and execution.
- ME and CS traces are checked against the analytic solution, FMI 2, and the
  linked checked kernel.
- Metadata and ABI negative controls prove unsupported capabilities stay absent.
- `packaged_fmi_tensor_products_and_transpose_match_independent_numpy_values`
  checks rectangular matrix/vector products and rank-three transpose against
  NumPy through both interfaces and FMI versions.
- `cargo test -p rumoca --features template-runtime-tests --test suite_template_runtime -- backend_template_runtime_regression::fmi2_and_fmi3_consume_an_exact_isolable_algebraic_schedule`
  proves both FMI consumers accept the checked exact schedule through their shared C kernel.
- `cargo test -p rumoca --features template-runtime-tests --test suite_template_runtime -- backend_template_runtime_regression::fmi3_exact_runtime_`
  compiles and runs final-RK4 refresh, non-finite rollback, and chained-singleton
  order discriminators against the rendered C.
- `cargo test -p rumoca --features template-runtime-tests --test suite_template_runtime -- backend_template_runtime_regression::fmi2_and_fmi3_reject_a_tunable_algebraic_coefficient`
  proves the explicit profile fails closed when a runtime coefficient would
  require a residual solver.

## Example

```sh
rumoca compile Plant.mo --model Plant --target fmi3 --output generated
fmpy validate generated/Plant.fmu
```
