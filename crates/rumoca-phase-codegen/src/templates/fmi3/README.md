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

The current profile rejects events, clocks, runtime event history, external
calls/tables, random operations, and coupled or non-isolable implicit residual
systems. A continuous algebraic system is supported only when every canonical
BLT block is a singleton with a construction-checked exact assignment. The
issued schedule preserves causal BLT order, including chains between singleton
algebraics. Dynamic-coefficient, coupled, and non-isolable systems remain
unsupported. Non-finite refreshed values fail the FMI call and a failed CS step
rolls back its state. Unimplemented FMI capabilities are absent from metadata
and reject at the ABI.

## Verification

- `suite_template_runtime` validates official FMI 3.0.2 schemas, FMPy, FMI-VDM, source
  declarations, direct C ABI lifecycle, native-array access, and execution.
- ME and CS traces are checked against the analytic solution, FMI 2, and the
  linked checked kernel.
- Metadata and ABI negative controls prove unsupported capabilities stay absent.
- `cargo test -p rumoca --features template-runtime-tests --test suite_template_runtime -- backend_template_runtime_regression::only_fmi3_consumes_an_exact_isolable_algebraic_schedule`
  proves only the FMI3 consumer accepts the checked exact schedule.
- `cargo test -p rumoca --features template-runtime-tests --test suite_template_runtime -- backend_template_runtime_regression::fmi3_exact_runtime_`
  compiles and runs final-RK4 refresh, non-finite rollback, and chained-singleton
  order discriminators against the rendered C.
- `cargo test -p rumoca --features template-runtime-tests --test suite_template_runtime -- backend_template_runtime_regression::fmi3_rejects_a_tunable_algebraic_coefficient`
  proves the explicit profile fails closed when a runtime coefficient would
  require a residual solver.

## Example

```sh
rumoca compile Plant.mo --model Plant --target fmi3 --output generated
fmpy validate generated/Plant.fmu
```
