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
calls/tables, random operations, and implicit residual systems. Unimplemented
FMI capabilities are absent from metadata and reject at the ABI.

## Verification

- `suite_fmi` validates official FMI 3.0.2 schemas, FMPy, FMI-VDM, source
  declarations, direct C ABI lifecycle, native-array access, and execution.
- ME and CS traces are checked against the analytic solution, FMI 2, and the
  linked checked kernel.
- Metadata and ABI negative controls prove unsupported capabilities stay absent.

## Example

```sh
rumoca compile Plant.mo --model Plant --target fmi3 --output generated
fmpy validate generated/Plant.fmu
```
