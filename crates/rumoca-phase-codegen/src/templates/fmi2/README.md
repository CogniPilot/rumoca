# `fmi2`

## Use case

Use this target to exchange a Rumoca model with tools that require FMI 2.0.5.
One source-code FMU advertises both Model Exchange and Co-Simulation so the
importer can own integration or use the component's fixed integrator.

## Contract

- Readiness 1: official schemas, an independent importer, ABI lifecycle tests,
  and numerical ME/CS trace parity validate the source FMU.
- Input: one checked FMI component aggregate linking DAE metadata to one checked
  executable kernel.
- Output: a flat `.fmu` plus unpacked source FMU with FMI 2.0.5 metadata.
- FMI 2 scalar value references are external views of tensor-native variables;
  they do not scalarize compiler IR.
- ME and CS share state, initialization data, and equation evaluation.

## Unsupported

The current profile rejects events, clocks, runtime event history, external
calls/tables, random operations, and implicit residual systems. It does not
advertise state serialization or derivatives it does not implement.

## Verification

- `suite_fmi` validates the official FMI 2.0.5 XSD, archive layout, FMPy,
  FMI-VDM, source compilation, direct C ABI lifecycle, and ME/CS execution.
- Tensor decay traces are compared with the analytic solution and FMI 3.
- Broken XML and duplicate-name FMUs prove independent validators reject.

## Example

```sh
rumoca compile Plant.mo --model Plant --target fmi2 --output generated
fmpy validate generated/Plant.fmu
```
