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
- Exact singleton algebraic assignments use the same checked schedule and C
  kernel as FMI 3. Outputs refresh after input, time, and state changes.

## Unsupported

The FMI 2 target shares the FMI 3 C kernel and profile: see the Unsupported
section of the `fmi3` target. Integer and enumeration ordinals export as
Integer, Boolean as Boolean, and String parameters and constants as String. It
does not advertise state serialization or derivatives it does not implement.

## Verification

- `suite_template_runtime` validates the official FMI 2.0.5 XSD, archive layout, FMPy,
  FMI-VDM, source compilation, direct C ABI lifecycle, and ME/CS execution.
- Tensor decay traces are compared with the analytic solution and FMI 3.
- Algebraic chains execute through both ME and CS with input overrides and
  analytic checks on every exported output sample.
- `packaged_fmi_tensor_products_and_transpose_match_independent_numpy_values`
  checks rectangular matrix/vector products and rank-three transpose against
  NumPy through both interfaces and FMI versions.
- `packaged_fmi2_and_fmi3_execute_reported_output_equation_and_pid` covers the
  exact #346 model, a PID output, and algebraic feedback into a derivative.
- Broken XML and duplicate-name FMUs prove independent validators reject.

## Example

```sh
rumoca compile Plant.mo --model Plant --target fmi2 --output generated
fmpy validate generated/Plant.fmu
```
