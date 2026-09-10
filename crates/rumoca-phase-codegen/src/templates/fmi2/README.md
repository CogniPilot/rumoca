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

The current profile supports parameter-dependent error assertions, including
assertions inside pure array functions. The checked profile retains their
predicates and original messages; invalid parameters fail initialization or the
next FMI evaluation after a legal parameter change. Time-, state-, and
input-dependent assertions remain outside this profile.

Dependent parameters retain their Solve initialization assignments and export
as calculated parameters. Changing an independent tunable parameter recomputes
these bindings before checking assertions or reading outputs. Initialization
residuals, projections, and state assignments remain unsupported.

The current profile rejects general events, clocks, runtime event history, external
calls/tables, random operations, and coupled or non-isolable implicit residual
systems. Tunable algebraic coefficients still require a residual solver. It does not
advertise state serialization or derivatives it does not implement.

## Verification

- `suite_template_runtime` validates the official FMI 2.0.5 XSD, archive layout, FMPy,
  FMI-VDM, source compilation, direct C ABI lifecycle, and ME/CS execution.
- Tensor decay traces are compared with the analytic solution and FMI 3.
- Algebraic chains execute through both ME and CS with input overrides and
  analytic checks on every exported output sample.
- `packaged_fmi2_and_fmi3_execute_reported_output_equation_and_pid` covers the
  exact #346 model, a PID output, and algebraic feedback into a derivative.
- Broken XML and duplicate-name FMUs prove independent validators reject.

## Example

```sh
rumoca compile Plant.mo --model Plant --target fmi2 --output generated
fmpy validate generated/Plant.fmu
```
