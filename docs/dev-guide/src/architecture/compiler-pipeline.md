# Compiler Pipeline

The compiler pipeline is specified in `spec/SPEC_0007_IR_PIPELINE.md`.

The main flow is:

1. parse Modelica source
2. resolve names and assign stable definitions
3. typecheck and evaluate structural parameters
4. instantiate classes and modifiers
5. flatten hierarchy and connections
6. lower to DAE
7. structurally prepare and lower to solve IR when simulation needs it
8. simulate or render a target

Spans should be carried from source data through every diagnostic-producing IR.
Fallback spans are only acceptable when no source exists. If source data exists,
the correct long-term fix is to preserve it at the phase boundary where it is
lost.

Visitor APIs are preferred for shared IR traversals when they remove duplicated
walk logic without hiding the phase-specific behavior.
