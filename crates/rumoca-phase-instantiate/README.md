# rumoca-phase-instantiate

Instantiation phase for model selection, inheritance, and modifier application.

## Role in Rumoca
Builds instantiated model views from typed class definitions, applying extends/modifier/redeclare semantics and preparing structure for flattening.

## Public Surface
- Main entry points: `instantiate`, `instantiate_model`, `instantiate_model_with_outcome`.
- Public outcome/error types: `InstantiationOutcome`, `InstantiateError`, `InstantiateResult`.
- Public helper APIs used by other crates: connection extraction helpers and inheritance/template utilities re-exported at crate root.

## Inputs
- Typed class tree plus a selected root model path.

## Outputs
- `InstancedTree` and overlay metadata for flattening.

## Design Constraints
- Respect Modelica modifier precedence and redeclare contracts.
- Keep structural-parameter decisions deterministic.
- Keep instantiation concerns separate from flattening and DAE conversion.
