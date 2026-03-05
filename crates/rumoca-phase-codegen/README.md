# rumoca-phase-codegen

Template-driven code generation from DAE IR to target runtimes.

## Role in Rumoca
Renders `rumoca-ir-dae` models into target code using MiniJinja templates.

## Public Surface
- Core API: `render_template`, `render_template_with_name`, `render_template_file`, `render_flat_template_with_name`.
- Public error type: `CodegenError`.
- Built-in template constants under `templates::*`.

## Inputs
- DAE model data and template content (built-in ID or file path).

## Outputs
- Rendered target code as strings.

## Design Constraints
- Keep target customization in templates, not hard-coded branching.
- Preserve source IR semantics without mutating model data.
- Keep codegen independent from simulation runtime policy.
