# rumoca-tool-fmt

Modelica source formatter used by CLI and editor integrations.

## Role in Rumoca
Provides formatting APIs and CLI integration points that normalize Modelica source style for local workflows and automated checks.

## Public Surface
- User/developer binary: `rumoca-fmt`.
- Core library APIs: `format`, `format_or_original`, `check_syntax`.
- Public config/error types: `FormatOptions`, `PartialFormatOptions`, `FormatError`.

## Inputs
- Modelica source text and formatting configuration.

## Outputs
- Formatted source text or format/syntax diagnostics.

## Design Constraints
- Keep formatting deterministic and idempotent.
- Keep configuration behavior consistent across CLI/editor usage.
- Avoid leaking formatter concerns into compile-phase crates.
