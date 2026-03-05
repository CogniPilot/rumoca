# rumoca-tool-lint

Modelica lint engine for style, quality, and best-practice diagnostics.

## Role in Rumoca
Runs lint rules over source text and is reused by CLI, LSP, and bindings for consistent lint behavior.

## Public Surface
- Core lint API: `lint(source, file_name, &LintOptions) -> Vec<LintMessage>`.
- Public message/rule types: `LintMessage`, `LintLevel`, `LintRule`.
- Public config types/helpers: `LintOptions`, `PartialLintOptions`, config loading helpers.

## Inputs
- Modelica source text plus lint configuration.

## Outputs
- Structured lint diagnostics with severity and location context.

## Design Constraints
- Deterministic rule evaluation for CI/editor use.
- Keep linting independent from full compile/sim pipelines.
- Share one lint behavior across all consuming frontends.
