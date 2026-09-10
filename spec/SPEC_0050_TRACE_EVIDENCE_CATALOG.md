# SPEC_0050: Trace Evidence Catalog

## Status
REFERENCE

## Summary
Lookup catalog for the trace-production and rejection rules governed by
SPEC_0033 §6a.

## Specification

Rows below are normative by reference from SPEC_0033 §6a and add no independent
requirements.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Produced traces MUST have finite nondecreasing time, unique channels, and rectangular data | trace producers | Interpolation needs a valid relation |
| A time regression MUST fail unless the preceding row is settled and the shared predicate proves one semantic instant; retain that settled row unchanged | trace producers | Proximity alone cannot hide lateness |
| At one coordinate, settled replaces initialization, event-left, or nominal; event-left never replaces settled; exact duplicate nominal suppresses without reevaluation | trace producers | Preserve superdense role order |
| Published state events MUST use the common host application coordinate; localization and continuation coordinates stay private | ME host | Solvers cannot change trace semantics |
| The comparator MUST reject malformed time, names, or shape before interpolation and MUST NOT repair evidence | trace comparator | Oracles cannot manufacture evidence |

## References

- [SPEC_0033 §6a](SPEC_0033_DEVELOPMENT_PROCESS.md#6a-two-tier-verification-cadence)
  — owning development-process rules.
