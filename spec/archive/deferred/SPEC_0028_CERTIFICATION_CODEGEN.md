# SPEC_0028: Safety-Oriented Code Generation

## Status
DEFERRED

> This is non-active future work. It does not gate the 0.9 release or normal
> PR review, and it must be reviewed by qualified certification expertise before
> promotion to an active spec.

## Summary
Rumoca should keep generated-code architecture friendly to later safety review without making certification promises before expert review.

## Motivation

- Safety-oriented generated code needs source traceability and deterministic output.
- eFMI, DO-178C/DO-330/DO-331, and MISRA-style constraints may affect future
  codegen design.
- The current 0.9 priority is a clean compiler architecture, not a qualified
  code generator.

## Specification

Future work should evaluate these design targets before this spec is promoted:

| Target | Likely Owner | Why |
|---|---|---|
| Generated lines trace back to IR spans | `rumoca-phase-codegen` | Reviewers need source-to-output evidence |
| Output is byte-deterministic | codegen/templates | Reproducible builds and audits |
| Solve operations stay backend-neutral | `rumoca-ir-solve` | Certification targets should not leak into IR |
| Generated C has bounded storage | C templates | Avoid dynamic allocation surprises |
| Optimization can be disabled or explained | codegen/phase passes | Auditable transformation history |
| eFMI/GALEC target feasibility is assessed | future backend/codegen work | Avoid inventing a parallel standard |

Any active version of this spec must use current Rumoca terminology and actual
types. It must not reintroduce old operation names or deleted spec references.

## Promotion Criteria

Promote this only after:

| Requirement | Evidence |
|---|---|
| Certification expert review | Written review notes or issue links |
| Current codegen types documented | Links to actual structs/functions |
| Traceability path proven | Test showing source spans reach generated output |
| Determinism gate exists | Repeated codegen output comparison |
| Scope is explicit | Clear statement of qualified-tool vs verified-output path |

## Open Questions

- Is verified generated output more practical than qualifying the generator?
- Should eFMI/GALEC be a primary target or an export option?
- What generated-code subset is realistic for initial MISRA-style review?
- Which optimization records are useful before optimization passes mature?

## References

- RTCA DO-178C / EUROCAE ED-12C
- RTCA DO-330 / EUROCAE ED-215
- RTCA DO-331 / EUROCAE ED-218
- MISRA C:2023
- eFMI Standard: https://efmi-standard.org/
