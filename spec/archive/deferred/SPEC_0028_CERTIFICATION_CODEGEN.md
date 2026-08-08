# SPEC_0028: Safety-Oriented Code Generation

## Status
DEFERRED

> This is non-active future work. It does not gate the 0.9 release or normal
> PR review, and it must be reviewed by qualified certification expertise before
> promotion to an active spec.

## Summary
Rumoca-generated C should support a pinned MISRA C assurance profile and
DO-178C evidence without claiming product certification before expert review.

## Motivation

- Safety-oriented generated code needs source traceability and deterministic output.
- eFMI, DO-178C/DO-330/DO-331/DO-333, and MISRA constraints affect future
  codegen design.
- The current 0.9 priority is a clean compiler architecture, not a qualified
  code generator.

## Specification

### Claim boundary and preferred assurance strategy

Rumoca should pursue a **verified-output-first** strategy: retain enough
traceability and deterministic evidence that a certification project can
independently verify each generated artifact. Tool qualification remains a
project-selectable path when repeated use makes its cost worthwhile; this
proposal does not select a Tool Qualification Level because that depends on
the tool's actual use, the software level, and whether errors can otherwise be
detected.

The phrases `MISRA compliant`, `DO-178C compliant`, `certified code`, and
`qualified code generator` are prohibited until qualified certification review
confirms the applicable process objectives and evidence. Before then, the
accurate description is `safety-assurance candidate` or `supports project
assurance evidence`.

Future work should evaluate these design targets before this spec is promoted:

| Target | Likely Owner | Why |
|---|---|---|
| Closed typed context reaches the template boundary | codegen projection | Invalid target shapes stay unrepresentable |
| Generated lines trace back to IR spans | `rumoca-phase-codegen` | Reviewers need source-to-output evidence |
| Output is byte-deterministic | codegen/templates | Reproducible builds and audits |
| Solve operations stay backend-neutral | `rumoca-ir-solve` | Certification targets should not leak into IR |
| Generated C has bounded storage | C templates | Avoid dynamic allocation surprises |
| Operation preconditions are proven or generation fails | codegen projection | Generated C must have no undefined behavior |
| Profile pins C dialect and MISRA C:2023 guidelines | assurance manifest | Compliance scope must be exact |
| MISRA Compliance:2020 process records enforcement and deviations | assurance tooling | A linter result is not a compliance claim |
| DO-178C objectives map to lifecycle evidence | evidence bundle | Source alone cannot establish compliance |
| Compiler use maps to DO-330/331/333 assumptions | assurance profile | Tool, model, and proof roles differ |
| ABI, floating-point, and runtime assumptions are explicit | target manifest | Target behavior needs a defined model |
| Translation validation checks each generated artifact | verified checker | Avoid trusting template intent alone |
| Optimization can be disabled or explained | codegen/phase passes | Auditable transformation history |
| eFMI/GALEC remains the production-C path | backend/codegen work | Avoid a parallel safety interface |
| Requirements-to-model-to-IR-to-C trace links use stable identifiers and exact byte ranges | evidence bundle | Reviews and change impact analysis need navigable provenance |
| Translation validation is independent of the code emitter and fails closed | assurance tooling | Verified output reduces reliance on trusting or qualifying the generator |
| MC/DC instrumentation maps back to model decisions without changing production bytes | coverage tooling | DO-178C structural coverage must remain explainable at the source/model boundary |
| Tool operational requirements, configuration index, problem reports, and reproducible qualification tests are retained from now | lifecycle data | Retrofitting tool-qualification history is expensive |

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
| MISRA profile and analyzer gate exist | Pinned rules, tool, config, and results |
| Deviations are explicit | Reviewed machine-readable deviation records |
| C undefined behavior is excluded | Proofs, checked operations, and negative tests |
| DO-178C claim boundary is reviewed | Objective-to-evidence map and assumptions |
| Scope is explicit | Clear statement of qualified-tool vs verified-output path |

## Open Questions

- Is verified generated output more practical than qualifying the generator?
- Should eFMI/GALEC be a primary target or an export option?
- What generated-code subset is realistic for initial MISRA C:2023 review?
- Which optimization records are useful before optimization passes mature?

## References

- [SPEC_0037](SPEC_0037_FORMALLY_VERIFIED_COMPILER.md) — deferred phase proofs
  and assurance-evidence architecture.
- [FAA AC 20-115D](https://www.faa.gov/regulations_policies/advisory_circulars/index.cfm/go/document.information/documentID/1032046)
- [RTCA DO-178C and supplements](https://www.rtca.org/do-178/)
- [MISRA Compliance:2020](https://www.misra.org.uk/app/uploads/2021/06/MISRA-Compliance-2020.pdf)
- MISRA C:2023
- [eFMI Standard](https://efmi-standard.org/)
