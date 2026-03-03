# SPEC_0028: Designing for Safety in Code Generation

## Status

DRAFT — Non-blocking. This spec does not gate any simulation milestone. It will be promoted to ACCEPTED after review by a qualified certification consultant.

## Summary

Rumoca's long-term goal is to generate code for safety-critical air mobility vehicles. This spec documents design decisions made now — while the cost of change is low — that reduce friction when pursuing DO-178C certification in the future.

## Disclaimer

This spec reflects our current understanding of DO-178C requirements and has not yet been reviewed by a qualified DER (Designated Engineering Representative) or certification authority. Relevant domain expertise includes eFMI consortium members, certification consultancies (AFuzion, ConsuNova), and DERs with code-generator qualification experience.

## Specification

### 1. Preserve Traceability Through the Pipeline

Every IR in the pipeline carries source location metadata. When rumoca produces output, each line should be traceable to the Modelica source that produced it. See SPEC_0026 for span preservation requirements.

**REQUIRED:** Generated code includes source-equation comments:
```c
w[0] = x[0];    /* state: body.position.x           [QuadRotor.mo:15] */
w[7] = -w[5];   /* = -gravity.g                                       */
res[3] = w[10]; /* residual: der(body.velocity.y) - (-g + thrust/mass) */
```

**REQUIRED:** Tape ops carry optional `OpOrigin` metadata (source location, variable names, equation index).

**REQUIRED:** Slot-to-variable mapping (`slot_names: Vec<Option<String>>`) in the `Tape` struct.

### 2. Generate Clean, Simple Code

| Concern | How tape-generated code avoids it |
|---------|----------------------------------|
| Dynamic memory | Work array is fixed-size, stack or static |
| Recursion | Flat op sequence, no function calls |
| goto | Branchless — only ternary for `Select` |
| VLAs | All array sizes are compile-time constants |
| Implicit type conversions | All values are `double` (or `float`) |
| Side effects | Each op is a pure assignment |

**REQUIRED:** Generated code compiles cleanly with `-Wall -Wextra -Werror`.

### 3. Deterministic Output

Given the same model and compiler version, generated code MUST be byte-identical.

**PROHIBITED:**
- HashMap iteration order dependence (use IndexMap / BTreeMap)
- Thread-dependent ordering
- Timestamps in generated code (timestamps belong in metadata)

### 4. Record What Transformations Were Applied

If optimization passes exist (CSE, constant folding, DCE), it MUST be possible to understand what was changed.

**REQUIRED:** A no-optimization mode that emits unoptimized code where every op maps 1:1 to a source expression.

### 5. eFMI Alignment (Future)

The eFMI standard defines a container format (eFMU) with three layers: Algorithm Code (GALEC), Production Code (C/C++), and Binary Code, connected by traceability manifests. Rumoca should consider GALEC as a structured codegen target from DAE IR.

### 6. Implementation Priority

Ordered by "cheap now, expensive later":

1. Source location propagation (SPEC_0026 — verify it reaches generated code)
2. Deterministic output (SPEC_0017 — verify end-to-end)
3. Traceability annotations in codegen templates
4. OpOrigin in tape (populate during linearization)
5. GALEC template from DAE IR
6. MISRA-friendly C templates
7. eFMI container packaging
8. Traceability report template
9. OptimizationTrace (when optimization passes exist)
10. Consult a certification expert

### 7. Open Questions for Certification Experts

1. Verified output vs qualified tool — which path is more practical for open-source?
2. DO-331 applicability — does Model-Based Development supplement apply when Modelica is the design spec?
3. Branchless evaluation and MC/DC — how do coverage tools handle unconditional branch evaluation?
4. Source-to-object traceability — does flat generated code simplify this for DAL A?
5. Optimization traceability — is a "no-optimization mode" sufficient for initial certification?
6. eFMI alignment — does following the eFMI container format provide certification credit?

## Related Specs

- SPEC_0026: Source Traceability (spans in IRs — this spec extends traceability to generated code)
- SPEC_0019: Array Preservation (array-aware tape depends on preserved arrays)
- SPEC_0017: Ordered Collections (deterministic output requires ordered maps)

## References

### Primary Standards

- **RTCA DO-178C** / EUROCAE ED-12C, "Software Considerations in Airborne Systems and Equipment Certification," Dec 2011
- **RTCA DO-331** / EUROCAE ED-218, "Model-Based Development and Verification Supplement to DO-178C," Dec 2011
- **RTCA DO-330** / EUROCAE ED-215, "Software Tool Qualification Considerations," Dec 2011
- **SAE ARP4754A**, "Guidelines for Development of Civil Aircraft and Systems," Dec 2010
- **MISRA C:2023**, "Guidelines for the Use of the C Language in Critical Systems," MISRA Ltd, 2023

### eFMI Standard

- [eFMI Standard](https://efmi-standard.org/) — official site
- [MAP eFMI Project](https://modelica.org/projects/efmi/) — Modelica Association Project page
