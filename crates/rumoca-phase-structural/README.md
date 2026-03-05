# rumoca-phase-structural

Structural analysis utilities for DAE sorting, incidence, tearing, and IC planning.

## Role in Rumoca
Provides structural analysis outputs consumed by simulation and diagnostics, including BLT decomposition and initial-condition planning.

## Public Surface
- Main analysis APIs: `sort_dae`, `analyze_structure`, `build_blt_from_incidence`.
- Public structural types: `SortedDae`, `BltBlock`, `StructuralDiagnostics`, `StructuralError`, `Incidence`.
- Public IC/tearing APIs and types re-exported from `ic_plan`, `tearing`, `eliminate`.

## Inputs
- Canonical DAE models.

## Outputs
- Structural diagnostics, sorted/block forms, incidence data, and IC plans.

## Design Constraints
- Keep analysis deterministic and solver-agnostic.
- Do not perform numeric integration here.
- Expose reusable structure to multiple backends.
