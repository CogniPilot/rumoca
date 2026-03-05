# rumoca-eval-ast

`rumoca-eval-ast` defines the AST-facing evaluation surface used by compiler
phases that reason about expressions before flattening.

## Role in Rumoca

- Layer: evaluation (`eval-*`) aligned with `rumoca-ir-ast`
- Input: AST expression trees and lookup contexts implementing `EvalLookup`
- Output: shared contracts/types for AST-level evaluation

## Design Constraints

- Keep this crate narrow and dependency-light.
- Avoid pulling Flat/DAE runtime concerns into AST consumers.
- Provide a stable surface for phase crates that work at AST level.

## Public API

- `EvalLookup` re-export (`rumoca_core::EvalLookup`)
- `ast` re-export (`rumoca_ir_ast`)
