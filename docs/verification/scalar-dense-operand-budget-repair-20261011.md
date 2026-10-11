# Compact dense-solve final operand admission

The original checked 200-by-200 identity solve is compact in canonical IR, but
its 200 final scalar component calls each list all 40,000 matrix entries and
200 RHS entries. Its 8,040,000 operand references exceed the existing 4,000,000
projection cap. The original before fixtures and independent two-element
native/projection control are preserved unchanged.

The shared `rumoca-eval-solve::lower_scalar_ops` admission owner now counts
`n*n + n + 1` for each `LinearSolveComponent` before the scalar-only shortcut
or any projection allocation. Checked arithmetic refuses overflow. Each emitted
component is counted once; repeated operand lists really occur in final target
output. Canonical tensor storage and the existing cap remain unchanged.
This implements SPEC_0032 §4 / SPEC_0041's bounded final-template scalar-view
contract without generating giant output or relying on a timeout.

Additional controls cover the scalar-only shortcut, arithmetic overflow, and
exact cap preservation. Formatting and diff checks passed. The root-owned
original before gate is terminal with exit101; its detailed receipt and all
after runtime qualification remain to be attached by the heavy executor.
