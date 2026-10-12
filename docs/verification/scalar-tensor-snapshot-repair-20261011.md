# Bounded scalar tensor snapshot repair

The original constructor-backed cross at source
`8e5ba0c3778ca705ce8774ae23c0c885d82accf2` produces canonical/native
`[-3, 6, -3]` and projected `[-3, 30, -135]` in the root-owned actual before
gate. Aggregate destination aliases are legal: the checked evaluator reads
aggregate inputs before committing results. The scalar projection committed
individual components while later components still read the original registers.

SPEC_0032 §4, SPEC_0007's faithful target contract, and the SPEC_0041 bounded
final-template scalar-view owner require this projection to preserve the checked
register semantics. This repair changes only that existing evaluation-owned
projection. Canonical programs and compact tensor shapes remain unchanged.

Overlapping cross, transpose, matmul, and binary results occupy a fresh temporary
range, then commit together. Admission checks compact source/destination
intersections, including strided gaps and broadcasts, and charges temporary
storage plus commit moves before any tensor projection loop. The existing
4,000,000 cap remains unchanged; there is no additional register coloring pass.
Unused axes of empty outputs remain untraversed.

The original cross regression is retained. New constructor-backed controls
compare both canonical and projected evaluation with independent expected values
for shifted/in-place transpose, either matmul operand, binary shifts and
broadcasts, and a disjoint strided source. Negative controls check overflow and
snapshot budget refusal without rendering large output.

Source formatting and diff checks passed. Cargo/runtime qualification of this
candidate and new controls is pending the exclusive heavy-executor queue.
The prepared native fast helpers for other aliased tensor operations are a
separate source-backed concern, not a qualified failure or repaired claim here.
