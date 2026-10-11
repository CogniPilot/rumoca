# DAE Modelica derivative projection repair

The frozen workshop run at `26f474b15` independently established the defect:
original NeuralPredatorPrey, base export and flat export passed OMC structure,
initialization and short-trace controls; the DAE export failed to parse at
`der(population)[1]`. The [before receipt](qualification-26f-20261011.md) binds
the exact original source, commands, failure and raw artifacts.

The checked DAE retains `Index(Coordinate::Derivative(state), subscripts)`.
Its state identity and all subscript expression identities are already checked.
The DAE template printed the derivative call first and then appended an index,
which is outside the primary grammar. The canonical DAE is unchanged.

The target template now renders an invariant derivative projection inside the
operator's argument: `der(population[1])`. It follows only checked index/field
nodes to their derivative coordinate and reads field names from the checked
value type. It does not inspect model names, parse rendered names, infer shapes,
select new states or repair IR. Flattened record-member states retain their
existing typed declaration name, including Modelica identifier quoting.

Only constant/parameter subscripts move inside the derivative. Domain binders
are construction-typed constant expressions. A varying subscript remains a
selection of the derivative value, rendered `(der(state))[index]`; it is not
silently differentiated. General selection bases are parenthesized according
to the Modelica grammar. No new claim about admitting varying-index models is
made by this syntax correction.

[MLS 3.7](https://specification.modelica.org/maint/3.7/MLS.pdf), Operator 3.12
(§3.7), defines array derivatives elementwise. Sections 3.6 and 10.5 and
Appendix A.2.7 specify selection of parenthesized general expressions. The
existing shared Flat/Base renderer consumes the original source expressions
with their indices already inside `der`; those actual exports passed the before
controls and do not require an independent repair.

Ownership checked: ACCEPTED SPEC_0007 code-generation stage boundaries,
SPEC_0029 §12 target syntax ownership and SPEC_0032 §4.9 aggregate projections;
REFERENCE SPEC_0022 EXPR-003/004 and array/index contracts. Rendering remains
in the DAE target template; no target-specific Rust renderer or semantic phase
shortcut is introduced.

The unchanged actual six-model regression remains under the existing strict
Modelica CI selector `modelica_interchange_runtime::`. An additional focused
compiler/OMC regression covers vector, matrix and flattened record-member
derivative projections with independent original-source execution, all three
exports, exact scalar counts, settled initials and the same short-trace
comparison. It is an extra fixture, not a seventh workshop model or substitute
for NeuralPredatorPrey/NeuralODEBackprop. Passed controls emit progress lines so
partial execution counts can be read directly.

The [4659 qualification attempt](qualification-4659-20261011.md) compiled and
executed the repair: NeuralPredatorPrey DAE now passes OMC, along with all three
exports of the other four reached models. Two subsequent failures remain in
that receipt. Full qualification must still rerun all six originals plus
eighteen exports, the additional fixture and Node scenario seeding under a
fresh frozen source recipe. Broader workspace/lint/canary/cohort gates are not
claimed.
