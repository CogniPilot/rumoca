# Fourbar generated position calculations

The retained OMC simulation source is
`target/fluid-campaign/tier2-bulk-clear-7e67b9954/omc_sim_work/Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1_02nls.c`.
Equation1715 at line1322 evaluates `c = cos(j5.phi)` once. Equations1716/1717
then use that shared value:

```text
R11 = e1*e1 + (1 - e1*e1)*c
R33 = e3*e3 + (1 - e3*e3)*c
```

Rumoca's source-bound ce1 schedule3 contains nine consecutive isolators,
steps54–62, for the nine `j5.R_rel.T` coordinates. They select outputs of the
same sourceprogram418, each invoking PureCallowner74 with P593..595 (axis),
Y1865 (`j5.phi`), and Y1866 (`j5.w`). The manifest-bound extraction is
`/tmp/rumoca-fourbar-schedule-owner-ce1/position-rotation-source-group.json`.
The retained typed body computes the same rotation construction from an outer
product, identity, cosine and sine, plus angular velocity `axis*w`. Its child
owners61/62 are checked cosine/sine bodies. The output selection/dispatch is
duplicated; the mathematical rotation rule is not missing.

There is already a native complete-input-bit cache in
`rumoca-exec-cranelift/src/emit/typed_program/input_results.rs`. Consequently,
nine call sites do not establish nine trigonometric evaluations. Repeated
calls still marshal inputs, compare complete coordinates, and copy results.
`compile_assignment_schedule` deliberately scopes SSA reuse to one row to
avoid stale values after writes to Y.

The first producer of this repeated source program is
`rumoca-eval-solve/src/prepared/torn_sweep.rs::torn_sweep_composite`, which
materializes one isolator per causal step. Ordinary refresh assignments already
have checked multi-output grouping in `rumoca-ir-solve/src/refresh.rs`.
Extending a shared checked grouping mechanism is a concrete compiler direction;
sharing numeric results across different simulation points is unnecessary.

Independent review rejects simply reusing `exact_rows_can_commit_together`:
its selected-value check does not by itself establish coefficient-guard and
whole-call failure independence. Also `RowLowerCtx::lower_assignments` currently
evaluates all outputs before finite checks and writes; torn execution commits
one finite output at a time. A safe shared-prefix owner must retain ordered
per-output guard/check/commit behavior and prove every shared-prefix input and
failure dependency remains unchanged by earlier target writes. Unproved cases
must retain the current schedule. No grouping implementation or speed claim
has been made.

The timed Rumoca product is a native JIT/shared Model Exchange runtime. Current
`c-ode` and FMI C targets refuse coupled implicit projection, so there is no
equivalent generated Fourbar C executable in this benchmark. The intended
improvement belongs in checked Solve construction and execution plans, so the
same calculation can serve native and C backends without template-owned
semantic transformations.
