# Scheduled current-read cycles and ordinary event ordering

The qualified owner snapshot is `a6c70a34e71dc95b4d1dd0b5b3fce16b18d04853`.
Source models, numerical assertions, periodic domains, clock coincidence,
wire versions and resource limits are unchanged. Exact source/log/command,
guardian and executable bindings are in [receipt.json](receipt.json).

Root's `301b4d5668382bf73826ef55f5f3d4a4ca7494b0` workspace census proves the
original cross-clock and mixed B.1b/B.1c current-read cycles incorrectly
simulate. The final scheduled owner admitted these cycles by applying an
ordinary-event mutual-observation waiver to producers carrying an issued
periodic task. Source-unowned Boolean periodic tasks correctly retain Current
reads; periodic execution still requires the SOLVE-C57 cycle proof.

The schedule now retains its existing nonempty-domain/coincidence boundary
and mandatory entry-reader precedence, without the mutual-observation cycle
or readiness waiver. History reads and never-coincident domains retain their
existing admission. Ordinary iterative ordering remains at the earlier
SOLVE-C58 `SameTickExchange` owner; the private four controls prove member
ordering, not blanket source-level ordinary-cycle admission.

That control exposed a separate ordering defect: with `n` already produced,
`a` directly reads `n` and its trigger must precede `b`, but transitive closure
through `n`'s old read of `b` made `a` wait for `b`. Actual order was `[n,b,a]`
instead of `[n,a,b]`. The exchange now carries its already-issued direct
adjacency for readiness. Transitive observations still establish mutual
components/fusion, and entry precedence remains mandatory. No graph is
re-walked, expression expanded, role reclassified, or runtime loop added.

Final owner results are structural **288/288** and phase-Solve **201/202**,
including all four new C58 controls. The sole phase failure is the inherited
`discrete_row_still_rejects_a_derivative_coordinate` assertion; the separate
qualified source-declaration/fixture correction is absent from this snapshot.
Scoped all-target/all-feature structural and phase-Solve Clippy and workspace
formatting pass. The original two source-cycle controls and full **45/45**
simulation contracts pass, including explicit-pre and valid chain controls.
The unchanged entry/rescheduling selector passes **2/2** under Interpreter and
Auto; self-rescheduling and another target's trigger-entry control remain
mandatory.

The first focused constructor attempt stopped at the host-memory floor with
zero tests. A later fixture used an undeclared alias and failed before its
cycle oracle; its correction preserves every assertion. Structural 288 then
passed, while C58's first three-control attempt was **2 pass / 1 fail** at the
unchanged `[n,a,b]` oracle. These attempts remain recorded separately.

Governing accepted contracts are SPEC_0007/SPEC_0040 SOLVE-C57/C58 and
SPEC_0033 producer-first triage. SPEC_0046 is draft context, not authority for
admitting cycles. This receipt establishes these focused construction/runtime
controls only: no OMC parity, whole-backend, combined-source workspace,
canonical quick/full, or remote-green CI claim. Root's final combined-source
qualification must include its inherited-fixture correction and combined
entry/clock/derivative selection.
