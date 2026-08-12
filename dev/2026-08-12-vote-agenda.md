# Vote Agenda: SPEC_0045–SPEC_0049 Series and Post-Acceptance Rulings

Date: 2026-08-12
Companion: `dev/2026-08-12-accepted-surface.md` (the vote artifact; citation
conventions — `MBX@<timestamp>` etc. — are defined there).

## Item 1 — Series promotion DRAFT → ACCEPTED

**Motion.** Promote SPEC_0045, SPEC_0046, and SPEC_0048 to ACCEPTED in ONE
atomic vote that simultaneously executes every acceptance-time amendment row:
SPEC_0045 §1, SPEC_0046 §1, SPEC_0048 §1, and the clause-verified map in
SPEC_0047 §8 (supersession inventory: accepted-surface doc §4). SPEC_0047 and
SPEC_0049 are REFERENCE annexes and need no vote (SPEC_0000 §3a), but their
rows bind when the parents are accepted.

**Prerequisites already discharged.** The mandatory eleven-rule SDO gate
closure and the full SEV/TRP closure (accepted-surface doc §1 table); the
SPEC_0025 §6 metadata repair (six commits, tree-hash-identical rewrite).

**Prerequisites still OPEN before the vote can be called:**

1. Process path for the vote itself: SPEC_0000 §5 previously had no
   DRAFT → PROPOSED edge. A drafted amendment now adds it
   (`spec/SPEC_0000_SPEC_GUIDELINES.md` §5, AMENDMENT-marked). Ratifying that
   edge is logically FIRST — without it the three parents cannot legally stand
   for a vote (MBX@2026-08-12 09:35 EDT blocker; 09:55 ruling).
2. The rule-ID-closure guard as an executable repository test (every cited ID
   resolves; every gate covered from a parent), replacing the manual offline
   audit procedure of SPEC_0047 (queued MBX@04:35; adopted into series scope
   MBX@2026-08-12 11:40). Owner: implementation stream, not this document.
3. Disposition of the remaining enumerated amendments (Item 5).

**Sequencing note.** Implementation credit for any slice continues to require
its named gates passing, independent of this vote (SPEC_0045:135-140 and the
acceptance terms).

## Item 2 — Word-cap: threshold or split (URGENT per MBX@05:40)

State: SPEC_0048 = 2,498 words (headroom 2), SPEC_0045 = 2,491 (headroom 9),
SPEC_0046 = 2,440 (headroom 60) against the SPEC_0000 §3a hard cap of 2,500.
Both 0045 and 0048 are "effectively closed" — a single future clarifying word
can breach CI.

Options for the vote (choose one):

- (a) **Freeze-and-route**: declare both parents word-frozen; every future
  clarification lands in SPEC_0047 (REFERENCE, uncapped) with the parent
  linking it. Zero process change; risks parent rules whose binding text
  drifts annex-ward against SPEC_0000 §3a's "annex is not a place to hide
  requirements".
- (b) **Split**: split SPEC_0048 (the tighter file) into two ACCEPTED specs.
  BLOCKED today: the series already fills the 20-spec cap (SPEC_0045:34-36);
  a split first needs a freed slot (or option d).
- (c) **Raise the cap**: amend SPEC_0000 §3a (e.g. 2,500 → 2,750). Cheapest,
  but weakens the working-memory rationale for every spec at once.
- (d) **Free a slot then split**: execute a slot-freeing consolidation
  (candidates: SPEC_0035 retirement once the Complex family lands — NOT now,
  see accepted-surface §4; or archive of an inactive DRAFT per SPEC_0000 §3),
  then (b).

No recommendation is recorded here; the mailbox flagged only urgency, not a
preference (MBX@05:40).

## Item 3 — OMC parity acceptance policy (RATIFIED; confirm codification)

James ratified **option 2, event-windowed** (MBX@2026-08-12 07:20 EDT), with
the mechanism amendment at MBX@07:40 EDT:

- Compare ALL channels over [0, disarm]; the window anchor is the
  MODEL-SEMANTIC disarm instant (observability loss of the optical estimator's
  vertical channel) — never a tuned duration.
- Post-disarm, everything is compared EXCEPT the classified unobservable
  optical-estimator vertical channels; the classification cites the model's
  observability structure and is bounded to the post-disarm window.
- The post-disarm tail stays in evidence for all other channels (retains the
  1e-10 truth-altitude agreement at 45 s).
- **Mechanism:** the EXISTING `msl_trace_compare_exclusions.json` registry —
  the same infrastructure recording ChuaCircuit chaotic non-identifiability
  and event-side convention aliases, same reason-prose discipline. The schema
  gains minimal OPTIONAL channel-list + time-window fields (today's entries
  are whole-model). The RDD2 entry names the optical-estimator vertical
  channels, window = post-disarm, with the observability citation. No parallel
  mechanism.
- Comparator gains explicit window/horizon inputs (folded with the
  hard-coded-10s timeout fix); `--reuse-traces` keeps the existing 664.5 s OMC
  reference authoritative — no new OMC run.

**To confirm at the vote:** (a) this policy text enters the voted series'
acceptance-policy section; (b) the full-trace parity gate goes green when the
windowed comparison passes; (c) window-anchor choice (disarm vs landing) and
the classification's evidence form — counterpositions were explicitly invited
and none has arrived; (d) `terminate()`-at-landing stays a FUTURE separate
scenario (it would exercise SPEC_0046 `Publish{terminate}`), not a change to
this mission.

**Caveat for the record (MBX@2026-08-12 10:05 EDT):** the 45 s OMC parity
green is NOT evidence on the SOLVE-C57 same-tick defect — sim_trace_compare
classifies the one-tick shift as EventTimeMismatch (tolerated sampling
convention). The exclusions registry must not be used to paper over C57; the
three galec_equivalence reds stay red until the SDO-001 owner lands.

## Item 4 — Credit-gating confirmations

Restate and confirm at the vote (all already agreed in the acceptance thread):

1. No implementation or compliance credit before the atomic vote AND cutover
   (MBX@2026-08-13 02:45).
2. Dirty pre-existing source families are PRIOR WORK, re-derived against the
   new roots — never credited against them (MBX@02:45, MBX@03:00).
3. The gate-closure and incident-correction commits carry correction/closure
   credit only, as stated per-commit in the thread (e.g. `975d4308`
   "Correction credit only", MBX@04:15).
4. A rule is implemented only when every gate its `Covers` row names passes
   (SPEC_0045:138-140, SPEC_0048:98-100); the promotion-readiness table
   (accepted-surface §5) is the baseline: zero green gates today.
5. The three deliberately-red galec_equivalence gates are named-ownership
   gates for SIM-010/SOLVE-C57 (sim side, not codegen) and remain red until
   the ClockPartitionTransactionProgram successor work lands under SDO-001
   ownership (MBX@2026-08-12 10:05 EDT).

## Item 5 — Enumerated post-acceptance amendments: status

The six amendments to older specs assembled from the acceptance thread's
pre-vote queues (MBX@2026-08-12 11:30 "remaining series items"; MBX@12:30 "four
mandatory pre-vote items"; audit dispositions MBX@2026-08-12 00:20; fan-out
claim MBX@2026-08-12 10:40 EDT). The mailbox enumerates them across several
messages, not as one numbered list of six; the reconstruction below names its
source for each. DRAFTED means a minimal AMENDMENT-marked diff exists in this
branch; nothing is ratified by this document.

| # | Amendment | Target | Status |
|---|---|---|---|
| 1 | GALEC Startup limit set: Startup-return `limit` covers externally-initialized inputs/tunables plus definite writes; Production C rejects incomplete Startup; executing oracle/C fixture | `spec/SPEC_0034_GALEC_EFMI_EXPORT.md` GAL-017 | **DRAFTED** (source: MBX@2026-08-11 20:55 EDT counterexample u=9 vs u=2; MBX@00:20 disposition "amended not reverted") |
| 2 | FUNC-036/037 contract-registry links: Partial-evidence annotation; Implemented stands only with generated linked cases and executed-test evidence | `spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md` §4.8 rows | **DRAFTED** spec-side (source: MBX@2026-08-11 20:50 EDT; MBX@00:20 "305b211b" disposition). **OPEN follow-up:** the registry data itself (`crates/rumoca-contracts/data/contracts.toml` still says `status = 'Implemented'` for both) and the generated linked cases are implementation-owner work outside this stream's file ownership |
| 3 | SPEC_0025 §6 trailer-repair record note | `spec/SPEC_0025_PR_REVIEW_PROCESS.md` §6 | **NO DIFF REQUIRED** — determination: no thread message requires a spec amendment; §6's rules are unchanged and now satisfied by the authorized tree-hash-identical rewrite, whose record lives in the mailbox (MBX@2026-08-12 06:50 EDT, independent rescan pass) and the 44-row hash map artifact. Overriding this determination is a one-line vote rider |
| 4 | SPEC_0000 lifecycle edges: DRAFT → PROPOSED (vote entry for a matured DRAFT) | `spec/SPEC_0000_SPEC_GUIDELINES.md` §5 | **DRAFTED** (source: MBX@2026-08-12 09:35 EDT blocker, 09:55 ruling; scoped in SPEC_0045:20-23). Note: PROPOSED already exists in the §5 status table; only the edge was missing. No README change needed until a spec actually enters PROPOSED |
| 5 | SOLVE-C50/SPEC_0043 seed-certificate extension: compact Y/P/seed range sets, derived max lengths, `has_seed_reads`; derived once incl. `TensorLoad` and recursive fold/conditional owners; replay rederives, never serializes; five consumer families share the ONE certificate | `spec/SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md` SOLVE-C50 + `spec/SPEC_0043_CONSTRUCTION_CATALOG.md` §9 certificate row (lockstep) | **DRAFTED** (source: MBX@2026-08-12 08:45 EDT diagnosis incl. the native-safety finding; MBX@09:05 sequencing "SOLVE-C50/SPEC_0043 amendment first"). **Interaction to resolve at the vote:** the series' acceptance-time map ALSO amends SOLVE-C50 toward final-emitter projection (SPEC_0047:464-472); this extension governs the transitional stored-program certificate until that cutover and must be reconciled in the same atomic pass |
| 6 | SPEC_0035 retirement | `spec/SPEC_0035_COMPLEX_NUMERIC_TYPES.md` | **SUPERSEDED — NO DIFF.** The retirement was re-ruled: Complex is fully OUT of admitted families and SPEC_0035 REMAINS the DRAFT owner until the Complex family lands (MBX@2026-08-12 23:45 item 3; MBX@00:00; SPEC_0045:25-29, SPEC_0047:479-486). Retirement returns as a slot-freeing option under Item 2(d) only after that slice |

**OPEN items summary:** Item 5.2's registry-data/linked-cases follow-up
(implementation owner); Item 1.2's executable rule-ID-closure guard
(implementation owner); Item 5.5's SOLVE-C50 reconciliation (vote-time);
SPEC_0034 word count now 2,488/2,500 after amendment 1 — headroom 12, worth
watching under Item 2.

## Item 6 — Housekeeping riders (no discussion expected)

- Confirm SPEC_0047/SPEC_0049 REFERENCE status needs no vote (SPEC_0000 §3a
  annex rules) and that their `Covers`/binding mechanics ride the parents.
- Confirm the coverage-accounting exclusions as labeled: TRP-004 governance
  cross-reference; TRP-040/041 reversal-only (SPEC_0047:403-408).
- README status/line-count refresh for any file the vote changes (SPEC_0000 §2
  trivial-fix lane).
