# Accepted Surface: SPEC_0045–SPEC_0049 DRAFT Roadmap (vote artifact)

Date: 2026-08-12
Prepared for: the series promotion vote (see `dev/2026-08-12-vote-agenda.md`)
Author: spec-series vote-prep stream (fan-out claim, mailbox @ 2026-08-12 10:40 EDT)

Citation key. `0045` = `spec/SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md`,
`0046` = `spec/SPEC_0046_SCHEDULED_DISCRETE_OWNERSHIP.md`,
`0047` = `spec/SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md`,
`0048` = `spec/SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md`,
`0049` = `spec/SPEC_0049_SOLVE_GRAMMAR_CATALOG.md`,
`MBX@<timestamp>` = the mailbox message with that heading timestamp in
`dev/2026-08-11-rdd2-agent-handoff.md`. The mailbox is live and prepends new
messages, so line numbers rot; heading timestamps are the stable anchor.
Undated timestamps are 2026-08-13 mailbox-clock headings; EDT-suffixed ones
are 2026-08-12 EDT headings.

## 1. Acceptance Record And Terms

The five-document series was ACCEPTED as the coherent negotiated **DRAFT
roadmap** by the Codex message "DRAFT roadmap ACCEPTED; gate closure mandatory
before credit" (MBX@2026-08-13 02:45) and recorded in the
Claude reply at MBX@2026-08-13 03:00. Operative terms, verbatim
in scope:

- **Scope limits** (MBX@02:45): acceptance "explicitly does not promote it,
  amend accepted C57/SIM/catalog text, or grant implementation/compliance
  credit before the declared atomic vote and cutover."
- **Confirmed surface, item for item** (MBX@02:45, restated MBX@03:00):
  total tuple-next; the single static event-system plan; Clock-before-Boolean
  strata; later-round algorithms; ordered Publish/Abort outcomes; full
  rollback; exact activation proof; compactness; RDD2 0/1/100 topology and
  four-leg evidence; target preparation; and the alternatives/reversal gates.
- **Mandatory gate closure before credit and vote** (MBX@02:45): the
  eleven previously-uncovered SDO rules (SDO-004, 011, 012, 020, 022, 023,
  030, 032, 035, 052, 053) close per Codex's per-rule prescriptions; "if a row
  is pure amendment/deletion governance, label it that way and give it a
  static cutover gate."
- **Credit rule** (MBX@02:45, MBX@03:00): "do not credit dirty
  preexisting source work against the new roots" — pre-existing dirty source
  families are prior work to be re-derived against the new roots.

### Gate-closure record (the acceptance prerequisite, landed)

| Commit | Content | Record |
|---|---|---|
| `268eed76` → rewritten `559f38f9` | Eleven-rule SDO closure: SDO-011 folded into extended SDO-221; SDO-012 into SDO-214b; new gates SDO-224..232; SDO uncovered = 0 (42 rules) | MBX@03:20; repaired hash MBX@06:50 EDT |
| `1af67b8e` | SDO-221 extent/repeated-consumer split; SDO-229 both-boundaries-one-fixture rewrite (closing the two 9/11 audit holes) | MBX@03:55, audit MBX@03:25 |
| `6f4c6e61` + `975d4308` | Full SEV/TRP closure per the 03:40 classification (reuse SEV-027→151, SEV-031→124, SEV-090→133+150a/b/d; reject gates SEV-157/158; governance statics SEV-159..162, TRP-100..103; executable SEV-163..168, TRP-104..106, TRP-105 matrix; labeled exclusions TRP-004/040/041). `975d4308` forward-corrects a disclosed index-capture incident: net change across the pair is the two spec paths only | MBX@04:15, classification MBX@03:40 |
| `3ed10540` | SEV-165 Indexing class; SEV-167 dominance negatives; SEV-168 issued-scope; coverage accounting documented as a MANUAL OFFLINE procedure | MBX@04:50, audit MBX@04:22 |
| `0321fbbf` | Eight-residual catalog pass (SEV-158 storage/reuse split, SEV-161 certificate branch, SEV-163 both halves, SEV-164 `16_777_217`, TRP-100 categories, TRP-103 two-branch, TRP-105 exhaustive negatives) | MBX@05:05, audit MBX@04:39 |
| `d42b99fa` | The atomic four: SEV-033 ONLY-WHEN amendment, SEV-166 named-relation check, SEV-161 wording, TRP-105 limit crossing; sweep SEV/TRP/SDO all zero uncovered | MBX@05:40, rulings MBX@05:10 and MBX@05:14 |

Post-closure guard state (MBX@05:40): 133 rules, gate
registry and citations zero-duplicate/zero-unresolved; word counts 2,491 /
2,440 / 2,498 (verified against the working tree 2026-08-12: headroom 9 / 60 /
2 under the SPEC_0000 §3a 2,500-word cap).

History-repair note: six campaign commits violated SPEC_0025 §6 metadata
(AI `Co-Authored-By`, missing `Signed-off-by`). A James-authorized msg-filter
rewrite repaired messages only — tip TREE HASH byte-identical — with map
`c8d7ac3e→b4f3fa47`, `ff2e2fcf→d67cfdda`, `1379fc8c→dc7e043f`,
`cc856d92→5eae6659`, `e57b32be→e268abc4`, `268eed76→559f38f9` (MBX@06:50 EDT;
independent rescan pass MBX@"live metadata re-audit"). Mailbox prose citing
old hashes stands as historical record with that map as its key.

## 2. Document Set

| Doc | Status | Words (cap 2,500) | Rule family | Role |
|---|---|---|---|---|
| SPEC_0045 | DRAFT | 2,491 | SEV-001..092 + SEV-110..114 | Grammar, type algebra, profiles, identity, wire |
| SPEC_0046 | DRAFT | 2,440 | SDO-001..091 + SDO-110..116 | Scheduled/clocked discrete ownership |
| SPEC_0048 | DRAFT | 2,498 | TRP-001..050 | Target refinement and prepared products |
| SPEC_0047 | REFERENCE | uncapped | gates SEV-1xx, TRP-100..106, SDO-200..232, SDO-223 | Shared evidence/field-catalog annex for all three parents (0047:8-13) |
| SPEC_0049 | REFERENCE | uncapped | none (bound rows) | Grammar variant catalog bound by SEV-001/002/007/011/024 and TRP-042 (0049:16-25) |

Series arithmetic: 17 active before the series; +0045 +0046 +0048 = 20, the
SPEC_0000 §3 cap; SPEC_0035 is NOT retired, so a later slice MUST free a slot
before any further ACCEPTED/DRAFT spec (0045:34-36).

Rule accounting used below (reconciles with the guard record of 133): 120
governing rules (numeric ID < 110, excluding reversal tables) + 13
reversal-gate rows (SEV-110/111/113/114, SDO-110..116, TRP-040/041). Of the
120: 119 name at least one gate in a `Covers` entry; 1 (TRP-004) carries a
labeled exclusion (0047:403-408).

## 3. Rule Surface

Every governing rule, its one-line normative content, and its gate(s). Gate
definitions: SPEC_0047 §2 (0047:62-142) and §7 (0047:354-390 plus SDO-223 at
0047:410-433). One-line statements are condensations; the cited line is
normative.

### 3.1 SEV — Solve Executable Vocabulary (SPEC_0045)

| Rule | Normative statement (condensed) | Gate(s) | Source |
|---|---|---|---|
| SEV-001 | Grammar factors into `ValueOp`/`InvokeOp`/`EffectOp`/`Terminator` with `RegionId` regions; no god enum | SEV-163 | 0045:48 |
| SEV-002 | One wire form, one definitional-semantics/total-dispatch contract, one provenance model, one capability union; executors are separate implementations checked against it; roots admit checked subsets, never dialects | SEV-105 | 0045:49 |
| SEV-003 | Canonical executable product binds ONE concrete profile; Binary32/Binary64 are different roots before folding/CSE/AD (`16_777_217`) | SEV-132 | 0045:50 |
| SEV-004 | Lowering code and immutable inputs shared; an executable profile-neutral body is not | SEV-164 | 0045:51 |
| SEV-005 | Rank-0 and rank-N are one grammar: no graph-kind bit, conversion, cache, AD path, or ABI fork | SEV-162 | 0045:52 |
| SEV-006 | `ScalarOp`/`LinearOp` is a frozen superseded adapter awaiting deletion; scalar projection is a borrowed view, never stored | SEV-159 | 0045:53 |
| SEV-007 | Consumers cover the vocabulary exhaustively or reject a declared capability; unclassified variants fail every matcher | SEV-148 | 0045:54 |
| SEV-010 | A value type owns kind, nominal identity, shape, domain, encoding — nothing else; representation change requires explicit `Convert` | SEV-122, SEV-146, SEV-149 | 0045:72 |
| SEV-011 | Root profile declares admissible contracts; construction resolves EXACTLY ONE arithmetic contract per applicable occurrence; `NotApplicable` leaves resolve none | SEV-165 | 0045:73 |
| SEV-012 | Admitted families are exactly §4.1; records finite acyclic by value | SEV-102 | 0045:74 |
| SEV-013 | Nominal record/field/shape identity kept; layout facts are prepared mappings, injective and round-tripping | SEV-146, SEV-147 | 0045:76 |
| SEV-014 | Enum type identity is `{EnumTypeId, cardinality}`; handles/references are one nominal capability family | SEV-123, SEV-146 | 0045:77 |
| SEV-015 | Zero storage is not zero identity; empty values keep §4.15 identities; zero domains never run bodies | SEV-121 | 0045:78 |
| SEV-016 | `volatile` is not ABI-only; only §4.16 owners or reject | SEV-120 | 0045:79 |
| SEV-017 | Complex is NOT admitted; every use REJECTS until the SPEC_0035 slice; SPEC_0035 stays DRAFT owner | SEV-157 | 0045:80 |
| SEV-018 | `Integer { repr }` owns signedness/width/encoding only; interval/range facts are separate root-bound facts | SEV-139, SEV-141 | 0045:75 |
| SEV-020 | Bound profile is root-identity-bearing; folding profile-bound; cross-profile interning forbidden | SEV-103 | 0045:86 |
| SEV-021 | Profile admits a type set plus one declared default each for source `Real` and `Integer` | SEV-137, SEV-142 | 0045:87 |
| SEV-022 | Registers exactly typed; each cross-format edge a licensed conversion | SEV-122 | 0045:88 |
| SEV-023 | Profile binds BEFORE root construction; later width change forbidden | SEV-108 | 0045:89 |
| SEV-024 | Per applicable class, construction resolves exactly one contract over §4.3 | SEV-101, SEV-125 | 0045:90 |
| SEV-025 | Integer arithmetic exact-in-domain or typed failure; UB/wrapping/saturation prohibited | SEV-100, SEV-139 | 0045:91 |
| SEV-026 | Observable status is an EFFECT; blocks execution CSE unless multiplicity proven unobservable | SEV-120 | 0045:92 |
| SEV-027 | Saturation/wrapping/Q-rescaling never a target strategy for ordinary arithmetic | SEV-151 | 0045:93 |
| SEV-030 | Ideal-real differentiation semantics; the only formal claim | SEV-124 | 0045:101 |
| SEV-031 | Derivative-of-quantized semantics; never the default | SEV-124 | 0045:102 |
| SEV-032 | Straight-through sensitivity; NAMED capability only | SEV-124 | 0045:103 |
| SEV-033 | Narrowing/fixed/saturating conversion in AD region rejects unless profile defines it; widening MAY refine ONLY WHEN the profile names the relation AND the SEV-034 edge validates it | SEV-166 | 0045:107 |
| SEV-034 | Primal and directional are DISTINCT roots with distinct digests, bound by a checked derivation edge | SEV-136, SEV-166 | 0045:108 |
| SEV-040 | `RootHandle` and root-local IDs never serialize | SEV-160 | 0045:114 |
| SEV-041 | `RootDigest` hashes §4.13 excluding the claim; decode recomputes | SEV-103, SEV-133, SEV-136 | 0045:115 |
| SEV-042 | Digest ladder closed and ancestral (`PreparedDigest` §4.28, `ArtifactDigest` §4.29, provenance §4.30); no cross-layer substitution | SEV-150a/b/c/d | 0045:116 |
| SEV-043 | `ValueDefinitionId` = one SSA definition; dominance structural; schedules are relations over handles | SEV-167 | 0045:117 |
| SEV-044 | Identity split: optional `PureTermId`, `OccurrenceId`/`OccurrenceFamilyId`, `FunctionRelationId`, `InvocationOwnerId`, typed projections | SEV-168 | 0045:118 |
| SEV-045 | `TermKey` binds the generative root handle and §4.5 — never a span or target strategy | SEV-128, SEV-129 | 0045:119 |
| SEV-046 | TermKey construction, optional hash-consing, and execution CSE are three separate decisions; a hash is never identity | SEV-131 | 0045:120 |
| SEV-047 | Occurrence sidecar keeps §4.6; structured generated uses own ONE provenance family | SEV-128 | 0045:121 |
| SEV-048 | Pure predicate/argument STORAGE may be shared; execution owners never merge | SEV-130 | 0045:122 |
| SEV-049 | Execution reuse discharges every §4.7 obligation; large tensor results key one compact owner | SEV-158 | 0045:123 |
| SEV-090 | A record binds the digest of the layer it annotates | SEV-133, SEV-150a/b/d | 0045:129 |
| SEV-091 | Forged wire mutations reject on decode, which replays checked constructors | SEV-104, SEV-131 | 0045:130 |
| SEV-092 | Derived range/interval facts rederived on replay, never stored, unless irreducible | SEV-161 | 0045:131 |

### 3.2 SDO — Scheduled Discrete Ownership (SPEC_0046)

| Rule | Normative statement (condensed) | Gate(s) | Source |
|---|---|---|---|
| SDO-001 | `next = active ? lazy(first-selected RHS, else held-entry) : held-entry`; ONE compact total relation per producer complete result tuple; no storage-read fallback | SDO-200, SDO-222, SDO-223 | 0046:61 |
| SDO-002 | Ordinary same-instant reads consume `next`; only explicit `pre`/`previous`/`sample(u)` consume named history lanes | SDO-200, SDO-222, SDO-223 | 0046:62 |
| SDO-003 | Laziness is semantic: inactive/unselected work executes ZERO times | SDO-201, SDO-222, SDO-223 | 0046:63 |
| SDO-004 | SDO-001 REPLACES SOLVE-C57's EXCHANGE/HOLD-FALLBACK split; no member kinds | SDO-224 (amendment governance) | 0046:64 |
| SDO-010 | ONE static `EventInstantExecutionPlan` per Solve semantic root and event system, reused by every attempt | SDO-214b | 0046:70 |
| SDO-011 | Compact body stored ONCE regardless of consumer count; no re-lowering/inlining/memoizing | SDO-221 | 0046:71 |
| SDO-012 | `EventAttempt` is runtime coordinate/private work state; issues no static structure; per-occurrence identity is `InvocationOwnerId` | SDO-214b | 0046:72 |
| SDO-020 | First scope: proved-acyclic scheduled equation owners plus ordinary Appendix-B iteration | SDO-225 | 0046:78 |
| SDO-021 | Legal coupled/nonlinear B.1b residual SCC is a TYPED REJECTION at owning spans; no invented order | SDO-202 | 0046:79 |
| SDO-022 | B.1c assignment cycles remain ILLEGAL | SDO-226 | 0046:80 |
| SDO-023 | SIM-010 stays `Partial` until a compact `ResidualSccOwner` lands | SDO-227 | 0046:81 |
| SDO-030 | Capture precedes execution over FOUR history lanes; no unified buffer | SDO-228 | 0046:87 |
| SDO-031 | Active synchronous base partitions execute ONCE; independent base clocks permutation-invariant (MLS §16.5.1.1) | SDO-203 | 0046:88 |
| SDO-032 | Unclocked round 1 follows: Boolean `sample(start, interval)` owners run once; Appendix-B iteration continues | SDO-229 | 0046:89 |
| SDO-033 | Scheduled total-next results stay CURRENT across later passes and never rerun | SDO-204, SDO-206 | 0046:90 |
| SDO-034 | Coincident directions pinned: Boolean `hold(clockVar)` sees THIS tick; Clock sampling a Boolean variable sees captured left limit | SDO-205 | 0046:94 |
| SDO-035 | `ScheduledActivationId` and `ClockId` are DISJOINT types; no conversion | SDO-230 | 0046:95 |
| SDO-036 | Condition-triggered unclocked algorithm may activate at round k>=2, consuming current definitions | SDO-206b | 0046:91 |
| SDO-037 | Such an algorithm cannot feed a once-only owner: joint owner or typed rejection | SDO-206b | 0046:92 |
| SDO-038 | Post-settle suffix runs after convergence, then the one outer commit | SDO-206b | 0046:93 |
| SDO-040 | One whole-event candidate/commit relation; success publishes atomically | SDO-207 | 0046:101 |
| SDO-041 | Abort restores ALL state categories; partial restore is a defect | SDO-207..210 | 0046:102 |
| SDO-042 | Outcome exactly `Publish { ordered_staged_effects, terminate? }` or `Abort { fatal_failure }` | SDO-211, SDO-222 | 0046:103 |
| SDO-043 | Fatal failure emits once and suppresses earlier staged effects of that attempt | SDO-211b, SDO-222 | 0046:104 |
| SDO-044 | `assert`/`terminate`/status STAGED until commit; retry duplicates no effect | SDO-207, SDO-211b, SDO-222 | 0046:105 |
| SDO-045 | Non-rollbackable effect REJECTS before the attempt begins | SDO-212 | 0046:107 |
| SDO-046 | Effect order within an owner observable; partition order between independent clocks not — distinct orderings | SDO-203, SDO-211 | 0046:106 |
| SDO-050 | `InvocationOwnerId` per source call occurrence + activation/domain/profile — never per body | SDO-213, SDO-223 | 0046:113 |
| SDO-051 | Identity issued STATICALLY; per-tick issuance forbidden | SDO-214, SDO-223 | 0046:114 |
| SDO-052 | `EventActionId` and output projection identity construction-issued, never positional | SDO-231 | 0046:115 |
| SDO-053 | Coordinate, entry/history lanes, probes, refresh closure, consumption token: DISTINCT issued identities | SDO-232 | 0046:116 |
| SDO-060 | Algorithm transaction is ONE outer producer; complete final tuple one SDO-001 value; intermediates visible only inside | SDO-206, SDO-215, SDO-223 | 0046:122 |
| SDO-061 | Cross-owner cycle treats the transaction as ONE OPAQUE BLOCK: reject or future joint solve | SDO-216 | 0046:123 |
| SDO-070 | Activation proofs symbolic and compact; hyperperiod tables/bitsets prohibited | SDO-219 | 0046:129 |
| SDO-071 | Cycle false when JOINT SATISFIABILITY (intersection) empty — accepts on lattice proof; jointly active rejects | SDO-217 | 0046:130 |
| SDO-072 | Schedules are normalized exact affine relations; no epsilon/drift/near-instant merge | SDO-218 | 0046:131 |
| SDO-073 | Tunable change REISSUES the activation certificate | SDO-218 | 0046:132 |
| SDO-074 | Coprime periods stay O(owners + compact edges + rank) | SDO-219 | 0046:133 |
| SDO-080 | `sample(0, T)` activates only AFTER Modelica initialization | SDO-220, SDO-223 | 0046:139 |
| SDO-081 | Counters report a three-way phase split | SDO-220, SDO-223 | 0046:140 |
| SDO-090 | Root, wire, preparation, stored bodies stay O(compact bodies + owners + edges + rank/ranges) | SDO-221, SDO-223 | 0046:146 |
| SDO-091 | Inherent payload scales with extent legally; extent-DERIVED metadata prohibited | SDO-221 | 0046:147 |

### 3.3 TRP — Target Refinement and Prepared Products (SPEC_0048)

| Rule | Normative statement (condensed) | Gate(s) | Source |
|---|---|---|---|
| TRP-001 | Four §4.14 prepared categories carry separate gates; mandatory legality carries NO speed gate | TRP-100 | 0048:42 |
| TRP-002 | Prepared artifact holds only §4.17 contents; op-DAG clone/second wire/stored scalar graph prohibited | TRP-101 | 0048:43 |
| TRP-003 | Backend-local DAG/SSA/CFG may carry correlated local identities with zero semantic/wire authority | TRP-102 | 0048:45 |
| TRP-004 | A second canonical graph requires a different stage contract PLUS an unrepresentable product witness | LABELED EXCLUSION — governance cross-reference to SEV-110 (0047:403-408) | 0048:46 |
| TRP-010 | One per-invocation build session; seals EACH issued root exactly once; artifacts derive once per ARTIFACT KEY | SEV-105, SEV-143 | 0048:52 |
| TRP-011 | Every product records its own layer's digest; `ArtifactDigest` claim strictly EXTERNAL to covered bytes | SEV-133 | 0048:53 |
| TRP-012 | `target.toml` extends the deny-unknown manifest schema with §4.8 typed fields incl. `CoverageMode` | SEV-137 | 0048:54 |
| TRP-013 | Kernel receipt binds every §4.9 field; name recognition/scalar recollapse prohibited | SEV-126, SEV-151 | 0048:69 |
| TRP-014 | Product plan is the closed §4.18 union; exact coverage over `(owner, logical domain point)` | SEV-127 | 0048:70 |
| TRP-015 | Unroll/tiling/fusion/kernels/dispatch stay `PreparedDigest`-only IFF a receipt proves the exact root relation | SEV-125, SEV-140c | 0048:71 |
| TRP-016 | `NativeRequired` rejects incomplete coverage; `HybridMigration` explicit and recorded | TRP-104 | 0048:72 |
| TRP-017 | No universal target program; common base promoted only at three products with IDENTICAL invariant/checker flow | TRP-103 | 0048:73 |
| TRP-018 | Emitter sees one sealed plan, no candidates | SEV-107 (plus prose witness SEV-109, 0047:143-148) | 0048:74 |
| TRP-019 | Kernel semantics end at a profile-bound root; lifecycle metadata enters prepared/artifact/package identity only | TRP-105 | 0048:75 |
| TRP-020 | No path expands a compact owner beyond source-authored operations; only final emitters create budgeted ephemeral instructions | SEV-106 | 0048:82 |
| TRP-021 | Prohibited: stored semantic scalar graph, extent-derived per-coordinate metadata, implicit scalar fallback | SEV-106 | 0048:83 |
| TRP-022 | Every non-`None`-root product carries checked work/code-size/resource budgets plus ONE admitted execution path | TRP-105 | 0048:84 |
| TRP-030 | eFMI siblings co-issued from one checked construction; AlgorithmCode owns a closed projection, never a second lowerer | SEV-134, SEV-135a | 0048:90 |
| TRP-031 | Four oracle legs; AC-to-PC relation DECLARED per §4.32; `eval-galec` parameterized by it | SEV-135a, SEV-135b | 0048:92 |
| TRP-032 | Ownership splits three ways: phase-solve (shared construction + root closure), phase-galec (projection only, NO lowering), rumoca-compile (atomic orchestration) | TRP-106 | 0048:91 |
| TRP-033 | `NumericProfile` is the closed §4.21 schema; source `Integer` default SIGNED; no reinterpretation | SEV-137, SEV-141, SEV-142 | 0048:58 |
| TRP-034 | Session normalizes the request BEFORE root construction; mismatch rejects, never converts | SEV-138 | 0048:66 |
| TRP-035 | Typed final-emission policy from §4.22; expansion = ephemeral post-seal emission only | SEV-140a, SEV-140b | 0048:67 |
| TRP-036 | Preparation records EXACTLY ONE plan from the complete §4.18 union; MiniJinja never decides | SEV-140a/b/c | 0048:68 |
| TRP-037 | Unsigned machine storage for signed values is a `PreparedDigest`-layer refinement with §4.23 receipt | SEV-141 | 0048:59 |
| TRP-038 | Roots shared ONLY when kind, inputs, profiles, AND lifecycle contract all identical | SEV-143 | 0048:76 |
| TRP-039 | Target declares closed `ValueCapabilityProfile` (§4.24) and `ExecutionEnvironmentProfile` (§4.25) at the preparation boundary | SEV-144, SEV-145a/b/c/d/g | 0048:62 |
| TRP-042 | Target declares `OperationEffectCapabilityProfile` (§4.26) keyed to the SPEC_0049 catalog; transitive closure checked | SEV-148, SEV-155, SEV-156 | 0048:64 |
| TRP-043 | ONE admission authority: capability profiles COVER the root; strict superset legal; narrowing never | SEV-145b/c/g | 0048:65 |
| TRP-044 | Every prepared layout refinement is a checked relation carrying its receipt and moving `PreparedDigest` | SEV-147 | 0048:60 |
| TRP-045 | Every environment selection carries its own receipt with normalized handler contract content | SEV-145a/e/h/f, SEV-151 | 0048:61 |
| TRP-046 | Persisted plan stores `RootDigest` + canonical `OwnerPath`, never handles/root-local IDs/cloned DAGs | SEV-152 | 0048:44 |
| TRP-047 | Manifest obligations product-tagged; omission of a capability key canonically means DENY | SEV-154 | 0048:57 |
| TRP-048 | Two-phase validation: root-intrinsic before selection, candidate-specific during | SEV-153, SEV-155 | 0048:63 |
| TRP-049 | Every advertised product discharges its SPEC_0047 §5 closure row through the one checker flow and the §6 failure/status mapping | TRP-105 | 0048:55 |
| TRP-050 | `ProductKind` × `RootKind` is a closed typed schema (§4.33/§10); no defaults | TRP-105 | 0048:56 |

### 3.4 Reversal gates (13 rows — reopening conditions, excluded from
implementation coverage by design, 0047:392-408)

| Row | Reopens | Source |
|---|---|---|
| SEV-110 | A second canonical graph | 0045:148 |
| SEV-111 | A new core type family | 0045:149 |
| SEV-113 | A profile-neutral construction recipe | 0045:150 |
| SEV-114 | Abandoning the §2 factoring | 0045:151 |
| SDO-110 | EXCHANGE/HOLD-FALLBACK over total next | 0046:166 |
| SDO-111 | A universal execution order | 0046:167 |
| SDO-112 | One re-lowered body over opaque composition | 0046:168 |
| SDO-113 | A journal over the private-arena attempt | 0046:169 |
| SDO-114 | One history buffer over four lanes | 0046:170 |
| SDO-115 | Admitting coupled residual SCCs | 0046:171 |
| SDO-116 | A runtime row cache | 0046:172 |
| TRP-040 | Retaining an OPTIONAL optimization | 0048:109 |
| TRP-041 | Either eFMI extreme | 0048:110 |

Labeled coverage exclusions (0047:403-408): TRP-004 (governance
cross-reference; evidence is SEV-110's reversal record), TRP-040 and TRP-041
(reversal-only; evaluated when a reversal is proposed). "Inventing a runtime
test for any of the three would be theatre."

## 4. Supersession Map

Everything here is PROPOSED by the DRAFT series and takes effect ONLY at the
atomic acceptance vote (0045:20, 0046:22-23, 0048:19); nothing is amended
today. Verified clause by clause per 0047:448-451.

| Superseded text | Superseding rule(s) | Source |
|---|---|---|
| SOLVE-C57 EXCHANGE/HOLD-FALLBACK split (hold-fallback storage read) — "deleted, not narrowed"; the SPEC_0043 §4 C57 EXTRACTION rows and §5 C57 EVIDENCE rows (HOLD tests, transaction-exclusion) cannot survive | SDO-001, SDO-004 (static cutover gate SDO-224) | 0046:24-41; 0047:382 |
| The ACCEPTED `ClockPartitionTransactionProgram` design (`dev/2026-08-11-clock-partition-transaction-design.md`; pending row in SPEC_0043 §4) — its first implementation step is the landed per-clock refresh slice; SDO-001 supersedes the design on acceptance | SDO-001 | MBX@2026-08-12 10:05 EDT; spec/SPEC_0043_CONSTRUCTION_CATALOG.md:141 |
| SPEC_0040 DAE-C07, DAE-C17, DAE-C21, SOLVE-C11, SOLVE-C22, SOLVE-C47/C48/C49, SOLVE-C55, SOLVE-C57; SPEC_0022 SIM-010 | SPEC_0046 §§2-11 (amended atomically, clause by clause) | 0046:22-36 |
| SPEC_0007:270 ("`SolveAlgorithmBlock` constructs ONLY FROM checked Algorithm Code") + SPEC_0040 SOLVE-C34/C38 | TRP-030 co-issuance with bidirectional correlation | 0047:453-458 |
| SPEC_0034 GAL-004/GAL-005 expression-lowering assignment to phase-galec; SPEC_0034 Summary, pipeline, GAL-027, GAL-038 | TRP-030/TRP-032 projection-and-admissibility only | 0047:458-462; 0048:22-24 |
| SPEC_0040 SOLVE-C03, SOLVE-C25, SOLVE-C39, SOLVE-C43, SOLVE-C50, SOLVE-C45 (stored scalar programs) and their SPEC_0043 §9 counterparts | TRP-020/021/035 — confined to post-seal final-emitter projection | 0047:464-472 |
| SPEC_0036 "Solve Algorithm Block Construction" sole-authority clause + its SPEC_0043 §9 link | TRP-030 co-issuance | 0047:473-477 |
| SPEC_0007 Stage 4 with lockstep SPEC_0040 C13/C14/C20 (scalar programs exist only at the final emitter, as an issued plan) | SPEC_0048 §1 | 0048:20-21 |
| SPEC_0032 §§2/4/5 shared `rumoca-eval-solve` scalar-fallback license (narrows to the final expansion boundary) | SPEC_0048 §1 | 0048:21-23 |
| SPEC_0029 §5/§12 + SPEC_0041 §4 ownership rows | TRP-032 three-way split (row-by-row table) | 0048:30-31; 0047:498-505 |
| SPEC_0035 Summary and §§1/3/4 (precision-neutral `Real`, codegen-time width, record scalarization) — SPEC_0035 NOT retired; stays DRAFT owner of Complex | SPEC_0045 §§3-4; SEV-017 | 0045:25-29; 0047:479-486 |
| `ScalarOp`/`LinearOp` vocabulary (frozen superseded adapter) | SEV-006 (static deletion gate SEV-159) | 0045:53; 0047:120 |
| SPEC_0036 and SPEC_0043 §9 rounding rows | SPEC_0045 §1 (profile-bound identity, §6 split) | 0045:30-31 |

## 5. Promotion Readiness (rule-by-gate state inventory)

Classification of every gate the 120 governing rules name. Baseline facts:
SPEC_0047 §1 records every implementation row `Partial` or `Absent`
(0047:29-58); SPEC_0046 §12 records "None of §2–§11 is implemented"
(0046:151); NEITHER SPEC_0049 promotion gate exists (0047:56); coverage
accounting is a MANUAL, OFFLINE audit procedure — no repository checker exists
(0047:392-401).

**Green (executable gate implemented and passing): NONE.** No preregistered
gate has an executable harness in the tree. Two behaviors are landed but
partial and are not green gates: the one-tick and settle restorations
(SDO-033's strata semantics, 0047:341) and the migration-period
uniformity/row-filter proofs with named deletion edges (SDO-031/032,
0047:340).

**Missing (rule with no gate and no labeled exclusion): NONE.** Post-closure
sweep: SEV 0, TRP 0, SDO 0 uncovered (MBX@05:40; verified
against the working tree by re-derivation of the Covers map for this
document — 119 rules gate-covered, TRP-004 excluded, 13 reversal rows).

**Red-by-design (all 119 gate-covered rules), subdivided:**

| Class | Gates | Covered rules | When it can go green |
|---|---|---|---|
| Executable semantic gates, red until their implementation slice lands | SEV-100..107, SEV-120..147 (incl. 135a/b, 140a/b/c, 145a-h, 150a-d), SEV-149, SEV-151..154, SEV-163..168, TRP-104..106, SDO-200..222, SDO-223, SDO-225..232 | the SEV §§2-7 surface, the SDO §§2-11 surface, most of TRP | Per-slice, in the roadmap dependency order; no credit before the corresponding gate passes (0045:135-140, 0046:149-160, 0048:94-100) |
| Standing RED witness by declaration | SEV-108 (WGSL f32 profile mismatch) | SEV-023 | Only when WGSL's typed root is truly Binary32 (0047:72) |
| Governance/static-scan gates — executable only at/after the atomic cutover amendment | SEV-159..162, SDO-224, TRP-100..103 | SEV-005/006/040/092, SDO-004, TRP-001/002/003/017 | At the voted atomic amendment (deletion scans cannot pass while the superseded vocabulary legally exists) |
| Gates asserting `NotImplemented`/`NotSelected` or reject-today as their CURRENT passing state | SEV-157 (Complex rejects), SEV-158 (no execution CSE), SEV-161 certificate branch, SEV-156 (external functions reject) | SEV-017, SEV-049, SEV-092, part of TRP-042 | Already the declared present-state semantics, but no executable harness asserts them yet; SEV-155 is presently FAILING-open in fact (`dae_has_external_functions` hard-coded `false`, 0047:52, 0047:139) |
| Prose witness without a table row | SEV-109 (first vertical witness) | TRP-014, TRP-018 | With the first real-target vertical slice (0047:143-148) |
| Labeled exclusions | none (no gate) | TRP-004 (+ reversal rows TRP-040/041) | Never — excluded by design (0047:403-408) |

**Pre-vote obligations already discharged:** the eleven-rule SDO closure and
the full SEV/TRP closure (§1 table above). **Pre-vote obligations still open:**
the rule-ID-closure guard as an executable repository test (queued MBX@04:35;
adopted MBX@2026-08-12 11:40), and the enumerated
amendments tracked in `dev/2026-08-12-vote-agenda.md`.

## 6. Rule-Surface Integrity Checks Run For This Document

- Rule count reconciles with the landed guard record: 133 total = 119
  gate-covered + 1 labeled exclusion (TRP-004) + 13 reversal rows.
- Every `Covers` entry in 0047 §2/§7 resolves to a defined governing rule; the
  inverse map (this document's Gate column) was machine-derived from the
  tables, then SDO-223's prose Covers list (0047:430-432) added.
- Word budgets re-verified against the working tree: 2,491 / 2,440 / 2,498.
