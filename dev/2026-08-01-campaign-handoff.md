# Campaign handoff — trace-parity + SPEC 0038 + verification tracks

Written 2026-08-01 at the end of a long coordination session, for a successor
agent with **no access to prior session state**. Everything needed to continue
is in this file, the repo, and the ledger it points to. Read this file, then
the ledger, before touching anything.

---

## 1. Mission and score

**Goal: >= 340 of the 566-model deterministic MSL 4.1.0 cohort in OMC
trace-parity strict-high agreement.** Next milestone: the 85-model (15%)
ratchet floor, which the quality gate already enforces (it exits 1 by design
until 85 is reached — see §10).

**Certified score right now: 59/566 strict-high** (bands: 59 high / 6 near /
4 deviation over 69 compared models). Eleven certifications this session,
every model movement named in advance, zero unexplained movement. Score arc
this campaign: 54 → 56 → 58 → 59.

**Doctrine (James, non-negotiable):**
- `sim_ok` is completion, NEVER parity evidence. The only score is the
  strict-high band from the repo comparator.
- Parity claims come only from repo tooling:
  `nix develop --command cargo xtask verify msl-parity --all-omc-targets
  --results-dir target/msl/results-landed-2 --stage-parallelism 8
  --sim-parallelism 6` (certification), and
  `cargo xtask repo msl -- transition-diff` for before/after band evidence
  BY NAME. Never hand-roll comparison scripts (an incident is recorded in
  the ledger: a scratch band_diff.py was strictly worse than the shipped
  tool).
- Never produce a plausible-but-wrong simulation. Correct by construction:
  if it compiles, it should be right with high confidence. Typed rejection
  over silent defaults, always.

## 2. Design pillars (standing, from James, 2026-08-01)

1. **No backwards compatibility, no legacy code — including the CLI.**
   Pre-1.0: delete the old path. No flags/enum values/dropdown entries/type
   stubs for features that do not work today; a clap error listing only
   working values IS the honest surface. Future-work intent lives on task
   boards and registry notes, never in user-facing code. An unreachable
   fallback is a defect (the retired diffsol general path is the precedent).
   Enforced by `crates/rumoca/tests/history_policy_test.rs` (bans the
   terminology).
2. **Correct by construction** (SPEC 0036): invalid stage values
   unrepresentable; acceptance-contract-before-rejection (SPEC 0008);
   constructor tags over structural tests (the Always/AnyRise lesson —
   a structural "is this constant?" check once made user `when true` fire
   every event).
3. **Formal verification is KEY, and Kani-compatibility is ENFORCED.**
   Kani (bounded model checking) IS formal verification. Standing pattern:
   when a wave lands an invariant testing cannot pin, it lands the Kani
   harness with it (property text citing the registry row; harness reviewed
   like code). Kani-compatibility is a fence: proof-covered semantic cores
   stay pure-functions-over-plain-values because their harnesses break on
   drift (Rc/RefCell/hash-state/unbounded loops). Architecture:
   verified core + validated pipeline (CompCert-shaped) — Lean 4 holds the
   spec (a small definitional interpreter for event semantics, NOT a
   compiler), Rust holds the product, differential tests + Kani hold them
   together. See §7.
4. **Tensor native** — "important for neural ODE, PDE support". Preserve
   array structure through the IRs; never scalarize on the way through.
   Current state is BAD: tensor preservation is 0% by construction (see §8).
5. **`--solver` is a HOST-SELECTION KNOB**: every method is an integrator
   host over the one FMI 3 ME kernel; event semantics are kernel-owned and
   identical across every choice; the flag trades accuracy/stiffness, never
   behavior. Methods are never lost — they re-home onto the kernel (SDIRK
   deleted with the old wiring, returns as an ME host, task #134).

## 3. Tree state

- Branch: **`msl-trace-parity-50`** (the integration branch — `origin/main`
  is ~280 commits behind and lacks the registry entirely; never target main).
- Tip at handoff: **`725d3500`**. 38 commits landed this session. Recent:
  - `725d3500` fix(typecheck): abstain when identity resolution yields no row
  - `8f33cea0` feat(solver): retire the general implicit-DAE path (B1-a)
  - `356a6410` fix(typecheck): read declared array extents from expansion
  - `2276c4d9` feat(solver): project the model once through an FMI 3 ME kernel
  - `7b4536b3` feat(contracts): seed the formal-statement registry
  - `9f522659` feat(dae): pre() on continuous coordinates as frozen left limit
  - `4032af2a` fix(events): seed condition memory + per-element when edges
- The living ledger (session-by-session evidence record):
  **`dev/2026-07-28-spec-0036-cutover-checklist.md`** — read its last ~40
  entries; every landing, review verdict, and process incident is there.
  NOTE: `dev/` is gitignored — the ledger is local working state, and
  worktrees created from commits DO NOT contain it.

## 4. IN-FLIGHT WORK (uncommitted, in worktrees — restart guidance)

Both agents have exited; no processes are running, nothing to wait for
or kill. **W1 has since LANDED on the branch (e62afc32) — §4a is now
historical record.** ONE worktree still holds live, valuable,
UNCOMMITTED work: the phase-2 migration (§4b). Its diff IS the
deliverable.

### 4a. W1 verification foundation — LANDED at e62afc32 (2026-08-01)

**This section is now historical record: the W1 work is COMMITTED on the
branch** (`e62afc32 feat(verify): seed the formal-verification
foundation`, signed, full gate green: fmt/clippy/msl-check 0, nextest
5452/5454 with only the pre-existing #102 pair, registry 75 rows no
duplicates). The worktree is landed history; safe to remove. Successor
notes that survive the landing: (1) the `#[cfg(kani)] mod proof` blocks
compile under NO configuration until the `devShells.verify` flake input
lands — re-read them by hand after any solver refactor; (2) FS-SIM
maximum on the branch is now 017, so the phase-2 worktree's
FS-SIM-017/018 rows MUST renumber before its landing gate;
(3) SPEC_0037 DRAFT status is still provisional pending James;
(4) certification #12 runs after this landing with a zero-band-movement
expectation (two production lines, both `mod` registrations).

- **Worktree:** `.claude/worktrees/agent-ab49e0420865a961b`, rebased onto
  `725d3500`, uncommitted (final: 31 files, +5,227/−8; only 2 lines changed
  in pre-existing .rs files — two `mod` declarations — everything else new).
- **Contents:** (1) dual-driver Kani/proptest harnesses (wire ordinal
  round-trips, timeline/condition-memory seed properties FS-EQN-001/002,
  FMI ME kernel lifecycle pins) — Kani is NOT in nixpkgs; the harness
  compiles under `cfg(kani)` with proptest fallbacks running today; the
  flake needs a `devShells.verify` input (Kani pins its own nightly,
  collides with `nightly-2026-02-27` which wasm threading needs);
  (2) **`crates/rumoca-reference`** — the definitional interpreter for the
  discrete/event core (the executable spec; §8.3.5.1 written literally);
  (3) registry rows FS-SIM-016 (Bdf self-rescheduling divergence) and
  FS-EQN-019 (buffer latching); (4) SPEC_0037 moved DEFERRED→DRAFT
  (**PROVISIONAL — James has not signed off**; revert is one rename if he
  declines).
- **State: FIX ROUND COMPLETE — READY TO LAND AS-IS.** Final report
  received 2026-08-01 ~14:55: F1 fixed by narrowing (the defect was an
  EXTRA inner fixed-point sweep Appendix B never asks for; new rule =
  the compiler's own — seed pre(b) once per instant, condition stays
  live, the outer Appendix B loop advances memory; P1 now agrees with
  both sessions AND self-rescheduling still settles; StateConditionCascade
  differential added). F2 re-tiered SpecSilent with the silence located
  precisely (Appendix B says "solve" without defining it for a system
  with no solution). F3 admissibility pass (check_admissible +
  UnlocatedCrossing, pinned). F4 universes derived from the total match
  (variant-add now fails the round-trip). F5 workspace check-cfg line
  (a hidden #![expect] was also found and deleted; cfg(kanii) warns,
  cfg(kani) silent — verified). F6-F16 done; the obligation-4 witness
  replaced with a true one (bare `a = not pre(a)` outside any
  activation). Gates on 725d3500: fmt/clippy clean zero suppressions,
  registry invariants 26/26, suite_gates 14/14, architecture 125/125,
  reference 5/5+14, nextest 5452/5454 (only #102). Diff 31 files
  +5227/−8, 2 production lines. Land with the documented procedure;
  certification after.
  **COLLISION WARNING: W1's report says its rows are FS-EQN-019 +
  FS-SIM-016 + FS-SIM-017 — but FS-SIM-017 is ALSO used by the
  phase-2 migration worktree (§4b, along with 018). Both are unlanded;
  whichever lands SECOND must renumber before its landing gate.**
  (Earlier state description follows for context:)
  The adversarial review (ACCEPT-WITH-FIXES) found **F1 HIGH: the reference
  is WRONG where the compiler is RIGHT** on state-condition cascades
  (`when time>=0.5 then x=3; when x>2 then y=1` — MLS Appendix B's event
  iteration re-solves within the instant to y=1; the reference's universal
  buffer-latching holds the entering value and never fires). The latching
  IS needed for the unsatisfiable self-rescheduling case (verified), but
  must be narrowed (e.g. to conditions whose own body writes their
  operands) or the scope refused, WITH a P1-shape differential test.
  F2: FS-EQN-019 re-tier toward SpecSilent. Smaller items: derive the wire
  universe lists from the total match (they are hand-maintained — a
  reviewer added a variant + demanded arms and round-trip stayed green);
  replace the two `#![allow(unexpected_cfgs)]` with one
  `unexpected_cfgs = { level = "warn", check-cfg = ['cfg(kani)'] }` line in
  the existing `[workspace.lints.rust]` (verified empirically better);
  ROADMAP broken link + refuted obligation-4 witness
  (`when b then b = not pre(b)` CONVERGES under latching); pin `next_up()`
  with a strict-inequality case; explicit `16 => Terminate` arm.
  The full fix list is in the ledger (W1 REVIEW entry).
- **Landing order: W1 lands BEFORE the phase-2 migration** — it cannot
  destabilize anything (no production logic), while the migration will move
  the ~15 SolveModel/SolveLayout fields W1's fixtures bind, and the
  `#[cfg(kani)] mod proof` blocks compile under NO configuration today, so
  they rot silently if the fields move (re-read them by hand after any
  solver refactor).

### 4b. Phase-2 migration (diffsol onto the ME kernel) — STOPPED AFTER
### STEP 2 WITH MAJOR FINDINGS (correct stop; read before restarting)

- **Worktree:** `.claude/worktrees/agent-abb1a687198218dbd`, base
  `725d3500`, uncommitted. Report + probes + `census.sh` under
  `/home/jgoppert/.claude/jobs/244ea9ad/tmp/t0038p2m/`. The agent was
  stopped by James after its stop-report; the worktree and report files
  below are the complete record of its findings.
- **DONE in the worktree (all gates green, 5448/5450):** step 2 trait
  additions (`get_directional_derivative`; `MeStage`+`MeError::Staged`
  preserving the worker failure-bucket mapping; B6 already satisfied),
  13 kernel tests, SEVEN planted negative controls for the fmi_me
  boundary test (it previously had none), divergence pins
  `crates/rumoca/tests/fmi_me_host_divergence.rs` (3 tests asserting the
  tree is CURRENTLY inconsistent — they fail when the migration unifies
  it), registry rows FS-SIM-017 (D-A) + FS-SIM-018 (D-B), the leak-fix
  design note (owned-problem via Bdf::new/into_state per advance;
  self-referential ruled out by workspace unsafe_code=deny; the
  per-advance Jacobian-refactorization cost must be MEASURED).
- **WHY IT STOPPED — the freeze is impossible through the current
  kernel, and one divergence is a KERNEL spec violation:**
  - **D-B (fix FIRST):** `kernel.rs::exit_initialization_mode_inner`
    latches `pending_event_pre_y` BEFORE `settle_initialization_system`
    runs (the comment above the latch claims otherwise). Probe:
    `x(start=0,fixed=false); initial equation x=5; when initial() then
    c=pre(x)` → diffsol c=5 (correct per §8.6 pre(vi):=vi settled),
    kernel/rk45 c=0. This is almost certainly open task #44's mechanism,
    now measured. Its fix MOVES rk45 traces — that movement IS the fix;
    declare it, band-expect it (rk45 side ⇒ zero band movement since the
    harness measures diffsol), land it before any migration step.
  - **D-A:** at a scheduled event diffsol emits two rows (left+right
    limit), the kernel one (right only) — O(1) trace differences, row
    counts shift, NOT shimmable (host sample scheduling, not the root
    evaluator). Adjudicate against OMC's result-file convention (what an
    oracle observes), THEN pick the shared behavior.
  - Also: the kernel has NO coincident scheduled+state-event handling
    (driver.rs:945's §8.5 branch has no kernel counterpart — move the
    semantics into the kernel, do not delete `coincident_scheduled_event`
    until then), and the algebraic settle differs (kernel 1e-10/32 vs
    diffsol atol.max(1e-10)/256).
  - Census corrected: diffsol private-surface occurrences are **508**
    (not 448); `driver.rs` has exactly one consumer (diffsol) and is not
    deletable until step 3; OdeModel has 42 production refs.
- **Recommended restart order:** (1) fix D-B in the kernel (small,
  spec-cited, rk45-side movement expected in traces, zero band
  movement); (2) OMC-adjudicate D-A and pick the shared convention;
  (3) THEN steps 3/5/6/7 with the freeze bar redefined against the
  post-D-B kernel. NOTE FS-SIM-016 is taken by the pending W1 landing;
  017/018 are taken by this worktree — check maxima at every landing.
- **Mandate (steps 2–7 of the phase-2 plan; step 8 is separate waves):**
  2. Trait additions (F/frozen): `get_directional_derivative`
     (fmi3-faithful), `MeError` gains a stage field mirroring
     `SimFailureStage` (**the worker failure-bucket histogram is a GATE** —
     losing stages silently re-buckets every MSL failure),
     `commit_delay_history_evaluated_at` kernel home. rk45 freeze corpus
     must stay bit-identical after this step.
  3. Adapter skeleton (F): `DiffsolMeHost` over `SolveMeKernel` for the
     state-only path. EXPECTED first failure: kernel vs old driver disagree
     on the root evaluator (`eval_root_conditions`+crossing filter vs
     `eval_root_search_conditions_into`+planned time roots) and the
     `apply_without_initial_event` flag. Resolution: a temporary
     `MeRootProfile` shim reproducing diffsol's current choices so
     bit-identity holds; the shim is DELETED one divergence at a time in
     step 8 with evidence.
  5. L3 zero-state: (F) replace `RuntimeOnlyDriver` +
     `simulate_no_state_solve_ir` with `MeNoStateSession` (rk45's 639→103
     precedent); (C/change) reconcile the batch-vs-session
     `apply_without_initial_event` split (runtime.rs:157 true vs :222
     false) to the kernel's `false`, with probe evidence + cohort sweep;
     STOP if any cohort model moves.
  6. L2 interactive (C, unavoidable): rebuild `BdfSession` as a host over
     the kernel (mirrors rk45's StateSession). This FIXES **D11 (task
     #126): the session today NEVER runs §8.6 initialization** (settles
     algebraics only — every scenario/LSP/wasm consumer on Auto gets it).
     Wiring is **B3-a**: closures hold `Rc<RefCell<SolveMeKernel>>` clones,
     one `borrow_mut` guard per set-then-get sequence, a
     panics-on-nested-borrow test; **fix the `Box::leak` at
     session.rs:410 with the wiring** (under B3-a it would leak a whole
     SolveRuntime per session and terminate() never runs).
     Evidence: session-vs-batch trace agreement on one model.
  7. Delete `rumoca-solver/src/runtime/driver.rs` (1049 lines, diffsol was
     its last consumer), `SolverAdvanceBackend` etc.; move
     `rumoca-ir-solve`/`rumoca-eval-solve` to dev-deps in diffsol's
     manifest; extend `crates/rumoca/tests/architecture_hardening/fmi_me_boundary.rs`
     to diffsol with its private-surface names; census 448 → 0. Most of
     `ode.rs`'s implicit surface (OdeModel's second SolveModel projection,
     MassOperator) becomes deletable only AFTER step 6.
- **Registry caution:** FS-SIM-016 is taken by W1 (pending landing). The
  registry file is append-heavy; ALWAYS re-check per-category ID maxima at
  merge time (`grep "^id = 'FS-SIM" crates/rumoca-contracts/data/formal_statements.toml`).
  One ID collision already happened and was caught (ledger).

### 4c. Other worktrees

`.claude/worktrees/` may contain older worktrees from landed waves; their
diffs are already on the branch. Safe to remove their `target/` dirs for
disk (60–170G each). Never remove `target/msl/results-landed-2/sim_traces`
or `.../omc` in the MAIN tree — that is the OMC reference cache (69G,
~524 model references; regenerating takes hours and has twice caused
resource kills — resumable via
`cargo xtask repo msl -- omc-simulation-reference --workers 6`).

## 5. Process discipline (all of it earned the hard way)

**Acceptance pipeline for EVERY code-writing wave:** implement (worktree,
no commits) → gates → independent adversarial review (MLS citations +
probes + comparator bands; reviewers build their OWN base binaries in
separate target dirs) → fix round(s) → land (3-way apply to main) →
full landing gate → signed commit → certification → ledger entry.
No exceptions; reviews this session found HIGH defects in >half the waves,
including in reviewer claims (fix rounds refuted two review premises by
instrumenting — encode nothing unverified).

**Landing procedure:**
- `git -C <worktree> diff <base> > patch` (+ `--binary`, and `git add -A`
  first if the worktree has untracked new files), `git apply --3way patch`
  on main, resolve, copy untracked files.
- Full gate with **per-step exit codes in SEPARATE commands** (`FMT_EXIT=`,
  `CLIPPY_EXIT=`, `MSLCHECK_EXIT=`, `NEXTEST_EXIT=`) — never trust a
  wrapper's exit 0 (two incidents: `tail && git commit` committed on red;
  a missing `set -o pipefail` masked a nextest failure).
- The gate floor is the FULL workspace: fmt, clippy (no new `#[allow]`),
  `cargo check -p rumoca-test-msl --features msl-full-test` (clippy
  --all-targets MISSES this feature-gated target — a break slipped through
  once), `cargo nextest run --workspace --no-fail-fast` with pipefail.
- Commits: `git commit -s`, **never any mention of AI/Claude/assistants,
  no Co-Authored-By**. Long evidence-narrative bodies are the house style
  (read `git log` for examples).
- Certification after each landing batch; record the parity line
  (strict-high N/566, entered/left/band-changed) in the ledger. Every
  mover must be named or the movement explained.

**Known-red things that are NOT regressions:**
- `rumoca-bind-python::fixed_wing_outer_loop_{embedded_c_galec,galec_production}_matches_cli_dispatch`
  — the ONLY expected nextest failures (task #102, pre-existing EGT017).
- `rumoca-tool-lsp::completion_timing_summary_reports_warm_source_root_namespace_cache_reuse`
  — load-sensitive timing flake; passes in isolation; verify in isolation
  before treating as real.
- The certification exits 1 BY DESIGN: the quality gate demands >=85
  strict-high (the ratchet milestone) and compares per-stage counts against
  a PRE-CUTOVER baseline (`msl_quality_baseline.json` from `08fac548`:
  compiled 545/flatten 565/sim_ok 207 vs today's 143/402/71). The parity
  and transition lines are the certification substance. Rebasing that
  baseline honestly is open work (part of tensor #130's pattern).
- `rumoca-tensor-scaling --enforce` (CI, ci.yml:388) is RED on the tree —
  real (see §8), not noise.

**Resource discipline (32-thread/62G box, James's interactive desktop):**
- NEVER use all 32 threads; total machine budget <= 28; load target ~16
  (load 55 incident recorded — James intervened).
- Sweep workers count against the budget like build jobs; **at most ONE
  full-cohort sweep on the box at a time**; builds and sweeps strictly
  sequential per lane; with 3-4 lanes active, per-lane budgets 6/6/4.
- Wall-clock results measured during contention are contention-suspect,
  never findings (LSP flake, DCPM timeout precedents).
- Wipe the main `target/debug` when free disk < ~100G (it regrows to
  ~170G; the wipe costs one warm rebuild). Prune landed worktrees' target
  dirs freely.
- Cold wide rebuilds at >=14 jobs get OOM-killed by a spawn-churn
  mechanism regardless of free RAM; 10 is the safe ceiling for main-tree
  gates, 6-8 in worktrees.

**Worktree/provenance traps (each caused a real incident):**
- Fresh worktrees base on the repo DEFAULT branch (~280 commits behind).
  ALWAYS verify base and `git reset --hard <intended>` before work.
- Bisect/before-after builds NEVER share a target dir (stale rlibs
  fabricated a false HIGH "determinism defect" once). Separate
  CARGO_TARGET_DIR + provenance marker strings (`strings -a <bin> | grep`)
  before attributing any behavioral difference.
- `cd` persists across tool calls; use `git -C` and check `pwd` before
  destructive git ops.
- OMC writes result files into CWD — `cd()` inside .mos scripts or the
  repo-scan gates trip on the litter.
- Band tables' `working_tree_digest` hashes the diff at DERIVATION time
  (identical across before/after dirs); cite `results_digest` /
  `trace_comparison_digest` / per-model sim-trace sha256 instead.

**Registry discipline** (`crates/rumoca-contracts/data/formal_statements.toml`,
loader `src/registry/formal.rs`, invariants
`tests/formal_statement_invariants.rs`): every semantics decision lands a
row. Tiers: SpecSourced (MLS-cited; Verbatim quotes must already exist in
the quoted file — the guard prevents INTRODUCING quotes but not
mis-transcribing them at the source: one laundered quote was found and
fixed at both ends) / OracleImplied (requires a real omc run config +
evidence — never fake it) / SpecSilent (spec silence established, not
assumed). Pin polarity: Asserts vs PinsDivergence (loader-enforced against
status). **SECTION NUMBERS ARE UNGUARDED AND ARE THE REVIEWER'S JOB**
(three wrong sections caught across two reviews). IDs are append-only per
category; check maxima before adding.

## 6. The roadmap to 340 (the funnel)

| Stage | Now | Gap |
|---|---|---|
| Compile (balanced DAE) | 143 | 423 fail earlier |
| Complete simulation | 71 | 62 solver failures + 1 timeout |
| Compared vs OMC | 69 | 2 excluded (#104) |
| **Strict-high** | **59** | 6 near + 4 deviation |

Ordered by leverage (details and task numbers in §9):

1. **62 solver-completion failures** — already compile; die in solve/Newton.
   Work: phase-2 step-8 divergence fixes on the ONE kernel loop (D-list
   below), #108 (algebraic refresh folded into the §8.6 solve — a recorded
   spec violation), EL005 whole-array connector residue (intersects tensor
   #131). Yield estimate 40–55.
2. **ED018 start-relative sample schedule — 52 models** (Blocks.Math.Mean
   family: `parameter t0(fixed=false); initial equation t0 = time;
   when sample(t0+1/f, 1/f)`). The value genuinely IS the runtime start
   instant (OMC's own binary shifts its grid with t_start — folding would
   be silent-wrong). Fix: an anchor flag on the periodic schedule +
   `t_start` threading (~11 files; timeline.rs). Everything beneath is
   landed (pre-on-continuous 9f522659, deferred-parameter classification
   5bb5de33). Do it AFTER the phase-2 migration (one event loop = built
   once). These models are simulation-viable → direct band movement.
3. **ED019 families ~111 models** (~19 families; biggest: 25+
   function-shape/unsupported-type, 12 clocked §16.5.2 conversion, 9
   impure-call (#76), 9 record-field aggregate + 6 slice residue, 4
   family-row-overlap = tensor #132). Mid-size waves, one or two/day pace.
4. **Frontend: 66 resolve + 53 flatten** — the Fluid/Media wave-queue item
   (stream connectors, media property functions) + the newly named
   **EF004 Connections.branch wall (23 models, #135** — MLS §9.4
   overdetermined connectors, QuasiStatic/Machines reference-angle
   propagation). Least characterized; scope with a census wave first.
5. **Band quality: 6 near + 4 deviation** (#15/#16/#18/#19 zero-circuit,
   #40/#99 comparator polish, #104 exclusions).

Every newly simulating model needs an OMC reference (cache discipline §4c).

## 7. SPEC 0038 (FMI unification) + verification status

- **Phase 1 LANDED** (`2276c4d9`): `rumoca_solver::fmi_me` kernel, rk45
  migrated, compile-enforced boundary (Solve IR crates out of rk45's deps +
  module-graph architecture test), freeze proven twice (184 artifacts
  byte-identical incl. ulp-sensitivity controls). Active-spec cap is 20
  (James). DAE_SCHEMA_VERSION is 15; APPEND wire variants at enum tails
  (decode runs before the version check — two near-misses recorded).
- **B1-a LANDED** (`8f33cea0`): general/implicit path + SDIRK retired,
  hard typed error instead of silent fallback, one solver-name authority
  in rumoca-core.
- **Phase-2 migration IN FLIGHT** (§4b). Then:
- **Step 8 — divergence fixes, ONE PER WAVE, on the shared loop.** The
  D-list (details in ledger; registry rows named): D1 initial-event
  multi-fire (`apply_without_initial_event` flag deletion; FS-SIM-010),
  D2 `when time > 0` start-instant (FS-SIM-014), D3/D3b coincident
  scheduled+located instants (FS-EQN-015/016 — NEITHER backend matches
  OMC; needs fresh OMC adjudication), D4 located-crossing accuracy
  (FS-SIM-011, adapter-side), D5 rk-like assert silence, D6 discrete
  relation-equation updates, #139 Bdf self-rescheduling never fires
  (FS-SIM-016 after W1 lands), D7/D8/D9 adapter items.
  **CRITICAL evidence rule: the MSL band harness is BACKEND-BLIND — every
  band number is a diffsol measurement** (Auto→diffsol;
  plot_compare.rs:282). Fixes to the measured (diffsol) side MUST move
  bands; fixes to the rk45 side must move ZERO; declare which before each
  wave. Deviation from the declared expectation is itself a defect.
- **Phase 3**: CS profile = an ME host + chosen integrator. **Phase 4**:
  packaged FMU / wasm FMI-LS. **#134**: SDIRK returns as an ME host
  (surfaces land WITH the host).
- **Verification** (§4a state): after W1 lands — #136 (Kani CI lane +
  flake input, provability column in SPEC 0040, wave-template obligation,
  MachineChecked registry status), reference slice 2 (continuous coupling
  via supplied trajectories), then the Lean 4 lane (lean4 IS in nixpkgs):
  port the reference, registry rows become lemmas; the crown theorem is
  the event-iteration termination boundary (define the terminating model
  class; termination is false in general). SPEC_0037 promotion needs
  James's sign-off. #119 (declarative buffer starts) and #114 (§8.3.5.1
  same-body simultaneity — compiler evaluates rows sequentially, spec says
  simultaneous; V12 probe: omc b=100, rumoca b=-100) both intersect here.

## 8. Tensor-native track (currently at ZERO — James's pillar 4)

Investigation results (ledger, task #128, closed): the cutover commit
`b14683d1` deleted BOTH the tensor-preservation metric's producer
(rumoca-worker.rs:1052 hardwired `tensor_kpi = None`) AND every
Map/AffineStencil/MatMul construction site — production lowering has
exactly two ComputeNode producers (one narrow LinSolve shape + the
ScalarPrograms sink). `rumoca-tensor-scaling --enforce` (CI) is RED. The
DAE half is healthy (families + compact domains to 2048 points); Solve
emits 0 tensor nodes. JAX/CasADi templates ignore the native_families the
render layer already passes — a neural ODE projects to per-state scalar
assignments. The old 310-model baseline measured report-filers, not
quality (4.86% preserved even then).

Work program: **#130** (S: restore the measurement + rebase the baseline
honestly + SPEC rows), **#131** (L: re-establish Map/AffineStencil
emission + shape in SolveLayout; acceptance = the red CI gate exits 0;
sequence #116 is_matrix conventions FIRST), **#132** (M: lift the ED019
family-row-overlap rejection — also the 2-D PDE/neural-ODE unlock),
**#133** (M: [capabilities.tensor] + native template arms; also the
--emit solve-json vs pipeline divergence found en route).

## 9. Task board snapshot (key open items)

The full board lived in session-local state; these are the items that
matter, with enough context to recreate them:

- **#44** HIGH: initial event discards the §8.6 discrete init result (pre
  snapshot before settle) — partially addressed by 4032af2a; re-validate.
- **#76** impure-call closure (9 ED019 models) + pure() wrapper.
- **#94** falling/<= relation edge semantics (vector half FIXED by
  AnyRise; relation half open).
- **#97** rk45 coincident scheduled+located instants (= D3 family).
- **#98** TimeTable reschedule stall (relates #139).
- **#102** the two fixed_wing GALEC failures (pre-existing, expected-red).
- **#104** certification exclusions (DCPM_Start 10s budget; NandGate).
- **#108** fold algebraic refresh into the §8.6 solve (closes a
  silent-wrong class; FS-SIM-007 records it as a spec violation).
- **#114** §8.3.5.1 same-body simultaneity (V12/P06; also cse3 t_min/t_max).
- **#115** AnyRise polish (dead memory rows; GALEC EGT017 test).
- **#116** is_matrix conventions (BEFORE tensor #131).
- **#118** lint lane misses msl-full-test targets (gate scripts now check
  it manually; wire it into xtask).
- **#119** declarative condition-buffer starts (verifiability).
- **#120** DECISION (James): MLS-conformance vs OMC-parity pinning when
  they genuinely CONFLICT (the two-tier registry is the landed middle way).
- **#123** duplicate reinit last-wins vs OMC; live-read-after-reinit (D1').
- **#124** component-modifier redeclare applies NEITHER type nor dims —
  silent wrong extent (probe C11: rumoca a[1..7] vs OMC a[1..2]).
- **#126** D11 interactive session skips §8.6 init (fixed by phase-2 step 6).
- **#127** test-integrity hygiene: 3 files with inert RkLike pins that
  actually run diffsol (the `simulate_dae` alias trap — it is diffsol
  UNCONDITIONALLY; use `simulate_dae_with_diagnostics`); ~22 never-compiled
  tests under tests/examples_smoke/ (autotests=false, no mods — includes
  the tensor smoke tests that would have caught §8's regression).
- **#130–#133** tensor program (§8). **#134** SDIRK ME host. **#135**
  Connections.branch wall. **#136** Kani enforcement infra. **#137** HIGH:
  binding resolution uses the component owner's instance, not
  binding_source_scope (instanced.rs:702) — wrong answers BOTH directions
  (probes A1/A4); D1-redeclared-record shape is this too. **#138**
  serde_json needs float_roundtrip (1-ULP JSON wire drift). **#139** Bdf
  self-rescheduling divergence (step 8).
- Also open: #11 (publish honest-parity metric — James), #81 (WR001
  warning filter — James), #15–#21, #26–#32 (comparator/CI honesty),
  #46–#63 (initialization + misc MEDIUM/LOW), #66, #70, #71 (reference
  cache lifecycle), #75, #78–#80, #82, #85, #87–#89, #95, #96, #99–#101,
  #110 (cache prune-storm), #112.

## 10. Key locations

- Ledger: `dev/2026-07-28-spec-0036-cutover-checklist.md` (this session's
  full evidence trail; append entries in its established style).
- Wave queue: `dev/2026-07-31-wave-queue.md`. Canary: `dev/msl-canary-20.json`.
- Registry: `crates/rumoca-contracts/data/formal_statements.toml` (74 rows
  after W1 lands; invariants in tests/formal_statement_invariants.rs).
- Specs: `spec/` (SPEC_0000 governance — active cap 20; SPEC_0008 phase
  errors; SPEC_0022 MLS compliance catalog — the section-number authority;
  SPEC_0029/0041 crate ownership; SPEC_0032 range-preserving tensors;
  SPEC_0036 valid-by-construction; SPEC_0037 verification [DRAFT,
  provisional]; SPEC_0038 unified FMI; SPEC_0039 sparsity [blocked on
  #131]; SPEC_0040 stage contracts).
- Certification results dir: `target/msl/results-landed-2` — its
  `sim_traces/omc` subtree is the SACRED reference cache; msl_band_table,
  transition diffs, msl_results.json (machine-readable per-model
  classification: the failure-census source).
- MSL sources: `target/msl/ModelicaStandardLibrary-4.1.0/`.
- omc `a96aa1a-cmake` is on PATH inside `nix develop`. ALL cargo commands
  need `nix develop --command` (bare cargo is not on PATH).
- The FMI kernel: `crates/rumoca-solver/src/fmi_me.rs` (+ kernel.rs,
  no_state.rs); boundary test
  `crates/rumoca/tests/architecture_hardening/fmi_me_boundary.rs`.
- Reference interpreter (after W1 lands): `crates/rumoca-reference`.

## 11. Open decisions awaiting James

1. SPEC_0037 DEFERRED→DRAFT sign-off (provisional; one-rename revert).
2. #120: pin MLS or OMC when they genuinely conflict.
3. #81: WR001-class warning severity filter.
4. #11: publish the honest-parity metric.

## 12. The one-paragraph orientation

This compiler is being driven to OMC trace parity wall-by-wall with an
adversarial-review-everything culture: every wave is measured (probes, OMC
matrices, byte-identity with provenance-checked binaries, cohort sweeps
through repo tooling only), every claim is attackable and several were
overturned by instrumentation, and every semantics decision lands in a
machine-checked registry. The three strategic threads — the parity funnel
(§6), the FMI-kernel consolidation that removes the two-backend tax (§7),
and the verification/tensor pillars (§7/§8) — are deliberately sequenced
so semantic work lands once, on one event loop, provable and
tensor-preserving. Keep the discipline and the score follows: it moved
54→59 while the compile funnel, the kernel, the registry, and the
verification foundation were all built in one session.
