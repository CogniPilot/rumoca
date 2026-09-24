# SMR_DOL first rejected trial at 499b5533f

Diagnostic investigation only; no recovery or parity claim. Main's canonical
12-second run still reports `EX001`, step size too small at time zero
(`sim_run_seconds = 0.011104607`). Its result is under
`/tmp/rumoca-fluid/target/fluid-campaign/recovery-initial-boundary-499b5533f/model_worker/Modelica.Electrical.Machines.Examples.SynchronousMachines.SMR_DOL/`.

Source commit: `499b5533fcf2d4d319bce9f80ab2c8e6f898a9d9`. Investigation artifacts:
`/tmp/rumoca-zero-rhs-test/fluid-diagnostic/smr-499b5533f/`.
`first-producer-proof.json` contains bounded values, typed channel-to-visible
correlation, storage bindings, source/IR/log hashes, and an independent source
calculation. `extract-proof.pl` regenerates it. Each diagnostic executable hash
is paired with its exact instrumentation patch in `probe*-hashes.txt`.
All runtime probes have been removed from the worktree.

## Established path

1. MSL `SMR_DOL` fixes the initial stator and rotor currents to zero. The
   switch closes at 0.1 s. Before then its **finite** conductance is 1e-5 S;
   the source equations are `v=s` and `i=Goff*s`. An off switch does not imply
   zero state derivatives. The phase voltages at zero are
   `[0, -70.71067811865476, 70.71067811865473]` V.
2. The observed shared-ME initial point has exactly zero stator and rotor
   currents, zero switch voltage/current, and `off=true`, `control=false`.
   At zero speed, the coupled q-axis source equations give
   `Leff = Lssigma + Lmq*Lrsigmaq/(Lmq+Lrsigmaq)`.
   The compiled parameters are 0.0003183098861837907,
   0.0028647889756541161, and 0.00015915494309189535 H, respectively.
   Thus `Leff=0.000469088253323481` H and
   `der(iq)=-sqrt(2/3)*100/Leff=-174060.33409339582` A/s.
   ME returns -174060.33409339574. The sixth runtime coordinate has derivative
   -164899.26387795387, versus source-derived -164899.26387795396;
   that coordinate has opposite sign to the published damper current.
3. The first converged implicit trial is at `2.1891181066867194e-13` s.
   Newton's remaining-correction observable norm is `3.128705511750054e-6`.
   The subsequent step-error norm is **12.040274117750219**. Its limiting
   channel is observable ordinal **551**, visible index **554**,
   `idealCloser.v[3]`, storage **Y483**, nominal 1. Actual/alternative voltages
   are 0.0032997613745919807 / 0.003299802309143729 V; the unchanged bound is
   `1e-10 + 1e-6*max(abs(actual),abs(alternative)) = 3.399802309143729e-9` V.
4. Halving the step to `1.0945590533433597e-13` s yields Newton norm
   `2.214510977553941e-6`, then step-error norm **5.848168839273632**.
   The next error-driven reduction crosses the numerical library's default
   absolute `min_timestep=1e-13` (Diffsol `ode_solver/problem.rs`), which
   `Bdf::_set_step_size` rejects. These small positive coordinates are
   representable. The RK plugin separately has an absolute 1e-12 floor;
   neither floor is supplied by `MeNumericalSetup`.

The relevant source laws survive shared-ME projection: over all 12 bounded
RHS observations, the largest `idealCloser.v[3] - idealCloser.i[3]/Goff`
residual is 5.313e-15 V. This rejects a wrong switch-law projection as the
explanation for the roughly 4.09e-8 V actual/alternative difference.
The polar-angle channel does not limit either observed trial. The relevant
source IC and derivative calculations reject an erroneous zero-flow startup
assumption; this is not a claim that every initialization equation was audited.

Clean c8a57c038 and probed 499b5533f Flat, DAE, and Solve files are byte-identical:

| IR | SHA256 |
|---|---|
| Flat | `dcd9ee741212546d2cdaf9772906e7ace8fd6fdf6022e00d047e847dfc6385d8` |
| DAE | `c15bdda7133632442e86bae2cc34ba505eb6ca208cd658525b789dac0aa32ee3` |
| Solve | `5f8a2b86d49f560370e4959e0bae3e1d7dd971d05262238c374baa9f26d2fbb3` |

## Reproduction and scope

Each actual-model diagnostic used this command with explicit workdir
`/tmp/rumoca-zero-rhs`; the only source differences were the frozen probes:

```sh
nix develop /tmp/rumoca-fluid#modelica --command bash -lc 'cd /tmp/rumoca-zero-rhs; export CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 CARGO_TARGET_DIR=/tmp/rumoca-zero-rhs-test; cargo xtask repo msl debug-model --model Modelica.Electrical.Machines.Examples.SynchronousMachines.SMR_DOL --json --simulate --dev --timeout-secs 120'
```

The 120-second command is explicitly diagnostic; it is not canonical solver
budget evidence. No additional canonical baseline run was made by this worker.
The clean c8a57c038 artifacts and worker hash remain in the sibling directory
`smr-c8a57c038/clean/` and `clean-hashes.txt`.

Governing active specs: SPEC0033 §§2–6 (first divergence, competing hypothesis,
strict verification), SPEC0007 Stage 4 (source-preserving checked execution),
SPEC0022 MLS §8.6 (initialization), SPEC0029 §12 (shared execution ownership),
SPEC0021 (maintainability). SPEC0038 is DRAFT; its numerical-policy discussion
is not independently an active rule. Existing observable-error behavior is
documented in SPEC0044 ME-INT-007. The user approved aligning both plugins
with the existing shared time-progress contract after reviewing this proof.

The ED008 checkpoint remains on signed commit f7568a59, and the accepted
census on cef98db1 (main integrated as 499b5533f). Neither is being edited.

## Bounded candidate

`MeAdvanceRequest::minimum_step_duration` derives the first representable
endpoint beyond the existing `accepted_step_roundoff` test. It adds that
roundoff to the current time and advances one representable coordinate if
the rounded result still fails the strict progress test. This is the same
time geometry already enforced when the host binds an accepted candidate.
Both numerical adapters consume the request's floor. Their independent error
norms, retry limits, timeout configuration, and the host's final admission
checks remain unchanged. No source equation or initial value changes.

The source fixture is a finite-conductance RL startup with closed form
`v(t)=70*(1-exp(-(t-origin)*secondsPerUnit/5e-9))`, `i=1e-5*v`.
It checks seconds, microseconds, and kiloseconds, plus positive and negative
time origins through the dispatching public simulation API. Its selected
samples satisfy `1e-10+1e-6*abs(exact)` for this analytic problem; this is
observed fixture accuracy, not a general assertion equating global error with
a local integration tolerance.

| Executed source-only baseline / candidate | Result |
|---|---|
| 499b5533f, seven-test fixture | 4 pass, 3 startup failures: BDF seconds/kiloseconds, RK kiloseconds |
| Shared-progress candidate, same fixture | 7 pass, 0 fail |
| Shared runtime including origin/progress and rounded-away refusal tests | 529 pass, 0 fail |
| BDF / RK adapter suites | 11 / 6 pass, 0 fail |

Exact old-red source patch, log, and executable hashes are
`source-old-red.{patch,log}` and `source-old-red-hashes.txt` in the artifact
directory. Candidate log: `source-new-green.log`; shared/interface log:
`shared-interface-tests.log`. Tests ran with the same post-Nix environment as
above, using `cargo test -p rumoca --test suite_core finite_conductance_startup
-- --nocapture` and `cargo test -p rumoca-solver -p rumoca-solver-diffsol -p
rumoca-solver-rk45 --lib`.

The earlier four-test `fixture-before` attempt inadvertently used
`simulate_dae`, an alias for BDF, so its RkLike-labelled rows **do not** establish
RK coverage. `fixture-dispatched` corrects that API and records 3 pass / 1 BDF
startup failure. The final seven-test old-red/new-green pair uses correct
dispatch throughout. Main was notified that its initial-boundary fixture
had the same alias trap; that separate correction remains main-owned.

The actual-model 120-second **dev diagnostic** completes: `sim_ok`,
`ic_ok`, run time **60.591173248 s**. Its first accepted endpoint is
`2.2762823599129497e-14` s, followed by `4.5525647198258994e-14` and
`8.450069845479811e-14` s. This proves removal of the first rejected-trial
blocker, not canonical-budget recovery or trace parity. Result, request, IR,
and trace are frozen in `after-progress-diagnostic/`; the bounded acceptance
probe and executable hashes are in `accepted-probe-and-policy.patch` and
`accepted-probe-hashes.txt`. The probe is removed from the candidate.

Strict Clippy passes for `rumoca-solver`, both numerical adapters, and `rumoca`,
all targets, with `-D warnings` (`strict-clippy.log`).

## Canonical result and handoff

Frozen implementation: `9b9811b09aa6ffe2d2954a74cdb5a2a7c9a7e7ea`, authored and
signed off by James Goppert. Main integrated it as a27611b4e.
One canonical attempt at that clean implementation used the standard
`msl-fast` artifacts, the unchanged 12-second solver budget, and this command
inside the same Nix environment and owned workdir/target:

```sh
cargo xtask verify msl-parity --sim-targets-file /tmp/rumoca-zero-rhs-test/fluid-diagnostic/smr-499b5533f/smr-target.json --results-dir /tmp/rumoca-zero-rhs-test/fluid-diagnostic/smr-9b9811b0-canonical-12 --stage-parallelism 1 --sim-parallelism 1 --require-selected-targets-success --no-remote-quality-baseline --sim-timeout-secs 12
```

Result: compile Success; `sim_status=sim_solver_fail`, `failure_bucket=Timeout`,
`sim_error="timeout after 12.000s"`, `sim_run_seconds=12.009455014`.
The focused gate exits nonzero. The initial minimum-step failure is repaired;
**canonical runtime recovery remains unresolved**. No strict-high or parity
claim follows from the longer diagnostic's completion. No retry was performed.

Canonical request/result reside under that results directory's
`model_worker/Modelica.Electrical.Machines.Examples.SynchronousMachines.SMR_DOL/`.
`canonical-12.log` and `canonical-hashes.txt` in the investigation directory
retain the command/build log, optimized worker hashes, and request/result
hashes. `frozen-9b9811b0.patch` is the reviewable implementation patch;
`frozen-hashes.txt` and `review-artifact-hashes.txt` pin its supporting evidence.

Main owns subsequent canary/canonical work and the independent correction of
the initial-boundary/output test dispatch. This worker is parked with no
runtime probes or pending source edits; ED008 remains paused.
