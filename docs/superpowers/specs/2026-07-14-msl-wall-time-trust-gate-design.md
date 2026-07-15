# MSL Wall-Time Trust Gate Design

## Context

The full MSL quality gate compares Rumoca runtime speedup against a promoted
baseline. System-time and wall-time medians currently use the same blocking
policy: either may regress by at most 35%.

The wall comparison is not always paired. The parity harness can reuse cached
OMC timings while refreshing Rumoca timings on the current host. In the
observed failure, the source commit did not change between runs, system-time
speedup stayed stable, and wall-time speedup moved materially while the host
was heavily loaded and worker CPU pinning failed. Treating that result as a
code regression makes the gate sensitive to measurement provenance and host
scheduling rather than only to Rumoca performance.

This is a ClimaMind-fork change. It will be committed and pushed only to the
ClimaMind `origin` feature branch; no pull request or push to the CogniPilot
`upstream` remote is part of this work.

## Goals

- Keep correctness, coverage, trace, and system-time regression checks
  fail-closed.
- Keep the existing 35% runtime tolerance.
- Make wall-time blocking conditional on a trustworthy, paired measurement.
- Preserve wall-time metrics and warnings when the measurement is not trusted.
- Record enough provenance in generated artifacts to explain every blocking or
  advisory decision.

## Non-Goals

- Do not relax stage-count, balance, simulation, trace, or system-time gates.
- Do not change the promoted baseline to make the current result pass.
- Do not silently ignore missing runtime data.
- Do not add retry loops or environment-specific bypass variables.
- Do not submit this downstream policy change to CogniPilot upstream.

## Considered Approaches

### 1. Increase the 35% tolerance

Rejected. A wider threshold hides real regressions without fixing the
comparison between cached and fresh measurements.

### 2. Make wall time permanently advisory

Rejected. Wall time remains valuable on a controlled, paired benchmark and can
catch scheduler, allocation, or I/O regressions that system time misses.

### 3. Trust-qualified wall-time gate

Selected. System time remains blocking on every complete parity run. Wall time
is blocking only when its provenance and host-health evidence show that the OMC
and Rumoca measurements are comparable. Otherwise the same regression is
reported as an advisory warning.

## Design

### Measurement provenance

The parity artifact and quality snapshot will record a structured wall-time
trust context. It will include:

- whether all OMC runtime samples used by the comparison were generated fresh
  in the current invocation or whether any were resumed from cache;
- requested and effective worker counts and OMC thread count;
- actual Rumoca worker CPU-affinity success and failure counts;
- normalized one-minute host load sampled before and after the measured work,
  when the platform exposes it safely;
- a reason list explaining why wall time is trusted or advisory.

Missing provenance is conservative: it makes wall time advisory, not trusted.
It does not make the full parity artifact valid when required runtime or trace
samples are missing.

### Trust decision

Wall time is blocking only when all of the following are true:

1. Every compared OMC runtime sample is fresh in the current invocation.
2. The Rumoca workers requested for pinning report successful affinity.
3. Host load is available and remains within the documented normalized limit
   before and after the measured work.
4. Runtime worker/thread context matches the comparison policy.

If any condition fails, the wall median and its baseline delta remain visible,
but `push_runtime_ratio_regression_reasons` does not add the wall regression to
the blocking reason list. It adds an advisory status with the failed trust
conditions instead. System-time regression remains blocking regardless of the
wall-time trust decision.

Normalized load is the one-minute load average divided by available logical
CPUs. The initial limit will be `1.5`. This allows the benchmark itself to
occupy the host while rejecting the observed multi-fold oversubscription.
Platforms without a safe load source report load as unavailable and therefore
cannot produce a blocking wall-time verdict.

### Affinity reporting

The model-worker ready handshake will report whether a requested CPU affinity
was applied. The MSL scheduler will aggregate actual successes and failures;
the existing `pinned_worker_count` field will represent successful pinning,
not merely the number of planned core assignments. This keeps the trust
decision based on worker evidence rather than parsing warning text.

### Output and documentation

The console status, `omc_simulation_reference.json`, and
`msl_quality_current.json` will distinguish:

- `PASS`: trusted wall measurement and no regression;
- `FAIL`: trusted wall measurement exceeds the existing tolerance;
- `ADVISORY`: wall measurement is present but not trusted, with reasons.

`SPEC_0025_PR_REVIEW_PROCESS.md` and the MSL quality-gate developer guide will
state that the 35% wall-time rule applies only to trust-qualified paired
measurements. The system-time rule remains unconditional.

## Error Handling

- Missing OMC parity, empty runtime samples, stale target sets, or missing
  trace data continue to fail closed under the existing rules.
- Missing trust metadata downgrades only the wall regression verdict to
  advisory.
- Malformed trust metadata is treated as untrusted and surfaced in the status
  reason list.
- No fallback promotes a cached or unhealthy wall measurement to trusted.

## Test Strategy

Development follows red-green-refactor:

1. Add a failing unit test proving a cached OMC reference cannot create a
   blocking wall regression while system regression still blocks.
2. Add failing trust-decision tests for affinity failure, excessive normalized
   load, missing provenance, and a fully trusted measurement.
3. Add protocol tests proving worker affinity status survives the ready
   handshake and is aggregated as actual success/failure counts.
4. Add snapshot/parsing tests for the new provenance and advisory status.
5. Run the focused runtime-ratio and cache-resume test filters.
6. Run formatting, clippy for affected crates, architecture/spec tests, and the
   repository-required verification appropriate to the final diff.

The heavyweight MSL gate will be used to confirm that an unhealthy or cached
measurement reports `ADVISORY` rather than a false performance regression. A
clean-host trusted-wall pass is reported only if the run actually satisfies
all trust conditions.

## Delivery

Implementation will remain on `cli-52-upstream-equivalent-cleanup` in the
ClimaMind fork. The branch will be pushed only to
`https://github.com/climamind/rumoca.git`. The `upstream` remote will be used
only as a read-only reference and will not receive commits, branches, or pull
requests.
