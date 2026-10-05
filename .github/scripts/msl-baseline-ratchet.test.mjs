import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import { fileURLToPath } from 'node:url';

import { cohortComparison } from './msl-baseline-cohort.mjs';
import {
  ensurePromotableSnapshot,
  promoteBaselineIfImproved,
  ratchetDecision,
} from './msl-baseline-ratchet.mjs';

const COHORT_CASES = JSON.parse(
  fs.readFileSync(new URL('./msl-baseline-cohort-cases.json', import.meta.url), 'utf8'),
);
const REAL_STATE_SETS = JSON.parse(
  fs.readFileSync(new URL('./msl-baseline-real-state-sets.json', import.meta.url), 'utf8'),
);

const CHECKED_IN_BASELINE_PATH = fileURLToPath(
  new URL(
    '../../crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json',
    import.meta.url,
  ),
);

function boundary(from, to, previous = undefined) {
  const migration = {
    from_quality_gate_version: from,
    to_quality_gate_version: to,
    change: `reviewed-boundary-v${to}`,
    exclusions_sha256: `digest-v${to}`,
    evidence_git_commit: `commit-v${to}`,
  };
  if (previous !== undefined) {
    migration.previous = previous;
  }
  return migration;
}

// Reviewed chain 1 -> 2 -> 3 -> 4; a snapshot at version N declares the
// boundary that ends at N, with every older boundary as `previous`.
function boundaryChain(version) {
  let chain;
  for (let to = 2; to <= version; to += 1) {
    chain = boundary(to - 1, to, chain);
  }
  return chain;
}

function fullSnapshot(version = 4) {
  return {
    quality_gate_version: version,
    run_scope: 'full',
    omc_version: 'OpenModelica 1.27.0',
    simulatable_attempted: 10,
    sim_target_models: 10,
    parse_models: 10,
    flatten_models: 9,
    dae_models: 8,
    compiled_models: 8,
    solve_models: 7,
    balanced_models: 8,
    balance_denominator: 8,
    initial_balanced_models: 8,
    initial_unbalanced_models: 0,
    sim_attempted: 7,
    ic_attempted: 6,
    ic_ok: 6,
    sim_ok: 5,
    partial_models: 1,
    unbalanced_models: 0,
    ic_solver_fail: 2,
    trace_exceptions_sha256: `digest-v${version}`,
    reference_boundary_migration: boundaryChain(version),
    unexcepted_non_high_models: ['Modelica.Open.Defect'],
    runtime_ratio_stats: {
      system_ratio_both_success: { median: 2.0 },
      wall_ratio_both_success: { median: 10.0 },
    },
    trace_accuracy_stats: {
      models_compared: 5,
      agreement_high: 3,
      agreement_minor: 1,
      agreement_deviation: 1,
      bad_channels_total: 10,
      severe_channels_total: 2,
      models_with_severe_channel: 1,
      models_with_any_channel_deviation: 2,
      violation_mass_total: 3.5,
      initial_condition: {
        deviation_channels_total: 4,
        severe_channels_total: 1,
        violation_mass_total: 2.0,
      },
      state_selection: {
        exact_state_set_match_models: 4,
        total_rumoca_only_states: 3,
        total_omc_only_states: 2,
      },
    },
  };
}

function approvedOmcMigration(from, to) {
  const checkedIn = fullSnapshot();
  checkedIn.omc_version = to;
  checkedIn.omc_context_migration = {
    from_omc_version: from,
    to_omc_version: to,
    sim_target_models: checkedIn.sim_target_models,
  };
  return checkedIn;
}

// Promote `current` over `baseline`, judged by the checked-in baseline of the
// same commit (the current snapshot's own reviewed schema).
function decide(current, baseline, checkedIn = fullSnapshot()) {
  return ratchetDecision(current, baseline, checkedIn);
}

test('promotable snapshot accepts full non-partial artifacts', () => {
  const snapshot = fullSnapshot();
  assert.doesNotThrow(() => ensurePromotableSnapshot(snapshot, 4));
  assert.doesNotThrow(() => ensurePromotableSnapshot({ ...snapshot, partial: false }, 4));
});

test('promotable snapshot rejects partial artifacts and a foreign schema', () => {
  assert.throws(
    () => ensurePromotableSnapshot({ ...fullSnapshot(), partial: true }, 4),
    /partial snapshots cannot be promoted/,
  );
  assert.throws(
    () => ensurePromotableSnapshot({ ...fullSnapshot(), run_scope: 'partial' }, 4),
    /only full MSL quality snapshots/,
  );
  assert.throws(
    () => ensurePromotableSnapshot(fullSnapshot(), 5),
    /differs from the reviewed checked-in baseline/,
  );
});

test('ratchet requires the reviewed checked-in baseline', () => {
  assert.throws(
    () => ratchetDecision(fullSnapshot(), fullSnapshot(), null),
    /reviewed checked-in baseline is required/,
  );
});

test('ratchet promotes non-regressing improvements', () => {
  const baseline = fullSnapshot();
  const current = fullSnapshot();
  current.solve_models = 8;
  current.trace_accuracy_stats.agreement_high = 4;
  current.trace_accuracy_stats.agreement_deviation = 0;
  current.trace_accuracy_stats.bad_channels_total = 8;
  current.trace_accuracy_stats.violation_mass_total = 2.5;

  const decision = decide(current, baseline);
  assert.equal(decision.promote, true);
  assert.match(decision.improvements.join('\n'), /solve models/);
  assert.match(decision.improvements.join('\n'), /high trace agreement/);
});

test('ratchet skips equivalent snapshots', () => {
  const decision = decide(fullSnapshot(), fullSnapshot());
  assert.equal(decision.promote, false);
  assert.match(decision.reason, /equivalent/);
});

test('ratchet skips when any ratchet metric regresses', () => {
  const baseline = fullSnapshot();
  const current = fullSnapshot();
  current.sim_ok = 6;
  current.trace_accuracy_stats.bad_channels_total = 11;

  const decision = decide(current, baseline);
  assert.equal(decision.promote, false);
  assert.match(decision.reason, /trace bad channels/);
});

test('ratchet rejects changed fixed target context', () => {
  const baseline = fullSnapshot();
  const current = fullSnapshot();
  current.sim_target_models = 11;

  assert.throws(() => decide(current, baseline), /sim_target_models changed/);
});

test('ratchet rejects a snapshot under a schema other than the checked-in one', () => {
  assert.throws(
    () => decide(fullSnapshot(4), fullSnapshot(4), fullSnapshot(3)),
    /differs from the reviewed checked-in baseline/,
  );
});

test('ratchet crosses the reviewed reference boundaries down to the promoted schema', () => {
  for (const version of [1, 2, 3]) {
    const decision = decide(fullSnapshot(4), fullSnapshot(version));
    assert.equal(decision.promote, true, `from version ${version}`);
    const boundaries = decision.improvements.filter((line) => line.startsWith('reference boundary'));
    assert.deepEqual(
      boundaries,
      Array.from(
        { length: 4 - version },
        (_, index) =>
          `reference boundary: ${version + index} -> ${version + index + 1} (reviewed-boundary-v${version + index + 1})`,
      ),
    );
    assert.match(decision.improvements.join('\n'), /trace exceptions: reviewed boundary/);
  }
});

test('ratchet still compares every metric across a reference boundary', () => {
  for (const lower of [
    (snapshot) => { snapshot.compiled_models -= 1; },
    (snapshot) => { snapshot.trace_accuracy_stats.agreement_high -= 1; },
    (snapshot) => { snapshot.runtime_ratio_stats.wall_ratio_both_success.median = 6.0; },
  ]) {
    const current = fullSnapshot(4);
    lower(current);
    const decision = decide(current, fullSnapshot(2));
    assert.equal(decision.promote, false);
    assert.match(decision.reason, /regression/);
  }
});

test('ratchet refuses a promoted schema the reviewed chain does not reach', () => {
  // The quality-schema-13 failure: the current snapshot carries an unrelated
  // historical migration record, and the promoted asset sits at a version the
  // chain reaches only through reviewed boundaries.
  const legacy = fullSnapshot(4);
  legacy.metric_schema_migration = { from_quality_gate_version: 2, to_quality_gate_version: 3 };
  assert.equal(decide(legacy, fullSnapshot(3)).promote, true);

  const unreached = fullSnapshot(0);
  delete unreached.reference_boundary_migration;
  assert.throws(() => decide(fullSnapshot(4), unreached), /reaches quality schema 0/);

  const newer = fullSnapshot(5);
  assert.throws(() => decide(fullSnapshot(4), newer), /newer than current/);
});

test('ratchet refuses a forged or skipped boundary chain', () => {
  const forgedPromoted = fullSnapshot(2);
  forgedPromoted.reference_boundary_migration.exclusions_sha256 = 'unreviewed';
  assert.throws(
    () => decide(fullSnapshot(4), forgedPromoted),
    /promoted reference boundary differs from the reviewed chain/,
  );

  const forgedCurrent = fullSnapshot(4);
  forgedCurrent.reference_boundary_migration.previous.change = 'unreviewed';
  assert.throws(
    () => decide(forgedCurrent, fullSnapshot(2)),
    /differs from the reviewed checked-in chain/,
  );

  const skipping = fullSnapshot(4);
  skipping.reference_boundary_migration.previous = boundary(1, 2, boundary(1, 1));
  assert.throws(
    () => decide(skipping, fullSnapshot(2), structuredClone(skipping)),
    /does not continue the chain at 3/,
  );
});

test('a crossed boundary admits its reviewed roster additions only', () => {
  const checkedIn = fullSnapshot(4);
  checkedIn.reference_boundary_migration.roster_additions = [
    { model_name: 'Modelica.Reviewed.Addition' },
  ];
  const current = structuredClone(checkedIn);
  current.unexcepted_non_high_models.push('Modelica.Reviewed.Addition');
  const baseline = fullSnapshot(3);
  const decision = decide(current, baseline, checkedIn);
  assert.equal(decision.promote, true);
  assert.match(decision.improvements.join('\n'), /reviewed addition Modelica.Reviewed.Addition/);

  current.unexcepted_non_high_models.push('Modelica.Unreviewed');
  const regressed = decide(current, baseline, checkedIn);
  assert.equal(regressed.promote, false);
  assert.match(regressed.reason, /roster gained Modelica.Unreviewed/);
});

test('a reference boundary cannot also change the OMC context', () => {
  const current = fullSnapshot(4);
  current.omc_version = 'OpenModelica new';
  const baseline = fullSnapshot(3);
  baseline.omc_version = 'OpenModelica old';
  const checkedIn = approvedOmcMigration(baseline.omc_version, current.omc_version);
  assert.throws(() => decide(current, baseline, checkedIn), /cannot change the OMC context/);
});

test('OMC migration compares independent metrics without cross-context trace rejection', () => {
  const baseline = fullSnapshot();
  baseline.omc_version = 'OpenModelica old';
  const current = fullSnapshot();
  current.omc_version = 'OpenModelica new';
  current.trace_accuracy_stats.agreement_high = 0;
  current.runtime_ratio_stats.system_ratio_both_success.median = 0.01;
  const checkedIn = approvedOmcMigration(baseline.omc_version, current.omc_version);

  const decision = decide(current, baseline, checkedIn);
  assert.equal(decision.promote, true);
  assert.match(decision.improvements.join('\n'), /OMC context/);

  current.compiled_models -= 1;
  const regressed = decide(current, baseline, checkedIn);
  assert.equal(regressed.promote, false);
  assert.match(regressed.reason, /compiled models/);
});

test('OMC migration rejects missing or mismatched checked-in approval', () => {
  const baseline = fullSnapshot();
  baseline.omc_version = 'OpenModelica old';
  const current = fullSnapshot();
  current.omc_version = 'OpenModelica new';

  const unapproved = fullSnapshot();
  unapproved.omc_version = current.omc_version;
  assert.throws(() => decide(current, baseline, unapproved), /reviewed checked-in migration/);
  const reversed = approvedOmcMigration(current.omc_version, baseline.omc_version);
  assert.throws(() => decide(current, baseline, reversed), /does not match current snapshot/);
});

test('ratchet rejects a runtime speedup drop beyond 35 percent', () => {
  const baseline = fullSnapshot();
  const current = fullSnapshot();
  current.sim_ok = 6;
  current.runtime_ratio_stats.system_ratio_both_success.median = 1.29;
  const decision = decide(current, baseline);
  assert.equal(decision.promote, false);
  assert.match(decision.reason, /runtime system speedup median/);
});

function writeFixtures(baseline, source, checkedIn) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'msl-ratchet-'));
  const paths = {
    baselinePath: path.join(dir, 'baseline.json'),
    sourcePath: path.join(dir, 'source.json'),
    checkedInBaselinePath: path.join(dir, 'checked-in.json'),
  };
  fs.writeFileSync(paths.baselinePath, JSON.stringify(baseline, null, 2));
  fs.writeFileSync(paths.sourcePath, JSON.stringify(source, null, 2));
  fs.writeFileSync(paths.checkedInBaselinePath, JSON.stringify(checkedIn, null, 2));
  return paths;
}

test('promoteBaselineIfImproved writes only when improved', () => {
  const source = fullSnapshot();
  source.sim_ok = 6;
  const paths = writeFixtures(fullSnapshot(), source, fullSnapshot());

  const decision = promoteBaselineIfImproved({ ...paths, log: () => {} });
  assert.equal(decision.promote, true);
  assert.equal(JSON.parse(fs.readFileSync(paths.baselinePath, 'utf8')).sim_ok, 6);
});

test('promotion across a reference boundary writes the new-schema snapshot', () => {
  const paths = writeFixtures(fullSnapshot(2), fullSnapshot(4), fullSnapshot(4));
  const decision = promoteBaselineIfImproved({ ...paths, log: () => {} });
  assert.equal(decision.promote, true);
  assert.equal(
    JSON.parse(fs.readFileSync(paths.baselinePath, 'utf8')).quality_gate_version,
    4,
  );
});

test('promotion loads the reviewed checked-in OMC migration', () => {
  const baseline = fullSnapshot();
  baseline.omc_version = 'OpenModelica old';
  const source = fullSnapshot();
  source.omc_version = 'OpenModelica new';
  const checkedIn = approvedOmcMigration(baseline.omc_version, source.omc_version);
  const paths = writeFixtures(baseline, source, checkedIn);

  const decision = promoteBaselineIfImproved({ ...paths, log: () => {} });
  assert.equal(decision.promote, true);
  assert.equal(
    JSON.parse(fs.readFileSync(paths.baselinePath, 'utf8')).omc_version,
    source.omc_version,
  );
});

test('promoteBaselineIfImproved leaves equivalent baseline unchanged', () => {
  const paths = writeFixtures(fullSnapshot(), fullSnapshot(), fullSnapshot());
  const baselineText = fs.readFileSync(paths.baselinePath, 'utf8');

  const decision = promoteBaselineIfImproved({ ...paths, log: () => {} });
  assert.equal(decision.promote, false);
  assert.equal(fs.readFileSync(paths.baselinePath, 'utf8'), baselineText);
});

// The tracked baseline is the reviewed schema of this commit: a run equal to it
// must ratchet over a promoted asset at every version its boundary chain
// reaches, so a new boundary cannot land while the ratchet refuses it.
test('the tracked baseline ratchets over every promoted schema its chain reaches', () => {
  const checkedIn = JSON.parse(fs.readFileSync(CHECKED_IN_BASELINE_PATH, 'utf8'));
  const current = structuredClone(checkedIn);
  const schema = checkedIn.quality_gate_version;
  assert.equal(ratchetDecision(current, structuredClone(checkedIn), checkedIn).promote, false);

  let link = checkedIn.reference_boundary_migration;
  let reached = 0;
  while (link && link.previous) {
    const promoted = structuredClone(checkedIn);
    promoted.quality_gate_version = link.from_quality_gate_version;
    promoted.reference_boundary_migration = structuredClone(link.previous);
    promoted.trace_exceptions_sha256 = link.previous.exclusions_sha256;
    const decision = ratchetDecision(current, promoted, checkedIn);
    assert.equal(decision.promote, true, `from version ${promoted.quality_gate_version}`);
    assert.match(
      decision.improvements.join('\n'),
      new RegExp(`reference boundary: ${link.from_quality_gate_version} -> ${link.to_quality_gate_version}`),
    );
    reached += 1;
    link = link.previous;
  }
  assert.ok(reached > 0, `quality schema ${schema} declares no crossable boundary`);
});

// RFC 7396 JSON merge patch: objects merge, null deletes, anything else
// replaces.
function mergePatch(target, patch) {
  if (typeof patch !== 'object' || patch === null || Array.isArray(patch)) {
    return structuredClone(patch);
  }
  const result =
    typeof target === 'object' && target !== null && !Array.isArray(target)
      ? structuredClone(target)
      : {};
  for (const [key, value] of Object.entries(patch)) {
    if (value === null) {
      delete result[key];
    } else {
      result[key] = mergePatch(result[key], value);
    }
  }
  return result;
}

function cohortCaseSnapshot(side) {
  const patches = Array.isArray(side) ? side : [side];
  return patches.reduce(
    (snapshot, patch) => mergePatch(snapshot, typeof patch === 'string' ? COHORT_CASES[patch] : patch),
    COHORT_CASES.base,
  );
}

const metricLabel = (line) => line.split(':')[0].split(' over ')[0];

for (const cohortCase of COHORT_CASES.cases) {
  test(`cohort comparison: ${cohortCase.name}`, () => {
    const reference = cohortCaseSnapshot(cohortCase.reference);
    const candidate = cohortCaseSnapshot(cohortCase.candidate);
    if (cohortCase.error) {
      assert.throws(() => cohortComparison(reference, candidate), new RegExp(cohortCase.error));
      return;
    }
    const verdict = cohortComparison(reference, candidate);
    assert.deepEqual(verdict.regressions.map(metricLabel), cohortCase.regressions);
    assert.deepEqual(verdict.improvements.map(metricLabel), cohortCase.improvements);
  });
}

// A snapshot from per-model evidence, with the totals the evidence sums to.
function evidenceSnapshot(evidence) {
  const models = Object.values(evidence);
  const sum = (pick) => models.reduce((total, entry) => total + pick(entry), 0);
  const snapshot = fullSnapshot();
  const roster = Object.keys(evidence);
  snapshot.certified_strict_high_models = roster;
  snapshot.trace_model_evidence = structuredClone(evidence);
  Object.assign(snapshot.trace_accuracy_stats, {
    models_compared: roster.length,
    agreement_high: roster.length,
    agreement_minor: 0,
    agreement_deviation: 0,
    models_with_severe_channel: 0,
  });
  snapshot.trace_accuracy_stats.initial_condition = {
    models_compared: roster.length,
    deviation_channels_total: sum((entry) => entry.ic_deviation_channels),
    severe_channels_total: sum((entry) => entry.ic_severe_channels),
    violation_mass_total: sum((entry) => entry.ic_violation_mass),
  };
  snapshot.trace_accuracy_stats.state_selection = {
    models_compared: roster.length,
    exact_state_set_match_models: sum((entry) => Number(entry.state_set.exact)),
    total_rumoca_only_states: sum((entry) => entry.state_set.rumoca_only),
    total_omc_only_states: sum((entry) => entry.state_set.omc_only),
  };
  return snapshot;
}

test('real state-set evidence: renamed states on certified models are refused, new models are not', () => {
  const reference = evidenceSnapshot(REAL_STATE_SETS.reference);
  const candidate = evidenceSnapshot(REAL_STATE_SETS.candidate);
  candidate.sim_ok += 2;
  const decision = decide(candidate, reference);
  assert.equal(decision.promote, false);
  assert.deepEqual(decision.regressions, [
    'state-set rumoca-only states over 10 shared models: 27 -> 53',
    'state-set omc-only states over 10 shared models: 27 -> 53',
  ]);

  // The same models as they were: the newly compared models alone, which the
  // raw totals counted as a regression, promote.
  const unchanged = evidenceSnapshot({
    ...REAL_STATE_SETS.candidate,
    ...REAL_STATE_SETS.reference,
  });
  unchanged.sim_ok += 2;
  const promoted = decide(unchanged, reference);
  assert.equal(promoted.promote, true, promoted.reason);
  assert.ok(
    unchanged.trace_accuracy_stats.state_selection.total_rumoca_only_states >
      reference.trace_accuracy_stats.state_selection.total_rumoca_only_states,
  );
});

test('the run cohort, not the whole run, carries the runtime medians', () => {
  const reference = fullSnapshot();
  reference.runtime_ratio_cohort_models = ['A', 'B'];
  const current = fullSnapshot();
  current.sim_ok = 6;
  current.runtime_ratio_cohort_models = ['A', 'B', 'C'];
  current.runtime_model_ratios = {
    A: { system: 2.0, wall: 10.0 },
    B: { system: 2.0, wall: 10.0 },
    C: { system: 0.1, wall: 0.5 },
  };
  current.runtime_ratio_stats.system_ratio_both_success.median = 2.0;
  current.runtime_ratio_stats.wall_ratio_both_success.median = 10.0;
  assert.equal(decide(current, reference).promote, true);

  current.runtime_model_ratios.B.wall = 1.0;
  current.runtime_model_ratios.A.wall = 1.0;
  current.runtime_ratio_stats.wall_ratio_both_success.median = 1.0;
  const regressed = decide(current, reference);
  assert.equal(regressed.promote, false);
  assert.match(regressed.reason, /runtime wall speedup median over 2 reference cohort models/);
});
