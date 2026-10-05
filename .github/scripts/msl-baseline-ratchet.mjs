#!/usr/bin/env node

import assert from 'node:assert/strict';
import fs from 'node:fs';
import { fileURLToPath, pathToFileURL } from 'node:url';

import { cohortComparison } from './msl-baseline-cohort.mjs';

const DEFAULT_CHECKED_IN_BASELINE_PATH = fileURLToPath(
  new URL(
    '../../crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json',
    import.meta.url,
  ),
);

const CONTEXT_INDEPENDENT_HIGHER_IS_BETTER = [
  ['parse models', ['parse_models']],
  ['flatten models', ['flatten_models']],
  ['DAE models', ['dae_models']],
  ['compiled models', ['compiled_models']],
  ['solve models', ['solve_models']],
  ['balanced models', ['balanced_models']],
  ['balance denominator', ['balance_denominator']],
  ['initial balanced models', ['initial_balanced_models']],
  ['simulation attempts', ['sim_attempted']],
  ['initial-condition attempts', ['ic_attempted']],
  ['initial-condition solves', ['ic_ok']],
  ['successful simulations', ['sim_ok']],
];

const OMC_DEPENDENT_HIGHER_IS_BETTER = [
  ['trace models compared', ['trace_accuracy_stats', 'models_compared']],
  ['high trace agreement', ['trace_accuracy_stats', 'agreement_high']],
  [
    'state-set exact matches',
    ['trace_accuracy_stats', 'state_selection', 'exact_state_set_match_models'],
  ],
];

const CONTEXT_INDEPENDENT_LOWER_IS_BETTER = [
  ['partial models', ['partial_models']],
  ['unbalanced models', ['unbalanced_models']],
  ['initial unbalanced models', ['initial_unbalanced_models']],
  ['initial-condition solver failures', ['ic_solver_fail']],
];

const OMC_DEPENDENT_LOWER_IS_BETTER = [
  ['trace deviation models', ['trace_accuracy_stats', 'agreement_deviation']],
  ['trace bad channels', ['trace_accuracy_stats', 'bad_channels_total']],
  ['trace severe channels', ['trace_accuracy_stats', 'severe_channels_total']],
  [
    'trace models with bad channels',
    ['trace_accuracy_stats', 'models_with_any_channel_deviation'],
  ],
];

const LOWER_FLOAT_IS_BETTER = [
  ['trace violation mass', ['trace_accuracy_stats', 'violation_mass_total']],
];

export function promoteBaselineIfImproved({
  sourcePath,
  baselinePath,
  checkedInBaselinePath = DEFAULT_CHECKED_IN_BASELINE_PATH,
  log = console.log,
}) {
  const sourceText = fs.readFileSync(sourcePath, 'utf8');
  const source = parseJson(sourceText, sourcePath);
  const baseline = parseJson(fs.readFileSync(baselinePath, 'utf8'), baselinePath);
  const checkedIn = parseJson(
    fs.readFileSync(checkedInBaselinePath, 'utf8'),
    checkedInBaselinePath,
  );

  const decision = ratchetDecision(source, baseline, checkedIn);
  if (!decision.promote) {
    log(`MSL quality baseline not promoted: ${decision.reason}`);
    return decision;
  }

  log('MSL quality baseline ratchet improvements:');
  for (const line of decision.improvements) {
    log(`  - ${line}`);
  }
  fs.writeFileSync(baselinePath, sourceText);
  return decision;
}

// The quality schema a snapshot must carry to be promoted is the one of the
// reviewed checked-in baseline of the same commit; the quality gate pins that
// baseline to its own schema version, so the ratchet keeps no copy of it.
export function ensurePromotableSnapshot(snapshot, expectedVersion, sourceName = 'source snapshot') {
  assert.equal(
    numberAt(snapshot, ['quality_gate_version'], sourceName),
    expectedVersion,
    `${sourceName}: quality_gate_version differs from the reviewed checked-in baseline`,
  );
  assert.equal(
    stringAt(snapshot, ['run_scope'], sourceName),
    'full',
    `${sourceName}: only full MSL quality snapshots can be promoted`,
  );
  assert.notEqual(
    stringAt(snapshot, ['omc_version'], sourceName).trim(),
    '',
    `${sourceName}: omc_version must be non-empty`,
  );
  if (Object.hasOwn(snapshot, 'partial')) {
    assert.equal(
      snapshot.partial,
      false,
      `${sourceName}: partial snapshots cannot be promoted`,
    );
  }
}

export function ratchetDecision(current, baseline, checkedIn) {
  assert.equal(
    typeof checkedIn === 'object' && checkedIn !== null,
    true,
    'cannot ratchet baseline: the reviewed checked-in baseline is required',
  );
  const schemaVersion = integerAt(checkedIn, ['quality_gate_version'], 'checked-in baseline');
  ensurePromotableSnapshot(current, schemaVersion, 'current snapshot');
  ensureSameContext(current, baseline, ['simulatable_attempted']);
  ensureSameContext(current, baseline, ['sim_target_models']);
  const crossed = crossedReferenceBoundaries(current, baseline, checkedIn);
  const currentOmc = nonEmptyStringAt(current, ['omc_version'], 'current snapshot');
  const baselineOmc = nonEmptyStringAt(baseline, ['omc_version'], 'baseline snapshot');
  const omcContextChanged = currentOmc !== baselineOmc;
  if (omcContextChanged) {
    assert.equal(
      crossed.length,
      0,
      'cannot ratchet baseline: a reference boundary cannot change the OMC context',
    );
    validateOmcContextMigration(current, baseline, checkedIn);
  }

  const improvements = [];
  const regressions = [];
  compareIntegerMetrics(
    CONTEXT_INDEPENDENT_HIGHER_IS_BETTER,
    current,
    baseline,
    true,
    improvements,
    regressions,
  );
  compareIntegerMetrics(
    CONTEXT_INDEPENDENT_LOWER_IS_BETTER,
    current,
    baseline,
    false,
    improvements,
    regressions,
  );
  if (!omcContextChanged) {
    compareIntegerMetrics(
      OMC_DEPENDENT_HIGHER_IS_BETTER,
      current,
      baseline,
      true,
      improvements,
      regressions,
    );
    compareIntegerMetrics(
      OMC_DEPENDENT_LOWER_IS_BETTER,
      current,
      baseline,
      false,
      improvements,
      regressions,
    );
    compareFloatMetrics(LOWER_FLOAT_IS_BETTER, current, baseline, improvements, regressions);
    compareDerivedMetrics(current, baseline, improvements, regressions);
    compareUnexceptedRoster(current, baseline, improvements, regressions);
    compareTraceExceptions(current, baseline, improvements, regressions);
    const cohort = cohortComparison(baseline, current);
    improvements.push(...cohort.improvements);
    regressions.push(...cohort.regressions);
  } else {
    improvements.push(`OMC context: ${baselineOmc} -> ${currentOmc}`);
  }
  for (const boundary of crossed) {
    improvements.push(
      `reference boundary: ${boundary.from_quality_gate_version} -> ${boundary.to_quality_gate_version} (${boundary.change})`,
    );
  }

  if (regressions.length > 0) {
    return {
      promote: false,
      reason: `ratchet metric regression(s): ${regressions.join('; ')}`,
      improvements,
      regressions,
    };
  }
  if (improvements.length === 0) {
    return {
      promote: false,
      reason: 'source snapshot is equivalent to the committed baseline',
      improvements,
      regressions,
    };
  }
  return { promote: true, improvements, regressions };
}

// A promoted baseline at an older quality schema is crossed only through the
// reviewed reference boundary chain (SPEC_0033, SPEC_0050), the same chain the
// quality gate resolves the promoted asset through: the current snapshot
// declares the checked-in chain, the chain steps down one reviewed boundary at
// a time to exactly the promoted version, and the promoted asset declares the
// boundary that chain names for its version. A boundary changes no ratchet
// floor, so the caller still compares every metric against the promoted asset.
// Returns the crossed boundaries, oldest first.
function crossedReferenceBoundaries(current, baseline, checkedIn) {
  const currentVersion = integerAt(current, ['quality_gate_version'], 'current snapshot');
  const baselineVersion = integerAt(baseline, ['quality_gate_version'], 'baseline snapshot');
  if (currentVersion === baselineVersion) {
    return [];
  }
  assert.ok(
    baselineVersion < currentVersion,
    `cannot ratchet baseline: promoted quality schema ${baselineVersion} is newer than current ${currentVersion}`,
  );
  const chain = valueAt(current, ['reference_boundary_migration']);
  assert.deepEqual(
    chain,
    valueAt(checkedIn, ['reference_boundary_migration']),
    'cannot ratchet baseline: current reference boundary chain differs from the reviewed checked-in chain',
  );
  const crossed = [];
  let boundary = chain;
  let target = currentVersion;
  while (target > baselineVersion) {
    assert.ok(
      typeof boundary === 'object' && boundary !== null,
      `cannot ratchet baseline: no reviewed reference boundary reaches quality schema ${baselineVersion}`,
    );
    const to = integerAt(boundary, ['to_quality_gate_version'], 'reference boundary');
    const from = integerAt(boundary, ['from_quality_gate_version'], 'reference boundary');
    assert.ok(
      to === target && from < target,
      `cannot ratchet baseline: reference boundary ${from} -> ${to} does not continue the chain at ${target}`,
    );
    crossed.push(boundary);
    target = from;
    boundary = boundary.previous;
  }
  assert.equal(
    target,
    baselineVersion,
    `cannot ratchet baseline: reference boundary chain skips quality schema ${baselineVersion}`,
  );
  assert.deepEqual(
    baseline.reference_boundary_migration,
    boundary,
    `cannot ratchet baseline: promoted reference boundary differs from the reviewed chain at quality schema ${baselineVersion}`,
  );
  return crossed.reverse();
}

function validateOmcContextMigration(current, baseline, checkedIn) {
  ensurePromotableSnapshot(
    checkedIn,
    integerAt(current, ['quality_gate_version'], 'current snapshot'),
    'checked-in migration baseline',
  );
  const currentOmc = nonEmptyStringAt(current, ['omc_version'], 'current snapshot');
  const baselineOmc = nonEmptyStringAt(baseline, ['omc_version'], 'baseline snapshot');
  assert.equal(
    nonEmptyStringAt(checkedIn, ['omc_version'], 'checked-in migration baseline'),
    currentOmc,
    'cannot ratchet baseline: checked-in OMC context does not match current snapshot',
  );
  assert.equal(
    Object.hasOwn(checkedIn, 'omc_context_migration'),
    true,
    'cannot ratchet baseline: changed OMC context requires the reviewed checked-in migration',
  );
  const migration = checkedIn.omc_context_migration;
  assert.equal(
    typeof migration,
    'object',
    'cannot ratchet baseline: checked-in omc_context_migration must be an object',
  );
  assert.equal(
    nonEmptyStringAt(migration, ['from_omc_version'], 'OMC context migration'),
    baselineOmc,
    'cannot ratchet baseline: OMC migration source does not match promoted baseline',
  );
  assert.equal(
    nonEmptyStringAt(migration, ['to_omc_version'], 'OMC context migration'),
    currentOmc,
    'cannot ratchet baseline: OMC migration target does not match current snapshot',
  );
  const currentTargetCount = integerAt(current, ['sim_target_models'], 'current snapshot');
  assert.equal(
    integerAt(migration, ['sim_target_models'], 'OMC context migration'),
    currentTargetCount,
    'cannot ratchet baseline: OMC migration target count does not match current snapshot',
  );
  assert.equal(
    integerAt(checkedIn, ['sim_target_models'], 'checked-in migration baseline'),
    currentTargetCount,
    'cannot ratchet baseline: checked-in OMC target count does not match current snapshot',
  );
}

function parseJson(text, path) {
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new Error(`failed to parse ${path}: ${error.message}`);
  }
}

function ensureSameContext(current, baseline, path) {
  const currentValue = integerAt(current, path, 'current snapshot');
  const baselineValue = integerAt(baseline, path, 'baseline snapshot');
  assert.equal(
    currentValue,
    baselineValue,
    `cannot ratchet baseline: ${path.join('.')} changed from ${baselineValue} to ${currentValue}`,
  );
}

function compareIntegerMetrics(
  metrics,
  current,
  baseline,
  higherIsBetter,
  improvements,
  regressions,
) {
  for (const [label, path] of metrics) {
    compareMetric(
      label,
      integerAt(current, path, 'current snapshot'),
      integerAt(baseline, path, 'baseline snapshot'),
      higherIsBetter,
      improvements,
      regressions,
    );
  }
}

function compareFloatMetrics(metrics, current, baseline, improvements, regressions) {
  for (const [label, path] of metrics) {
    compareFloatMetric(
      label,
      numberAt(current, path, 'current snapshot'),
      numberAt(baseline, path, 'baseline snapshot'),
      improvements,
      regressions,
    );
  }
}

function compareDerivedMetrics(current, baseline, improvements, regressions) {
  compareMetric(
    'high+near trace agreement',
    traceHighNearCount(current),
    traceHighNearCount(baseline),
    true,
    improvements,
    regressions,
  );
  compareMetric(
    'trace models without severe channels',
    traceNoSevereCount(current),
    traceNoSevereCount(baseline),
    true,
    improvements,
    regressions,
  );
}

// The typed trace exception file only changes through a reviewed boundary: a
// snapshot read under a different file than its baseline is a regression
// unless its own reference boundary pins that file (SPEC_0050).
function compareTraceExceptions(current, baseline, improvements, regressions) {
  const read = nonEmptyStringAt(current, ['trace_exceptions_sha256'], 'current snapshot');
  const previous = Object.hasOwn(baseline, 'trace_exceptions_sha256')
    ? nonEmptyStringAt(baseline, ['trace_exceptions_sha256'], 'baseline snapshot')
    : nonEmptyStringAt(
      baseline,
      ['reference_boundary_migration', 'exclusions_sha256'],
      'baseline snapshot',
    );
  if (read === previous) {
    return;
  }
  const pinned = nonEmptyStringAt(
    current,
    ['reference_boundary_migration', 'exclusions_sha256'],
    'current snapshot',
  );
  if (pinned === read) {
    improvements.push(`trace exceptions: reviewed boundary ${previous} -> ${read}`);
  } else {
    regressions.push(`trace exceptions changed without a reviewed boundary: ${previous} -> ${read}`);
  }
}

// The roster of completions without strict-high parity or a typed trace
// exception only shrinks: a model outside the baseline roster is a regression
// even when the count holds (SPEC_0033 simulation soundness). A baseline from
// before the roster existed is compared by the reviewed boundary that
// introduces it, and a model joins the roster only through the
// `roster_additions` of a reviewed boundary the comparison crosses, naming the
// defect (SPEC_0050).
function compareUnexceptedRoster(current, baseline, improvements, regressions) {
  const currentRoster = modelRosterAt(current, 'current snapshot');
  if (!Object.hasOwn(baseline, 'unexcepted_non_high_models')) {
    improvements.push(`unexcepted non-high roster introduced: ${currentRoster.length}`);
    return;
  }
  const baselineRoster = new Set(modelRosterAt(baseline, 'baseline snapshot'));
  const reviewed = crossedRosterAdditions(current, baseline);
  for (const model of currentRoster) {
    if (baselineRoster.has(model)) {
      continue;
    }
    if (reviewed.has(model)) {
      improvements.push(`unexcepted non-high roster: reviewed addition ${model}`);
    } else {
      regressions.push(`unexcepted non-high roster gained ${model}`);
    }
  }
  if (reviewed.size === 0) {
    compareMetric(
      'unexcepted non-high models',
      currentRoster.length,
      baselineRoster.size,
      false,
      improvements,
      regressions,
    );
  }
}

// The roster additions of the current snapshot's reference boundaries that
// lie above the baseline's quality gate version.
function crossedRosterAdditions(current, baseline) {
  const baselineVersion = integerAt(baseline, ['quality_gate_version'], 'baseline snapshot');
  const models = new Set();
  let boundary = current.reference_boundary_migration;
  while (boundary && boundary.to_quality_gate_version > baselineVersion) {
    for (const addition of boundary.roster_additions ?? []) {
      models.add(addition.model_name);
    }
    boundary = boundary.previous;
  }
  return models;
}

function modelRosterAt(snapshot, name) {
  const roster = valueAt(snapshot, ['unexcepted_non_high_models']);
  assert.equal(Array.isArray(roster), true, `${name}: unexcepted_non_high_models must be an array`);
  return roster;
}

function compareMetric(label, current, baseline, higherIsBetter, improvements, regressions) {
  const improved = higherIsBetter ? current > baseline : current < baseline;
  const regressed = higherIsBetter ? current < baseline : current > baseline;
  if (improved) {
    improvements.push(`${label}: ${baseline} -> ${current}`);
  } else if (regressed) {
    regressions.push(`${label}: ${baseline} -> ${current}`);
  }
}

function compareFloatMetric(label, current, baseline, improvements, regressions) {
  const epsilon = 1.0e-9;
  if (current < baseline - epsilon) {
    improvements.push(`${label}: ${baseline.toExponential(6)} -> ${current.toExponential(6)}`);
  } else if (current > baseline + epsilon) {
    regressions.push(`${label}: ${baseline.toExponential(6)} -> ${current.toExponential(6)}`);
  }
}

function traceHighNearCount(snapshot) {
  return (
    integerAt(snapshot, ['trace_accuracy_stats', 'agreement_high'], 'snapshot') +
    integerAt(snapshot, ['trace_accuracy_stats', 'agreement_minor'], 'snapshot')
  );
}

function traceNoSevereCount(snapshot) {
  const compared = integerAt(snapshot, ['trace_accuracy_stats', 'models_compared'], 'snapshot');
  const severe = integerAt(
    snapshot,
    ['trace_accuracy_stats', 'models_with_severe_channel'],
    'snapshot',
  );
  return Math.max(0, compared - severe);
}

function integerAt(snapshot, path, name) {
  const value = numberAt(snapshot, path, name);
  assert.equal(Number.isInteger(value), true, `${name}: ${path.join('.')} must be an integer`);
  return value;
}

function numberAt(snapshot, path, name) {
  const value = valueAt(snapshot, path);
  assert.equal(typeof value, 'number', `${name}: ${path.join('.')} must be numeric`);
  assert.equal(Number.isFinite(value), true, `${name}: ${path.join('.')} must be finite`);
  return value;
}

function stringAt(snapshot, path, name) {
  const value = valueAt(snapshot, path);
  assert.equal(typeof value, 'string', `${name}: ${path.join('.')} must be a string`);
  return value;
}

function nonEmptyStringAt(snapshot, path, name) {
  const value = stringAt(snapshot, path, name).trim();
  assert.notEqual(value, '', `${name}: ${path.join('.')} must be non-empty`);
  return value;
}

function valueAt(snapshot, path) {
  let value = snapshot;
  for (const key of path) {
    assert.notEqual(value, null, `missing quality metric ${path.join('.')}`);
    assert.equal(typeof value, 'object', `missing quality metric ${path.join('.')}`);
    assert.equal(Object.hasOwn(value, key), true, `missing quality metric ${path.join('.')}`);
    value = value[key];
  }
  return value;
}

function parseArgs(argv) {
  const args = {};
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === '--source' || arg === '--baseline') {
      const value = argv[index + 1];
      assert.ok(value, `missing value for ${arg}`);
      args[arg.slice(2)] = value;
      index += 1;
    } else {
      throw new Error(`unknown argument: ${arg}`);
    }
  }
  assert.ok(args.source, 'missing --source');
  assert.ok(args.baseline, 'missing --baseline');
  return args;
}

function main() {
  const args = parseArgs(process.argv.slice(2));
  promoteBaselineIfImproved({
    sourcePath: args.source,
    baselinePath: args.baseline,
  });
}

const invokedPath = process.argv[1] ? pathToFileURL(process.argv[1]).href : '';

if (import.meta.url === invokedPath) {
  main();
}
