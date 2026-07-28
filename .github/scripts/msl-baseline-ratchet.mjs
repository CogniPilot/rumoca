#!/usr/bin/env node

import assert from 'node:assert/strict';
import fs from 'node:fs';
import { fileURLToPath, pathToFileURL } from 'node:url';

const EXPECTED_QUALITY_GATE_VERSION = 2;
const DEFAULT_CHECKED_IN_BASELINE_PATH = fileURLToPath(
  new URL(
    '../../crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json',
    import.meta.url,
  ),
);
const V2_FLATTEN_MODELS_BEFORE = 565;
const V2_FLATTEN_MODELS_AFTER = 555;
const V2_REATTRIBUTED_ERROR_CODE = 'ER002';
const V2_REATTRIBUTED_MODELS = [
  'Modelica.Fluid.Examples.AST_BatchPlant.BatchPlant_StandardWater',
  'Modelica.Fluid.Examples.AST_BatchPlant.Test.OneTank',
  'Modelica.Fluid.Examples.AST_BatchPlant.Test.TankWithEmptyingPipe1',
  'Modelica.Fluid.Examples.AST_BatchPlant.Test.TankWithEmptyingPipe2',
  'Modelica.Fluid.Examples.AST_BatchPlant.Test.TanksWithEmptyingPipe1',
  'Modelica.Fluid.Examples.AST_BatchPlant.Test.TanksWithEmptyingPipe2',
  'Modelica.Fluid.Examples.AST_BatchPlant.Test.TwoTanks',
  'Modelica.Fluid.Examples.Explanatory.MeasuringTemperature',
  'Modelica.Fluid.Examples.Explanatory.MomentumBalanceFittings',
  'Modelica.Fluid.Examples.InverseParameterization',
];

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
  [
    'initial-condition deviation channels',
    ['trace_accuracy_stats', 'initial_condition', 'deviation_channels_total'],
  ],
  [
    'initial-condition severe channels',
    ['trace_accuracy_stats', 'initial_condition', 'severe_channels_total'],
  ],
  [
    'state-set rumoca-only states',
    ['trace_accuracy_stats', 'state_selection', 'total_rumoca_only_states'],
  ],
  [
    'state-set omc-only states',
    ['trace_accuracy_stats', 'state_selection', 'total_omc_only_states'],
  ],
];

const LOWER_FLOAT_IS_BETTER = [
  ['trace violation mass', ['trace_accuracy_stats', 'violation_mass_total']],
  [
    'initial-condition violation mass',
    ['trace_accuracy_stats', 'initial_condition', 'violation_mass_total'],
  ],
];

export function promoteBaselineIfImproved({
  sourcePath,
  baselinePath,
  checkedInBaselinePath = DEFAULT_CHECKED_IN_BASELINE_PATH,
  log = console.log,
}) {
  const sourceText = fs.readFileSync(sourcePath, 'utf8');
  const baselineText = fs.readFileSync(baselinePath, 'utf8');
  const source = parseJson(sourceText, sourcePath);
  const baseline = parseJson(baselineText, baselinePath);
  ensurePromotableSnapshot(source, sourcePath);
  const checkedInBaseline = loadCheckedInBaselineForOmcMigration({
    source,
    baseline,
    checkedInBaselinePath,
  });

  const decision = ratchetDecision(source, baseline, checkedInBaseline);
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

export function ensurePromotableSnapshot(snapshot, sourceName = 'source snapshot') {
  assert.equal(
    numberAt(snapshot, ['quality_gate_version'], sourceName),
    EXPECTED_QUALITY_GATE_VERSION,
    `${sourceName}: unsupported quality_gate_version`,
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

export function ratchetDecision(current, baseline, checkedInBaseline = null) {
  ensureSameContext(current, baseline, ['simulatable_attempted']);
  ensureSameContext(current, baseline, ['sim_target_models']);
  const schemaMigration = validatedSchemaMigration(current, baseline);
  const currentOmc = nonEmptyStringAt(current, ['omc_version'], 'current snapshot');
  const baselineOmc = nonEmptyStringAt(baseline, ['omc_version'], 'baseline snapshot');
  const omcContextChanged = currentOmc !== baselineOmc;
  if (omcContextChanged) {
    validateOmcContextMigration(current, baseline, checkedInBaseline);
  }
  const skippedPaths = new Set(
    schemaMigration === null ? [] : ['flatten_models'],
  );

  const improvements = [];
  const regressions = [];
  compareIntegerMetrics(
    CONTEXT_INDEPENDENT_HIGHER_IS_BETTER,
    current,
    baseline,
    true,
    improvements,
    regressions,
    skippedPaths,
  );
  compareIntegerMetrics(
    CONTEXT_INDEPENDENT_LOWER_IS_BETTER,
    current,
    baseline,
    false,
    improvements,
    regressions,
    skippedPaths,
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
    compareRuntimeSpeedups(current, baseline, improvements, regressions);
  } else {
    improvements.push(`OMC context: ${baselineOmc} -> ${currentOmc}`);
  }
  if (schemaMigration !== null) {
    improvements.push(
      `quality schema: ${schemaMigration.from_quality_gate_version} -> ${schemaMigration.to_quality_gate_version}`,
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

function loadCheckedInBaselineForOmcMigration({ source, baseline, checkedInBaselinePath }) {
  const sourceOmc = nonEmptyStringAt(source, ['omc_version'], 'source snapshot');
  const baselineOmc = nonEmptyStringAt(baseline, ['omc_version'], 'baseline snapshot');
  if (sourceOmc === baselineOmc) {
    return null;
  }
  const checkedInText = fs.readFileSync(checkedInBaselinePath, 'utf8');
  return parseJson(checkedInText, checkedInBaselinePath);
}

function validateOmcContextMigration(current, baseline, checkedInBaseline) {
  assert.notEqual(
    checkedInBaseline,
    null,
    'cannot ratchet baseline: changed OMC context requires the reviewed checked-in migration',
  );
  assert.equal(
    typeof checkedInBaseline,
    'object',
    'cannot ratchet baseline: checked-in OMC migration baseline must be an object',
  );
  ensurePromotableSnapshot(checkedInBaseline, 'checked-in migration baseline');
  const currentOmc = nonEmptyStringAt(current, ['omc_version'], 'current snapshot');
  const baselineOmc = nonEmptyStringAt(baseline, ['omc_version'], 'baseline snapshot');
  assert.equal(
    nonEmptyStringAt(checkedInBaseline, ['omc_version'], 'checked-in migration baseline'),
    currentOmc,
    'cannot ratchet baseline: checked-in OMC context does not match current snapshot',
  );
  const migration = valueAt(checkedInBaseline, ['omc_context_migration']);
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
    integerAt(checkedInBaseline, ['sim_target_models'], 'checked-in migration baseline'),
    currentTargetCount,
    'cannot ratchet baseline: checked-in OMC target count does not match current snapshot',
  );
}

function validatedSchemaMigration(current, baseline) {
  const currentVersion = integerAt(current, ['quality_gate_version'], 'current snapshot');
  const baselineVersion = integerAt(baseline, ['quality_gate_version'], 'baseline snapshot');
  if (currentVersion === baselineVersion) {
    return null;
  }
  assert.equal(
    currentVersion,
    EXPECTED_QUALITY_GATE_VERSION,
    'cannot ratchet baseline: current quality schema is unsupported',
  );
  const migration = valueAt(current, ['metric_schema_migration']);
  assert.equal(
    typeof migration,
    'object',
    'cannot ratchet baseline: metric_schema_migration must be an object',
  );
  assert.equal(
    integerAt(migration, ['from_quality_gate_version'], 'metric schema migration'),
    baselineVersion,
    'cannot ratchet baseline: schema migration source does not match baseline',
  );
  assert.equal(
    integerAt(migration, ['to_quality_gate_version'], 'metric schema migration'),
    currentVersion,
    'cannot ratchet baseline: schema migration target does not match current snapshot',
  );
  const before = integerAt(
    migration,
    ['flatten_models_before'],
    'metric schema migration',
  );
  const after = integerAt(
    migration,
    ['flatten_models_after'],
    'metric schema migration',
  );
  assert.equal(
    before,
    V2_FLATTEN_MODELS_BEFORE,
    'cannot ratchet baseline: migration before-count differs from the reviewed correction',
  );
  assert.equal(
    after,
    V2_FLATTEN_MODELS_AFTER,
    'cannot ratchet baseline: migration after-count differs from the reviewed correction',
  );
  assert.equal(
    integerAt(baseline, ['flatten_models'], 'baseline snapshot'),
    before,
    'cannot ratchet baseline: migration before-count does not match baseline',
  );
  assert.equal(
    integerAt(current, ['flatten_models'], 'current snapshot'),
    after,
    'cannot ratchet baseline: migration after-count does not match current snapshot',
  );
  const models = valueAt(migration, ['reattributed_models']);
  assert.equal(Array.isArray(models), true, 'metric schema migration model set must be an array');
  assert.equal(
    models.every((model) => typeof model === 'string' && model.length > 0),
    true,
    'metric schema migration model names must be non-empty strings',
  );
  assert.equal(
    models.length,
    before - after,
    'metric schema migration model count must explain the count delta',
  );
  assert.equal(
    new Set(models).size,
    models.length,
    'metric schema migration model set must be unique',
  );
  assert.deepEqual(
    [...models].sort(),
    [...V2_REATTRIBUTED_MODELS].sort(),
    'metric schema migration model set differs from the reviewed correction',
  );
  assert.equal(
    stringAt(migration, ['reattributed_error_code'], 'metric schema migration'),
    V2_REATTRIBUTED_ERROR_CODE,
    'metric schema migration diagnostic cohort differs from the reviewed correction',
  );
  return migration;
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
  skippedPaths = new Set(),
) {
  for (const [label, path] of metrics) {
    if (skippedPaths.has(path.join('.'))) {
      continue;
    }
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

function compareRuntimeSpeedups(current, baseline, improvements, regressions) {
  for (const [label, path] of [
    [
      'runtime system speedup median',
      ['runtime_ratio_stats', 'system_ratio_both_success', 'median'],
    ],
    [
      'runtime wall speedup median',
      ['runtime_ratio_stats', 'wall_ratio_both_success', 'median'],
    ],
  ]) {
    const currentRatio = numberAt(current, path, 'current snapshot');
    const baselineRatio = numberAt(baseline, path, 'baseline snapshot');
    if (currentRatio < baselineRatio * 0.65) {
      regressions.push(
        `${label}: ${baselineRatio.toExponential(6)} -> ${currentRatio.toExponential(6)}`,
      );
    } else if (currentRatio > baselineRatio) {
      improvements.push(
        `${label}: ${baselineRatio.toExponential(6)} -> ${currentRatio.toExponential(6)}`,
      );
    }
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
