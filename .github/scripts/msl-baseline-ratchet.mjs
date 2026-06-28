#!/usr/bin/env node

import assert from 'node:assert/strict';
import fs from 'node:fs';
import { pathToFileURL } from 'node:url';

const EXPECTED_QUALITY_GATE_VERSION = 1;

const HIGHER_IS_BETTER = [
  ['parse models', ['parse_models']],
  ['flatten models', ['flatten_models']],
  ['compiled models', ['compiled_models']],
  ['solve models', ['solve_models']],
  ['balanced models', ['balanced_models']],
  ['initial balanced models', ['initial_balanced_models']],
  ['initial-condition solves', ['ic_ok']],
  ['successful simulations', ['sim_ok']],
  ['trace models compared', ['trace_accuracy_stats', 'models_compared']],
  ['high trace agreement', ['trace_accuracy_stats', 'agreement_high']],
  [
    'state-set exact matches',
    ['trace_accuracy_stats', 'state_selection', 'exact_state_set_match_models'],
  ],
];

const LOWER_IS_BETTER = [
  ['partial models', ['partial_models']],
  ['unbalanced models', ['unbalanced_models']],
  ['initial-condition solver failures', ['ic_solver_fail']],
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

export function promoteBaselineIfImproved({ sourcePath, baselinePath, log = console.log }) {
  const sourceText = fs.readFileSync(sourcePath, 'utf8');
  const baselineText = fs.readFileSync(baselinePath, 'utf8');
  const source = parseJson(sourceText, sourcePath);
  const baseline = parseJson(baselineText, baselinePath);
  ensurePromotableSnapshot(source, sourcePath);

  const decision = ratchetDecision(source, baseline);
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

export function ratchetDecision(current, baseline) {
  ensureSameContext(current, baseline, ['simulatable_attempted']);
  ensureSameContext(current, baseline, ['sim_target_models']);

  const improvements = [];
  const regressions = [];
  compareIntegerMetrics(HIGHER_IS_BETTER, current, baseline, true, improvements, regressions);
  compareIntegerMetrics(LOWER_IS_BETTER, current, baseline, false, improvements, regressions);
  compareFloatMetrics(LOWER_FLOAT_IS_BETTER, current, baseline, improvements, regressions);
  compareDerivedMetrics(current, baseline, improvements, regressions);

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
