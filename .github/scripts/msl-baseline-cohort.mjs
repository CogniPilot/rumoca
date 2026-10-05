// Cohort-relative comparison of MSL quality snapshots.
//
// Some quality totals accrue on every compared model whatever its trace band:
// initial-condition deviations, state-set disagreements, and runtime ratios. A
// run that compares more models raises those totals without any model getting
// worse, so a reference and a candidate are compared over the models both of
// them measured. Strict-high trace channels carry zero deviations (SPEC_0033),
// so the trace channel totals are not cohort-dependent and stay raw.
//
// The same rule is implemented by the quality gate's baseline resolver
// (crates/xtask/src/verify_cmd/msl_quality_baseline/cohort.rs); the shared
// cases in msl-baseline-cohort-cases.json hold both to one verdict.

import assert from 'node:assert/strict';

// The gate's runtime rule (runtime_cohort.rs): the candidate's median over the
// reference's runtime cohort, which must still time at least 90 percent of it,
// may not fall more than 35 percent below the reference median.
export const RUNTIME_COHORT_MIN_COVERAGE = 0.9;
export const RUNTIME_MEDIAN_REL_TOLERANCE = 0.35;
const FLOAT_EPSILON = 1.0e-9;

const COHORT_METRICS = [
  {
    label: 'initial-condition deviation channels',
    family: 'trace',
    field: 'ic_deviation_channels',
    total: ['trace_accuracy_stats', 'initial_condition', 'deviation_channels_total'],
  },
  {
    label: 'initial-condition severe channels',
    family: 'trace',
    field: 'ic_severe_channels',
    total: ['trace_accuracy_stats', 'initial_condition', 'severe_channels_total'],
  },
  {
    label: 'initial-condition violation mass',
    family: 'trace',
    field: 'ic_violation_mass',
    total: ['trace_accuracy_stats', 'initial_condition', 'violation_mass_total'],
    float: true,
  },
  {
    label: 'state-set rumoca-only states',
    family: 'state',
    field: 'rumoca_only',
    total: ['trace_accuracy_stats', 'state_selection', 'total_rumoca_only_states'],
  },
  {
    label: 'state-set omc-only states',
    family: 'state',
    field: 'omc_only',
    total: ['trace_accuracy_stats', 'state_selection', 'total_omc_only_states'],
  },
  {
    label: 'state-set exact matches',
    family: 'state',
    field: 'exact',
    total: ['trace_accuracy_stats', 'state_selection', 'exact_state_set_match_models'],
    higherIsBetter: true,
  },
];

const FAMILY_SIZE = {
  trace: ['trace_accuracy_stats', 'initial_condition', 'models_compared'],
  state: ['trace_accuracy_stats', 'state_selection', 'models_compared'],
};

const RUNTIME_MEDIANS = [
  ['runtime system speedup median', 'system', 'system_ratio_both_success'],
  ['runtime wall speedup median', 'wall', 'wall_ratio_both_success'],
];

// Compare the cohort-dependent metrics of `candidate` against `reference`.
// Returns improvement and regression lines; throws when either snapshot's
// per-model evidence disagrees with its own aggregates.
export function cohortComparison(reference, candidate) {
  const improvements = [];
  const regressions = [];
  const referenceCohorts = snapshotCohorts(reference, 'reference snapshot');
  const candidateCohorts = snapshotCohorts(candidate, 'candidate snapshot');
  for (const metric of COHORT_METRICS) {
    const line = compareCohortMetric(
      metric,
      reference,
      candidate,
      referenceCohorts[metric.family],
      candidateCohorts[metric.family],
    );
    if (line) {
      (line.regressed ? regressions : improvements).push(line.text);
    }
  }
  compareRuntimeMedians(reference, candidate, improvements, regressions);
  return { improvements, regressions };
}

// The per-model population behind each family of totals: `values` maps a model
// to its evidence when the snapshot records it; a snapshot from before the
// evidence existed names only its `models`, and only when its strict-high
// roster provably is its compared set.
function snapshotCohorts(snapshot, name) {
  const evidence = snapshot.trace_model_evidence;
  if (evidence !== undefined) {
    return recordedCohorts(snapshot, evidence, name);
  }
  return legacyCohorts(snapshot);
}

function recordedCohorts(snapshot, evidence, name) {
  assert.equal(
    typeof evidence === 'object' && evidence !== null && !Array.isArray(evidence),
    true,
    `${name}: trace_model_evidence must be an object`,
  );
  const trace = new Map();
  const state = new Map();
  for (const [model, entry] of Object.entries(evidence)) {
    const where = `${name}: trace_model_evidence.${model}`;
    trace.set(model, {
      ic_deviation_channels: countOf(entry, 'ic_deviation_channels', where),
      ic_severe_channels: countOf(entry, 'ic_severe_channels', where),
      ic_violation_mass: massOf(entry, 'ic_violation_mass', where),
    });
    if (entry.state_set !== undefined) {
      state.set(model, {
        rumoca_only: countOf(entry.state_set, 'rumoca_only', `${where}.state_set`),
        omc_only: countOf(entry.state_set, 'omc_only', `${where}.state_set`),
        exact: flagOf(entry.state_set, 'exact', `${where}.state_set`),
      });
    }
  }
  const cohorts = {
    trace: { models: new Set(trace.keys()), values: trace },
    state: { models: new Set(state.keys()), values: state },
  };
  ensureEvidenceMatchesTotals(snapshot, cohorts, name);
  return cohorts;
}

// Evidence that does not add up to the snapshot's own totals describes another
// population; it is refused rather than compared.
function ensureEvidenceMatchesTotals(snapshot, cohorts, name) {
  for (const [family, path] of Object.entries(FAMILY_SIZE)) {
    assert.equal(
      cohorts[family].models.size,
      integerAt(snapshot, path, name),
      `${name}: per-model ${family} evidence does not cover ${path.join('.')}`,
    );
  }
  for (const metric of COHORT_METRICS) {
    const recorded = sumOver(cohorts[metric.family], metric, cohorts[metric.family].models);
    const total = numberAt(snapshot, metric.total, name);
    assert.equal(
      sameValue(recorded, total, metric.float),
      true,
      `${name}: per-model evidence sums ${metric.label} to ${recorded}, totals record ${total}`,
    );
  }
}

function legacyCohorts(snapshot) {
  const roster = snapshot.certified_strict_high_models;
  const stats = snapshot.trace_accuracy_stats ?? {};
  if (!Array.isArray(roster) || roster.length === 0) {
    return { trace: null, state: null };
  }
  const models = new Set(roster);
  const isCompared = (count) => count === models.size;
  const traceKnown =
    models.size === roster.length &&
    isCompared(stats.models_compared) &&
    isCompared(stats.agreement_high) &&
    isCompared(stats.initial_condition?.models_compared);
  return {
    trace: traceKnown ? { models, values: null } : null,
    state: traceKnown && isCompared(stats.state_selection?.models_compared)
      ? { models, values: null }
      : null,
  };
}

function compareCohortMetric(metric, reference, candidate, referenceCohort, candidateCohort) {
  const higherIsBetter = metric.higherIsBetter === true;
  if (!candidateCohort?.values || !referenceCohort) {
    // No population to intersect: the totals are compared as recorded.
    return verdict(
      metric,
      numberAt(reference, metric.total, 'reference snapshot'),
      numberAt(candidate, metric.total, 'candidate snapshot'),
      higherIsBetter,
      '',
    );
  }
  const shared = [...referenceCohort.models].filter((model) => candidateCohort.models.has(model));
  if (!referenceCohort.values && shared.length !== referenceCohort.models.size) {
    // Only the reference's whole-cohort total is known, so the comparison
    // needs every one of its models.
    return {
      regressed: true,
      text: `${metric.label}: ${referenceCohort.models.size - shared.length} of the reference's ${referenceCohort.models.size} models have no candidate evidence`,
    };
  }
  const referenceValue = referenceCohort.values
    ? sumOver(referenceCohort, metric, shared)
    : numberAt(reference, metric.total, 'reference snapshot');
  const candidateValue = sumOver(candidateCohort, metric, shared);
  return verdict(
    metric,
    referenceValue,
    candidateValue,
    higherIsBetter,
    ` over ${shared.length} shared models`,
  );
}

function verdict(metric, referenceValue, candidateValue, higherIsBetter, scope) {
  if (sameValue(referenceValue, candidateValue, metric.float)) {
    return null;
  }
  const improved = higherIsBetter
    ? candidateValue > referenceValue
    : candidateValue < referenceValue;
  const format = (value) => (metric.float ? value.toExponential(6) : `${value}`);
  return {
    regressed: !improved,
    text: `${metric.label}${scope}: ${format(referenceValue)} -> ${format(candidateValue)}`,
  };
}

function sumOver(cohort, metric, models) {
  let total = 0;
  for (const model of models) {
    const value = cohort.values.get(model)[metric.field];
    total += typeof value === 'boolean' ? Number(value) : value;
  }
  return total;
}

function sameValue(left, right, float) {
  if (!float) {
    return left === right;
  }
  return Math.abs(left - right) <= FLOAT_EPSILON * Math.max(1, Math.abs(left), Math.abs(right));
}

// Runtime speedups are compared as the gate compares them: over the
// reference's runtime cohort when it names one and the candidate records its
// per-model ratios, else as the whole-run medians both snapshots record.
function compareRuntimeMedians(reference, candidate, improvements, regressions) {
  const cohort = reference.runtime_ratio_cohort_models;
  const ratios = candidateRuntimeRatios(candidate);
  const useCohort = Array.isArray(cohort) && cohort.length > 0 && ratios !== null;
  const matched = useCohort ? cohort.filter((model) => ratios.has(model)) : [];
  if (useCohort && matched.length < cohort.length * RUNTIME_COHORT_MIN_COVERAGE) {
    regressions.push(
      `runtime cohort coverage: ${matched.length}/${cohort.length} reference models timed`,
    );
    return;
  }
  for (const [label, key, statsKey] of RUNTIME_MEDIANS) {
    const path = ['runtime_ratio_stats', statsKey, 'median'];
    const referenceMedian = numberAt(reference, path, 'reference snapshot');
    const candidateMedian = useCohort
      ? median(matched.map((model) => ratios.get(model)[key]))
      : numberAt(candidate, path, 'candidate snapshot');
    const scope = useCohort ? ` over ${matched.length} reference cohort models` : '';
    const text = `${label}${scope}: ${referenceMedian.toExponential(6)} -> ${candidateMedian.toExponential(6)}`;
    if (candidateMedian < referenceMedian * (1 - RUNTIME_MEDIAN_REL_TOLERANCE)) {
      regressions.push(text);
    } else if (candidateMedian > referenceMedian) {
      improvements.push(text);
    }
  }
}

// The candidate's per-model runtime ratios, bound to its own recorded cohort
// and medians; `null` for a snapshot from before they were recorded.
function candidateRuntimeRatios(candidate) {
  const recorded = candidate.runtime_model_ratios;
  if (recorded === undefined) {
    return null;
  }
  const name = 'candidate snapshot';
  assert.equal(
    typeof recorded === 'object' && recorded !== null && !Array.isArray(recorded),
    true,
    `${name}: runtime_model_ratios must be an object`,
  );
  const ratios = new Map();
  for (const [model, entry] of Object.entries(recorded)) {
    const where = `${name}: runtime_model_ratios.${model}`;
    ratios.set(model, { system: ratioOf(entry, 'system', where), wall: ratioOf(entry, 'wall', where) });
  }
  assert.deepEqual(
    [...ratios.keys()].sort(),
    [...(candidate.runtime_ratio_cohort_models ?? [])].sort(),
    `${name}: runtime_model_ratios does not cover runtime_ratio_cohort_models`,
  );
  for (const [, key, statsKey] of RUNTIME_MEDIANS) {
    const recordedMedian = numberAt(candidate, ['runtime_ratio_stats', statsKey, 'median'], name);
    const derived = median([...ratios.values()].map((ratio) => ratio[key]));
    assert.equal(
      sameValue(derived, recordedMedian, true),
      true,
      `${name}: runtime_model_ratios give a ${key} median of ${derived}, the stats record ${recordedMedian}`,
    );
  }
  return ratios;
}

export function median(values) {
  const sorted = [...values].sort((left, right) => left - right);
  const middle = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1
    ? sorted[middle]
    : (sorted[middle - 1] + sorted[middle]) / 2;
}

function countOf(entry, key, where) {
  const value = entry?.[key];
  assert.equal(Number.isInteger(value) && value >= 0, true, `${where}.${key} must be a count`);
  return value;
}

function massOf(entry, key, where) {
  const value = entry?.[key];
  assert.equal(
    typeof value === 'number' && Number.isFinite(value) && value >= 0,
    true,
    `${where}.${key} must be a finite non-negative number`,
  );
  return value;
}

function ratioOf(entry, key, where) {
  const value = entry?.[key];
  assert.equal(
    typeof value === 'number' && Number.isFinite(value) && value > 0,
    true,
    `${where}.${key} must be a finite positive ratio`,
  );
  return value;
}

function flagOf(entry, key, where) {
  const value = entry?.[key];
  assert.equal(typeof value, 'boolean', `${where}.${key} must be a boolean`);
  return value;
}

function integerAt(snapshot, path, name) {
  const value = numberAt(snapshot, path, name);
  assert.equal(Number.isInteger(value), true, `${name}: ${path.join('.')} must be an integer`);
  return value;
}

function numberAt(snapshot, path, name) {
  let value = snapshot;
  for (const key of path) {
    assert.equal(
      typeof value === 'object' && value !== null && Object.hasOwn(value, key),
      true,
      `${name}: missing quality metric ${path.join('.')}`,
    );
    value = value[key];
  }
  assert.equal(
    typeof value === 'number' && Number.isFinite(value),
    true,
    `${name}: ${path.join('.')} must be finite`,
  );
  return value;
}
