import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';

import {
  ensurePromotableSnapshot,
  promoteBaselineIfImproved,
  ratchetDecision,
} from './msl-baseline-ratchet.mjs';

function fullSnapshot() {
  return {
    quality_gate_version: 1,
    run_scope: 'full',
    omc_version: 'OpenModelica 1.27.0',
    simulatable_attempted: 10,
    sim_target_models: 10,
    parse_models: 10,
    flatten_models: 9,
    compiled_models: 8,
    solve_models: 7,
    balanced_models: 8,
    initial_balanced_models: 8,
    ic_ok: 6,
    sim_ok: 5,
    partial_models: 1,
    unbalanced_models: 0,
    ic_solver_fail: 2,
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

test('promotable snapshot accepts full non-partial artifacts', () => {
  const snapshot = fullSnapshot();
  assert.doesNotThrow(() => ensurePromotableSnapshot(snapshot));
  assert.doesNotThrow(() => ensurePromotableSnapshot({ ...snapshot, partial: false }));
});

test('promotable snapshot rejects partial artifacts', () => {
  assert.throws(
    () => ensurePromotableSnapshot({ ...fullSnapshot(), partial: true }),
    /partial snapshots cannot be promoted/,
  );
  assert.throws(
    () => ensurePromotableSnapshot({ ...fullSnapshot(), run_scope: 'partial' }),
    /only full MSL quality snapshots/,
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

  const decision = ratchetDecision(current, baseline);
  assert.equal(decision.promote, true);
  assert.match(decision.improvements.join('\n'), /solve models/);
  assert.match(decision.improvements.join('\n'), /high trace agreement/);
});

test('ratchet skips equivalent snapshots', () => {
  const decision = ratchetDecision(fullSnapshot(), fullSnapshot());
  assert.equal(decision.promote, false);
  assert.match(decision.reason, /equivalent/);
});

test('ratchet skips when any ratchet metric regresses', () => {
  const baseline = fullSnapshot();
  const current = fullSnapshot();
  current.sim_ok = 6;
  current.trace_accuracy_stats.bad_channels_total = 11;

  const decision = ratchetDecision(current, baseline);
  assert.equal(decision.promote, false);
  assert.match(decision.reason, /trace bad channels/);
});

test('ratchet rejects changed fixed target context', () => {
  const baseline = fullSnapshot();
  const current = fullSnapshot();
  current.sim_target_models = 11;

  assert.throws(() => ratchetDecision(current, baseline), /sim_target_models changed/);
});

test('promoteBaselineIfImproved writes only when improved', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'msl-ratchet-'));
  const baselinePath = path.join(dir, 'baseline.json');
  const sourcePath = path.join(dir, 'source.json');
  const baseline = fullSnapshot();
  const source = fullSnapshot();
  source.sim_ok = 6;
  fs.writeFileSync(baselinePath, JSON.stringify(baseline, null, 2));
  fs.writeFileSync(sourcePath, JSON.stringify(source, null, 2));

  const decision = promoteBaselineIfImproved({
    sourcePath,
    baselinePath,
    log: () => {},
  });
  assert.equal(decision.promote, true);
  assert.equal(JSON.parse(fs.readFileSync(baselinePath, 'utf8')).sim_ok, 6);
});

test('promoteBaselineIfImproved leaves equivalent baseline unchanged', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'msl-ratchet-'));
  const baselinePath = path.join(dir, 'baseline.json');
  const sourcePath = path.join(dir, 'source.json');
  const baselineText = JSON.stringify(fullSnapshot(), null, 2);
  fs.writeFileSync(baselinePath, baselineText);
  fs.writeFileSync(sourcePath, baselineText);

  const decision = promoteBaselineIfImproved({
    sourcePath,
    baselinePath,
    log: () => {},
  });
  assert.equal(decision.promote, false);
  assert.equal(fs.readFileSync(baselinePath, 'utf8'), baselineText);
});
