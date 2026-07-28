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
    quality_gate_version: 2,
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

function exactV2Migration() {
  return {
    from_quality_gate_version: 1,
    to_quality_gate_version: 2,
    flatten_models_before: 565,
    flatten_models_after: 555,
    reattributed_error_code: 'ER002',
    reattributed_models: [
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
    ],
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

test('ratchet accepts an exact versioned metric-attribution migration', () => {
  const baseline = fullSnapshot();
  baseline.quality_gate_version = 1;
  baseline.flatten_models = 565;
  const current = fullSnapshot();
  current.flatten_models = 555;
  current.metric_schema_migration = exactV2Migration();

  const decision = ratchetDecision(current, baseline);
  assert.equal(decision.promote, true);
  assert.match(decision.improvements.join('\n'), /quality schema/);
});

test('ratchet rejects an unproven metric-attribution migration', () => {
  const baseline = fullSnapshot();
  baseline.quality_gate_version = 1;
  baseline.flatten_models = 565;
  const current = fullSnapshot();
  current.flatten_models = 555;
  current.metric_schema_migration = exactV2Migration();
  current.metric_schema_migration.reattributed_models[9] = 'Modelica.HandLowered.Substitute';
  assert.throws(() => ratchetDecision(current, baseline), /reviewed correction/);
});

test('schema migration rejects an unrelated cumulative metric regression', () => {
  const baseline = fullSnapshot();
  baseline.quality_gate_version = 1;
  baseline.flatten_models = 565;
  const current = fullSnapshot();
  current.flatten_models = 555;
  current.compiled_models -= 1;
  current.metric_schema_migration = exactV2Migration();

  const decision = ratchetDecision(current, baseline);
  assert.equal(decision.promote, false);
  assert.match(decision.reason, /compiled models/);
});

test('schema migration rejects an unrelated headline regression', () => {
  const baseline = fullSnapshot();
  baseline.quality_gate_version = 1;
  baseline.flatten_models = 565;
  const current = fullSnapshot();
  current.flatten_models = 555;
  current.trace_accuracy_stats.agreement_high -= 1;
  current.metric_schema_migration = exactV2Migration();

  const decision = ratchetDecision(current, baseline);
  assert.equal(decision.promote, false);
  assert.match(decision.reason, /high trace agreement/);
});

test('OMC migration compares independent metrics without cross-context trace rejection', () => {
  const baseline = fullSnapshot();
  baseline.quality_gate_version = 1;
  baseline.flatten_models = 565;
  baseline.omc_version = 'OpenModelica old';
  const current = fullSnapshot();
  current.flatten_models = 555;
  current.omc_version = 'OpenModelica new';
  current.metric_schema_migration = exactV2Migration();
  current.trace_accuracy_stats.agreement_high = 0;
  current.runtime_ratio_stats.system_ratio_both_success.median = 0.01;
  const checkedIn = approvedOmcMigration(baseline.omc_version, current.omc_version);

  const decision = ratchetDecision(current, baseline, checkedIn);
  assert.equal(decision.promote, true);
  assert.match(decision.improvements.join('\n'), /OMC context/);

  current.compiled_models -= 1;
  const regressed = ratchetDecision(current, baseline, checkedIn);
  assert.equal(regressed.promote, false);
  assert.match(regressed.reason, /compiled models/);
});

test('OMC migration rejects missing or mismatched checked-in approval', () => {
  const baseline = fullSnapshot();
  baseline.omc_version = 'OpenModelica old';
  const current = fullSnapshot();
  current.omc_version = 'OpenModelica new';

  assert.throws(
    () => ratchetDecision(current, baseline),
    /reviewed checked-in migration/,
  );
  const reversed = approvedOmcMigration(current.omc_version, baseline.omc_version);
  assert.throws(
    () => ratchetDecision(current, baseline, reversed),
    /does not match current snapshot/,
  );
});

test('ratchet rejects a runtime speedup drop beyond 35 percent', () => {
  const baseline = fullSnapshot();
  const current = fullSnapshot();
  current.sim_ok = 6;
  current.runtime_ratio_stats.system_ratio_both_success.median = 1.29;
  const decision = ratchetDecision(current, baseline);
  assert.equal(decision.promote, false);
  assert.match(decision.reason, /runtime system speedup median/);
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

test('promotion loads the reviewed checked-in OMC migration', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'msl-ratchet-'));
  const baselinePath = path.join(dir, 'baseline.json');
  const sourcePath = path.join(dir, 'source.json');
  const checkedInBaselinePath = path.join(dir, 'checked-in.json');
  const baseline = fullSnapshot();
  baseline.quality_gate_version = 1;
  baseline.flatten_models = 565;
  baseline.omc_version = 'OpenModelica old';
  const source = fullSnapshot();
  source.flatten_models = 555;
  source.omc_version = 'OpenModelica new';
  source.metric_schema_migration = exactV2Migration();
  const checkedIn = approvedOmcMigration(baseline.omc_version, source.omc_version);
  fs.writeFileSync(baselinePath, JSON.stringify(baseline, null, 2));
  fs.writeFileSync(sourcePath, JSON.stringify(source, null, 2));
  fs.writeFileSync(checkedInBaselinePath, JSON.stringify(checkedIn, null, 2));

  const decision = promoteBaselineIfImproved({
    sourcePath,
    baselinePath,
    checkedInBaselinePath,
    log: () => {},
  });
  assert.equal(decision.promote, true);
  assert.equal(JSON.parse(fs.readFileSync(baselinePath, 'utf8')).omc_version, source.omc_version);
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
