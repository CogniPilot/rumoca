// Actual authored Booster input/mission lifecycle, shared by the existing cell.
const observerKey = "__rumocaGuideBoosterMission";

function requireValue(condition, message) {
  if (!condition) throw new Error(message);
}

function missionSample(state) {
  const names = ["mission_phase", "mission_elapsed", "landing_elapsed", "plan_duration",
    "wind_speed_input", "wind_speed_display", "launch_command", "thrust", "vehicle_mass",
    "altitude", "crash_indicator", "touchdown_safe", "plan_initial_velocity[3]",
    "mean_wind_velocity[1]", "mean_wind_velocity[2]", "wind_velocity[1]", "wind_velocity[2]"];
  const result = { time: state.time };
  for (const name of names) {
    requireValue(Number.isFinite(state.values[name]), `missing/nonfinite Booster observable ${name}`);
    result[name] = state.values[name];
  }
  return result;
}

function checkWind(sample, expected) {
  requireValue(Math.abs(sample.wind_speed_input - expected) < 1e-9, "scene slider did not route its model input");
  const mean = Math.hypot(sample["mean_wind_velocity[1]"], sample["mean_wind_velocity[2]"]);
  requireValue(Math.abs(mean - expected) < 1e-9, "live mean wind did not change without recompilation");
  const displayed = Math.hypot(sample["wind_velocity[1]"], sample["wind_velocity[2]"]);
  requireValue(Math.abs(sample.wind_speed_display - displayed) < 1e-9, "wind display disagrees with actual wind");
}

function checkNominalMission(trace, dt) {
  requireValue(trace.every((sample) => Number.isInteger(sample.mission_phase)), "mission phase must be an integer");
  const phases = trace.map((sample) => sample.mission_phase)
    .filter((phase, index, values) => index === 0 || phase !== values[index - 1]);
  requireValue(JSON.stringify(phases) === JSON.stringify([0, 1, 2, 3, 4]), `nominal mission phases ${phases}`);
  const ascent = trace.find((sample) => sample.mission_phase === 1);
  const coast = trace.find((sample) => sample.mission_phase === 2);
  const landing = trace.find((sample) => sample.mission_phase === 3);
  const landed = trace.at(-1);
  requireValue(ascent.launch_command === 1 && ascent.mission_elapsed >= 0, "Space did not ignite the mission");
  requireValue(trace.some((sample) => sample.mission_phase === 1 && sample.thrust > 0), "ascent has no physical thrust");
  requireValue(coast.mission_elapsed >= 6 - 1e-9 && coast.mission_elapsed <= 6 + dt + 1e-9,
    `authored six-second burn ended at ${coast.mission_elapsed}`);
  requireValue(landing["plan_initial_velocity[3]"] <= -8 + 1e-6, "landing did not capture the authored descending trigger");
  requireValue(trace.every((sample) => sample.plan_duration === 18), "authored nominal reference duration changed");
  requireValue(landed.mission_phase === 4 && landed.crash_indicator === 0, "nominal mission crashed instead of landing");
  requireValue(landed.vehicle_mass < trace[0].vehicle_mass, "mission consumed no propellant");
  return { phases, ascent, coast, landing, landed };
}

export async function observeBoosterMission({ controller, session, host, snapshot, dt, setRuntimeReject, getSession }) {
  requireValue(dt === 0.05, "retain the authored Booster output interval");
  requireValue(getSession() === session, "observe the actual configured session");
  const initial = missionSample(snapshot(session));
  requireValue(initial.time === 0 && initial.mission_phase === 0, "mission must start waiting at zero");
  const step = session.step.bind(session);
  const reset = session.reset.bind(session);
  const trace = [];
  const bridge = { stage: "ready", initial, latest: initial, resetState: null, resetNext: null,
    controller, host, nominal: false, wind: null, resetCalls: 0, postResetSteps: 0,
    beginNominal() { trace.push(bridge.latest); bridge.nominal = true; } };
  let timer;
  let resolveMission;
  const finished = new Promise((resolve, reject) => {
    resolveMission = resolve;
    bridge.fail = (error) => reject(new Error(`${error}; stage=${bridge.stage}; last=${JSON.stringify(bridge.latest)}`));
    setRuntimeReject(bridge.fail);
    timer = setTimeout(() => bridge.fail("Booster mission exceeded the existing 30s cell watchdog"), 30000);
  });
  session.reset = () => {
    requireValue(getSession() === session, "controller replaced the observed session before reset");
    reset();
    requireValue(getSession() === session, "reset replaced the observed JavaScript session");
    bridge.resetCalls++;
    bridge.resetState = missionSample(snapshot(session));
    bridge.stage = "reset";
  };
  session.step = (interval) => {
    try {
      requireValue(getSession() === session, "controller replaced the observed session before stepping");
      requireValue(interval === dt, "mission pacing changed the physical output interval");
      const before = session.time();
      step(interval);
      const sample = missionSample(snapshot(session));
      requireValue(Math.abs(sample.time - before - dt) < 1e-9, "mission skipped an authored interval");
      bridge.latest = sample;
      if (bridge.resetState) {
        bridge.postResetSteps++;
        if (!bridge.resetNext) bridge.resetNext = sample;
      }
      if (bridge.nominal) {
        trace.push(sample);
        if (sample.mission_phase === 5) bridge.fail("nominal mission entered the authored crash phase");
        if (sample.mission_phase === 4) resolveMission();
      }
    } catch (error) {
      bridge.fail(error);
      throw error;
    }
  };
  globalThis[observerKey] = bridge;
  host.focus();
  try {
    await finished;
    controller.stop();
    requireValue(bridge.resetCalls === 1 && bridge.postResetSteps > 0, "observe exactly one R reset and its subsequent steps");
    const nominal = checkNominalMission(trace, dt);
    return { initial, wind: bridge.wind, reset: bridge.resetState, resetNext: bridge.resetNext,
      resetCalls: bridge.resetCalls, postResetSteps: bridge.postResetSteps, nominal, intervals: trace.length - 1, trace };
  } finally {
    clearTimeout(timer);
    delete globalThis[observerKey];
    session.step = step;
    session.reset = reset;
  }
}

async function waitForMission(page, predicate) {
  await page.waitForFunction(predicate, undefined, { timeout: 30000 });
}

export async function driveBoosterMission(page) {
  await waitForMission(page, () => globalThis.__rumocaGuideBoosterMission?.stage === "ready");
  // Use the actual authored scene slider and trusted browser keyboard input.
  await page.locator("summary").filter({ hasText: "Tune environment and controller" }).click();
  const wind = page.locator("label").filter({ has: page.locator("span", { hasText: /^Mean wind$/ }) }).locator('input[type="range"]');
  requireValue(await wind.count() === 1, "the actual Mean wind control must exist once");
  requireValue(await wind.inputValue() === "5", "retain the authored default wind");
  await page.evaluate(() => globalThis.__rumocaGuideBoosterMission.controller.start());
  await waitForMission(page, () => globalThis.__rumocaGuideBoosterMission?.latest.time >= 0.05);
  const beforeKnob = await page.evaluate(() => globalThis.__rumocaGuideBoosterMission.latest.time);
  await wind.focus();
  await page.keyboard.press("ArrowRight");
  requireValue(await wind.inputValue() === "5.1", "actual slider increment did not execute");
  await waitForMission(page, () => globalThis.__rumocaGuideBoosterMission?.latest.wind_speed_input === 5.1);
  await page.evaluate(() => {
    const bridge = globalThis.__rumocaGuideBoosterMission;
    bridge.controller.stop();
    bridge.wind = bridge.latest;
    bridge.host.focus();
  });
  const changed = await page.evaluate(() => globalThis.__rumocaGuideBoosterMission.wind);
  checkWind(changed, 5.1);
  requireValue(changed.mission_phase === 0 && changed.time > beforeKnob, "knob route must advance without resetting");
  await page.keyboard.press("r");
  await waitForMission(page, () => Boolean(globalThis.__rumocaGuideBoosterMission?.resetNext));
  const reset = await page.evaluate(() => {
    const bridge = globalThis.__rumocaGuideBoosterMission;
    bridge.controller.stop();
    bridge.host.focus();
    return { atReset: bridge.resetState, next: bridge.resetNext };
  });
  requireValue(reset.atReset.time === 0 && reset.atReset.mission_phase === 0
    && reset.atReset.mission_elapsed === 0 && reset.atReset.landing_elapsed === 0, "R did not restore waiting at exact zero");
  checkWind(reset.next, 5);
  requireValue(reset.next.launch_command === 0, "R did not restore the unlaunched input");
  requireValue(await wind.inputValue() === "5", "R did not restore the scene local");
  await page.evaluate(() => {
    const bridge = globalThis.__rumocaGuideBoosterMission;
    bridge.beginNominal();
  });
  // T is an authored pacing control; it preserves dt, solver, and tolerances.
  await page.keyboard.press("t");
  await page.keyboard.press("Space");
  await page.evaluate(() => globalThis.__rumocaGuideBoosterMission.controller.start());
}
