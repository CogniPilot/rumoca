import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import * as THREE from "three";

const exampleUrl = new URL("../../../examples/interactive/reusable_booster/", import.meta.url);

class FakeElement {
  constructor(tagName) {
    this.tagName = tagName;
    this.style = {};
    this.children = [];
    this.listeners = new Map();
    this.parentElement = null;
    this.open = false;
    this.value = "";
    this.textContent = "";
  }

  append(...children) {
    for (const child of children) this.appendChild(child);
  }

  appendChild(child) {
    child.parentElement = this;
    this.children.push(child);
    return child;
  }

  addEventListener(type, listener) {
    this.listeners.set(type, listener);
  }
}

function fakeCanvasContext() {
  const gradient = () => ({ addColorStop() {} });
  return {
    beginPath() {},
    clearRect() {},
    createLinearGradient: gradient,
    createRadialGradient: gradient,
    ellipse() {},
    fill() {},
    fillRect() {},
    fillText() {},
  };
}

function fakeDocument() {
  return {
    activeElement: null,
    createElement(tagName) {
      const element = new FakeElement(tagName);
      if (tagName === "canvas") {
        element.width = 0;
        element.height = 0;
        element.getContext = () => fakeCanvasContext();
      }
      return element;
    },
  };
}

function sceneApi(signals) {
  const host = new FakeElement("div");
  const canvas = new FakeElement("canvas");
  canvas.clientWidth = 1024;
  host.appendChild(canvas);
  const locals = new Map([
    ["wind_speed_setting", 5],
    ["wind_direction_setting", 35],
    ["wind_swing_setting", 15],
    ["gust_setting", 4],
    ["deck_heave_setting", 0.35],
    ["deck_roll_setting", 1.2],
    ["deck_pitch_setting", 0.8],
    ["position_gain_setting", 1],
    ["velocity_gain_setting", 1],
    ["attitude_gain_setting", 1],
    ["rate_gain_setting", 1],
  ]);
  return {
    THREE,
    scene: new THREE.Scene(),
    state: {},
    canvas,
    camera: new THREE.PerspectiveCamera(60, 1, 0.1, 2000),
    cameraMode: "scene",
    cam: { angle: 0, elev: 0, dist: 100 },
    frames: new Map([["body", new THREE.Matrix4().makeTranslation(10, 20, 30)]]),
    pointer: {
      captured: false,
      buttons: 0,
      dx: 0,
      dy: 0,
      wheel: 0,
    },
    get: (name) => signals.get(name),
    getLocal: (name) => locals.get(name),
    setLocal(name, value) {
      if (!locals.has(name)) return false;
      locals.set(name, value);
      return true;
    },
  };
}

test("Reusable booster landing viewer uses the scheduled transport and resolved body frame", async () => {
  const [scenario, scene] = await Promise.all([
    readFile(new URL("rumoca-scenario.toml", exampleUrl), "utf8"),
    readFile(new URL("reusable_booster_scene.js", exampleUrl), "utf8"),
  ]);

  assert.match(scenario, /\[viewer\][\s\S]*?mode\s*=\s*"external_web"/);
  assert.match(scene, /api\.frames\s*&&\s*api\.frames\.get\("body"\)/);
  assert.match(scene, /s\.booster\.matrix\.copy\(bodyFrame\)/);
});

test("Reusable booster roll plumes depict opposite body-axis torques", async () => {
  const scene = await readFile(new URL("reusable_booster_scene.js", exampleUrl), "utf8");

  const rollTorque = (signal) => {
    const escapedSignal = signal.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    const vector = "new THREE\\.Vector3\\(([-0-9.]+), ([-0-9.]+), ([-0-9.]+)\\)";
    const match = scene.match(new RegExp(`addJet\\("${escapedSignal}", ${vector}, ${vector}\\);`));
    assert.ok(match, `missing ${signal} plume geometry`);
    const position = match.slice(1, 4).map(Number);
    const exhaust = match.slice(4, 7).map(Number);
    const force = exhaust.map((component) => -component);
    return position[2] * force[0] - position[0] * force[2];
  };

  assert.ok(rollTorque("rcs_roll_positive") > 0);
  assert.ok(rollTorque("rcs_roll_negative") < 0);
});

test("Reusable booster reference path consumes every quintic endpoint derivative", async () => {
  const [scenario, scene] = await Promise.all([
    readFile(new URL("rumoca-scenario.toml", exampleUrl), "utf8"),
    readFile(new URL("reusable_booster_scene.js", exampleUrl), "utf8"),
  ]);

  for (const signal of ["plan_a0x", "plan_vf_x", "plan_af_x"]) {
    assert.match(scenario, new RegExp(`^${signal}\\s*=`, "m"));
  }
  assert.match(scene, /function quinticPosition\(time, duration, p0, v0, a0, pf, vf, af\)/);
  assert.match(scene, /rebuildReferencePath\(api, \{ p0, v0, a0, pf, vf, af, duration \}\)/);
});

test("Reusable booster scene initializes and advances through nominal, crash, and reset frames", async () => {
  const source = await readFile(new URL("reusable_booster_scene.js", exampleUrl), "utf8");
  const signals = new Map([
    ["t", 0],
    ["mission_phase", 0],
    ["crash", 0],
    ["leg_deploy", 1],
    ["thrust", 0],
    ["wall_ms", 16],
    ["plan_tf", 18],
    ["plan_p0z", 80],
    ["plan_pf_z", 20],
  ]);
  const api = sceneApi(signals);
  const previousDocument = globalThis.document;
  globalThis.document = fakeDocument();

  try {
    const ctx = new Function("ctx", "api", `${source}\nreturn ctx;`)({}, api);
    assert.equal(typeof ctx.onInit, "function");
    assert.equal(typeof ctx.onFrame, "function");
    await ctx.onInit(api);

    assert.equal(api.state.booster.children.length, 49);
    assert.equal(api.state.engines.length, 9);
    assert.equal(api.state.centerNozzle, api.state.engines[8]);
    assert.equal(api.state.booster.children[24], api.state.plumePivot);
    assert.equal(api.state.legs.length, 4);
    assert.equal(api.state.jets.length, 6);
    const stowedEndpoints = api.state.legs.map((leg) => leg.stowed.clone());

    ctx.onFrame(api);
    assert.equal(api.state.trailCount, 1);
    assert.deepEqual(api.state.bodyPosition.toArray(), [10, 20, 30]);

    signals.set("t", 1);
    signals.set("mission_phase", 5);
    signals.set("crash", 1);
    signals.set("leg_deploy", 0.5);
    signals.set("thrust", 0.7);
    signals.set("gimbal_x", 0.04);
    signals.set("gimbal_y", -0.03);
    signals.set("wind_vx", 5);
    signals.set("wind_vy", 2);
    signals.set("wind_speed", 5.4);
    signals.set("gps_update", 1);
    signals.set("wall_ms", 32);
    api.pointer.captured = true;
    api.pointer.dx = 4;
    api.pointer.dy = -2;
    api.pointer.wheel = 1;
    ctx.onFrame(api);

    assert.equal(api.state.crashActive, true);
    assert.equal(api.state.booster.visible, false);
    assert.ok(api.state.exhaustDirection.toArray().every(Number.isFinite));
    assert.ok(api.camera.position.toArray().every(Number.isFinite));
    for (const [index, leg] of api.state.legs.entries()) {
      assert.equal(leg.stowed.distanceTo(stowedEndpoints[index]), 0);
      assert.ok(leg.footPosition.toArray().every(Number.isFinite));
      assert.ok(leg.braceEnd.toArray().every(Number.isFinite));
    }

    signals.set("t", 0);
    signals.set("mission_phase", 0);
    signals.set("crash", 0);
    signals.set("wall_ms", 48);
    api.pointer.captured = false;
    ctx.onFrame(api);

    assert.equal(api.state.crashActive, false);
    assert.equal(api.state.trailCount, 0);
    assert.ok(api.state.cameraPosition.toArray().every(Number.isFinite));
    assert.ok(api.state.cameraTarget.toArray().every(Number.isFinite));
  } finally {
    if (previousDocument === undefined) {
      delete globalThis.document;
    } else {
      globalThis.document = previousDocument;
    }
  }
});
