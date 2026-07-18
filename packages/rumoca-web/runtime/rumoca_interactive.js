import { ensureParsedSourceRootCache } from './rumoca_runtime.js';

const GAMEPAD_AXES = {
  LeftStickX: { index: 0, sign: 1 },
  LeftStickY: { index: 1, sign: -1 },
  RightStickX: { index: 2, sign: 1 },
  RightStickY: { index: 3, sign: -1 },
};

const GAMEPAD_BUTTONS = {
  South: 0,
  East: 1,
  West: 2,
  North: 3,
  Select: 8,
  Start: 9,
  LeftShoulder: 4,
  RightShoulder: 5,
};

function trimMaybeString(value) {
  return typeof value === 'string' ? value.trim() : '';
}

function hasJsonObjectPayload(value) {
  const text = trimMaybeString(value);
  return Boolean(text && text !== '{}' && text !== 'null');
}

function finiteNumber(value, fallback = 0) {
  const number = Number(value);
  return Number.isFinite(number) ? number : fallback;
}

function sortedEntries(object) {
  return object && typeof object === 'object'
    ? Object.entries(object).sort(([a], [b]) => a.localeCompare(b))
    : [];
}

function clamp(value, limits) {
  return Array.isArray(limits) && limits.length >= 2
    ? Math.max(finiteNumber(limits[0]), Math.min(finiteNumber(limits[1]), value))
    : value;
}

function normalizePacingMode(value) {
  const text = trimMaybeString(value).toLowerCase().replace(/[-\s]+/g, '_');
  return text === 'as_fast_as_possible' ? 'as_fast_as_possible' : 'realtime';
}

function pacingModeLabel(mode) {
  return mode === 'as_fast_as_possible' ? 'fast' : 'realtime';
}

function speedRatioLabel(value) {
  const ratio = finiteNumber(value, 0);
  if (ratio >= 100) {
    return `${ratio.toFixed(0)}x`;
  }
  if (ratio >= 10) {
    return `${ratio.toFixed(1)}x`;
  }
  return `${ratio.toFixed(2)}x`;
}

function ensureInteractiveRuntimeStyles(ownerDocument) {
  if (!ownerDocument || ownerDocument.getElementById('rumoca-interactive-runtime-styles')) {
    return;
  }
  const style = ownerDocument.createElement('style');
  style.id = 'rumoca-interactive-runtime-styles';
  style.textContent = `
.rumoca-interactive-root {
  position: relative;
  overflow: hidden;
  touch-action: none;
}
.rumoca-interactive-root:fullscreen {
  width: 100vw;
  height: 100vh;
  background: #071825;
}
.rumoca-interactive-root:-webkit-full-screen {
  width: 100vw;
  height: 100vh;
  background: #071825;
}
.rumoca-interactive-canvas {
  display: block;
  width: 100%;
  height: 100%;
}
.rumoca-interactive-flight-hud {
  position: absolute;
  inset: 0;
  z-index: 4;
  pointer-events: none;
}
.rumoca-interactive-controls {
  position: absolute;
  left: 12px;
  right: 12px;
  bottom: 12px;
  z-index: 5;
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  align-items: center;
  pointer-events: auto;
}
.rumoca-interactive-controls button,
.rumoca-interactive-controls .rumoca-interactive-key-echo {
  min-height: 28px;
  padding: 4px 9px;
  border: 1px solid rgba(255, 255, 255, 0.3);
  border-radius: 6px;
  background: rgba(10, 14, 18, 0.82);
  color: #f4f7fb;
  font: inherit;
  font-size: 12px;
  line-height: 18px;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.3);
}
.rumoca-interactive-controls button {
  cursor: pointer;
}
.rumoca-interactive-controls.is-capturing .rumoca-interactive-capture-toggle,
.rumoca-interactive-controls .rumoca-interactive-pacing-toggle.is-fast,
.rumoca-interactive-controls .rumoca-interactive-fullscreen-toggle.is-fullscreen {
  border-color: #37b7ff;
  background: rgba(0, 103, 168, 0.88);
}
.rumoca-interactive-controls .rumoca-interactive-run-state[data-state="finished"] {
  border-color: #f2c14f;
  color: #ffe7a3;
}
.rumoca-interactive-controls .rumoca-interactive-run-state[data-state="failed"] {
  border-color: #f06a6a;
  color: #ffc1c1;
}
@media (max-width: 640px) {
  .rumoca-interactive-controls button,
  .rumoca-interactive-controls .rumoca-interactive-key-echo {
    min-height: 42px;
    font-size: 16px;
  }
}
`;
  (ownerDocument.head || ownerDocument.documentElement).appendChild(style);
}

function localDefault(def) {
  if (!def || typeof def !== 'object') {
    return 0;
  }
  if (trimMaybeString(def.type).toLowerCase() === 'bool') {
    return Boolean(def.default);
  }
  return finiteNumber(def.default, 0);
}

function inferredKeyboardDecayTargets(keyboardBindings) {
  const targets = new Set();
  for (const binding of Object.values(keyboardBindings || {})) {
    if (trimMaybeString(binding?.action).toLowerCase() !== 'set') {
      continue;
    }
    const target = trimMaybeString(binding.target);
    if (target) {
      targets.add(target);
    }
  }
  return Array.from(targets).sort((a, b) => a.localeCompare(b));
}

function createKeyboardDecaySpec(decay, keyboardBindings) {
  const raw = decay && typeof decay === 'object' ? decay : null;
  const hasExplicitTargets = raw && Object.prototype.hasOwnProperty.call(raw, 'targets');
  const targets = hasExplicitTargets
    ? (Array.isArray(raw.targets) ? raw.targets.map(trimMaybeString).filter(Boolean) : [])
    : inferredKeyboardDecayTargets(keyboardBindings);
  if (targets.length === 0) {
    return null;
  }
  return {
    factor: finiteNumber(raw?.factor, 0.85),
    ref_dt: raw?.ref_dt ?? raw?.refDt ?? 0.016,
    targets,
  };
}

function sourceValue(source, locals, session, runtime) {
  const text = trimMaybeString(source);
  if (!text) {
    return 0;
  }
  if (text.startsWith('local:')) {
    return locals.get(text.slice('local:'.length)) ?? 0;
  }
  if (text.startsWith('model:')) {
    const name = text.slice('model:'.length);
    return name === 'time' ? session.time() : (session.get(name) ?? 0);
  }
  if (text.startsWith('runtime:')) {
    return runtime[text.slice('runtime:'.length)] ?? 0;
  }
  return locals.get(text) ?? session.get(text) ?? 0;
}

function routeValue(route, locals, session, runtime) {
  if (typeof route === 'string') {
    return sourceValue(route, locals, session, runtime);
  }
  if (!route || typeof route !== 'object') {
    return 0;
  }
  if (Object.prototype.hasOwnProperty.call(route, 'const')) {
    return finiteNumber(route.const, 0);
  }
  const value = sourceValue(route.from, locals, session, runtime);
  if (typeof value === 'boolean') {
    return value ? finiteNumber(route.when_true, 1) : finiteNumber(route.when_false, 0);
  }
  return Number.isFinite(Number(value)) ? Number(value) : finiteNumber(route.default, 0);
}

function comparePrecondition(left, op, right) {
  switch (op) {
    case '<': return left < right;
    case '<=': return left <= right;
    case '>': return left > right;
    case '>=': return left >= right;
    case '==': return left === right;
    case '!=': return left !== right;
    default: return false;
  }
}

function preconditionAllows(expression, locals) {
  const text = trimMaybeString(expression);
  if (!text) {
    return true;
  }
  const match = /^([A-Za-z_][A-Za-z0-9_]*)\s*(<=|>=|==|!=|<|>)\s*(-?(?:\d+\.?\d*|\.\d+))$/.exec(text);
  if (!match) {
    return false;
  }
  return comparePrecondition(finiteNumber(locals.get(match[1]), 0), match[2], Number(match[3]));
}

function sourceGamepadAxis(source, gamepad) {
  const axis = GAMEPAD_AXES[trimMaybeString(source)];
  if (!axis || !gamepad || !Array.isArray(gamepad.axes)) {
    return 0;
  }
  return finiteNumber(gamepad.axes[axis.index], 0) * axis.sign;
}

function sourceGamepadButton(source, gamepad) {
  const index = GAMEPAD_BUTTONS[trimMaybeString(source)];
  return index !== undefined && Boolean(gamepad?.buttons?.[index]?.pressed);
}

function normalizedKeyboardKey(eventOrKey) {
  const key = typeof eventOrKey === 'string' ? eventOrKey : eventOrKey?.key;
  if (key === ' ') {
    return 'Space';
  }
  if (key === 'Esc') {
    return 'Escape';
  }
  return typeof key === 'string' && key.length === 1 ? key.toLowerCase() : trimMaybeString(key);
}

export function createInputRuntime(config) {
  const locals = new Map();
  const keyboardBindings = {};
  for (const [key, binding] of sortedEntries(config?.input?.keyboard?.keys)) {
    const normalized = normalizedKeyboardKey(key);
    if (normalized) {
      keyboardBindings[normalized] = binding;
    }
  }
  for (const [name, def] of sortedEntries(config?.locals)) {
    locals.set(name, localDefault(def));
  }
  const pressedKeys = new Set();
  const signals = new Set();
  const debounceUntil = new Map();
  const pressedButtons = new Set();
  let connectedGamepad = null;
  let lastMode = 'keyboard';
  const keyboardDecay = createKeyboardDecaySpec(config?.input?.keyboard?.decay, keyboardBindings);

  function resetLocals() {
    locals.clear();
    for (const [name, def] of sortedEntries(config?.locals)) {
      locals.set(name, localDefault(def));
    }
    signals.clear();
  }

  function signal(name) {
    const text = trimMaybeString(name);
    if (text) {
      signals.add(text);
    }
  }

  function applyAction(id, binding, active, nowMs) {
    const action = trimMaybeString(binding?.action).toLowerCase();
    if (action === 'set') {
      if (active) {
        locals.set(trimMaybeString(binding.target), finiteNumber(binding.value, 0));
      }
      return;
    }
    if (!active) {
      return;
    }
    const waitUntil = debounceUntil.get(id) || 0;
    if (nowMs < waitUntil || !preconditionAllows(binding?.precondition, locals)) {
      return;
    }
    const debounceMs = Math.max(0, finiteNumber(binding?.debounce_ms ?? binding?.debounceMs, 0));
    if (debounceMs > 0) {
      debounceUntil.set(id, nowMs + debounceMs);
    }
    if (action === 'toggle') {
      const state = trimMaybeString(binding.state);
      locals.set(state, !Boolean(locals.get(state)));
    } else if (action === 'signal') {
      signal(binding.signal);
    }
  }

  function applyHeldKeyboardAction(id, binding, nowMs) {
    if (trimMaybeString(binding?.action).toLowerCase() === 'set') {
      applyAction(id, binding, true, nowMs);
    }
  }

  function keyDown(event) {
    const key = normalizedKeyboardKey(event);
    const binding = keyboardBindings[key];
    if (!binding) {
      return false;
    }
    const wasPressed = pressedKeys.has(key);
    pressedKeys.add(key);
    const id = `key:${key}`;
    if (wasPressed) {
      applyHeldKeyboardAction(id, binding, performance.now());
    } else {
      applyAction(id, binding, true, performance.now());
    }
    event.preventDefault();
    return true;
  }

  function keyUp(event) {
    const key = normalizedKeyboardKey(event);
    const binding = keyboardBindings[key];
    if (!binding) {
      return false;
    }
    pressedKeys.delete(key);
    applyAction(`key:${key}`, binding, false, performance.now());
    event.preventDefault();
    return true;
  }

  function pollGamepad() {
    const pads = typeof navigator !== 'undefined' && navigator.getGamepads
      ? Array.from(navigator.getGamepads()).filter(Boolean)
      : [];
    connectedGamepad = pads[0] || null;
    if (connectedGamepad) {
      lastMode = 'gamepad';
    }
    return connectedGamepad;
  }

  function update(dt) {
    const input = config?.input || {};
    const gamepad = input.mode === 'keyboard' ? null : pollGamepad();
    if (!gamepad && pressedKeys.size > 0) {
      lastMode = 'keyboard';
    }
    const nowMs = performance.now();
    applyKeyboardDecay(keyboardDecay, dt);
    for (const [key, binding] of sortedEntries(keyboardBindings)) {
      if (pressedKeys.has(key)) {
        applyHeldKeyboardAction(`key:${key}`, binding, nowMs);
      }
    }
    applyIntegrators(input.keyboard?.integrators, dt, null);
    if (gamepad) {
      applyGamepadAxes(input.gamepad?.axes, gamepad);
      applyIntegrators(input.gamepad?.integrators, dt, gamepad);
      applyGamepadButtons(input.gamepad?.buttons, gamepad, nowMs);
    }
  }

  function applyKeyboardDecay(decay, dt) {
    if (!decay || typeof decay !== 'object') {
      return;
    }
    const targets = Array.isArray(decay.targets)
      ? decay.targets.map(trimMaybeString).filter(Boolean)
      : [];
    if (targets.length === 0) {
      return;
    }
    const elapsed = finiteNumber(dt, 0);
    if (elapsed <= 0) {
      return;
    }
    const factor = clamp(finiteNumber(decay.factor, 1), [0, 1]);
    const refDt = Math.max(finiteNumber(decay.ref_dt ?? decay.refDt, 0.016), Number.EPSILON);
    const scale = Math.pow(factor, elapsed / refDt);
    for (const target of targets) {
      const current = locals.get(target);
      if (typeof current === 'number' && Number.isFinite(current)) {
        locals.set(target, current * scale);
      }
    }
  }

  function applyIntegrators(integrators, dt, gamepad) {
    for (const [name, spec] of sortedEntries(integrators)) {
      const raw = gamepad ? sourceGamepadAxis(spec.source || name, gamepad) : sourceValue(spec.source, locals, { get: () => 0, time: () => 0 }, {});
      const deadband = Math.abs(finiteNumber(spec.deadband, 0));
      const active = Math.abs(raw) > deadband ? raw : 0;
      const write = trimMaybeString(spec.write || name);
      const next = finiteNumber(locals.get(write), 0) + active * finiteNumber(spec.rate, 1) * dt;
      locals.set(write, clamp(next, spec.clamp));
    }
  }

  function applyGamepadAxes(axes, gamepad) {
    for (const [name, spec] of sortedEntries(axes)) {
      const value = sourceGamepadAxis(spec.source || name, gamepad)
        * finiteNumber(spec.scale, 1)
        * (spec.invert ? -1 : 1);
      locals.set(trimMaybeString(spec.write || name), value);
    }
  }

  function applyGamepadButtons(buttons, gamepad, nowMs) {
    for (const [name, spec] of sortedEntries(buttons)) {
      const id = `button:${name}`;
      const active = sourceGamepadButton(spec.source || name, gamepad);
      const action = trimMaybeString(spec?.action).toLowerCase();
      if (action === 'set') {
        applyAction(id, spec, active, nowMs);
      } else if (active && !pressedButtons.has(id)) {
        applyAction(id, spec, true, nowMs);
      }
      if (active) {
        pressedButtons.add(id);
      } else {
        pressedButtons.delete(id);
      }
    }
  }

  function takeSignal(name) {
    const text = trimMaybeString(name);
    const found = signals.has(text);
    signals.delete(text);
    return found;
  }

  return {
    locals,
    keyDown,
    keyUp,
    hasKeyboardBinding(eventOrKey) {
      return Boolean(keyboardBindings[normalizedKeyboardKey(eventOrKey)]);
    },
    releaseKeys() {
      pressedKeys.clear();
      pressedButtons.clear();
    },
    resetLocals,
    takeSignal,
    update,
    runtimeFields(frameNum, modelTime = 0) {
      return {
        frame_num: frameNum,
        wall_ms: performance.now(),
        input_connected: Boolean(connectedGamepad),
        input_mode: lastMode,
        model_time: modelTime,
      };
    },
  };
}

export function takeRuntimeControlSignal(config, input) {
  const resetSignal = trimMaybeString(config?.reset?.on_signal);
  if (resetSignal && input.takeSignal(resetSignal)) {
    return {
      action: 'reset',
      resetLocals: Boolean(config?.reset?.reset_locals),
      resetSession: Boolean(config?.reset?.reset_session),
    };
  }
  const quitSignal = trimMaybeString(config?.quit?.on_signal) || 'quit';
  return input.takeSignal(quitSignal) ? { action: 'quit' } : null;
}

function createViewerRuntime({ THREE, container, viewerSignals, assetBaseUrl, pointer, config, input }) {
  const ownerDocument = container.ownerDocument || document;
  const ownerWindow = ownerDocument.defaultView || globalThis;
  const canvas = ownerDocument.createElement('canvas');
  canvas.className = 'rumoca-interactive-canvas';
  const flightHud = ownerDocument.createElement('canvas');
  flightHud.className = 'rumoca-interactive-flight-hud';
  container.replaceChildren(canvas, flightHud);

  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(60, 1, 0.01, 5000);
  camera.position.set(4, 2.4, 6);
  const renderer = new THREE.WebGLRenderer({ canvas, antialias: true, alpha: false });
  renderer.setPixelRatio(Math.max(1, Math.min(2, ownerWindow.devicePixelRatio || 1)));
  const flightHudCtx = flightHud.getContext('2d');
  const state = {};
  const cam = {
    target: new THREE.Vector3(0, 0, 0),
    dist: 7,
    angle: -Math.PI / 2,
    elev: 0.22,
  };
  const viewerConfig = config?.viewer || {};
  const frames = Array.isArray(viewerConfig.frame) ? viewerConfig.frame : [];
  const frameMatrices = new Map(frames.map((frame) => [frame.name, new THREE.Matrix4()]));
  const configuredCameras = (Array.isArray(viewerConfig.camera) ? viewerConfig.camera : []).map((cameraConfig) => ({
    name: trimMaybeString(cameraConfig.name),
    frame: trimMaybeString(cameraConfig.frame),
    mount: fluVector(THREE, cameraConfig.mount, [0, 0, 0]),
    look: fluVector(THREE, cameraConfig.look, [1, 0, 0]).normalize(),
    up: fluVector(THREE, cameraConfig.up, [0, 0, 1]).normalize(),
  })).filter((cameraConfig) => cameraConfig.name && cameraConfig.frame);
  const cameraModes = ['scene', ...configuredCameras.map((cameraConfig) => cameraConfig.name)];
  const cameraScratch = {
    mount: new THREE.Vector3(),
    look: new THREE.Vector3(),
    up: new THREE.Vector3(),
    target: new THREE.Vector3(),
  };
  let cameraMode = cameraModes[0];
  let flightHudVisible = Boolean(viewerConfig.hud);

  if (assetBaseUrl && THREE.DefaultLoadingManager?.setURLModifier) {
    THREE.DefaultLoadingManager.setURLModifier((url) => {
      if (String(url).startsWith('/assets/')) {
        return new URL(String(url).slice('/assets/'.length), assetBaseUrl).href;
      }
      return url;
    });
  }

  function resize() {
    const rect = container.getBoundingClientRect();
    const width = Math.max(1, Math.floor(rect.width));
    const height = Math.max(240, Math.floor(rect.height || width * 0.56));
    renderer.setSize(width, height, false);
    camera.aspect = width / height;
    camera.updateProjectionMatrix();
    const dpr = Math.max(1, Math.min(2, ownerWindow.devicePixelRatio || 1));
    flightHud.width = Math.floor(width * dpr);
    flightHud.height = Math.floor(height * dpr);
    flightHud.style.width = `${width}px`;
    flightHud.style.height = `${height}px`;
    flightHudCtx.setTransform(dpr, 0, 0, dpr, 0, 0);
  }
  resize();

  function currentState() {
    return Object.fromEntries(viewerSignals.entries());
  }

  function drawFlightHud(hud, hudState, roll, pitch, speed) {
    const rect = canvas.getBoundingClientRect();
    const width = Math.max(1, Math.floor(rect.width));
    const height = Math.max(1, Math.floor(rect.height));
    flightHudCtx.clearRect(0, 0, width, height);
    if (!flightHudVisible || (!hudState.t && hudState.t !== 0)) {
      return;
    }
    const cx = width / 2;
    const cy = height / 2;
    const hudColor = 'rgba(94, 255, 190, 0.92)';
    const hudDim = 'rgba(94, 255, 190, 0.42)';
    const textShadow = 'rgba(0, 0, 0, 0.75)';
    const pitchPxPerRad = Math.min(width, height) * 0.42;

    flightHudCtx.save();
    flightHudCtx.translate(cx, cy);
    flightHudCtx.rotate(-roll);
    flightHudCtx.translate(0, pitch * pitchPxPerRad);
    flightHudCtx.strokeStyle = hudColor;
    flightHudCtx.lineWidth = 2;
    flightHudCtx.beginPath();
    flightHudCtx.moveTo(-160, 0);
    flightHudCtx.lineTo(-35, 0);
    flightHudCtx.moveTo(35, 0);
    flightHudCtx.lineTo(160, 0);
    flightHudCtx.stroke();

    flightHudCtx.strokeStyle = hudDim;
    flightHudCtx.lineWidth = 1.5;
    flightHudCtx.font = '12px monospace';
    flightHudCtx.textAlign = 'center';
    flightHudCtx.textBaseline = 'middle';
    for (let deg = -30; deg <= 30; deg += 10) {
      if (deg === 0) {
        continue;
      }
      const y = -deg * Math.PI / 180 * pitchPxPerRad;
      const half = Math.abs(deg) % 20 === 0 ? 72 : 45;
      flightHudCtx.beginPath();
      flightHudCtx.moveTo(-half, y);
      flightHudCtx.lineTo(-16, y);
      flightHudCtx.moveTo(16, y);
      flightHudCtx.lineTo(half, y);
      flightHudCtx.stroke();
      flightHudCtx.fillStyle = hudColor;
      flightHudCtx.fillText(String(Math.abs(deg)), -half - 18, y);
      flightHudCtx.fillText(String(Math.abs(deg)), half + 18, y);
    }
    flightHudCtx.restore();

    flightHudCtx.save();
    flightHudCtx.translate(cx, cy);
    flightHudCtx.strokeStyle = hudColor;
    flightHudCtx.lineWidth = 2;
    flightHudCtx.beginPath();
    flightHudCtx.moveTo(-18, 0);
    flightHudCtx.lineTo(-6, 0);
    flightHudCtx.lineTo(0, 8);
    flightHudCtx.lineTo(6, 0);
    flightHudCtx.lineTo(18, 0);
    flightHudCtx.stroke();
    flightHudCtx.beginPath();
    flightHudCtx.arc(0, 0, 4, 0, Math.PI * 2);
    flightHudCtx.stroke();
    flightHudCtx.restore();

    flightHudCtx.save();
    flightHudCtx.translate(cx, 76);
    flightHudCtx.rotate(-roll);
    flightHudCtx.strokeStyle = hudColor;
    flightHudCtx.lineWidth = 2;
    flightHudCtx.beginPath();
    flightHudCtx.arc(0, 0, 54, Math.PI * 1.1, Math.PI * 1.9);
    flightHudCtx.stroke();
    for (const deg of [-45, -30, -15, 0, 15, 30, 45]) {
      const angle = (deg - 90) * Math.PI / 180;
      const r1 = deg === 0 ? 43 : 48;
      const r2 = 56;
      flightHudCtx.beginPath();
      flightHudCtx.moveTo(Math.cos(angle) * r1, Math.sin(angle) * r1);
      flightHudCtx.lineTo(Math.cos(angle) * r2, Math.sin(angle) * r2);
      flightHudCtx.stroke();
    }
    flightHudCtx.restore();

    const hudText = (text, x, y, align = 'left') => {
      flightHudCtx.font = '15px monospace';
      flightHudCtx.textAlign = align;
      flightHudCtx.textBaseline = 'middle';
      flightHudCtx.fillStyle = textShadow;
      flightHudCtx.fillText(text, x + 1, y + 1);
      flightHudCtx.fillStyle = hudColor;
      flightHudCtx.fillText(text, x, y);
    };

    if (hud.altitude) {
      hudText(`ALT ${Number(hudState[hud.altitude] ?? 0).toFixed(1)} m`, width - 34, cy - 40, 'right');
    }
    if (Array.isArray(hud.speed) && hud.speed.length > 0) {
      hudText(`SPD ${speed.toFixed(1)} m/s`, 34, cy - 40);
    }
    hudText(`ROLL ${(roll * 180 / Math.PI).toFixed(1)}°`, 34, cy + 44);
    hudText(`PITCH ${(pitch * 180 / Math.PI).toFixed(1)}°`, width - 34, cy + 44, 'right');
    if (hud.sticks) {
      const stick = (name) => Number(hudState[name] ?? 0).toFixed(2);
      const rows = [];
      if (hud.sticks.roll) rows.push(`AIL ${stick(hud.sticks.roll)}`);
      if (hud.sticks.pitch) rows.push(`ELE ${stick(hud.sticks.pitch)}`);
      if (hud.sticks.yaw) rows.push(`RUD ${stick(hud.sticks.yaw)}`);
      if (hud.sticks.throttle) rows.push(`THR ${stick(hud.sticks.throttle)}`);
      if (rows.length > 0) {
        hudText(rows.join('  '), cx, height - 44, 'center');
      }
    }
  }

  function applyConfiguredCamera(cameraConfig) {
    const matrix = frameMatrices.get(cameraConfig.frame);
    if (!matrix) {
      return;
    }
    const mount = cameraScratch.mount.copy(cameraConfig.mount).applyMatrix4(matrix);
    const look = cameraScratch.look.copy(cameraConfig.look).transformDirection(matrix);
    const up = cameraScratch.up.copy(cameraConfig.up).transformDirection(matrix);
    camera.position.copy(mount);
    camera.up.copy(up);
    camera.lookAt(cameraScratch.target.copy(mount).add(look));
  }

  const api = {
    THREE,
    GLTFLoader: THREE.GLTFLoader,
    canvas,
    camera,
    cameraMode,
    cam,
    frames: frameMatrices,
    get: (name) => viewerSignals.get(name),
    getLocal: (name) => input?.locals?.get(name),
    setLocal(name, value) {
      if (!input?.locals?.has(name)) {
        return false;
      }
      input.locals.set(name, value);
      return true;
    },
    motors: {},
    pointer,
    renderer,
    scene,
    state,
  };

  return {
    api,
    canvas,
    ownerDocument,
    ownerWindow,
    cycleCamera() {
      if (cameraModes.length <= 1) {
        return cameraMode;
      }
      const next = (cameraModes.indexOf(cameraMode) + 1) % cameraModes.length;
      cameraMode = cameraModes[next];
      api.cameraMode = cameraMode;
      return cameraMode;
    },
    toggleHud() {
      if (!viewerConfig.hud) {
        return flightHudVisible;
      }
      flightHudVisible = !flightHudVisible;
      flightHud.style.display = flightHudVisible ? '' : 'none';
      return flightHudVisible;
    },
    render() {
      resize();
      const hudState = currentState();
      updateFrameMatrices(frames, frameMatrices, hudState);
      const activeCamera = configuredCameras.find((cameraConfig) => cameraConfig.name === cameraMode);
      if (activeCamera) {
        applyConfiguredCamera(activeCamera);
      } else {
        camera.up.set(0, 1, 0);
      }
      const hud = viewerConfig.hud;
      if (hud && flightHudVisible) {
        const matrix = frameMatrices.get(hud.frame);
        const attitude = matrix ? visualAttitudeFromMatrix(THREE, matrix) : { roll: 0, pitch: 0 };
        const speedSignals = Array.isArray(hud.speed) ? hud.speed : [];
        const speed = Math.sqrt(speedSignals.reduce((sum, name) => {
          const value = Number(hudState[name] ?? 0);
          return sum + value * value;
        }, 0));
        drawFlightHud(hud, hudState, attitude.roll, attitude.pitch, speed);
      } else {
        const rect = canvas.getBoundingClientRect();
        flightHudCtx.clearRect(0, 0, Math.max(1, rect.width), Math.max(1, rect.height));
      }
      renderer.render(scene, camera);
    },
  };
}

function keyDisplayName(key) {
  switch (key) {
    case 'ArrowUp': return '↑';
    case 'ArrowDown': return '↓';
    case 'ArrowLeft': return '←';
    case 'ArrowRight': return '→';
    case 'Space': return 'Space';
    default: return key.length === 1 ? key.toUpperCase() : key;
  }
}

function fluVector(THREE, value, fallback) {
  const vector = Array.isArray(value) && value.length === 3 && value.every(Number.isFinite)
    ? value
    : fallback;
  return new THREE.Vector3(-vector[1], vector[2], vector[0]);
}

function signalCoord(state, ref) {
  if (typeof ref === 'number') {
    return ref;
  }
  const value = Number(state[ref]);
  return Number.isFinite(value) ? value : 0;
}

function updateFrameMatrices(frames, frameMatrices, state) {
  for (const frame of frames) {
    const matrix = frameMatrices.get(frame.name);
    if (!matrix) {
      continue;
    }
    const position = frame.position ?? [];
    const px = signalCoord(state, position[0] ?? 0);
    const py = signalCoord(state, position[1] ?? 0);
    const pz = signalCoord(state, position[2] ?? 0);
    let q0 = 1;
    let q1 = 0;
    let q2 = 0;
    let q3 = 0;
    if (frame.quaternion) {
      q0 = signalCoord(state, frame.quaternion[0]);
      q1 = signalCoord(state, frame.quaternion[1]);
      q2 = signalCoord(state, frame.quaternion[2]);
      q3 = signalCoord(state, frame.quaternion[3]);
      if (q0 === 0 && q1 === 0 && q2 === 0 && q3 === 0) {
        q0 = 1;
      }
    } else if (frame.heading !== undefined && frame.heading !== null) {
      const psi = signalCoord(state, frame.heading);
      q0 = Math.cos(psi / 2);
      q3 = Math.sin(psi / 2);
    }
    const r11 = 1 - 2 * (q2 * q2 + q3 * q3);
    const r12 = 2 * (q1 * q2 - q0 * q3);
    const r13 = 2 * (q1 * q3 + q0 * q2);
    const r21 = 2 * (q1 * q2 + q0 * q3);
    const r22 = 1 - 2 * (q1 * q1 + q3 * q3);
    const r23 = 2 * (q2 * q3 - q0 * q1);
    const r31 = 2 * (q1 * q3 - q0 * q2);
    const r32 = 2 * (q2 * q3 + q0 * q1);
    const r33 = 1 - 2 * (q1 * q1 + q2 * q2);
    matrix.set(
      r22, -r23, -r21, -py,
      -r32, r33, r31, pz,
      -r12, r13, r11, px,
      0, 0, 0, 1
    );
  }
}

function visualAttitudeFromMatrix(THREE, matrix) {
  const forward = new THREE.Vector3(-1, 0, 0).transformDirection(matrix);
  const right = new THREE.Vector3(0, 0, 1).transformDirection(matrix);
  const up = new THREE.Vector3(0, 1, 0).transformDirection(matrix);
  return {
    roll: Math.atan2(right.y, up.y),
    pitch: Math.asin(clamp(forward.y, [-1, 1])),
  };
}

function compileSceneScript(scriptText, api) {
  const ctx = {};
  const fn = new Function('ctx', 'api', `${scriptText || ''}\nreturn ctx;`);
  return fn(ctx, api) || ctx;
}

function buildModelInputs(config, input, session, runtime) {
  const routes = config?.signals?.model_inputs || {};
  return sortedEntries(routes).map(([name, route]) => [
    name,
    routeValue(route, input.locals, session, runtime),
  ]);
}

function selectedModelSignalNames(config, locals) {
  const names = new Set();
  for (const [, route] of sortedEntries(config?.signals?.viewer)) {
    const source = typeof route === 'string' ? route : route?.from;
    const text = trimMaybeString(source);
    if (!text || text === 'model:time' || text.startsWith('local:') || text.startsWith('runtime:')) {
      continue;
    }
    const name = text.startsWith('model:') ? text.slice('model:'.length) : text;
    if (name && (text.startsWith('model:') || !locals.has(name))) {
      names.add(name);
    }
  }
  return Array.from(names).sort((a, b) => a.localeCompare(b));
}

export function createViewerSignalReader(config, input, session) {
  const signalNames = selectedModelSignalNames(config, input.locals);
  const routes = sortedEntries(config?.signals?.viewer);
  const values = new Map();
  let snapshotTime = 0;
  const snapshot = {
    time: () => snapshotTime,
    get: (name) => values.get(name),
  };
  return (frameNum, result = new Map()) => {
    snapshotTime = finiteNumber(session.time(), 0);
    values.clear();
    for (const name of signalNames) {
      values.set(name, session.get(name));
    }
    const runtime = input.runtimeFields(frameNum, snapshotTime);
    result.clear();
    for (const [name, route] of routes) {
      result.set(name, routeValue(route, input.locals, snapshot, runtime));
    }
    return result;
  };
}

export function scenarioUsesInputRuntime(config) {
  return Boolean(config?.input);
}

async function createInteractiveSession(options, onStatus) {
  const {
    wasm,
    source,
    modelName,
    config,
    sourceRootCacheUrl = '',
    sourceRoots = '{}',
    workspaceSources = '{}',
  } = options;
  if (!wasm || typeof wasm.WasmSimulationSession !== 'function') {
    throw new Error('Interactive simulation sessions are missing from this WASM package.');
  }
  await ensureParsedSourceRootCache(wasm, sourceRootCacheUrl);
  if (hasJsonObjectPayload(sourceRoots)) {
    if (typeof wasm.load_source_roots !== 'function') {
      throw new Error('Source-root loading is missing from this WASM package.');
    }
    onStatus('loading source roots');
    wasm.load_source_roots(sourceRoots);
  }
  if (hasJsonObjectPayload(workspaceSources)) {
    if (typeof wasm.sync_workspace_sources !== 'function') {
      throw new Error('Workspace-source syncing is missing from this WASM package.');
    }
    onStatus('syncing workspace sources');
    wasm.sync_workspace_sources(workspaceSources);
  }
  onStatus('compiling session');
  const simConfig = config?.sim || {};
  return wasm.WasmSimulationSession.withInteractiveOptions(
    source,
    modelName,
    finiteNumber(simConfig.dt, 0),
    trimMaybeString(simConfig.solver),
    finiteNumber(simConfig.atol, 0),
    finiteNumber(simConfig.rtol, 0),
  );
}

function createPointerState() {
  return {
    captured: false,
    buttons: 0,
    x: 0,
    y: 0,
    dx: 0,
    dy: 0,
    wheel: 0,
    pointerType: '',
  };
}

class InteractiveSimulationController {
  constructor(context) {
    Object.assign(this, context);
    this.simDt = Math.max(0.001, finiteNumber(this.config?.sim?.dt, 0.01));
    this.pacingMode = normalizePacingMode(this.config?.sim?.mode);
    this.frameNum = 0;
    this.running = false;
    this.raf = null;
    this.scheduledWithTimeout = false;
    this.lastTime = 0;
    this.accumulator = 0;
    this.speedRatio = 0;
    this.ui = { updatePacing: () => {}, updateRunState: () => {} };
    this.readViewerSignals = createViewerSignalReader(this.config, this.input, this.session);
    this.tick = this.tick.bind(this);
  }

  setUi(ui) {
    this.ui = ui;
  }

  currentPacingMode() {
    return this.pacingMode;
  }

  currentSpeedRatio() {
    return this.speedRatio;
  }

  refreshViewerSignals() {
    this.readViewerSignals(this.frameNum, this.viewerSignals);
  }

  stopAnimation() {
    this.running = false;
    if (this.raf !== null) {
      if (this.scheduledWithTimeout) {
        this.viewer.ownerWindow.clearTimeout(this.raf);
      } else {
        this.viewer.ownerWindow.cancelAnimationFrame(this.raf);
      }
      this.raf = null;
    }
  }

  scheduleNextTick() {
    if (this.pacingMode === 'as_fast_as_possible') {
      this.scheduledWithTimeout = true;
      this.raf = this.viewer.ownerWindow.setTimeout(() => this.tick(performance.now()), 0);
    } else {
      this.scheduledWithTimeout = false;
      this.raf = this.viewer.ownerWindow.requestAnimationFrame(this.tick);
    }
  }

  reportRuntimeError(error) {
    this.stopAnimation();
    const message = error?.message || error || 'Interactive simulation runtime error';
    this.ui.updateRunState('Failed', 'failed');
    this.onStatus(`failed: ${message}`);
    this.onError(error);
  }

  statusLine() {
    const inputMode = this.input.runtimeFields(this.frameNum, this.session.time()).input_mode;
    return `live t=${this.session.time().toFixed(2)} s · ${pacingModeLabel(this.pacingMode)} · ${speedRatioLabel(this.speedRatio)} · ${inputMode}`;
  }

  recordSpeed(simAdvanced, wallDt) {
    if (wallDt <= 0) {
      return;
    }
    const instant = Math.max(0, simAdvanced) / wallDt;
    this.speedRatio = this.speedRatio === 0 ? instant : (this.speedRatio * 0.82 + instant * 0.18);
  }

  renderFrame() {
    if (typeof this.scene.onFrame === 'function') {
      this.scene.onFrame(this.viewer.api);
    }
    this.pointer.dx = 0;
    this.pointer.dy = 0;
    this.pointer.wheel = 0;
    this.viewer.render();
  }

  resetSimulation(options = {}) {
    const {
      resetLocals = true,
      resetSession = true,
      render = true,
      statusText = 'reset',
    } = options;
    if (resetLocals) {
      this.input.resetLocals();
    }
    this.input.releaseKeys();
    if (resetSession) {
      this.session.reset();
    }
    this.frameNum = 0;
    this.accumulator = 0;
    this.lastTime = 0;
    this.speedRatio = 0;
    this.refreshViewerSignals();
    if (render) {
      this.renderFrame();
    }
    this.ui.updatePacing();
    this.ui.updateRunState('Reset', 'reset');
    this.onStatus(statusText);
  }

  togglePacingMode() {
    this.pacingMode = this.pacingMode === 'realtime' ? 'as_fast_as_possible' : 'realtime';
    this.accumulator = 0;
    this.lastTime = 0;
    this.speedRatio = 0;
    this.ui.updatePacing();
    this.onStatus(this.statusLine());
    return this.pacingMode;
  }

  applyRuntimeControlSignal(control, options = {}) {
    if (control?.action === 'reset') {
      const wasRunning = this.running;
      const resume = Boolean(options.resume) && !this.running;
      this.resetSimulation({
        resetLocals: control.resetLocals,
        resetSession: control.resetSession,
        render: Boolean(options.render),
        statusText: 'reset',
      });
      if (resume) {
        this.startAnimation();
      } else if (wasRunning) {
        this.ui.updateRunState('Running', 'running');
      }
      return true;
    }
    if (control?.action === 'quit') {
      this.stopAnimation();
      this.ui.updateRunState('Stopped', 'stopped');
      this.onStatus('stopped');
      return true;
    }
    return false;
  }

  routeFrame(dt) {
    this.input.update(dt);
    const control = takeRuntimeControlSignal(this.config, this.input);
    if (control?.action === 'reset') {
      this.applyRuntimeControlSignal(control, { render: false });
    } else if (control?.action === 'quit') {
      this.applyRuntimeControlSignal(control);
      return false;
    }
    const runtime = this.input.runtimeFields(this.frameNum, this.session.time());
    for (const [name, value] of buildModelInputs(this.config, this.input, this.session, runtime)) {
      this.session.set_input(name, finiteNumber(value, 0));
    }
    if (typeof this.session.step === 'function') {
      this.session.step(this.simDt);
    } else {
      this.session.advance_to(this.session.time() + this.simDt);
    }
    this.frameNum += 1;
    return true;
  }

  tick(now) {
    try {
      this.tickFrame(now);
    } catch (error) {
      this.reportRuntimeError(error);
    }
  }

  tickFrame(now) {
    if (!this.running) {
      return;
    }
    if (this.lastTime === 0) {
      this.lastTime = now;
    }
    const wallDt = Math.min(0.08, Math.max(0, (now - this.lastTime) / 1000));
    this.lastTime = now;
    let simAdvanced = 0;
    let steps = 0;
    if (this.pacingMode === 'as_fast_as_possible') {
      const started = performance.now();
      do {
        if (!this.routeFrame(this.simDt)) {
          return;
        }
        simAdvanced += this.simDt;
        steps += 1;
      } while (steps < 500 && performance.now() - started < 30);
    } else {
      this.accumulator += wallDt;
      while (this.accumulator >= this.simDt && steps < 8) {
        if (!this.routeFrame(this.simDt)) {
          return;
        }
        this.accumulator -= this.simDt;
        simAdvanced += this.simDt;
        steps += 1;
      }
    }
    this.refreshViewerSignals();
    this.recordSpeed(simAdvanced, wallDt);
    this.renderFrame();
    this.ui.updatePacing();
    this.onStatus(this.statusLine());
    this.scheduleNextTick();
  }

  startAnimation() {
    if (this.running) {
      return;
    }
    try {
      this.refreshViewerSignals();
      this.renderFrame();
      this.running = true;
      this.ui.updateRunState('Running', 'running');
      this.lastTime = 0;
      this.scheduleNextTick();
    } catch (error) {
      this.reportRuntimeError(error);
    }
  }

  start() {
    this.startAnimation();
  }

  stop() {
    this.stopAnimation();
    this.ui.updateRunState('Stopped', 'stopped');
    this.onStatus('stopped');
  }

  reset() {
    const wasRunning = this.running;
    this.resetSimulation({ resetLocals: true, resetSession: true });
    if (!wasRunning) {
      this.startAnimation();
    } else {
      this.ui.updateRunState('Running', 'running');
    }
  }
}

class InteractiveControls {
  constructor(context) {
    Object.assign(this, context);
    this.ownerDocument = this.viewer.ownerDocument;
    this.ownerWindow = this.viewer.ownerWindow;
    this.inputCaptureActive = false;
    this.lastCapturedKey = '';
    this.pointerLockExitReleasesCapture = true;
    this.eventCaptureOptions = { capture: true, passive: false };
    this.handlers = this.createHandlers();
    this.createElements();
    this.controller.setUi({
      updatePacing: () => this.updatePacingUi(),
      updateRunState: (label, state) => this.updateRunStateUi(label, state),
    });
    this.install();
  }

  createHandlers() {
    return {
      captureKeyDown: (event) => this.captureKeyDown(event),
      captureKeyUp: (event) => this.captureKeyUp(event),
      captureKeyPress: (event) => this.captureKeyPress(event),
      capturePointerDown: (event) => this.capturePointerDown(event),
      capturePointerEvent: (event) => this.capturePointerEvent(event),
      captureWheel: (event) => this.captureWheel(event),
      focus: () => this.focus(),
      handlePointerLockChange: () => this.handlePointerLockChange(),
      keyDown: (event) => this.keyDown(event),
      keyUp: (event) => this.input.keyUp(event),
      releaseCapture: () => this.setCaptureActive(false),
      resize: () => this.viewer.render(),
      updateFullscreenUi: () => this.updateFullscreenUi(),
    };
  }

  applyInputKeyDown(event) {
    if (event.repeat && this.input.hasKeyboardBinding(event)) {
      event.preventDefault();
      return true;
    }
    const handled = this.input.keyDown(event);
    if (handled) {
      this.controller.applyRuntimeControlSignal(takeRuntimeControlSignal(this.config, this.input), {
        render: true,
        resume: true,
      });
    }
    return handled;
  }

  keyDown(event) {
    if (!event.repeat && this.handleViewerKeyDown(event)) {
      event.preventDefault();
      return true;
    }
    return this.applyInputKeyDown(event);
  }

  updatePointerFromEvent(event) {
    const rect = this.viewer.canvas.getBoundingClientRect();
    this.pointer.buttons = event.buttons || 0;
    this.pointer.pointerType = trimMaybeString(event.pointerType);
    this.pointer.x = rect.width > 0 ? (event.clientX - rect.left) / rect.width : 0;
    this.pointer.y = rect.height > 0 ? (event.clientY - rect.top) / rect.height : 0;
    this.pointer.dx += finiteNumber(event.movementX, 0);
    this.pointer.dy += finiteNumber(event.movementY, 0);
  }

  requestPointerCapture() {
    if (this.ownerDocument.pointerLockElement === this.viewer.canvas
        || typeof this.viewer.canvas.requestPointerLock !== 'function') {
      return;
    }
    try {
      this.viewer.canvas.requestPointerLock();
    } catch {
      this.pointer.captured = false;
    }
  }

  releasePointerCapture() {
    if (this.ownerDocument.pointerLockElement !== this.viewer.canvas
        || typeof this.ownerDocument.exitPointerLock !== 'function') {
      return;
    }
    this.pointerLockExitReleasesCapture = false;
    this.ownerDocument.exitPointerLock();
    queueMicrotask(() => {
      this.pointerLockExitReleasesCapture = true;
    });
  }

  isFullscreenActive() {
    const activeElement = this.ownerDocument.fullscreenElement
      || this.ownerDocument.webkitFullscreenElement
      || null;
    return activeElement === this.container || this.container.contains(activeElement);
  }

  async setFullscreenActive(active) {
    try {
      if (active) {
        if (!this.isFullscreenActive()) {
          const request = this.container.requestFullscreen || this.container.webkitRequestFullscreen;
          if (typeof request === 'function') {
            await Promise.resolve(request.call(this.container));
          }
        }
      } else if (this.isFullscreenActive()) {
        const exit = this.ownerDocument.exitFullscreen || this.ownerDocument.webkitExitFullscreen;
        if (typeof exit === 'function') {
          await Promise.resolve(exit.call(this.ownerDocument));
        }
      }
    } finally {
      this.updateFullscreenUi();
      this.focus();
      this.viewer.render();
    }
  }

  toggleFullscreen() {
    this.setFullscreenActive(!this.isFullscreenActive()).catch((error) => {
      this.onStatus(`fullscreen unavailable: ${error?.message || error}`);
    });
  }

  setCaptureActive(active, options = {}) {
    this.inputCaptureActive = Boolean(active);
    if (!this.inputCaptureActive) {
      this.input.releaseKeys();
      this.pointer.buttons = 0;
      if (!options.keepPointerLock) {
        this.releasePointerCapture();
      }
    } else if (options.requestPointerLock) {
      this.requestPointerCapture();
    }
    this.updateCaptureUi(this.inputCaptureActive);
  }

  handleViewerKeyDown(event) {
    if (event.repeat) {
      return false;
    }
    const key = normalizedKeyboardKey(event);
    if (key === 'c') {
      this.lastCapturedKey = `camera ${this.viewer.cycleCamera()}`;
      this.updateCaptureUi(this.inputCaptureActive);
      return true;
    }
    if (key === 'h') {
      this.lastCapturedKey = `hud ${this.viewer.toggleHud() ? 'on' : 'off'}`;
      this.updateCaptureUi(this.inputCaptureActive);
      return true;
    }
    if (key === 't') {
      this.lastCapturedKey = `time ${pacingModeLabel(this.controller.togglePacingMode())}`;
      this.updateCaptureUi(this.inputCaptureActive);
      return true;
    }
    if (key === 'f') {
      this.lastCapturedKey = 'fullscreen';
      this.toggleFullscreen();
      this.updateCaptureUi(this.inputCaptureActive);
      return true;
    }
    return false;
  }

  captureKeyDown(event) {
    if (!this.inputCaptureActive) {
      return;
    }
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
    if (normalizedKeyboardKey(event) === 'Escape') {
      this.setCaptureActive(false);
      return;
    }
    this.lastCapturedKey = normalizedKeyboardKey(event);
    if (this.handleViewerKeyDown(event)) {
      return;
    }
    if (this.input.hasKeyboardBinding(event)) {
      this.updateCaptureUi(true);
      this.applyInputKeyDown(event);
    }
  }

  captureKeyUp(event) {
    if (!this.inputCaptureActive) {
      return;
    }
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
    if (this.input.hasKeyboardBinding(event)) {
      this.input.keyUp(event);
    }
  }

  captureKeyPress(event) {
    if (!this.inputCaptureActive) {
      return;
    }
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
  }

  capturePointerEvent(event) {
    if (!this.inputCaptureActive) {
      return;
    }
    if (event.target?.closest?.('.rumoca-interactive-controls')) {
      return;
    }
    this.updatePointerFromEvent(event);
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
  }

  captureWheel(event) {
    if (!this.inputCaptureActive) {
      return;
    }
    this.pointer.wheel += finiteNumber(event.deltaY, 0);
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
  }

  handlePointerLockChange() {
    this.pointer.captured = this.ownerDocument.pointerLockElement === this.viewer.canvas;
    if (!this.pointer.captured && this.inputCaptureActive && this.pointerLockExitReleasesCapture) {
      this.setCaptureActive(false, { keepPointerLock: true });
    } else {
      this.updateCaptureUi(this.inputCaptureActive);
    }
  }

  focus() {
    this.container.focus({ preventScroll: true });
  }

  capturePointerDown(event) {
    if (event.target?.closest?.('.rumoca-interactive-controls')) {
      return;
    }
    if (this.container.contains(event.target)) {
      this.focus();
      if (this.inputCaptureActive) {
        this.capturePointerEvent(event);
      }
    } else {
      this.setCaptureActive(false);
    }
  }

  createElement(tag, className, title = '') {
    const element = this.ownerDocument.createElement(tag);
    element.className = className;
    if (tag === 'button') {
      element.type = 'button';
      element.title = title;
    }
    return element;
  }

  createElements() {
    this.controls = this.createElement('div', 'rumoca-interactive-controls');
    this.captureToggle = this.createElement(
      'button',
      'rumoca-interactive-capture-toggle',
      'Capture input. Move to orbit, drag middle/right to pan, wheel to zoom, Escape to release.',
    );
    this.pacingToggle = this.createElement(
      'button',
      'rumoca-interactive-pacing-toggle',
      'Toggle simulation pacing. Shortcut: T.',
    );
    this.speedReadout = this.createElement('span', 'rumoca-interactive-key-echo rumoca-interactive-speed-readout');
    this.runStateReadout = this.createElement('span', 'rumoca-interactive-key-echo rumoca-interactive-run-state');
    this.fullscreenToggle = this.createElement(
      'button',
      'rumoca-interactive-fullscreen-toggle',
      'Toggle fullscreen. Shortcut: F.',
    );
    for (const element of [
      this.captureToggle,
      this.pacingToggle,
      this.runStateReadout,
      this.speedReadout,
      this.fullscreenToggle,
    ]) {
      this.controls.appendChild(element);
    }
  }

  updateCaptureUi(active) {
    this.captureToggle.textContent = active
      ? `Release: Esc${this.lastCapturedKey ? ` · ${keyDisplayName(this.lastCapturedKey)}` : ''}${this.pointer.captured ? ' · mouse' : ''}`
      : 'Capture';
    this.captureToggle.setAttribute('aria-pressed', active ? 'true' : 'false');
    this.controls.classList.toggle('is-capturing', active);
  }

  updatePacingUi() {
    const mode = this.controller.currentPacingMode();
    const label = pacingModeLabel(mode);
    this.pacingToggle.textContent = label === 'fast' ? 'Fast' : 'Realtime';
    this.pacingToggle.setAttribute('aria-pressed', mode === 'as_fast_as_possible' ? 'true' : 'false');
    this.pacingToggle.classList.toggle('is-fast', mode === 'as_fast_as_possible');
    this.speedReadout.textContent = `Speed ${speedRatioLabel(this.controller.currentSpeedRatio())}`;
  }

  updateFullscreenUi() {
    const active = this.isFullscreenActive();
    this.fullscreenToggle.textContent = active ? 'Exit Fullscreen' : 'Fullscreen';
    this.fullscreenToggle.setAttribute('aria-pressed', active ? 'true' : 'false');
    this.fullscreenToggle.classList.toggle('is-fullscreen', active);
  }

  updateRunStateUi(label, state = 'ready') {
    this.runStateReadout.textContent = `State ${label}`;
    this.runStateReadout.dataset.state = state;
  }

  installElementHandlers() {
    this.captureToggle.addEventListener('pointerdown', (event) => {
      event.preventDefault();
      event.stopPropagation();
      this.setCaptureActive(!this.inputCaptureActive, { requestPointerLock: true });
      this.focus();
    });
    this.pacingToggle.addEventListener('pointerdown', (event) => {
      event.preventDefault();
      event.stopPropagation();
      this.controller.togglePacingMode();
      this.focus();
    });
    this.fullscreenToggle.addEventListener('pointerdown', (event) => {
      event.preventDefault();
      event.stopPropagation();
      this.toggleFullscreen();
    });
  }

  install() {
    this.container.classList.add('rumoca-interactive-root');
    ensureInteractiveRuntimeStyles(this.ownerDocument);
    this.installElementHandlers();
    this.updateCaptureUi(false);
    this.updatePacingUi();
    this.updateFullscreenUi();
    this.updateRunStateUi('Ready', 'ready');
    this.container.appendChild(this.controls);
    this.container.tabIndex = 0;
    this.viewer.canvas.tabIndex = -1;
    this.registerListeners('addEventListener');
    this.focus();
  }

  registerListeners(method) {
    const h = this.handlers;
    this.container[method]('keydown', h.keyDown);
    this.container[method]('keyup', h.keyUp);
    this.container[method]('pointerdown', h.focus);
    this.viewer.canvas[method]('pointerdown', h.focus);
    this.ownerWindow[method]('keydown', h.captureKeyDown, true);
    this.ownerWindow[method]('keyup', h.captureKeyUp, true);
    this.ownerWindow[method]('keypress', h.captureKeyPress, true);
    this.ownerDocument[method]('keydown', h.captureKeyDown, true);
    this.ownerDocument[method]('keyup', h.captureKeyUp, true);
    this.ownerDocument[method]('keypress', h.captureKeyPress, true);
    this.ownerDocument[method]('pointerdown', h.capturePointerDown, true);
    this.ownerDocument[method]('pointermove', h.capturePointerEvent, true);
    this.ownerDocument[method]('pointerup', h.capturePointerEvent, true);
    this.ownerDocument[method]('pointercancel', h.capturePointerEvent, true);
    this.ownerDocument[method]('wheel', h.captureWheel, this.eventCaptureOptions);
    this.ownerDocument[method]('pointerlockchange', h.handlePointerLockChange);
    this.ownerDocument[method]('fullscreenchange', h.updateFullscreenUi);
    this.ownerDocument[method]('webkitfullscreenchange', h.updateFullscreenUi);
    this.ownerWindow[method]('blur', h.releaseCapture);
    this.ownerWindow[method]('resize', h.resize);
  }

  dispose() {
    this.registerListeners('removeEventListener');
    this.releasePointerCapture();
  }
}

export async function createInteractiveSimulation(options) {
  const {
    THREE,
    config = {},
    container,
    scriptText = '',
    assetBaseUrl = '',
    onStatus = () => {},
    onError = () => {},
  } = options || {};
  const input = createInputRuntime(config);
  const session = await createInteractiveSession(options || {}, onStatus);
  const viewerSignals = new Map();
  const pointer = createPointerState();
  const viewer = createViewerRuntime({
    THREE,
    container,
    viewerSignals,
    assetBaseUrl,
    pointer,
    config,
    input,
  });
  const scene = compileSceneScript(scriptText, viewer.api);
  onStatus('initializing scene');
  if (typeof scene.onInit === 'function') {
    await Promise.resolve(scene.onInit(viewer.api));
  }
  onStatus('ready');
  const controller = new InteractiveSimulationController({
    config,
    input,
    session,
    viewer,
    scene,
    pointer,
    viewerSignals,
    onStatus,
    onError,
  });
  const controls = new InteractiveControls({
    config,
    input,
    viewer,
    pointer,
    container,
    controller,
    onStatus,
  });

  return {
    start: () => controller.start(),
    stop: () => controller.stop(),
    dispose() {
      controller.stop();
      controls.dispose();
    },
    reset: () => controller.reset(),
  };
}
