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

function createViewerRuntime({ THREE, container, viewerSignals, assetBaseUrl, pointer, config }) {
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

function buildViewerSignals(config, input, session, runtime) {
  const result = new Map();
  for (const [name, route] of sortedEntries(config?.signals?.viewer)) {
    result.set(name, routeValue(route, input.locals, session, runtime));
  }
  return result;
}

export function scenarioUsesInputRuntime(config) {
  return Boolean(config?.input);
}

export async function createInteractiveSimulation(options) {
  const {
    wasm,
    THREE,
    source,
    modelName,
    config,
    sourceRootCacheUrl = '',
    sourceRoots = '{}',
    workspaceSources = '{}',
    container,
    scriptText = '',
    assetBaseUrl = '',
    onStatus = () => {},
    onError = () => {},
  } = options || {};
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
  const input = createInputRuntime(config || {});
  onStatus('compiling session');
  const simConfig = config?.sim || {};
  const session = wasm.WasmSimulationSession.withOptions(
    source,
    modelName,
    finiteNumber(simConfig.t_end, 0),
    finiteNumber(simConfig.dt, 0),
    trimMaybeString(simConfig.solver),
    finiteNumber(simConfig.atol, 0),
    finiteNumber(simConfig.rtol, 0),
  );
  const viewerSignals = new Map();
  const pointer = {
    captured: false,
    buttons: 0,
    x: 0,
    y: 0,
    dx: 0,
    dy: 0,
    wheel: 0,
    pointerType: '',
  };
  const viewer = createViewerRuntime({ THREE, container, viewerSignals, assetBaseUrl, pointer, config });
  const scene = compileSceneScript(scriptText, viewer.api);
  onStatus('initializing scene');
  if (typeof scene.onInit === 'function') {
    await Promise.resolve(scene.onInit(viewer.api));
  }
  onStatus('ready');

  const simDt = Math.max(0.001, finiteNumber(config?.sim?.dt, 0.01));
  let pacingMode = normalizePacingMode(config?.sim?.mode);
  let frameNum = 0;
  let running = false;
  let raf = null;
  let lastTime = 0;
  let accumulator = 0;
  let speedRatio = 0;
  let updateCaptureUi = () => {};
  let updatePacingUi = () => {};
  let updateFullscreenUi = () => {};

  function refreshViewerSignals() {
    viewerSignals.clear();
    const nextSignals = buildViewerSignals(
      config,
      input,
      session,
      input.runtimeFields(frameNum, session.time()),
    );
    for (const [name, value] of nextSignals) {
      viewerSignals.set(name, value);
    }
  }

  function stopAnimation() {
    running = false;
    if (raf !== null) {
      ownerWindow.cancelAnimationFrame(raf);
      raf = null;
    }
  }

  function reportRuntimeError(error) {
    stopAnimation();
    const message = error?.message || error || 'Interactive simulation runtime error';
    onStatus(`failed: ${message}`);
    onError(error);
  }

  function statusLine() {
    const inputMode = input.runtimeFields(frameNum, session.time()).input_mode;
    return `live t=${session.time().toFixed(2)} s · ${pacingModeLabel(pacingMode)} · ${speedRatioLabel(speedRatio)} · ${inputMode}`;
  }

  function simulationFinished() {
    const end = session.end_time();
    const tolerance = 1e-12 * (1 + Math.abs(end));
    return session.time() >= end - tolerance;
  }

  function recordSpeed(simAdvanced, wallDt) {
    if (wallDt <= 0) {
      return;
    }
    const instant = Math.max(0, simAdvanced) / wallDt;
    speedRatio = speedRatio === 0 ? instant : (speedRatio * 0.82 + instant * 0.18);
  }

  function resetSimulation(options = {}) {
    const {
      resetLocals = true,
      resetSession = true,
      render = true,
      statusText = 'reset',
    } = options;
    if (resetLocals) {
      input.resetLocals();
    }
    input.releaseKeys();
    if (resetSession) {
      session.reset();
    }
    frameNum = 0;
    accumulator = 0;
    lastTime = 0;
    speedRatio = 0;
    refreshViewerSignals();
    if (render) {
      viewer.render();
    }
    updatePacingUi();
    onStatus(statusText);
  }

  function togglePacingMode() {
    pacingMode = pacingMode === 'realtime' ? 'as_fast_as_possible' : 'realtime';
    accumulator = 0;
    lastTime = 0;
    speedRatio = 0;
    updatePacingUi();
    onStatus(statusLine());
    return pacingMode;
  }

  function routeFrame(dt) {
    input.update(dt);
    if (config?.reset?.on_signal && input.takeSignal(config.reset.on_signal)) {
      resetSimulation({
        resetLocals: Boolean(config.reset.reset_locals),
        resetSession: Boolean(config.reset.reset_session),
        render: false,
        statusText: 'reset',
      });
    }
    if (input.takeSignal(trimMaybeString(config?.quit?.on_signal) || 'quit')) {
      stopAnimation();
      onStatus('stopped');
      return false;
    }
    const runtime = input.runtimeFields(frameNum, session.time());
    for (const [name, value] of buildModelInputs(config, input, session, runtime)) {
      session.set_input(name, finiteNumber(value, 0));
    }
    if (typeof session.step === 'function') {
      session.step(simDt);
    } else {
      session.advance_to(Math.min(session.time() + simDt, session.end_time()));
    }
    refreshViewerSignals();
    frameNum += 1;
    if (simulationFinished()) {
      stopAnimation();
      onStatus('finished');
      return false;
    }
    return true;
  }

  function tick(now) {
    try {
      tickFrame(now);
    } catch (error) {
      reportRuntimeError(error);
    }
  }

  function tickFrame(now) {
    if (!running) {
      return;
    }
    if (lastTime === 0) {
      lastTime = now;
    }
    const wallDt = Math.min(0.08, Math.max(0, (now - lastTime) / 1000));
    lastTime = now;
    let simAdvanced = 0;
    let steps = 0;
    if (pacingMode === 'as_fast_as_possible') {
      const started = performance.now();
      do {
        if (!routeFrame(simDt)) {
          return;
        }
        simAdvanced += simDt;
        steps += 1;
      } while (steps < 250 && performance.now() - started < 12);
    } else {
      accumulator += wallDt;
      while (accumulator >= simDt && steps < 8) {
        if (!routeFrame(simDt)) {
          return;
        }
        accumulator -= simDt;
        simAdvanced += simDt;
        steps += 1;
      }
    }
    recordSpeed(simAdvanced, wallDt);
    if (typeof scene.onFrame === 'function') {
      scene.onFrame(viewer.api);
    }
    pointer.dx = 0;
    pointer.dy = 0;
    pointer.wheel = 0;
    viewer.render();
    updatePacingUi();
    onStatus(statusLine());
    raf = ownerWindow.requestAnimationFrame(tick);
  }

  let inputCaptureActive = false;
  let lastCapturedKey = '';
  let pointerLockExitReleasesCapture = true;
  const eventCaptureOptions = { capture: true, passive: false };
  const ownerDocument = viewer.ownerDocument;
  const ownerWindow = viewer.ownerWindow;
  container.classList.add('rumoca-interactive-root');
  ensureInteractiveRuntimeStyles(ownerDocument);
  const keyDown = (event) => {
    if (!event.repeat && handleViewerKeyDown(event)) {
      event.preventDefault();
      return true;
    }
    return input.keyDown(event);
  };
  const keyUp = (event) => input.keyUp(event);
  const updatePointerFromEvent = (event) => {
    const rect = viewer.canvas.getBoundingClientRect();
    pointer.buttons = event.buttons || 0;
    pointer.pointerType = trimMaybeString(event.pointerType);
    pointer.x = rect.width > 0 ? (event.clientX - rect.left) / rect.width : 0;
    pointer.y = rect.height > 0 ? (event.clientY - rect.top) / rect.height : 0;
    pointer.dx += finiteNumber(event.movementX, 0);
    pointer.dy += finiteNumber(event.movementY, 0);
  };
  const requestPointerCapture = () => {
    if (ownerDocument.pointerLockElement === viewer.canvas || typeof viewer.canvas.requestPointerLock !== 'function') {
      return;
    }
    try {
      viewer.canvas.requestPointerLock();
    } catch {
      pointer.captured = false;
    }
  };
  const releasePointerCapture = () => {
    if (ownerDocument.pointerLockElement !== viewer.canvas || typeof ownerDocument.exitPointerLock !== 'function') {
      return;
    }
    pointerLockExitReleasesCapture = false;
    ownerDocument.exitPointerLock();
    queueMicrotask(() => {
      pointerLockExitReleasesCapture = true;
    });
  };
  const fullscreenElement = () => ownerDocument.fullscreenElement || ownerDocument.webkitFullscreenElement || null;
  const isFullscreenActive = () => {
    const activeElement = fullscreenElement();
    return activeElement === container || container.contains(activeElement);
  };
  const setFullscreenActive = async (active) => {
    try {
      if (active) {
        if (!isFullscreenActive()) {
          const request = container.requestFullscreen || container.webkitRequestFullscreen;
          if (typeof request === 'function') {
            await Promise.resolve(request.call(container));
          }
        }
      } else if (isFullscreenActive()) {
        const exit = ownerDocument.exitFullscreen || ownerDocument.webkitExitFullscreen;
        if (typeof exit === 'function') {
          await Promise.resolve(exit.call(ownerDocument));
        }
      }
    } finally {
      updateFullscreenUi();
      focus();
      viewer.render();
    }
  };
  const toggleFullscreen = () => {
    setFullscreenActive(!isFullscreenActive()).catch((error) => {
      onStatus(`fullscreen unavailable: ${error?.message || error}`);
    });
  };
  const setCaptureActive = (active, options = {}) => {
    inputCaptureActive = Boolean(active);
    if (!inputCaptureActive) {
      input.releaseKeys();
      pointer.buttons = 0;
      if (!options.keepPointerLock) {
        releasePointerCapture();
      }
    } else if (options.requestPointerLock) {
      requestPointerCapture();
    }
    updateCaptureUi(inputCaptureActive);
  };
  const handleViewerKeyDown = (event) => {
    if (event.repeat) {
      return false;
    }
    const key = normalizedKeyboardKey(event);
    if (key === 'c') {
      lastCapturedKey = `camera ${viewer.cycleCamera()}`;
      updateCaptureUi(inputCaptureActive);
      return true;
    }
    if (key === 'h') {
      lastCapturedKey = `hud ${viewer.toggleHud() ? 'on' : 'off'}`;
      updateCaptureUi(inputCaptureActive);
      return true;
    }
    if (key === 't') {
      lastCapturedKey = `time ${pacingModeLabel(togglePacingMode())}`;
      updateCaptureUi(inputCaptureActive);
      return true;
    }
    if (key === 'f') {
      lastCapturedKey = 'fullscreen';
      toggleFullscreen();
      updateCaptureUi(inputCaptureActive);
      return true;
    }
    return false;
  };
  const captureKeyDown = (event) => {
    if (!inputCaptureActive) {
      return;
    }
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
    if (normalizedKeyboardKey(event) === 'Escape') {
      setCaptureActive(false);
      return;
    }
    lastCapturedKey = normalizedKeyboardKey(event);
    if (handleViewerKeyDown(event)) {
      return;
    }
    if (input.hasKeyboardBinding(event)) {
      updateCaptureUi(true);
      input.keyDown(event);
    }
  };
  const captureKeyUp = (event) => {
    if (!inputCaptureActive) {
      return;
    }
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
    if (input.hasKeyboardBinding(event)) {
      input.keyUp(event);
    }
  };
  const captureKeyPress = (event) => {
    if (!inputCaptureActive) {
      return;
    }
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
  };
  const capturePointerEvent = (event) => {
    if (!inputCaptureActive) {
      return;
    }
    if (event.target?.closest?.('.rumoca-interactive-controls')) {
      return;
    }
    updatePointerFromEvent(event);
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
  };
  const captureWheel = (event) => {
    if (!inputCaptureActive) {
      return;
    }
    pointer.wheel += finiteNumber(event.deltaY, 0);
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
  };
  const handlePointerLockChange = () => {
    pointer.captured = ownerDocument.pointerLockElement === viewer.canvas;
    if (!pointer.captured && inputCaptureActive && pointerLockExitReleasesCapture) {
      setCaptureActive(false, { keepPointerLock: true });
    } else {
      updateCaptureUi(inputCaptureActive);
    }
  };
  const releaseCapture = () => {
    setCaptureActive(false);
  };
  const focus = () => {
    container.focus({ preventScroll: true });
  };
  const capturePointerDown = (event) => {
    if (event.target?.closest?.('.rumoca-interactive-controls')) {
      return;
    }
    if (container.contains(event.target)) {
      focus();
      if (inputCaptureActive) {
        capturePointerEvent(event);
      }
    } else {
      releaseCapture();
    }
  };
  const controls = ownerDocument.createElement('div');
  controls.className = 'rumoca-interactive-controls';
  const captureToggle = ownerDocument.createElement('button');
  captureToggle.type = 'button';
  captureToggle.className = 'rumoca-interactive-capture-toggle';
  captureToggle.title = 'Capture keyboard and mouse input. Press Escape to release capture.';
  const pacingToggle = ownerDocument.createElement('button');
  pacingToggle.type = 'button';
  pacingToggle.className = 'rumoca-interactive-pacing-toggle';
  pacingToggle.title = 'Toggle simulation pacing. Shortcut: T.';
  const speedReadout = ownerDocument.createElement('span');
  speedReadout.className = 'rumoca-interactive-key-echo rumoca-interactive-speed-readout';
  const fullscreenToggle = ownerDocument.createElement('button');
  fullscreenToggle.type = 'button';
  fullscreenToggle.className = 'rumoca-interactive-fullscreen-toggle';
  fullscreenToggle.title = 'Toggle fullscreen. Shortcut: F.';
  updateCaptureUi = (active) => {
    captureToggle.textContent = active
      ? `Release: Esc${lastCapturedKey ? ` · ${keyDisplayName(lastCapturedKey)}` : ''}${pointer.captured ? ' · mouse' : ''}`
      : 'Capture';
    captureToggle.setAttribute('aria-pressed', active ? 'true' : 'false');
    controls.classList.toggle('is-capturing', active);
  };
  updatePacingUi = () => {
    const label = pacingModeLabel(pacingMode);
    pacingToggle.textContent = label === 'fast' ? 'Fast' : 'Realtime';
    pacingToggle.setAttribute('aria-pressed', pacingMode === 'as_fast_as_possible' ? 'true' : 'false');
    pacingToggle.classList.toggle('is-fast', pacingMode === 'as_fast_as_possible');
    speedReadout.textContent = `Speed ${speedRatioLabel(speedRatio)}`;
  };
  updateFullscreenUi = () => {
    const active = isFullscreenActive();
    fullscreenToggle.textContent = active ? 'Exit Fullscreen' : 'Fullscreen';
    fullscreenToggle.setAttribute('aria-pressed', active ? 'true' : 'false');
    fullscreenToggle.classList.toggle('is-fullscreen', active);
  };
  captureToggle.addEventListener('pointerdown', (event) => {
    event.preventDefault();
    event.stopPropagation();
    setCaptureActive(!inputCaptureActive, { requestPointerLock: true });
    container.focus({ preventScroll: true });
  });
  pacingToggle.addEventListener('pointerdown', (event) => {
    event.preventDefault();
    event.stopPropagation();
    togglePacingMode();
    container.focus({ preventScroll: true });
  });
  fullscreenToggle.addEventListener('pointerdown', (event) => {
    event.preventDefault();
    event.stopPropagation();
    toggleFullscreen();
  });
  controls.appendChild(captureToggle);
  controls.appendChild(pacingToggle);
  controls.appendChild(speedReadout);
  controls.appendChild(fullscreenToggle);
  updateCaptureUi(false);
  updatePacingUi();
  updateFullscreenUi();
  container.appendChild(controls);
  container.tabIndex = 0;
  viewer.canvas.tabIndex = -1;
  container.addEventListener('keydown', keyDown);
  container.addEventListener('keyup', keyUp);
  container.addEventListener('pointerdown', focus);
  viewer.canvas.addEventListener('pointerdown', focus);
  ownerWindow.addEventListener('keydown', captureKeyDown, true);
  ownerWindow.addEventListener('keyup', captureKeyUp, true);
  ownerWindow.addEventListener('keypress', captureKeyPress, true);
  ownerDocument.addEventListener('keydown', captureKeyDown, true);
  ownerDocument.addEventListener('keyup', captureKeyUp, true);
  ownerDocument.addEventListener('keypress', captureKeyPress, true);
  ownerDocument.addEventListener('pointerdown', capturePointerDown, true);
  ownerDocument.addEventListener('pointermove', capturePointerEvent, true);
  ownerDocument.addEventListener('pointerup', capturePointerEvent, true);
  ownerDocument.addEventListener('pointercancel', capturePointerEvent, true);
  ownerDocument.addEventListener('wheel', captureWheel, eventCaptureOptions);
  ownerDocument.addEventListener('pointerlockchange', handlePointerLockChange);
  ownerDocument.addEventListener('fullscreenchange', updateFullscreenUi);
  ownerDocument.addEventListener('webkitfullscreenchange', updateFullscreenUi);
  ownerWindow.addEventListener('blur', releaseCapture);
  container.focus({ preventScroll: true });
  const resize = () => viewer.render();
  ownerWindow.addEventListener('resize', resize);

  return {
    start() {
      if (running) {
        return;
      }
      try {
        refreshViewerSignals();
        if (typeof scene.onFrame === 'function') {
          scene.onFrame(viewer.api);
        }
        pointer.dx = 0;
        pointer.dy = 0;
        pointer.wheel = 0;
        viewer.render();
        running = true;
        lastTime = 0;
        raf = ownerWindow.requestAnimationFrame(tick);
      } catch (error) {
        reportRuntimeError(error);
      }
    },
    stop() {
      stopAnimation();
    },
    dispose() {
      this.stop();
      container.removeEventListener('keydown', keyDown);
      container.removeEventListener('keyup', keyUp);
      container.removeEventListener('pointerdown', focus);
      viewer.canvas.removeEventListener('pointerdown', focus);
      ownerWindow.removeEventListener('keydown', captureKeyDown, true);
      ownerWindow.removeEventListener('keyup', captureKeyUp, true);
      ownerWindow.removeEventListener('keypress', captureKeyPress, true);
      ownerDocument.removeEventListener('keydown', captureKeyDown, true);
      ownerDocument.removeEventListener('keyup', captureKeyUp, true);
      ownerDocument.removeEventListener('keypress', captureKeyPress, true);
      ownerDocument.removeEventListener('pointerdown', capturePointerDown, true);
      ownerDocument.removeEventListener('pointermove', capturePointerEvent, true);
      ownerDocument.removeEventListener('pointerup', capturePointerEvent, true);
      ownerDocument.removeEventListener('pointercancel', capturePointerEvent, true);
      ownerDocument.removeEventListener('wheel', captureWheel, eventCaptureOptions);
      ownerDocument.removeEventListener('pointerlockchange', handlePointerLockChange);
      ownerDocument.removeEventListener('fullscreenchange', updateFullscreenUi);
      ownerDocument.removeEventListener('webkitfullscreenchange', updateFullscreenUi);
      ownerWindow.removeEventListener('blur', releaseCapture);
      ownerWindow.removeEventListener('resize', resize);
      releasePointerCapture();
    },
    reset() {
      resetSimulation({ resetLocals: true, resetSession: true });
    },
  };
}
