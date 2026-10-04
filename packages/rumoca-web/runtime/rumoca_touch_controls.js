// On-screen thumb sticks and buttons for touch devices.
//
// The overlay drives a virtual gamepad that follows the browser Gamepad API
// "standard" layout: axes [LeftX, LeftY, RightX, RightY] with +X right and
// +Y down, and buttons indexed like a physical pad (South = 0, Start = 9).
// The input runtime samples it exactly like a connected controller, so the
// scenario's `[input.gamepad]` axes, integrators, deadbands and buttons apply
// unchanged and no touch-specific bindings are needed.

const TOUCH_BUTTON_INDEX = {
  South: 0,
  East: 1,
  West: 2,
  North: 3,
  LeftShoulder: 4,
  RightShoulder: 5,
  Select: 8,
  Start: 9,
};

const BUTTON_COUNT = 17;
const COARSE_POINTER_QUERY = '(pointer: coarse)';

function finite(value, fallback = 0) {
  const number = Number(value);
  return Number.isFinite(number) ? number : fallback;
}

// Map a thumb offset from the stick center (CSS pixels, +y down) to gamepad
// axes in [-1, 1]. The deflection is clamped to the stick's travel circle so
// diagonal input never exceeds unit magnitude, matching a physical stick gate.
export function stickAxes(dx, dy, radius) {
  const travel = finite(radius, 0);
  if (travel <= 0) {
    return { x: 0, y: 0 };
  }
  let x = finite(dx) / travel;
  let y = finite(dy) / travel;
  const magnitude = Math.hypot(x, y);
  if (magnitude > 1) {
    x /= magnitude;
    y /= magnitude;
  }
  return { x: x === 0 ? 0 : x, y: y === 0 ? 0 : y };
}

// Touch sticks are shaped like an RC transmitter so a small thumb travel does not
// command a large deflection: a rescaled deadzone removes jitter around center
// and a cubic expo softens the response near center while keeping full range.
export const TOUCH_DEADZONE = 0.12;
export const TOUCH_EXPO = 0.7;

const TOUCH_AXIS_SOURCES = {
  LeftStickX: 0,
  LeftStickY: 1,
  RightStickX: 2,
  RightStickY: 3,
};

// Deadzone with rescale (the output still reaches +-1) followed by an expo
// blend `expo * v^3 + (1 - expo) * v`. `expo = 0` keeps the response linear.
export function shapeStickAxis(value, deadzone = TOUCH_DEADZONE, expo = TOUCH_EXPO) {
  const v = finite(value);
  const zone = Math.min(Math.max(finite(deadzone), 0), 0.99);
  const blend = Math.min(Math.max(finite(expo), 0), 1);
  const magnitude = Math.abs(v);
  if (magnitude <= zone) {
    return 0;
  }
  const rescaled = Math.sign(v) * Math.min(1, (magnitude - zone) / (1 - zone));
  return blend * rescaled ** 3 + (1 - blend) * rescaled;
}

// Per-axis shaping derived from what the scenario binds each stick axis to.
// Integrator sources already apply their own `deadband` and a rate, so they get
// no extra deadzone. A throttle-like axis (`throttle`, gas) stays linear so
// stick position maps directly to the command; every other set-style axis gets
// the deadzone and expo. Unbound axes keep only the deadzone.
export function touchAxisShapes(config) {
  const shapes = Array.from({ length: 4 }, () => ({ deadzone: TOUCH_DEADZONE, expo: 0 }));
  const gamepad = config?.input?.gamepad || {};
  for (const [name, spec] of Object.entries(gamepad.axes || {})) {
    const index = TOUCH_AXIS_SOURCES[String(spec?.source || name).trim()];
    if (index !== undefined) {
      const linear = /throttle|gas/i.test(String(spec?.write || name));
      shapes[index] = { deadzone: TOUCH_DEADZONE, expo: linear ? 0 : TOUCH_EXPO };
    }
  }
  for (const [name, spec] of Object.entries(gamepad.integrators || {})) {
    const index = TOUCH_AXIS_SOURCES[String(spec?.source || name).trim()];
    if (index !== undefined) {
      shapes[index] = { deadzone: 0, expo: 0 };
    }
  }
  return shapes;
}

export function createVirtualGamepad() {
  const axes = [0, 0, 0, 0];
  const held = new Array(BUTTON_COUNT).fill(false);
  const tapped = new Array(BUTTON_COUNT).fill(false);
  let engaged = false;

  return {
    id: 'Rumoca touch controls',
    get engaged() {
      return engaged;
    },
    engage() {
      engaged = true;
    },
    disengage() {
      engaged = false;
    },
    setStick(side, x, y) {
      const offset = side === 'right' ? 2 : 0;
      axes[offset] = finite(x);
      axes[offset + 1] = finite(y);
    },
    pressButton(index) {
      if (index >= 0 && index < BUTTON_COUNT) {
        held[index] = true;
        tapped[index] = true;
      }
    },
    releaseButton(index) {
      if (index >= 0 && index < BUTTON_COUNT) {
        held[index] = false;
      }
    },
    releaseAll() {
      axes.fill(0);
      held.fill(false);
    },
    // A gamepad-shaped snapshot. A tap shorter than one simulation frame still
    // reports `pressed` once, so quick taps on Arm/Reset are never lost.
    sample() {
      const buttons = held.map((isHeld, index) => {
        const pressed = isHeld || tapped[index];
        return { pressed, value: pressed ? 1 : 0 };
      });
      tapped.fill(false);
      return { id: this.id, connected: true, axes: axes.slice(), buttons };
    },
  };
}

// Buttons shown next to the sticks: exactly the gamepad buttons the scenario
// binds, labelled with the binding name (e.g. `arm`, `reset`, `log`).
export function touchControlButtons(config) {
  const buttons = config?.input?.gamepad?.buttons;
  if (!buttons || typeof buttons !== 'object') {
    return [];
  }
  return Object.entries(buttons)
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([name, spec]) => {
      const source = typeof spec?.source === 'string' && spec.source.trim()
        ? spec.source.trim()
        : name;
      return {
        name,
        source,
        index: TOUCH_BUTTON_INDEX[source],
        label: name.replace(/[_-]+/g, ' ').replace(/^\w/, (c) => c.toUpperCase()),
      };
    })
    .filter((button) => button.index !== undefined);
}

export function scenarioUsesTouchControls(config) {
  const input = config?.input;
  if (!input || input.mode === 'keyboard') {
    return false;
  }
  const gamepad = input.gamepad || {};
  return ['axes', 'integrators', 'buttons'].some((section) => (
    gamepad[section] && Object.keys(gamepad[section]).length > 0
  ));
}

function ensureTouchControlStyles(ownerDocument) {
  if (ownerDocument.getElementById('rumoca-touch-controls-styles')) {
    return;
  }
  const style = ownerDocument.createElement('style');
  style.id = 'rumoca-touch-controls-styles';
  style.textContent = `
.rumoca-touch-controls {
  position: absolute;
  inset: 0;
  z-index: 6;
  pointer-events: none;
  -webkit-user-select: none;
  user-select: none;
  -webkit-touch-callout: none;
  -webkit-tap-highlight-color: transparent;
}
.rumoca-touch-controls[hidden] {
  display: none;
}
.rumoca-touch-stick {
  position: absolute;
  bottom: calc(14px + env(safe-area-inset-bottom, 0px));
  width: var(--rumoca-touch-stick-size, clamp(88px, 30%, 132px));
  aspect-ratio: 1;
  border: 1px solid rgba(255, 255, 255, 0.3);
  border-radius: 50%;
  background: rgba(10, 14, 18, 0.45);
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.3);
  pointer-events: auto;
  touch-action: none;
}
.rumoca-touch-stick-left {
  left: calc(14px + env(safe-area-inset-left, 0px));
}
.rumoca-touch-stick-right {
  right: calc(14px + env(safe-area-inset-right, 0px));
}
.rumoca-touch-stick.is-active {
  border-color: #37b7ff;
}
.rumoca-touch-stick-knob {
  position: absolute;
  left: 50%;
  top: 50%;
  width: 44%;
  height: 44%;
  margin: -22% 0 0 -22%;
  border: 1px solid rgba(255, 255, 255, 0.45);
  border-radius: 50%;
  background: rgba(10, 14, 18, 0.82);
  pointer-events: none;
}
.rumoca-touch-stick.is-active .rumoca-touch-stick-knob {
  border-color: #37b7ff;
  background: rgba(0, 103, 168, 0.88);
}
.rumoca-touch-buttons {
  position: absolute;
  left: 50%;
  bottom: calc(14px + env(safe-area-inset-bottom, 0px));
  transform: translateX(-50%);
  display: flex;
  flex-direction: column;
  gap: 8px;
  pointer-events: auto;
  touch-action: none;
}
.rumoca-touch-buttons button {
  min-width: 60px;
  min-height: 44px;
  padding: 4px 10px;
  border: 1px solid rgba(255, 255, 255, 0.3);
  border-radius: 6px;
  background: rgba(10, 14, 18, 0.82);
  color: #f4f7fb;
  font: inherit;
  font-size: 15px;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.3);
  touch-action: none;
}
.rumoca-touch-buttons button.is-pressed {
  border-color: #37b7ff;
  background: rgba(0, 103, 168, 0.88);
}
.rumoca-interactive-root.has-touch-controls .rumoca-interactive-controls {
  top: calc(12px + env(safe-area-inset-top, 0px));
  bottom: auto;
  left: calc(12px + env(safe-area-inset-left, 0px));
  right: calc(12px + env(safe-area-inset-right, 0px));
}
@media (orientation: landscape) and (max-height: 500px) {
  .rumoca-touch-controls {
    --rumoca-touch-stick-size: clamp(88px, 22%, 112px);
  }
  .rumoca-touch-stick-left {
    left: calc(20px + env(safe-area-inset-left, 0px));
  }
  .rumoca-touch-stick-right {
    right: calc(20px + env(safe-area-inset-right, 0px));
  }
}
`;
  (ownerDocument.head || ownerDocument.documentElement).appendChild(style);
}

function suppressDefault(event) {
  event.preventDefault();
  event.stopPropagation();
}

function bindStick(element, knob, side, gamepad, shapes) {
  const [shapeX, shapeY] = side === 'right' ? [shapes[2], shapes[3]] : [shapes[0], shapes[1]];
  let pointerId = null;
  let center = { x: 0, y: 0 };
  let radius = 1;

  const update = (event) => {
    const axes = stickAxes(event.clientX - center.x, event.clientY - center.y, radius);
    // The knob follows the thumb; the gamepad sees the shaped command.
    gamepad.setStick(
      side,
      shapeStickAxis(axes.x, shapeX.deadzone, shapeX.expo),
      shapeStickAxis(axes.y, shapeY.deadzone, shapeY.expo),
    );
    knob.style.transform = `translate(${axes.x * radius}px, ${axes.y * radius}px)`;
  };
  const release = () => {
    pointerId = null;
    gamepad.setStick(side, 0, 0);
    knob.style.transform = '';
    element.classList.remove('is-active');
  };
  const handlers = {
    pointerdown(event) {
      suppressDefault(event);
      if (pointerId !== null) {
        return;
      }
      pointerId = event.pointerId;
      const rect = element.getBoundingClientRect();
      center = { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
      // Knob travel: the knob edge stops at the base edge.
      radius = Math.max(1, rect.width * 0.28);
      element.setPointerCapture?.(pointerId);
      element.classList.add('is-active');
      gamepad.engage();
      update(event);
    },
    pointermove(event) {
      if (event.pointerId === pointerId) {
        suppressDefault(event);
        update(event);
      }
    },
    pointerup(event) {
      if (event.pointerId === pointerId) {
        suppressDefault(event);
        release();
      }
    },
  };
  handlers.pointercancel = handlers.pointerup;
  handlers.lostpointercapture = handlers.pointerup;
  return { handlers, release };
}

function bindButton(element, index, gamepad) {
  const pointers = new Set();
  const release = () => {
    pointers.clear();
    gamepad.releaseButton(index);
    element.classList.remove('is-pressed');
  };
  const handlers = {
    pointerdown(event) {
      suppressDefault(event);
      pointers.add(event.pointerId);
      element.setPointerCapture?.(event.pointerId);
      element.classList.add('is-pressed');
      gamepad.engage();
      gamepad.pressButton(index);
    },
    pointerup(event) {
      if (pointers.delete(event.pointerId)) {
        suppressDefault(event);
      }
      if (pointers.size === 0) {
        gamepad.releaseButton(index);
        element.classList.remove('is-pressed');
      }
    },
  };
  handlers.pointercancel = handlers.pointerup;
  handlers.lostpointercapture = handlers.pointerup;
  return { handlers, release };
}

function listen(element, handlers, method) {
  for (const [type, handler] of Object.entries(handlers)) {
    element[method](type, handler, { passive: false });
  }
}

function createStick(ownerDocument, side, label) {
  const stick = ownerDocument.createElement('div');
  stick.className = `rumoca-touch-stick rumoca-touch-stick-${side}`;
  stick.setAttribute('role', 'application');
  stick.setAttribute('aria-label', label);
  const knob = ownerDocument.createElement('div');
  knob.className = 'rumoca-touch-stick-knob';
  stick.appendChild(knob);
  return { stick, knob };
}

// Mount the overlay into an interactive viewer container. It is visible only
// while the primary pointer is coarse (phones, tablets) and tracks changes to
// that media query, so desktop browsers never show it.
export function mountTouchControls({ container, config, gamepad }) {
  const ownerDocument = container.ownerDocument || document;
  const ownerWindow = ownerDocument.defaultView || globalThis;
  ensureTouchControlStyles(ownerDocument);

  const overlay = ownerDocument.createElement('div');
  overlay.className = 'rumoca-touch-controls';
  const bindings = [];
  const shapes = touchAxisShapes(config);

  for (const side of ['left', 'right']) {
    const { stick, knob } = createStick(ownerDocument, side, `${side} thumb stick`);
    const binding = bindStick(stick, knob, side, gamepad, shapes);
    listen(stick, binding.handlers, 'addEventListener');
    bindings.push({ element: stick, ...binding });
    overlay.appendChild(stick);
  }

  const buttonSpecs = touchControlButtons(config);
  if (buttonSpecs.length > 0) {
    const column = ownerDocument.createElement('div');
    column.className = 'rumoca-touch-buttons';
    for (const spec of buttonSpecs) {
      const button = ownerDocument.createElement('button');
      button.type = 'button';
      button.textContent = spec.label;
      button.dataset.source = spec.source;
      const binding = bindButton(button, spec.index, gamepad);
      listen(button, binding.handlers, 'addEventListener');
      bindings.push({ element: button, ...binding });
      column.appendChild(button);
    }
    listen(column, { contextmenu: suppressDefault }, 'addEventListener');
    overlay.appendChild(column);
  }

  const query = typeof ownerWindow.matchMedia === 'function'
    ? ownerWindow.matchMedia(COARSE_POINTER_QUERY)
    : null;
  const applyVisibility = () => {
    const visible = Boolean(query?.matches);
    overlay.hidden = !visible;
    container.classList.toggle('has-touch-controls', visible);
    if (!visible) {
      bindings.forEach((binding) => binding.release());
      gamepad.releaseAll();
      gamepad.disengage();
    }
  };
  applyVisibility();
  query?.addEventListener?.('change', applyVisibility);
  container.appendChild(overlay);

  return {
    element: overlay,
    get visible() {
      return !overlay.hidden;
    },
    release() {
      bindings.forEach((binding) => binding.release());
      gamepad.releaseAll();
    },
    dispose() {
      query?.removeEventListener?.('change', applyVisibility);
      bindings.forEach((binding) => listen(binding.element, binding.handlers, 'removeEventListener'));
      gamepad.releaseAll();
      gamepad.disengage();
      container.classList.remove('has-touch-controls');
      overlay.remove();
    },
  };
}
