# Arrays and Discretized PDEs

Modelica has no built-in partial differential equations, but array variables
plus `for`-equations make *method of lines* discretizations natural: slice
the spatial domain into cells, give each cell a state, and let the compiler
unroll the equations.

## Cooking a Turkey

A turkey in the oven is (approximately!) a sphere heated from the outside —
the 1-D radial heat equation. We split the sphere into `N` concentric
shells, write an energy balance for each shell, and drive the outer surface
with oven convection and radiation.

Press **▶ Simulate**, then press play on the cross-section: colors show the
temperature field conducting inward over four hours of cooking. Drag the
slider to scrub through time.

```modelica,interactive,viz-radial
model Turkey "Roasting a turkey: 1-D spherical heat equation, method of lines"
  parameter Integer N = 10 "Number of radial shells";
  final parameter Integer Np1 = N + 1;
  parameter Real M = 5.0 "Turkey mass [kg]";
  parameter Real rho = 1050.0 "Density [kg/m3]";
  parameter Real cp = 3500.0 "Specific heat [J/(kg.K)]";
  parameter Real kc = 0.5 "Thermal conductivity [W/(m.K)]";
  parameter Real T_oven = 450.0 "Oven temperature [K] (~177 degC)";
  parameter Real h = 15.0 "Convective film coefficient [W/(m2.K)]";
  parameter Real epsilon = 0.85 "Surface emissivity";
  parameter Real sigma = 5.67e-8 "Stefan-Boltzmann constant";
  parameter Real pi = 3.14159265359;
  parameter Real R = (3.0 * M / (4.0 * pi * rho)) ^ (1.0 / 3.0) "Radius [m]";
  parameter Real dr = R / N "Shell thickness [m]";
  Real T[N](each start = 277.0) "Shell temperatures [K] (fridge-cold start)";
  Real Q_cond[Np1] "Conductive heat flow across shell interfaces [W]";
  Real Q_surf "Heat into the surface from the oven [W]";
  Real r[Np1] "Interface radii [m]";
  Real A_interface[Np1] "Interface areas [m2]";
  Real V_shell[N] "Shell volumes [m3]";
  Real m_shell[N] "Shell masses [kg]";
equation
  for i in 1:Np1 loop
    r[i] = (i - 1) * dr;
    A_interface[i] = 4.0 * pi * r[i] ^ 2;
  end for;
  for i in 1:N loop
    V_shell[i] = (4.0 / 3.0) * pi * (r[i + 1] ^ 3 - r[i] ^ 3);
    m_shell[i] = rho * V_shell[i];
  end for;
  Q_cond[1] = 0.0 "No flux through the center";
  for i in 2:N loop
    Q_cond[i] = kc * A_interface[i] * (T[i - 1] - T[i]) / dr;
  end for;
  Q_cond[Np1] = 0.0;
  Q_surf = h * A_interface[Np1] * (T_oven - T[N])
    + epsilon * sigma * A_interface[Np1] * (T_oven ^ 4 - T[N] ^ 4);
  for i in 1:N - 1 loop
    m_shell[i] * cp * der(T[i]) = Q_cond[i] - Q_cond[i + 1];
  end for;
  m_shell[N] * cp * der(T[N]) = Q_cond[N] + Q_surf;
  annotation(experiment(StopTime = 14400.0, Interval = 60.0));
end Turkey;
```

The cross-section animation above is itself an editable JavaScript block —
expand it, change the colors or geometry, and re-run **▶ Simulate** to see
your version. It receives the simulation results and a small helper API
(`api.arrayField`, `api.makeCanvas`, `api.addAnimation`,
`api.addColorbar`, `api.heatColor`, …) from the book's live harness.

```js,rumoca-viz
// Draw the turkey cross-section: concentric shells colored by temperature.
const field = api.arrayField();            // T[1..N], sorted by index
const { vMin, vMax } = api.valueRange(field.members);
const T_done = 347;                        // 74 degC: poultry-safe core temp

const size = 300;
const { ctx2d } = api.makeCanvas(size, size);
const n = field.members.length;

api.addAnimation(times, (frame) => {
  ctx2d.clearRect(0, 0, size, size);
  const maxR = size / 2 - 8;
  // Outermost shell first so inner shells paint on top.
  for (let i = n - 1; i >= 0; i--) {
    const T = field.members[i].values[frame];
    ctx2d.beginPath();
    ctx2d.arc(size / 2, size / 2, maxR * ((i + 1) / n), 0, 2 * Math.PI);
    ctx2d.fillStyle = api.heatColor((T - vMin) / (vMax - vMin));
    ctx2d.fill();
  }
  ctx2d.beginPath();
  ctx2d.arc(size / 2, size / 2, maxR, 0, 2 * Math.PI);
  ctx2d.strokeStyle = '#777';
  ctx2d.lineWidth = 2;
  ctx2d.stroke();

  const T_core = field.members[0].values[frame];
  const doneness = T_core >= T_done ? ' — done!' : '';
  return `t = ${api.formatClock(times[frame])} · core `
    + `${(T_core - 273.15).toFixed(1)} degC${doneness}`;
}, 12000);

api.addColorbar(vMin, vMax, api.heatColor);
```

The poultry-safe core temperature is 347 K (74 °C / 165 °F). Watch `T[1]`
(the center) in the plot: with these parameters a 5 kg bird is not done
after four hours at 177 °C — try a hotter oven, a smaller turkey, or a
longer `StopTime` in the experiment annotation.

## What to Notice in the Model

- **One state per cell.** `Real T[N](each start = 277.0)` declares `N`
  states at once; `each` applies the modification to every element.
- **`for`-equations are unrolled at compile time.** The loop range must be
  known structurally, which is why `N` is a `parameter Integer`. After
  flattening, the compiler sees `5 * N + 4` plain scalar equations — press
  **Show DAE** to look at them.
- **Energy balances, not finite-difference formulas.** Writing
  `m_shell[i] * cp * der(T[i]) = Q_cond[i] - Q_cond[i+1]` per shell, with an
  explicit interface flux array, conserves energy exactly by construction
  and reads like the physics. (This formulation follows the
  [Dyad turkey demo](https://github.com/DyadLang/DyadDemos/tree/main/TurkeyDemo).)
- **Mixed boundary condition.** The surface shell receives both convection
  (`h·A·ΔT`) and radiation (`ε·σ·A·(T⁴_oven − T⁴)`) — the `T⁴` terms make
  the system nonlinear, which the solver handles without any special
  treatment.

## 2-D Fields: A Vibrating Membrane

The same technique extends to two dimensions with matrix states and nested
`for`-equations. This is the 2-D wave equation on a square membrane with
clamped edges, started from a Gaussian pluck in the center. The grid
resolution is the `N` parameter — edit it (try 8 to 20 on CPU, or larger
with GPU enabled) and re-run:

```modelica,interactive,gpu
model Wave2D "2-D wave equation on a square membrane, method of lines"
  parameter Integer N = 20 "Grid cells per side";
  parameter Real L = 1.0 "Side length [m]";
  parameter Real c = 1.0 "Wave speed [m/s]";
  parameter Real d = 0.05 "Damping [1/s]";
  parameter Real dx = L / (N - 1);
  Real u[N, N] "Displacement";
  Real w[N, N] "Velocity";
initial equation
  for i in 1:N loop
    for j in 1:N loop
      u[i, j] = exp(-200.0 * (((i - 1) * dx - 0.5 * L) ^ 2
                            + ((j - 1) * dx - 0.5 * L) ^ 2));
      w[i, j] = 0.0;
    end for;
  end for;
equation
  for i in 1:N loop
    for j in 1:N loop
      der(u[i, j]) = w[i, j];
    end for;
  end for;
  // Fixed boundary: edges clamped to zero motion.
  for i in 1:N loop
    der(w[i, 1]) = 0.0;
    der(w[i, N]) = 0.0;
  end for;
  for j in 2:N - 1 loop
    der(w[1, j]) = 0.0;
    der(w[N, j]) = 0.0;
  end for;
  // Interior: five-point Laplacian with light damping.
  for i in 2:N - 1 loop
    for j in 2:N - 1 loop
      der(w[i, j]) = c ^ 2 * (u[i + 1, j] + u[i - 1, j] + u[i, j + 1]
                            + u[i, j - 1] - 4.0 * u[i, j]) / dx ^ 2
                   - d * w[i, j];
    end for;
  end for;
  annotation(experiment(StopTime = 2.0, Interval = 0.02, Solver = "rk-like"));
end Wave2D;
```

The surface below is an editable visualization script, like the turkey's.
`api.matrixField()` collects the `u[i,j]` states into a grid; the script
draws a smoothed blue Three.js surface. Drag to rotate, shift-drag or
right-drag to pan, and scroll to zoom:

```js,rumoca-viz
// Animate the membrane displacement field u[i,j] as a rotatable surface.
api.hideDefaultPlot();
const field = api.matrixField();
const { vMin, vMax } = api.valueRange(field.members);
const span = Math.max(Math.abs(vMin), Math.abs(vMax)) || 1;
const { THREE } = await api.loadThree();

container.classList.add('rumoca-live-surface');
const host = document.createElement('div');
host.className = 'rumoca-live-surface-host';
container.appendChild(host);

const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
renderer.setClearColor(0x071825, 1);
renderer.outputColorSpace = THREE.SRGBColorSpace;
host.appendChild(renderer.domElement);

const scene = new THREE.Scene();
scene.fog = new THREE.Fog(0x071825, 3.5, 8.5);

const camera = new THREE.PerspectiveCamera(42, 1, 0.05, 30);
const target = new THREE.Vector3(0, 0, 0);
const orbit = { theta: -0.75, phi: 0.9, radius: 3.1 };

scene.add(new THREE.HemisphereLight(0x96d7ff, 0x082744, 1.4));
const sun = new THREE.DirectionalLight(0xffffff, 1.9);
sun.position.set(-2.2, 3.5, 2.4);
scene.add(sun);

const positions = new Float32Array(field.rows * field.cols * 3);
const colors = new Float32Array(field.rows * field.cols * 3);
const uvs = new Float32Array(field.rows * field.cols * 2);
const indices = [];
for (let i = 0; i < field.rows; i++) {
  for (let j = 0; j < field.cols; j++) {
    const k = i * field.cols + j;
    uvs[k * 2] = j / Math.max(1, field.cols - 1);
    uvs[k * 2 + 1] = i / Math.max(1, field.rows - 1);
  }
}
for (let i = 0; i < field.rows - 1; i++) {
  for (let j = 0; j < field.cols - 1; j++) {
    const a = i * field.cols + j;
    const b = a + 1;
    const c = a + field.cols;
    const d = c + 1;
    indices.push(a, c, b, b, c, d);
  }
}

const geometry = new THREE.BufferGeometry();
geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
geometry.setAttribute('uv', new THREE.BufferAttribute(uvs, 2));
geometry.setIndex(indices);

function makeWaterTexture() {
  const canvas = document.createElement('canvas');
  canvas.width = 512;
  canvas.height = 512;
  const ctx = canvas.getContext('2d');
  const gradient = ctx.createLinearGradient(0, 0, 512, 512);
  gradient.addColorStop(0, '#0b4f88');
  gradient.addColorStop(0.45, '#1391b9');
  gradient.addColorStop(1, '#56d6df');
  ctx.fillStyle = gradient;
  ctx.fillRect(0, 0, 512, 512);
  let seed = 7;
  const rand = () => {
    seed = (1664525 * seed + 1013904223) >>> 0;
    return seed / 4294967296;
  };
  ctx.globalAlpha = 0.28;
  for (let y = -80; y < 600; y += 26) {
    ctx.strokeStyle = y % 52 === 0 ? '#d8fbff' : '#74e7f1';
    ctx.lineWidth = y % 52 === 0 ? 2.2 : 1.2;
    ctx.beginPath();
    for (let x = -20; x <= 540; x += 20) {
      const yy = y + Math.sin((x + y) * 0.035) * 8 + Math.sin(x * 0.08) * 3;
      if (x === -20) {
        ctx.moveTo(x, yy);
      } else {
        ctx.lineTo(x, yy);
      }
    }
    ctx.stroke();
  }
  ctx.globalAlpha = 0.16;
  for (let i = 0; i < 180; i++) {
    const x = rand() * 512;
    const y = rand() * 512;
    const r = 0.8 + rand() * 2.4;
    ctx.fillStyle = '#e9ffff';
    ctx.beginPath();
    ctx.ellipse(x, y, r * 2.5, r, rand() * Math.PI, 0, Math.PI * 2);
    ctx.fill();
  }
  const texture = new THREE.CanvasTexture(canvas);
  texture.colorSpace = THREE.SRGBColorSpace;
  texture.wrapS = THREE.RepeatWrapping;
  texture.wrapT = THREE.RepeatWrapping;
  texture.repeat.set(1.4, 1.4);
  return texture;
}

const surface = new THREE.Mesh(
  geometry,
  new THREE.MeshStandardMaterial({
    color: 0xffffff,
    map: makeWaterTexture(),
    vertexColors: true,
    roughness: 0.28,
    metalness: 0.02,
    side: THREE.DoubleSide,
  })
);
scene.add(surface);

function updateCamera() {
  orbit.phi = Math.max(0.22, Math.min(1.38, orbit.phi));
  orbit.radius = Math.max(1.6, Math.min(7.0, orbit.radius));
  camera.position.set(
    target.x + orbit.radius * Math.sin(orbit.phi) * Math.sin(orbit.theta),
    target.y + orbit.radius * Math.cos(orbit.phi),
    target.z + orbit.radius * Math.sin(orbit.phi) * Math.cos(orbit.theta)
  );
  camera.lookAt(target);
}

function render() {
  updateCamera();
  renderer.render(scene, camera);
}

function resize() {
  const width = Math.max(320, Math.floor(host.clientWidth || 640));
  const height = Math.max(300, Math.min(500, Math.floor(width * 0.62)));
  renderer.setSize(width, height, false);
  camera.aspect = width / height;
  camera.updateProjectionMatrix();
  render();
}

function updateSurface(frameIndex) {
  const blue = new THREE.Color();
  let k = 0;
  for (let i = 1; i <= field.rows; i++) {
    for (let j = 1; j <= field.cols; j++) {
      const u = field.at(i, j)[frameIndex];
      const x = ((j - 1) / (field.cols - 1) - 0.5) * 2;
      const z = ((i - 1) / (field.rows - 1) - 0.5) * 2;
      const y = u * 0.85 / span;
      positions[k * 3] = x;
      positions[k * 3 + 1] = y;
      positions[k * 3 + 2] = z;
      const shade = 0.42 + 0.24 * (0.5 + 0.5 * (u / span));
      blue.setHSL(0.53, 0.9, shade);
      colors[k * 3] = blue.r;
      colors[k * 3 + 1] = blue.g;
      colors[k * 3 + 2] = blue.b;
      k++;
    }
  }
  geometry.attributes.position.needsUpdate = true;
  geometry.attributes.color.needsUpdate = true;
  geometry.computeVertexNormals();
  render();
}

let pointer = null;
renderer.domElement.addEventListener('contextmenu', (event) => event.preventDefault());
renderer.domElement.addEventListener('pointerdown', (event) => {
  renderer.domElement.setPointerCapture(event.pointerId);
  pointer = {
    id: event.pointerId,
    x: event.clientX,
    y: event.clientY,
    pan: event.shiftKey || event.button === 1 || event.button === 2,
  };
});
renderer.domElement.addEventListener('pointermove', (event) => {
  if (!pointer || pointer.id !== event.pointerId) {
    return;
  }
  const dx = event.clientX - pointer.x;
  const dy = event.clientY - pointer.y;
  pointer.x = event.clientX;
  pointer.y = event.clientY;
  if (pointer.pan) {
    const direction = new THREE.Vector3();
    camera.getWorldDirection(direction);
    const right = new THREE.Vector3().crossVectors(direction, camera.up).normalize();
    const up = new THREE.Vector3().copy(camera.up).normalize();
    const scale = orbit.radius * 0.0018;
    target.addScaledVector(right, -dx * scale);
    target.addScaledVector(up, dy * scale);
  } else {
    orbit.theta -= dx * 0.008;
    orbit.phi -= dy * 0.008;
  }
  render();
});
for (const type of ['pointerup', 'pointercancel']) {
  renderer.domElement.addEventListener(type, (event) => {
    if (pointer && pointer.id === event.pointerId) {
      pointer = null;
    }
  });
}
renderer.domElement.addEventListener('wheel', (event) => {
  event.preventDefault();
  orbit.radius *= Math.exp(event.deltaY * 0.001);
  render();
}, { passive: false });

new ResizeObserver(resize).observe(host);
resize();
function fixedTime(seconds) {
  return seconds.toFixed(2).padStart(5, '0');
}
api.addAnimation(times, (frame) => {
  updateSurface(frame);
  return `t = ${fixedTime(times[frame])} s`;
}, 8000);
```

Notice the cost of resolution: each cell adds two states (`u` and `w`), so
`N = 20` is an 800-state system. Rumoca compiles and simulates it fine, but
output volume grows as `N²` — keep `Interval` coarse enough for the
browser. The regular interior loops are also the shape Rumoca preserves as
source-proven affine stencils in Solve IR: GPU/codegen targets can emit a
single parametric kernel for the repeated grid operation, while CPU and
embedded targets still have an exact scalar fallback.

## 2-D Navier–Stokes: Flow over a NACA 2412 Airfoil

The same machinery scales up to fluid dynamics. This example solves the 2-D
incompressible Navier–Stokes equations around a NACA 2412 airfoil at an
adjustable angle of attack, using two classic tricks that keep the system a
pure ODE — exactly what the method of lines wants:

- **Artificial compressibility**: instead of the pressure-Poisson algebraic
  constraint, pressure gets its own fast dynamics
  `der(q) = -cs² · div(V)`. The continuity error propagates away as an
  artificial acoustic wave, and no algebraic loop is needed.
- **Brinkman penalization**: the airfoil is not meshed. Each cell gets a
  smooth solid fraction `sig ∈ [0, 1]` (computed per grid column from the
  standard NACA camber and thickness polynomials) and a drag term `-sig·V/tau`
  that drives the velocity to zero, so the flow sees a solid body on a plain
  Cartesian grid. The mask is a *differentiable* `tanh` indicator rather than a
  hard `if/else` threshold: it is `≈1` deep inside the body, `0.5` on the
  geometric contour, and `≈0` in the fluid, with a transition band about one
  grid cell wide. That smoothness is what makes the shape sensitivities
  `∂sig/∂(camber, thickness)` nonzero in the boundary band — the prerequisite
  for differentiating the flow with respect to the airfoil shape.

The freestream is horizontal and the *airfoil itself pitches*: each cell's
coordinates are rotated into the airfoil frame, so the solid mask turns with
the angle of attack the way a real wind-tunnel model would. `aoa` is the
pre-simulation angle parameter. The model also exposes `input Real aoa_cmd`;
an `aoa_motor` state follows that command with
`der(aoa_motor) = (aoa_cmd - aoa_motor) / aoa_tau`. A structural
`interactive` flag selects whether the immersed-boundary mask uses the
pre-simulation parameter `aoa` or the lagged state `aoa_motor`. With
**Interactive** off, the **AoA slider** is a normal parameter tuner and re-runs
the simulation from the selected pre-simulation angle. With **Interactive** on,
the same slider feeds `aoa_cmd` during stepping, so the physical airfoil angle
moves through the first-order lag.

This example defaults the **GPU** checkbox
on: the compiler's experimental `wgsl-solve` backend lowers the system to
WebGPU compute kernels and an in-page RK4 integrator runs them. Interior
finite-volume loops are preserved as source-proven affine stencils, so the
WebGPU path emits native row-parallel stencil kernels instead of rediscovering
grid structure from scalarized equations. If WebGPU is unavailable the run
fails with a clear error instead of silently falling back (uncheck GPU for the
CPU path). GPU v1 runs in f32 with events and algebraics frozen at their
settled initial values, which is exact for the normal batch run because
`interactive = false` makes the mask depend only on pre-simulation parameters.
The named-input interactive stepping path reads `aoa_cmd` before each step and
uses `interactive = true`. The run is an impulsive wind-tunnel start:
the field begins at rest and the freestream sweeps in from the inlet and
far-field boundaries. This is the heaviest example in the book
(~6,500 integrated states on the default grid): expect the first run to take a
while.

```modelica,interactive,gpu
model AirfoilFlow "2-D flow over a NACA 2412: artificial compressibility + penalization"
  parameter Integer NX = 30 "Cells along the channel";
  parameter Integer NY = 18 "Cells across the channel";
  parameter Real Lx = 4.0 "Domain length [chords]";
  parameter Real Ly = 1.5 "Domain height [chords]";
  parameter Real xle = 1.0 "Leading edge distance from inlet [chords]";
  parameter Real aoa = 8.0 "Initial/pre-simulation angle of attack [deg]";
  parameter Boolean interactive = false
    "Use live AoA motor state for the airfoil mask" annotation(Evaluate = true);
  input Real aoa_cmd(start = aoa) "Commanded angle of attack [deg]";
  parameter Real aoa_tau = 1.0 "First-order AoA motor time constant [s]";
  parameter Real U = 1.0 "Freestream speed (horizontal)";
  parameter Real nu = 0.01 "Kinematic viscosity (Re = U/nu = 100)";
  parameter Real cs = 3.0 "Artificial-compressibility wave speed";
  parameter Real qnu = 0.01 "Pressure-mode damping diffusivity";
  parameter Real tau = 0.02 "Solid penalization time constant [s]";
  parameter Real taub = 0.05 "Boundary relaxation time constant [s]";
  parameter Real mc0 = 0.02 "Initial/pre-simulation NACA max camber";
  parameter Real pc0 = 0.4 "Initial/pre-simulation NACA camber position";
  parameter Real tk0 = 0.12 "Initial/pre-simulation NACA thickness";
  input Real mc(start = mc0) "Commanded NACA max camber";
  input Real pc(start = pc0) "Commanded NACA camber position";
  input Real tk(start = tk0) "Commanded NACA thickness";
  parameter Real shape_tau = 1.0 "First-order airfoil shape actuator time constant [s]";
  parameter Real dx = Lx / NX;
  parameter Real dy = Ly / NY;
  parameter Real pi = 3.14159265359;
  parameter Real epsn = 0.6 * dy "Mask transition width, chord-normal [chords]";
  parameter Real epss = 0.8 * dx "Mask transition width, chordwise [chords]";
  parameter Real tmin = 0.6 * dy "Smooth half-thickness floor: keeps the coarse mask closed";
  Real aoa_motor(start = aoa, fixed = true) "Lagged physical angle of attack [deg]";
  Real mc_motor(start = mc0, fixed = true) "Lagged NACA max camber";
  Real pc_motor(start = pc0, fixed = true) "Lagged NACA camber position";
  Real tk_motor(start = tk0, fixed = true) "Lagged NACA thickness";
  Real u[NX, NY] "x-velocity";
  Real v[NX, NY] "y-velocity";
  Real q[NX, NY] "pressure / rho";
  Real sc[NX, NY] "Chordwise coordinate in the pitched airfoil frame";
  Real nc[NX, NY] "Chord-normal coordinate in the pitched airfoil frame";
  Real sig[NX, NY] "Solid mask (1 inside the airfoil)";
  // States start at rest (default start = 0): an impulsive wind-tunnel
  // start where the freestream sweeps in through the boundary relaxation.
equation
  der(aoa_motor) =
    if interactive then (aoa_cmd - aoa_motor) / aoa_tau else 0.0;
  der(mc_motor) = if interactive then (mc - mc_motor) / shape_tau else 0.0;
  der(pc_motor) = if interactive then (pc - pc_motor) / shape_tau else 0.0;
  der(tk_motor) = if interactive then (tk - tk_motor) / shape_tau else 0.0;
  for i in 1:NX loop
    for j in 1:NY loop
      if interactive then
        sc[i, j] = ((i - 0.5) * dx - xle) * cos(aoa_motor * pi / 180.0)
          - ((j - 0.5) * dy - Ly / 2.0) * sin(aoa_motor * pi / 180.0);
        nc[i, j] = ((i - 0.5) * dx - xle) * sin(aoa_motor * pi / 180.0)
          + ((j - 0.5) * dy - Ly / 2.0) * cos(aoa_motor * pi / 180.0);
        sig[i, j] =
          0.5 * (1.0 - tanh((abs(nc[i, j]
              - (if sc[i, j] < pc_motor then mc_motor / pc_motor ^ 2 * (2.0 * pc_motor * sc[i, j] - sc[i, j] ^ 2)
                 else mc_motor / (1.0 - pc_motor) ^ 2
                   * ((1.0 - 2.0 * pc_motor) + 2.0 * pc_motor * sc[i, j] - sc[i, j] ^ 2)))
            - sqrt((5.0 * tk_motor * (0.2969 * sqrt(max(sc[i, j], 0.0)) - 0.1260 * sc[i, j]
                    - 0.3516 * sc[i, j] ^ 2 + 0.2843 * sc[i, j] ^ 3
                    - 0.1036 * sc[i, j] ^ 4)) ^ 2 + tmin ^ 2)) / epsn))
          * (0.5 * (1.0 + tanh(sc[i, j] / epss)))
          * (0.5 * (1.0 + tanh((1.0 - sc[i, j]) / epss)));
      else
        sc[i, j] = ((i - 0.5) * dx - xle) * cos(aoa * pi / 180.0)
          - ((j - 0.5) * dy - Ly / 2.0) * sin(aoa * pi / 180.0);
        nc[i, j] = ((i - 0.5) * dx - xle) * sin(aoa * pi / 180.0)
          + ((j - 0.5) * dy - Ly / 2.0) * cos(aoa * pi / 180.0);
        sig[i, j] =
          0.5 * (1.0 - tanh((abs(nc[i, j]
              - (if sc[i, j] < pc0 then mc0 / pc0 ^ 2 * (2.0 * pc0 * sc[i, j] - sc[i, j] ^ 2)
                 else mc0 / (1.0 - pc0) ^ 2
                   * ((1.0 - 2.0 * pc0) + 2.0 * pc0 * sc[i, j] - sc[i, j] ^ 2)))
            - sqrt((5.0 * tk0 * (0.2969 * sqrt(max(sc[i, j], 0.0)) - 0.1260 * sc[i, j]
                    - 0.3516 * sc[i, j] ^ 2 + 0.2843 * sc[i, j] ^ 3
                    - 0.1036 * sc[i, j] ^ 4)) ^ 2 + tmin ^ 2)) / epsn))
          * (0.5 * (1.0 + tanh(sc[i, j] / epss)))
          * (0.5 * (1.0 + tanh((1.0 - sc[i, j]) / epss)));
      end if;
    end for;
  end for;
  // Interior: momentum + artificial-compressibility continuity.
  for i in 2:NX - 1 loop
    for j in 2:NY - 1 loop
      der(u[i, j]) = -u[i, j] * (u[i + 1, j] - u[i - 1, j]) / (2.0 * dx)
        - v[i, j] * (u[i, j + 1] - u[i, j - 1]) / (2.0 * dy)
        - (q[i + 1, j] - q[i - 1, j]) / (2.0 * dx)
        + nu * ((u[i + 1, j] - 2.0 * u[i, j] + u[i - 1, j]) / dx ^ 2
              + (u[i, j + 1] - 2.0 * u[i, j] + u[i, j - 1]) / dy ^ 2)
        - sig[i, j] * u[i, j] / tau;
      der(v[i, j]) = -u[i, j] * (v[i + 1, j] - v[i - 1, j]) / (2.0 * dx)
        - v[i, j] * (v[i, j + 1] - v[i, j - 1]) / (2.0 * dy)
        - (q[i, j + 1] - q[i, j - 1]) / (2.0 * dy)
        + nu * ((v[i + 1, j] - 2.0 * v[i, j] + v[i - 1, j]) / dx ^ 2
              + (v[i, j + 1] - 2.0 * v[i, j] + v[i, j - 1]) / dy ^ 2)
        - sig[i, j] * v[i, j] / tau;
      der(q[i, j]) = -cs ^ 2 * ((u[i + 1, j] - u[i - 1, j]) / (2.0 * dx)
                              + (v[i, j + 1] - v[i, j - 1]) / (2.0 * dy))
        + qnu * ((q[i + 1, j] - 2.0 * q[i, j] + q[i - 1, j]) / dx ^ 2
               + (q[i, j + 1] - 2.0 * q[i, j] + q[i, j - 1]) / dy ^ 2);
    end for;
  end for;
  // Inlet (left): horizontal freestream; pressure zero-gradient.
  for j in 1:NY loop
    der(u[1, j]) = (U - u[1, j]) / taub;
    der(v[1, j]) = (0.0 - v[1, j]) / taub;
    der(q[1, j]) = (q[2, j] - q[1, j]) / taub;
    // Outlet (right): zero-gradient velocities, reference pressure.
    der(u[NX, j]) = (u[NX - 1, j] - u[NX, j]) / taub;
    der(v[NX, j]) = (v[NX - 1, j] - v[NX, j]) / taub;
    der(q[NX, j]) = (0.0 - q[NX, j]) / taub;
  end for;
  // Far field (top/bottom): freestream; pressure zero-gradient.
  for i in 2:NX - 1 loop
    der(u[i, 1]) = (U - u[i, 1]) / taub;
    der(v[i, 1]) = (0.0 - v[i, 1]) / taub;
    der(q[i, 1]) = (q[i, 2] - q[i, 1]) / taub;
    der(u[i, NY]) = (U - u[i, NY]) / taub;
    der(v[i, NY]) = (0.0 - v[i, NY]) / taub;
    der(q[i, NY]) = (q[i, NY - 1] - q[i, NY]) / taub;
  end for;
  // Interval controls the output/readback cadence; __rumoca(Solver(FixedStep))
  // controls the internal fixed-step RK4 step. CFL note: the explicit step must
  // stay below the acoustic/diffusive limit ~ 1 / (cs/h + 2*nu/h^2) with
  // h = min(dx, dy); if you refine NX/NY, drop FixedStep roughly in proportion
  // or the run will diverge.
  annotation(__rumoca(Solver(FixedStep = 0.005)), experiment(StopTime = 30, Interval = 0.1, Solver = "rk-like"));
end AirfoilFlow;
```

```js,rumoca-viz
// Field heatmap over the smooth solid mask (gray) with the true NACA 2412
// contour on top. The default field is vorticity, which makes separated shear
// layers and stall visible. The picker can also show pressure q or speed |V|.
// Velocity-direction and streamline overlays are computed from the returned
// u/v states only; they add no solver work or extra readback. Grid size is
// discovered from the result names, so editing NX/NY just works. Only the
// integrated states u/v/q plus the lagged airfoil motor states are read; the
// mask is recomputed below so the overlay follows the same geometry on every
// solver path.
const cell = new Map();
let NX = 0, NY = 0;
names.forEach((n, k) => {
  const m = /^([uvq])\[(\d+),(\d+)\]$/.exec(n);
  if (!m) return;
  cell.set(`${m[1]}:${m[2]},${m[3]}`, data[k]);
  if (m[1] === 'u') {
    NX = Math.max(NX, Number(m[2]));
    NY = Math.max(NY, Number(m[3]));
  }
});
const speed = (i, j, f) => {
  const u = cell.get(`u:${i},${j}`);
  const v = cell.get(`v:${i},${j}`);
  if (!u || !v) return 0;
  return Math.hypot(u[f], v[f]);
};
const press = (i, j, f) => {
  const q = cell.get(`q:${i},${j}`);
  return q ? q[f] : 0;
};
const fieldDx = api.parameter('Lx', 4.0) / NX;
const fieldDy = api.parameter('Ly', 1.5) / NY;
const clippedIndex = (value, hi) => Math.max(1, Math.min(hi, value));
const vorticity = (i, j, f) => {
  const im = clippedIndex(i - 1, NX);
  const ip = clippedIndex(i + 1, NX);
  const jm = clippedIndex(j - 1, NY);
  const jp = clippedIndex(j + 1, NY);
  const dvdx = (cell.get(`v:${ip},${j}`)?.[f] - cell.get(`v:${im},${j}`)?.[f])
    / ((ip - im) * fieldDx);
  const dudy = (cell.get(`u:${i},${jp}`)?.[f] - cell.get(`u:${i},${jm}`)?.[f])
    / ((jp - jm) * fieldDy);
  return Number.isFinite(dvdx) && Number.isFinite(dudy) ? dvdx - dudy : 0;
};
// Velocity arrows use a global speed reference so their visibility does not
// flicker as the colorbar rescales frame-by-frame.
let vMax = 0;
for (let f = 0; f < times.length; f += 5) {
  for (let i = 1; i <= NX; i++) {
    for (let j = 1; j <= NY; j++) {
      vMax = Math.max(vMax, speed(i, j, f));
    }
  }
}
if (vMax <= 0) vMax = 1;
const clamp01 = (x) => Math.max(0, Math.min(1, x));

function framePressureRange(frame) {
  let lo = Infinity, hi = -Infinity;
  for (let i = 1; i <= NX; i++) {
    for (let j = 1; j <= NY; j++) {
      const q = press(i, j, frame);
      if (!Number.isFinite(q)) continue;
      lo = Math.min(lo, q);
      hi = Math.max(hi, q);
    }
  }
  if (!(hi > lo)) return { lo: -1, hi: 1 };
  return { lo, hi };
}

function frameSpeedRange(frame) {
  let hi = 0;
  for (let i = 1; i <= NX; i++) {
    for (let j = 1; j <= NY; j++) hi = Math.max(hi, speed(i, j, frame));
  }
  return { lo: 0, hi: hi > 0 ? hi : 1 };
}

function frameVorticityRange(frame) {
  let span = 0;
  for (let i = 1; i <= NX; i++) {
    for (let j = 1; j <= NY; j++) {
      const w = vorticity(i, j, frame);
      if (Number.isFinite(w)) span = Math.max(span, Math.abs(w));
    }
  }
  span = span > 0 ? span : 1;
  return { lo: -span, hi: span };
}

// Available fields. `norm` maps a cell value into [0,1] for the heat colormap.
const fields = {
  vorticity: {
    label: 'Vorticity',
    range: frameVorticityRange,
    value: vorticity,
  },
  q: {
    label: 'Pressure q',
    range: framePressureRange,
    value: press,
  },
  speed: {
    label: 'Speed |V|',
    range: frameSpeedRange,
    value: speed,
  },
};
let mode = 'vorticity';   // default to the field that shows stall/separation.
let refreshColorbar = () => {};
// Geometry for the overlay — keep in sync with the model parameters.
// The command inputs seed the run; the lagged motor states drive the moving
// contour and mask frame-by-frame.
const mc0 = api.parameter('mc', api.parameter('mc0', 0.02));
const pc0 = api.parameter('pc', api.parameter('pc0', 0.4));
const tk0 = api.parameter('tk', api.parameter('tk0', 0.12));
const geo = {
  Lx: api.parameter('Lx', 4.0),
  Ly: api.parameter('Ly', 1.5),
  xle: api.parameter('xle', 1.0),
  mc: mc0,
  pc: pc0,
  tk: tk0,
};
const aoa = api.parameter('aoa', 8.0);
const aoaSeries = api.series('aoa_motor') || [aoa];
const mcSeries = api.series('mc_motor') || [mc0];
const pcSeries = api.series('pc_motor') || [pc0];
const tkSeries = api.series('tk_motor') || [tk0];
const frameSeriesValue = (series, frame, fallback) => {
  const value = series[Math.min(frame, series.length - 1)];
  return Number.isFinite(value) ? value : fallback;
};
const frameAoa = (frame) => frameSeriesValue(aoaSeries, frame, aoa);
function refreshGeometryParameters(frame) {
  geo.xle = api.parameter('xle', 1.0);
  geo.mc = frameSeriesValue(mcSeries, frame, mc0);
  geo.pc = Math.max(1e-3, Math.min(0.999, frameSeriesValue(pcSeries, frame, pc0)));
  geo.tk = Math.max(1e-6, frameSeriesValue(tkSeries, frame, tk0));
};
const camber = (sc) => sc < geo.pc
  ? geo.mc / geo.pc ** 2 * (2 * geo.pc * sc - sc ** 2)
  : geo.mc / (1 - geo.pc) ** 2 * ((1 - 2 * geo.pc) + 2 * geo.pc * sc - sc ** 2);
const halfThick = (sc) => 5 * geo.tk * (0.2969 * Math.sqrt(sc) - 0.1260 * sc
  - 0.3516 * sc ** 2 + 0.2843 * sc ** 3 - 0.1036 * sc ** 4);

// Smooth solid fraction sig(i, j) in [0, 1], recomputed here exactly as the
// model does so the overlay never depends on the solver returning algebraics.
// The grid spacing and tanh band widths mirror the AirfoilFlow parameters.
const dx = geo.Lx / NX, dy = geo.Ly / NY;
const epsn = api.parameter('epsn', 0.6 * dy);
const epss = api.parameter('epss', 0.8 * dx);
const tmin = api.parameter('tmin', 0.6 * dy);
function maskAt(i, j, angleDeg) {
  const ca = Math.cos(angleDeg * Math.PI / 180);
  const sa = Math.sin(angleDeg * Math.PI / 180);
  const xa = (i - 0.5) * dx - geo.xle;        // chord-frame offsets, pitched
  const ya = (j - 0.5) * dy - geo.Ly / 2;     // about the leading edge
  const sc = xa * ca - ya * sa;               // chordwise coordinate
  const nc = xa * sa + ya * ca;               // chord-normal coordinate
  const s = Math.max(sc, 0);
  const traw = 5 * geo.tk * (0.2969 * Math.sqrt(s) - 0.1260 * sc
    - 0.3516 * sc ** 2 + 0.2843 * sc ** 3 - 0.1036 * sc ** 4);
  const teff = Math.sqrt(traw ** 2 + tmin ** 2);   // softly floored half-thick
  const dthick = Math.abs(nc - camber(sc)) - teff; // signed distance to surface
  return 0.5 * (1 - Math.tanh(dthick / epsn))      // inside the thickness band
    * 0.5 * (1 + Math.tanh(sc / epss))             // past the leading edge
    * 0.5 * (1 + Math.tanh((1 - sc) / epss));      // before the trailing edge
}
const solidCache = new Map();
function solidFracFor(angleDeg) {
  const key = [
    angleDeg.toFixed(3),
    geo.xle.toFixed(4),
    geo.mc.toFixed(4),
    geo.pc.toFixed(4),
    geo.tk.toFixed(4),
  ].join(':');
  const cached = solidCache.get(key);
  if (cached) return cached;

  const solidFrac = new Map();
  for (let i = 1; i <= NX; i++) {
    for (let j = 1; j <= NY; j++) solidFrac.set(`${i},${j}`, maskAt(i, j, angleDeg));
  }
  solidCache.set(key, solidFrac);
  return solidFrac;
}

const W = 600;
const H = Math.round(W * (geo.Ly / geo.Lx));
const { ctx2d } = api.makeCanvas(W, H);
const cw = W / NX;
const ch = H / NY;
const px = (x) => (x / geo.Lx) * W;                    // physical x -> canvas
const py = (y) => H - ((y + geo.Ly / 2) / geo.Ly) * H; // physical y -> canvas

function airfoilFrame(angleDeg) {
  const ca = Math.cos(angleDeg * Math.PI / 180);
  const sa = Math.sin(angleDeg * Math.PI / 180);
  return {
    fx: (sc, h) => geo.xle + sc * ca + h * sa,
    fy: (sc, h) => -sc * sa + h * ca,
  };
}

function drawAirfoil(angleDeg) {
  const { fx, fy } = airfoilFrame(angleDeg);
  ctx2d.beginPath();
  for (let k = 0; k <= 60; k++) {            // upper surface, LE -> TE
    const sc = k / 60;
    const h = camber(sc) + halfThick(sc);
    const fn = k === 0 ? 'moveTo' : 'lineTo';
    ctx2d[fn](px(fx(sc, h)), py(fy(sc, h)));
  }
  for (let k = 60; k >= 0; k--) {            // lower surface, TE -> LE
    const sc = k / 60;
    const h = camber(sc) - halfThick(sc);
    ctx2d.lineTo(px(fx(sc, h)), py(fy(sc, h)));
  }
  ctx2d.closePath();
  ctx2d.fillStyle = '#111';
  ctx2d.fill();
  ctx2d.strokeStyle = '#fff';
  ctx2d.lineWidth = 1;
  ctx2d.stroke();
}

let showVelocityDirections = true;

function drawVelocityDirections(frame, solidFrac) {
  if (!showVelocityDirections || !NX || !NY || !Number.isFinite(vMax)) return;

  const stride = Math.max(1, Math.ceil(Math.max(NX, NY) / 28));
  const len = Math.max(4, 0.65 * stride * Math.min(cw, ch));

  function sampledVelocity(i0, j0) {
    let su = 0, sv = 0, sw = 0;
    for (let di = -1; di <= 1; di++) {
      for (let dj = -1; dj <= 1; dj++) {
        const i = i0 + di, j = j0 + dj;
        if (i < 1 || i > NX || j < 1 || j > NY) continue;

        const m = solidFrac.get(`${i},${j}`) ?? 0;

        const uArr = cell.get(`u:${i},${j}`);
        const vArr = cell.get(`v:${i},${j}`);
        if (!uArr || !vArr) continue;

        const u = uArr[frame];
        const v = vArr[frame];
        if (!Number.isFinite(u) || !Number.isFinite(v)) continue;

        const w = (di === 0 && dj === 0 ? 2 : 1) * Math.max(0.05, 1 - m);
        su += w * u;
        sv += w * v;
        sw += w;
      }
    }
    return sw > 0 ? [su / sw, sv / sw] : [NaN, NaN];
  }

  ctx2d.save();
  ctx2d.lineCap = 'round';
  ctx2d.lineJoin = 'round';

  for (let i = 1; i <= NX; i += stride) {
    for (let j = 1; j <= NY; j += stride) {
      const [u, v] = sampledVelocity(i, j);
      const sp = Math.hypot(u, v);
      if (!Number.isFinite(sp) || sp <= 0) continue;

      const ux = u / sp;
      const uy = v / sp;
      const cx = (i - 0.5) * cw;
      const cy = (NY - j + 0.5) * ch;
      const dxp = ux * len;
      const dyp = -uy * len; // Physical +v points up; canvas +y points down.
      const x1 = cx - 0.5 * dxp;
      const y1 = cy - 0.5 * dyp;
      const x2 = cx + 0.5 * dxp;
      const y2 = cy + 0.5 * dyp;
      const ang = Math.atan2(y2 - y1, x2 - x1);
      const head = Math.max(3, 0.25 * len);
      const alpha = 0.35 + 0.55 * clamp01(sp / (0.45 * vMax));

      ctx2d.strokeStyle = `rgba(0,0,0,${0.55 * alpha})`;
      ctx2d.lineWidth = 3.5;
      ctx2d.beginPath();
      ctx2d.moveTo(x1, y1);
      ctx2d.lineTo(x2, y2);
      ctx2d.lineTo(
        x2 - head * Math.cos(ang - Math.PI / 6),
        y2 - head * Math.sin(ang - Math.PI / 6)
      );
      ctx2d.moveTo(x2, y2);
      ctx2d.lineTo(
        x2 - head * Math.cos(ang + Math.PI / 6),
        y2 - head * Math.sin(ang + Math.PI / 6)
      );
      ctx2d.stroke();

      ctx2d.strokeStyle = `rgba(255,255,255,${alpha})`;
      ctx2d.lineWidth = 1.3;
      ctx2d.beginPath();
      ctx2d.moveTo(x1, y1);
      ctx2d.lineTo(x2, y2);
      ctx2d.lineTo(
        x2 - head * Math.cos(ang - Math.PI / 6),
        y2 - head * Math.sin(ang - Math.PI / 6)
      );
      ctx2d.moveTo(x2, y2);
      ctx2d.lineTo(
        x2 - head * Math.cos(ang + Math.PI / 6),
        y2 - head * Math.sin(ang + Math.PI / 6)
      );
      ctx2d.stroke();
    }
  }

  ctx2d.restore();
}

let showStreamlines = true;

function sampleGridValue(getValue, x, y, frame) {
  const fi = x / dx + 0.5;
  const fj = (y + geo.Ly / 2) / dy + 0.5;
  if (fi < 1 || fi > NX || fj < 1 || fj > NY) return NaN;

  const i0 = Math.max(1, Math.min(NX - 1, Math.floor(fi)));
  const j0 = Math.max(1, Math.min(NY - 1, Math.floor(fj)));
  const tx = clamp01(fi - i0);
  const ty = clamp01(fj - j0);

  const v00 = getValue(i0, j0, frame);
  const v10 = getValue(i0 + 1, j0, frame);
  const v01 = getValue(i0, j0 + 1, frame);
  const v11 = getValue(i0 + 1, j0 + 1, frame);
  if (![v00, v10, v01, v11].every(Number.isFinite)) return NaN;

  const a = v00 * (1 - tx) + v10 * tx;
  const b = v01 * (1 - tx) + v11 * tx;
  return a * (1 - ty) + b * ty;
}

function sampleVelocityAt(x, y, frame) {
  const u = sampleGridValue((i, j, f) => cell.get(`u:${i},${j}`)?.[f], x, y, frame);
  const v = sampleGridValue((i, j, f) => cell.get(`v:${i},${j}`)?.[f], x, y, frame);
  return Number.isFinite(u) && Number.isFinite(v) ? [u, v] : null;
}

function sampleSolidAt(x, y, solidFrac) {
  return sampleGridValue((i, j) => solidFrac.get(`${i},${j}`) ?? 0, x, y, 0);
}

function traceStreamline(seed, frame, solidFrac) {
  const ds = geo.Lx / 120;
  const pts = [];
  let x = seed.x, y = seed.y;

  for (let step = 0; step < 170; step++) {
    if (x < 0 || x > geo.Lx || y < -geo.Ly / 2 || y > geo.Ly / 2) break;
    if (sampleSolidAt(x, y, solidFrac) > 0.85) break;

    const vel = sampleVelocityAt(x, y, frame);
    if (!vel) break;

    const sp = Math.hypot(vel[0], vel[1]);
    if (!Number.isFinite(sp) || sp <= 0) break;

    pts.push([px(x), py(y)]);

    let ux = vel[0] / sp;
    let uy = vel[1] / sp;
    const midVel = sampleVelocityAt(x + 0.5 * ds * ux, y + 0.5 * ds * uy, frame);
    const midSpeed = midVel ? Math.hypot(midVel[0], midVel[1]) : 0;
    if (Number.isFinite(midSpeed) && midSpeed > 0) {
      ux = midVel[0] / midSpeed;
      uy = midVel[1] / midSpeed;
    }

    x += ds * ux;
    y += ds * uy;
  }

  return pts;
}

function streamlineSeeds() {
  const seeds = [];
  const inletX = 0.5 * dx;
  for (let k = 1; k <= 13; k++) {
    seeds.push({
      x: inletX,
      y: -geo.Ly / 2 + (k / 14) * geo.Ly,
    });
  }
  return seeds;
}

function drawStreamlines(frame, solidFrac) {
  if (!showStreamlines || !NX || !NY) return;

  ctx2d.save();
  ctx2d.lineCap = 'round';
  ctx2d.lineJoin = 'round';

  for (const seed of streamlineSeeds()) {
    const pts = traceStreamline(seed, frame, solidFrac);
    if (pts.length < 2) continue;

    ctx2d.beginPath();
    pts.forEach(([x, y], index) => {
      if (index === 0) ctx2d.moveTo(x, y);
      else ctx2d.lineTo(x, y);
    });
    ctx2d.strokeStyle = 'rgba(0,0,0,0.45)';
    ctx2d.lineWidth = 3;
    ctx2d.stroke();

    ctx2d.beginPath();
    pts.forEach(([x, y], index) => {
      if (index === 0) ctx2d.moveTo(x, y);
      else ctx2d.lineTo(x, y);
    });
    ctx2d.strokeStyle = 'rgba(255,255,255,0.72)';
    ctx2d.lineWidth = 1.15;
    ctx2d.stroke();
  }

  ctx2d.restore();
}

let lastFrame = 0;
const anim = api.addAnimation(times, (frame) => {
  lastFrame = frame;
  refreshGeometryParameters(frame);
  const fld = fields[mode];
  const range = fld.range(frame);
  const span = Math.max(1e-12, range.hi - range.lo);
  const angle = frameAoa(frame);
  const solidFrac = solidFracFor(angle);
  refreshColorbar(range);
  for (let i = 1; i <= NX; i++) {
    for (let j = 1; j <= NY; j++) {
      // The mask is smooth (sig in [0,1]); shade the selected field, then
      // overlay gray with opacity = sig so the differentiable boundary band
      // shows as a soft halo rather than a hard on/off cell edge.
      const m = solidFrac.get(`${i},${j}`);
      // j = 1 is the bottom row: flip the y axis for drawing.
      const rx = (i - 1) * cw, ry = (NY - j) * ch;
      ctx2d.fillStyle = api.heatColor(clamp01((fld.value(i, j, frame) - range.lo) / span));
      ctx2d.fillRect(rx, ry, cw + 1, ch + 1);
      if (m > 0.01) {
        ctx2d.fillStyle = `rgba(70,70,70,${Math.min(1, m)})`;
        ctx2d.fillRect(rx, ry, cw + 1, ch + 1);
      }
    }
  }
  try {
    drawStreamlines(frame, solidFrac);
    drawVelocityDirections(frame, solidFrac);
  } catch (e) {
    console.warn('Velocity overlay failed:', e);
    showStreamlines = false;
    showVelocityDirections = false;
  }
  drawAirfoil(angle);
  return `t = ${times[frame].toFixed(1)} s · ${fld.label} `
    + `∈ [${api.formatTick(range.lo)}, ${api.formatTick(range.hi)}]`;
}, 10000);

// Colorbar we can relabel when the field changes (api.addColorbar is static).
const bar = document.createElement('div');
bar.className = 'rumoca-live-radial-colorbar';
const grad = document.createElement('span');
grad.className = 'rumoca-live-radial-gradient';
const stops = [];
for (let i = 0; i <= 10; i++) stops.push(api.heatColor(i / 10));
grad.style.background = `linear-gradient(to right, ${stops.join(', ')})`;
const loEl = document.createElement('span'), hiEl = document.createElement('span');
bar.append(loEl, grad, hiEl);
container.appendChild(bar);
refreshColorbar = (range = fields[mode].range(lastFrame)) => {
  loEl.textContent = api.formatTick(range.lo);
  hiEl.textContent = api.formatTick(range.hi);
};
refreshColorbar();

// Field picker: pressure (default) or speed. Switching repaints the current
// frame and relabels the colorbar; no recompile or re-run needed.
const fieldRow = document.createElement('div');
fieldRow.className = 'rumoca-live-tuner';
const fieldLabel = document.createElement('span');
fieldLabel.textContent = 'Field';
const fieldSel = document.createElement('select');
[
  ['vorticity', 'Vorticity'],
  ['q', 'Pressure'],
  ['speed', 'Speed |V|'],
].forEach(([value, text]) => {
  const opt = document.createElement('option');
  opt.value = value; opt.textContent = text;
  fieldSel.appendChild(opt);
});
fieldSel.value = mode;
fieldSel.addEventListener('change', () => {
  mode = fieldSel.value;
  refreshColorbar();
  anim.redraw(lastFrame);
});
const dirLabel = document.createElement('label');
dirLabel.style.display = 'inline-flex';
dirLabel.style.alignItems = 'center';
dirLabel.style.gap = '0.35rem';
dirLabel.style.marginLeft = '0.75rem';
const dirCheck = document.createElement('input');
dirCheck.type = 'checkbox';
dirCheck.checked = showVelocityDirections;
dirCheck.addEventListener('change', () => {
  showVelocityDirections = dirCheck.checked;
  anim.redraw(lastFrame);
});
dirLabel.append(dirCheck, document.createTextNode('Velocity direction'));

const streamLabel = document.createElement('label');
streamLabel.style.display = 'inline-flex';
streamLabel.style.alignItems = 'center';
streamLabel.style.gap = '0.35rem';
streamLabel.style.marginLeft = '0.75rem';
const streamCheck = document.createElement('input');
streamCheck.type = 'checkbox';
streamCheck.checked = showStreamlines;
streamCheck.addEventListener('change', () => {
  showStreamlines = streamCheck.checked;
  anim.redraw(lastFrame);
});
streamLabel.append(streamCheck, document.createTextNode('Streamlines'));

fieldRow.append(fieldLabel, fieldSel, dirLabel, streamLabel);
container.appendChild(fieldRow);

// Pitch the airfoil. With Interactive off this is a normal pre-run `aoa`
// parameter override. With Interactive on, the same slider drives the named
// model input `aoa_cmd`; the model's `aoa_motor` state follows it with a
// first-order lag.
api.addTuner('aoa', {
  min: -45,
  max: 45,
  step: 1,
  value: aoa,
  label: 'AoA °',
  interactiveInput: 'aoa_cmd',
});

api.addTuner('mc', {
  min: 0,
  max: 0.08,
  step: 0.005,
  value: mc0,
  label: 'Camber',
  interactiveInput: 'mc',
});

api.addTuner('pc', {
  min: 0.2,
  max: 0.8,
  step: 0.05,
  value: pc0,
  label: 'Camber pos',
  interactiveInput: 'pc',
});

api.addTuner('tk', {
  min: 0.06,
  max: 0.24,
  step: 0.01,
  value: tk0,
  label: 'Thickness',
  interactiveInput: 'tk',
});
```

In the animation, the black shape is the *true* NACA 2412 contour and the
gray haze around it is the smooth solid fraction `sig` — darker toward the
core of the body, fading through the one-cell `tanh` transition band to the
fluid. That soft edge *is* the differentiable mask the flow actually feels at
this resolution. The **Field** picker chooses what the heatmap shows: it
defaults to **vorticity**, so separated shear layers, vortices, and stall are
visible directly. Switch to **Pressure** to see the stagnation/suction pattern
that drives lift, or **Speed |V|** to see the kinematics. Watch the impulsive
start settle: the stagnation point appears at the leading edge, flow
accelerates over the upper surface, and the wake trails downstream. Things to
try:

- Slide **AoA** to `0` — the wake straightens and the up/down asymmetry
  mostly disappears (the residual comes from camber, the *2* in 2412).
  This is a simulation parameter update; the GPU path refreshes the prepared
  vectors and reruns without relowering the model.
- Slide **AoA** negative — the airfoil visibly pitches nose-down and the
  suction side flips.
- Slide **AoA** toward `25`–`45` in **Interactive** mode — the upper-surface
  shear layer separates and the vorticity/streamline overlays show the
  qualitative onset of stall.
- Switch **Field** between *Vorticity*, *Pressure*, and *Speed* — the same
  run, recolored instantly with no recompile.

**Honest caveats**: at this grid and Reynolds number (`U/nu = 100`), this is a
*qualitative* separated-flow visualization, not an aerodynamic prediction. The
half-thickness is softly floored to `tmin` (a fraction of a cell) so the thin
profile stays closed, pressure uses artificial compressibility plus damping,
and central differencing limits how low the viscosity may go. Resolving
boundary layers at flight Reynolds numbers needs orders of magnitude more
cells and a more specialized incompressible-flow discretization.

## Scaling the Resolution

Increase the cell counts for finer fields. Each extra cell adds states and
equations; the structural analysis and solver scale with the system size.
For 1-D problems, tens of cells are usually plenty; for large 2-D/3-D
fields you would generate the Modelica programmatically or move to a
dedicated PDE solver. GPU-accelerated execution of large discretized fields
is on the roadmap — the targets table already includes experimental CUDA
backends (`rumoca targets`), and the same solve-IR pathway is how a
WebGPU/WGSL backend would land.
