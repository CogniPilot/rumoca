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
`api.addColorbar`, `api.heatColor`, …) from the book's live runner.

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
resolution is the `N` parameter — edit it (try 8 to 20) and re-run:

```modelica,interactive
model Wave2D "2-D wave equation on a square membrane, method of lines"
  parameter Integer N = 12 "Grid cells per side";
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

The heatmap below is an editable visualization script, like the turkey's.
`api.matrixField()` collects the `u[i,j]` states into a grid; the script
draws each cell, colored by displacement:

```js,rumoca-viz
// Animate the membrane displacement field u[i,j] as a heatmap.
const field = api.matrixField();
const { vMin, vMax } = api.valueRange(field.members);
const span = Math.max(Math.abs(vMin), Math.abs(vMax)) || 1;

const size = 300;
const { ctx2d } = api.makeCanvas(size, size);
const cell = size / field.rows;

api.addAnimation(times, (frame) => {
  for (let i = 1; i <= field.rows; i++) {
    for (let j = 1; j <= field.cols; j++) {
      const u = field.at(i, j)[frame];
      // Map displacement [-span, span] onto the color scale.
      ctx2d.fillStyle = api.heatColor(0.5 + 0.5 * (u / span));
      ctx2d.fillRect((i - 1) * cell, (j - 1) * cell, cell + 1, cell + 1);
    }
  }
  return `t = ${api.formatClock(times[frame])}`;
}, 8000);

api.addColorbar(-span, span, api.heatColor);
```

Notice the cost of resolution: each cell adds two states (`u` and `w`), so
`N = 20` is an 800-state system. Rumoca compiles and simulates it fine, but
output volume grows as `N²` — keep `Interval` coarse enough for the
browser.

## 2-D Navier–Stokes: Flow over a NACA 2412 Airfoil

The same machinery scales up to fluid dynamics. This example solves the 2-D
incompressible Navier–Stokes equations around a NACA 2412 airfoil at an
adjustable angle of attack, using two classic tricks that keep the system a
pure ODE — exactly what the method of lines wants:

- **Artificial compressibility**: instead of the pressure-Poisson algebraic
  constraint, pressure gets its own fast dynamics
  `der(q) = -cs² · div(V)`. The continuity error propagates away as an
  artificial acoustic wave, and no algebraic loop is needed.
- **Brinkman penalization**: the airfoil is not meshed. Cells inside the
  NACA 2412 outline (computed per grid column from the standard camber and
  thickness polynomials) get a strong drag term `-sig·V/tau` that drives the
  velocity to zero, so the flow sees a solid body on a plain Cartesian grid.

The angle of attack enters only through the freestream direction `(uin,
vin)` — edit `aoa` and re-run. This example defaults the **GPU** checkbox
on: the compiler's experimental `wgsl-solve` backend lowers the system to
WebGPU compute kernels and an in-page RK4 integrator runs them — about
5× faster than the CPU (WASM) path even on a software GPU adapter. If
WebGPU is unavailable the run fails with a clear error instead of
silently falling back (uncheck GPU for the CPU path). GPU v1 runs in
f32 with events and algebraics frozen at their initial values, which is
exact for this model's constant geometry masks. The run is an impulsive wind-tunnel start:
the field begins at rest and the freestream sweeps in from the inlet and
far-field boundaries. This is the heaviest example in the book
(~3,500 unknowns after unrolling): expect the first run to take a while.

```modelica,interactive,gpu
model AirfoilFlow "2-D flow over a NACA 2412: artificial compressibility + penalization"
  parameter Integer NX = 30 "Cells along the channel";
  parameter Integer NY = 18 "Cells across the channel";
  parameter Real Lx = 4.0 "Domain length [chords]";
  parameter Real Ly = 1.5 "Domain height [chords]";
  parameter Real xle = 1.0 "Leading edge distance from inlet [chords]";
  parameter Real aoa = 8.0 "Angle of attack [deg]";
  parameter Real U = 1.0 "Freestream speed";
  parameter Real nu = 0.05 "Kinematic viscosity (Re = U/nu = 20)";
  parameter Real cs = 3.0 "Artificial-compressibility wave speed";
  parameter Real tau = 0.02 "Solid penalization time constant [s]";
  parameter Real taub = 0.05 "Boundary relaxation time constant [s]";
  parameter Real mc = 0.02 "NACA 2412 max camber";
  parameter Real pc = 0.4 "NACA 2412 camber position";
  parameter Real tk = 0.12 "NACA 2412 thickness";
  parameter Real dx = Lx / NX;
  parameter Real dy = Ly / NY;
  parameter Real pi = 3.14159265359;
  parameter Real uin = U * cos(aoa * pi / 180.0);
  parameter Real vin = U * sin(aoa * pi / 180.0);
  Real u[NX, NY] "x-velocity";
  Real v[NX, NY] "y-velocity";
  Real q[NX, NY] "pressure / rho";
  Real s[NX] "Chordwise coordinate of each column";
  Real ycam[NX] "Camber line height per column";
  Real ythk[NX] "Half thickness per column";
  Real sig[NX, NY] "Solid mask (1 inside the airfoil)";
  // States start at rest (default start = 0): an impulsive wind-tunnel
  // start where the freestream sweeps in through the boundary relaxation.
equation
  // NACA 2412 geometry, evaluated per grid column.
  for i in 1:NX loop
    s[i] = (i - 0.5) * dx - xle;
    ycam[i] = if s[i] < 0.0 or s[i] > 1.0 then 0.0
      elseif s[i] < pc then mc / pc ^ 2 * (2.0 * pc * s[i] - s[i] ^ 2)
      else mc / (1.0 - pc) ^ 2 * ((1.0 - 2.0 * pc) + 2.0 * pc * s[i] - s[i] ^ 2);
    // Half thickness, floored to one grid cell so the coarse mask stays closed.
    ythk[i] = if s[i] < 0.0 or s[i] > 1.0 then 0.0
      else max(0.8 * dy,
        5.0 * tk * (0.2969 * sqrt(max(s[i], 0.0)) - 0.1260 * s[i] - 0.3516 * s[i] ^ 2
                    + 0.2843 * s[i] ^ 3 - 0.1036 * s[i] ^ 4));
    for j in 1:NY loop
      sig[i, j] = if ythk[i] > 0.0
          and abs((j - 0.5) * dy - Ly / 2.0 - ycam[i]) <= ythk[i] then 1.0 else 0.0;
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
                              + (v[i, j + 1] - v[i, j - 1]) / (2.0 * dy));
    end for;
  end for;
  // Inlet (left): freestream; pressure zero-gradient.
  for j in 1:NY loop
    der(u[1, j]) = (uin - u[1, j]) / taub;
    der(v[1, j]) = (vin - v[1, j]) / taub;
    der(q[1, j]) = (q[2, j] - q[1, j]) / taub;
    // Outlet (right): zero-gradient velocities, reference pressure.
    der(u[NX, j]) = (u[NX - 1, j] - u[NX, j]) / taub;
    der(v[NX, j]) = (v[NX - 1, j] - v[NX, j]) / taub;
    der(q[NX, j]) = (0.0 - q[NX, j]) / taub;
  end for;
  // Far field (top/bottom): freestream; pressure zero-gradient.
  for i in 2:NX - 1 loop
    der(u[i, 1]) = (uin - u[i, 1]) / taub;
    der(v[i, 1]) = (vin - v[i, 1]) / taub;
    der(q[i, 1]) = (q[i, 2] - q[i, 1]) / taub;
    der(u[i, NY]) = (uin - u[i, NY]) / taub;
    der(v[i, NY]) = (vin - v[i, NY]) / taub;
    der(q[i, NY]) = (q[i, 2] - q[i, NY]) / taub;
  end for;
  annotation(experiment(StopTime = 2.5, Interval = 0.0125, Solver = "rk-like"));
end AirfoilFlow;
```

```js,rumoca-viz
// Speed heatmap |V(x,y,t)| with the penalized cells in gray and the true
// NACA 2412 contour drawn on top. Grid size is discovered from the result
// names, so editing NX/NY in the model just works.
const cell = new Map();
let NX = 0, NY = 0;
names.forEach((n, k) => {
  const m = /^([uvq]|sig)\[(\d+),(\d+)\]$/.exec(n);
  if (!m) return;
  cell.set(`${m[1]}:${m[2]},${m[3]}`, data[k]);
  if (m[1] === 'u') {
    NX = Math.max(NX, Number(m[2]));
    NY = Math.max(NY, Number(m[3]));
  }
});
const speed = (i, j, f) => Math.hypot(
  cell.get(`u:${i},${j}`)[f], cell.get(`v:${i},${j}`)[f]);
let vMax = 0;
for (let f = 0; f < times.length; f += 5) {
  for (let i = 1; i <= NX; i++) {
    for (let j = 1; j <= NY; j++) {
      vMax = Math.max(vMax, speed(i, j, f));
    }
  }
}
// The mask is constant; sample it at the end of the run (algebraic values
// at the very first output point may not be settled yet).
const maskFrame = times.length - 1;

// Geometry for the overlay — keep in sync with the model parameters.
const geo = { Lx: 4.0, Ly: 1.5, xle: 1.0, mc: 0.02, pc: 0.4, tk: 0.12 };
const camber = (sc) => sc < geo.pc
  ? geo.mc / geo.pc ** 2 * (2 * geo.pc * sc - sc ** 2)
  : geo.mc / (1 - geo.pc) ** 2 * ((1 - 2 * geo.pc) + 2 * geo.pc * sc - sc ** 2);
const halfThick = (sc) => 5 * geo.tk * (0.2969 * Math.sqrt(sc) - 0.1260 * sc
  - 0.3516 * sc ** 2 + 0.2843 * sc ** 3 - 0.1036 * sc ** 4);

const W = 600;
const H = Math.round(W * (geo.Ly / geo.Lx));
const { ctx2d } = api.makeCanvas(W, H);
const cw = W / NX;
const ch = H / NY;
const px = (x) => (x / geo.Lx) * W;                    // physical x -> canvas
const py = (y) => H - ((y + geo.Ly / 2) / geo.Ly) * H; // physical y -> canvas

function drawAirfoil() {
  ctx2d.beginPath();
  for (let k = 0; k <= 60; k++) {            // upper surface, LE -> TE
    const sc = k / 60;
    const fn = k === 0 ? 'moveTo' : 'lineTo';
    ctx2d[fn](px(geo.xle + sc), py(camber(sc) + halfThick(sc)));
  }
  for (let k = 60; k >= 0; k--) {            // lower surface, TE -> LE
    const sc = k / 60;
    ctx2d.lineTo(px(geo.xle + sc), py(camber(sc) - halfThick(sc)));
  }
  ctx2d.closePath();
  ctx2d.fillStyle = '#111';
  ctx2d.fill();
  ctx2d.strokeStyle = '#fff';
  ctx2d.lineWidth = 1;
  ctx2d.stroke();
}

api.addAnimation(times, (frame) => {
  for (let i = 1; i <= NX; i++) {
    for (let j = 1; j <= NY; j++) {
      const solid = cell.get(`sig:${i},${j}`)[maskFrame] > 0.5;
      ctx2d.fillStyle = solid
        ? '#666'
        : api.heatColor(speed(i, j, frame) / vMax);
      // j = 1 is the bottom row: flip the y axis for drawing.
      ctx2d.fillRect((i - 1) * cw, (NY - j) * ch, cw + 1, ch + 1);
    }
  }
  drawAirfoil();
  return `t = ${api.formatTick(times[frame])} s · max |V| = ${api.formatTick(vMax)}`;
}, 10000);

api.addColorbar(0, vMax, api.heatColor);
```

In the animation, the black shape is the *true* NACA 2412 contour and the
gray blocks around it are the penalized grid cells — the body the flow
actually feels at this resolution. Watch the impulsive start settle: the
stagnation point appears at the leading edge (dark blue), flow accelerates
over the upper surface (red), and a slow wake trails downstream. The pressure field (plotted below the
animation as the most dynamic states) shows the suction side developing —
the lift. Things to try:

- `aoa = 0.0` — the wake straightens and the up/down asymmetry mostly
  disappears (the residual comes from camber, which is the *2* in 2412).
- `aoa = -8.0` — the suction side flips.
- `nu = 0.02` — less viscosity, sharper wake (drop `Interval` and the
  solver step accordingly if it goes unstable).

**Honest caveats**: at this grid (~0.1 chord cells) and Reynolds number
(`U/nu = 20`), this is a *qualitative* low-Re flow — a teaching
visualization, not an aerodynamic prediction. The thickness polynomial is
floored to one grid cell so the thin profile stays closed, and central
differencing limits how low the viscosity may go. Resolving boundary layers
at flight Reynolds numbers needs orders of magnitude more cells, which is
GPU-backend territory (see the roadmap note below).

## Scaling the Resolution

Increase the cell counts for finer fields. Each extra cell adds states and
equations; the structural analysis and solver scale with the system size.
For 1-D problems, tens of cells are usually plenty; for large 2-D/3-D
fields you would generate the Modelica programmatically or move to a
dedicated PDE solver. GPU-accelerated execution of large discretized fields
is on the roadmap — the targets table already includes experimental CUDA
backends (`rumoca targets`), and the same solve-IR pathway is how a
WebGPU/WGSL backend would land.
