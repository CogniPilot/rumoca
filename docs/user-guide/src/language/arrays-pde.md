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

## Scaling the Resolution

Increase the cell counts for finer fields. Each extra cell adds states and
equations; the structural analysis and solver scale with the system size.
For 1-D problems, tens of cells are usually plenty; for large 2-D/3-D
fields you would generate the Modelica programmatically or move to a
dedicated PDE solver. GPU-accelerated execution of large discretized fields
is on the roadmap — the targets table already includes experimental CUDA
backends (`rumoca targets`), and the same solve-IR pathway is how a
WebGPU/WGSL backend would land.
