# Sport Cub firmware Monte Carlo

This is a pure-Python Monte Carlo mission example for the HobbyZone Sport Cub
S2. It sweeps commanded cruise velocity from **1 m/s through 5 m/s in 0.5 m/s
steps** and writes both the raw trial metrics and publication-ready figures.

`sport_cub_firmware.py` exposes one public closed-loop model,
`SportCubFirmwareModel`, composed of:

- the identified CUBS2 6-DOF Sport Cub aerodynamic model;
- the onboard HobbyZone SAFE attitude/rate stabilizer approximation; and
- the 50 Hz `FixedWingOuterLoop` state update from the firmware source of truth,
  `cerebri_cubs2/src/FixedWingOuterLoop.mo`, at commit
  `e25c108fc2c4762fda8303594b82eb8ddccee0ad`.

The closed-loop signal path is:

```text
Zephyr FixedWingOuterLoop → PWM/SAFE receiver behavior → 6-DOF Sport Cub physics
             ↑                                            │
             └────────────── pose feedback ────────────────┘
```

`monte_carlo.py` only creates parameter draws and calls this model; it does not
contain another controller or airframe implementation. The aileron/elevator
polarities match `cerebri_cubs2/src/main.c`, which maps the generated controller
outputs onto the physical receiver channels.

The controller gains, TECS limits, estimator, and waypoint logic are kept
together in that model. The example configures a closed four-waypoint circuit:
`(-4,-5) → (-3,2) → (16.2,2) → (16,-4.22) → (-4,-5)`, at 3 m altitude.
`SportCubFirmwareMission.mo` is the single-source Modelica comparison model; it
is not invoked by the Monte Carlo runner. The Python implementation is used for
the sweep so hundreds of 60 s missions do not pay Rumoca's interpreted solver
cost on every trial.

## Run

From this directory, using the examples environment:

```bash
../../.venv/bin/python monte_carlo.py
```

The defaults run 16 trials at each of the 9 velocities for a 60 s mission. Use
a small smoke run while editing:

```bash
../../.venv/bin/python monte_carlo.py \
  --samples 2 --duration 10 --workers 2 --output-dir /tmp/sport-cub-smoke
```

The script writes:

- `figures/mission_trajectories.png` — nominal route and trajectory at every
  commanded velocity;
- `figures/airspeed_tracking.png` — commanded and actual nominal airspeed at
  every commanded velocity;
- `figures/mission_performance.png` — median and 10–90% mission metrics; and
- `figures/mission_performance.csv` — one row per Monte Carlo trial.

The checked-in 16-trial, seed-7 sweep produced:

| Cruise command | Success | Median actual airspeed | Median airspeed RMSE | Median cross-track RMSE |
|---:|---:|---:|---:|---:|
| 1.0 m/s | 0% | 4.86 m/s | 3.89 m/s | 3.07 m |
| 1.5 m/s | 0% | 4.86 m/s | 3.40 m/s | 3.07 m |
| 2.0 m/s | 0% | 4.86 m/s | 2.91 m/s | 3.07 m |
| 2.5 m/s | 0% | 4.86 m/s | 2.42 m/s | 3.07 m |
| 3.0 m/s | 0% | 4.86 m/s | 1.93 m/s | 3.07 m |
| 3.5 m/s | 0% | 4.86 m/s | 1.46 m/s | 3.05 m |
| 4.0 m/s | 44% | 4.86 m/s | 1.01 m/s | 3.05 m |
| 4.5 m/s | 100% | 4.91 m/s | 0.66 m/s | 3.09 m |
| 5.0 m/s | 100% | 5.32 m/s | 0.63 m/s | 3.38 m |

Every trial completed the 60 s route without ground impact. The median was 3.75
circuits through 4.5 m/s and 4.00 circuits at 5.0 m/s. The success transition is
therefore driven by airspeed tracking: this firmware/airframe combination
remains near its natural 4–5 m/s operating range rather than slowing to the
lower commands.

The overlapping low-speed cases are controller behavior, not a disconnected
Monte Carlo parameter. The firmware first computes
`K_V * (vCruise - abs(v_est))`, then clips that request between
`-envelopeDrag / weight` and `(thrustMax - envelopeDrag) / weight`. With the
deployed parameters, every requested acceleration is limited to approximately
`[-0.113, 0.372]`. The firmware also estimates acceleration as the filtered
per-sample speed difference without dividing by `dt`; the Python model retains
both details so it represents the deployed controller rather than correcting
it inside the study.

![Monte Carlo mission performance](figures/mission_performance.png)

![Commanded and actual airspeed](figures/airspeed_tracking.png)

![Nominal mission trajectories](figures/mission_trajectories.png)

## Monte Carlo definition

Trial zero is nominal. The remaining trials use common random numbers across
all cruise velocities and independently perturb:

| Quantity | Distribution (1 sigma) |
|---|---:|
| mass | 4% |
| lift-curve slope `CLa` | 6% |
| parasitic drag `CD0` | 10% |
| maximum thrust | 5% |
| initial airspeed about 4 m/s | 0.15 m/s |
| initial heading | 2 degrees |

The mission starts airborne at 3 m at the first of the four waypoints, aligned
with the first route leg. This keeps the study focused on cruise guidance
instead of conflating low cruise commands with ground-roll and launch
performance. A trial succeeds when it completes the requested duration,
completes at least one route circuit, stays above the ground, and holds airspeed
RMSE within `max(0.5 m/s, 25% of the command)`.

This remains a simulation study: wind, mocap dropouts, actuator dynamics, and
ground contact are outside its scope.
