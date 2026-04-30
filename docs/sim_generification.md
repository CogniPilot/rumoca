# Simulation generification — session notes + current architecture

This document records the refactor that turned Rumoca's hardcoded quadrotor
SIL app into a generic, config-driven simulation framework. It covers:

1. [What we did](#1-what-we-did) — the sequence of changes
2. [How the simulation works now](#2-how-the-simulation-works-now) — architecture + data flow
3. [What still needs to be done](#3-what-still-needs-to-be-done) — open items + testing

---

## 1. What we did

### Starting point

A single crate, `rumoca-sim-fb`, contained ~1000 LOC of code that could
only simulate one vehicle (a quadrotor) coupled to one specific autopilot
(Cerebri) over one specific protocol (FlatBuffer-encoded UDP). The
physics, wire format, input handling, and viewer were all locked together.

Every layer was hardcoded in Rust:

- Model: `QuadrotorSIL.mo`, referenced by name string throughout
- Wire format: `FlatBuffer` codec, with `gyro_x`/`accel_x`/`rc_0..15`
  names baked into the code
- Input: 16 RC channels, arm/disarm state machine, gamepad/keyboard
  handlers all hardcoded for quadrotor conventions
- Viewer scene: a quadrotor, with `px/py/pz/q0..q3/omega_m1..4` keys
  hardcoded in the WebSocket JSON

### Goal

Move to a shape where the vehicle is data, not code. Users should be
able to simulate any Modelica model by providing three files:

- `.mo` — physics
- `.toml` — everything else (autopilot, transport, codec, inputs, viewer)
- `.js` — Three.js scene

And each swappable concern (transport, codec, solver, input device)
should live in its own crate so combinations can be mixed without forking
the framework.

### Naming scheme proposal

We adopted the structure documented in
`rumoca_naming_scheme_architecture_proposal.md`: each concern is one
axis, each axis lives in one crate family.

| Family | Purpose |
|---|---|
| `rumoca-codec`, `rumoca-codec-flatbuffers` | Wire format |
| `rumoca-transport-udp`, `-websocket` | Byte transport |
| `rumoca-solver-diffsol`, `-rk45` | Numerical backend |
| `rumoca-input`, `-gamepad`, `-keyboard` | Input device |
| `rumoca-viz-web` | HTTP viewer + assets |
| `rumoca-compile::config` | Shared TOML facade |
| `rumoca` (CLI) | Top-level composition |

### The commits

Twenty commits on branch `pr_arch_fix` (first to last):

| Commit | What |
|---|---|
| `942d490` | Rename 4 crates to the layered naming scheme |
| `aa89d99` | Lock the TOML schema (`quadrotor.toml`) + add Rust types |
| `6598914` | Add config-driven input engine (24 unit tests) |
| `da5c3c6` | Add config-driven signal mapper (7 unit tests) |
| `253047e` | Rewrite `sim_loop.rs` to be fully config-driven; delete ~400 LOC of hardcoded quadrotor code |
| `86231ea` | Comply with SPEC_0025 (remove `#[allow(clippy::*)]` via refactor) |
| `d7c277c` | Make autopilot/FB coupling optional (enable standalone mode) |
| `ba768ee` | Add standalone rover example (proof of generalization) |
| `fb2f6fa` | Extract `rumoca-transport-udp` |
| `0958464` | Extract `rumoca-transport-websocket`; move HTTP viewer to `rumoca-viz-web` |
| `8450fa6` | Extract `rumoca-input` (engine + signal mapper + config types) |
| `60cb434` | Move `SimulationConfig` to `rumoca-compile::config` |
| `51b6332` | Dissolve `rumoca-sim-fb`: move executor into the CLI |
| `43e697f2` | CLI: `sim-fb` → `sim {run,check,init}` |
| `fb38ffc` | Extract `rumoca-input-gamepad` + `rumoca-input-keyboard` |
| `be4d757` | Rename `sim-fb` feature → `sim`; write real quadrotor + rover docs |
| `96b3b42` | Track `viewer.html` (was silently gitignored) + add pipeline diagnostics |

Plus three pre-existing commits on the branch (`d50ce0f`, `d690ba5`,
`7f00c8c`, `f4f1d8a`, `6e34583`) from earlier arch work, unchanged.

### Size delta

| Area | Before | After |
|---|---|---|
| `rumoca-sim-fb/sim_loop.rs` | 809 LOC (mostly quadrotor code) | deleted |
| `rumoca-sim-fb` crate | ~1000 LOC, 4 files | deleted |
| New `rumoca-input` | 0 | ~1700 LOC + 31 tests |
| New `rumoca-input-{gamepad,keyboard}` | 0 | ~200 LOC + 5 tests |
| New `rumoca-transport-{udp,websocket}` | 0 | ~200 LOC |
| New `SimulationConfig` in session | 0 | ~250 LOC |
| New CLI `sim_fb` module | 0 | ~500 LOC |

Per-vehicle content is now three files (`.mo` + `.toml` + `.js`).
Zero Rust code per vehicle.

---

## 2. How the simulation works now

### One command

```bash
rumoca lockstep run -c examples/quadrotor_sil/quadrotor.toml
```

That loads the TOML, compiles the Modelica model, wires up input devices
+ codec + transports + viewer, and enters a lockstep loop.

### The TOML is the full spec

A single file describes everything:

```toml
[model]
file = "QuadrotorSIL.mo"          # relative to this TOML's directory
name = "QuadrotorSIL"

[sim]
dt = 0.00125                       # simulation timestep [s]
realtime = true                    # wall-clock paced

[autopilot]
command = "/path/to/autopilot-binary"

[transport.udp]
listen = "0.0.0.0:4244"
send   = "127.0.0.1:4242"

[transport.websocket]
port = 8081

[transport.http]
port  = 8080
scene = "quadrotor_scene.js"       # relative to TOML

# FlatBuffer codec layer
[schema]
bfbs = ["/path/to/schema.bfbs"]

[receive]
root_type = "your.topic.MotorOutput"

[receive.route]
"motors.m0" = { to = "stepper:omega_m1", scale = 1100.0 }
# ...

[send]
root_type = "your.topic.SimInput"

[send.route]
"gyro.x" = { key = "gyro_x" }
# ...

# Input engine
[locals]
armed    = { type = "bool", default = false }
throttle = { type = "float", default = 0.0 }
# ...

[derive]
"rc.2" = { from = "throttle", scale = 1000, offset = 1000, clamp = [1000, 2000] }
# ...

[input]
mode = "auto"                      # gamepad | keyboard | auto

[input.gamepad.axes]
roll = { source = "RightStickX", write = "roll_cmd" }
# ...

# Signal routing
[signals.send]
gyro_x = "stepper:gyro_x"
rc_valid = { from = "runtime:input_connected", when_true = 1, when_false = 0 }
# ...

[signals.viewer]
t  = "stepper:time"
px = "stepper:px"
# ...

[signals.stepper_inputs]           # used in standalone mode (no autopilot)
# throttle = "local:throttle_cmd"  # gamepad drives stepper directly

[reset]
on_signal = "reset"
reset_locals = true
rebuild_stepper = true
```

### Two modes: autopilot-coupled vs standalone

**Autopilot-coupled** (quadrotor example): `[schema]` + `[receive]` +
`[send]` + `[autopilot]` all present. The sim spawns the autopilot as
a subprocess, receives motor commands over UDP, steps physics, sends
sensor readings back. Gamepad input goes to the autopilot as RC
channels — not directly to the model.

**Standalone** (rover example): no `[schema]`/`[receive]`/`[send]`/
`[autopilot]` — those sections are all optional. `[signals.stepper_inputs]`
wires gamepad-driven local values straight to the model's inputs. No
external process, no wire format.

The runtime auto-detects which mode from the config. See
`SimulationConfig::has_fb()` and `validate()` in
[rumoca-compile/src/config.rs](../crates/rumoca-compile/src/config.rs).

### Two pacing modes

The outer loop has two modes, picked by `[sim].mode` (or defaulted by
whether autopilot coupling is configured):

**Lockstep** (default when `[schema]`/`[receive]`/`[send]` are present):

```
┌─ poll input engine + handle engine signals ─────────────┐
└─────────────────────────────────────────────────────────┘
                     ↓
┌─ BLOCK on UDP recv (one autopilot packet) ──────────────┐
│   unpack FB → apply to stepper inputs / locals          │
└─────────────────────────────────────────────────────────┘
                     ↓
┌─ stepper.step(dt) — fine sub-steps at MAX_SUB_DT ───────┐
└─────────────────────────────────────────────────────────┘
                     ↓
┌─ build SignalFrame + viewer JSON ──────────────────────┐
│   pack, send UDP back to autopilot; push JSON to WS     │
└─────────────────────────────────────────────────────────┘
                     ↓
                  (repeat)
```

The autopilot paces the simulation. Deterministic across runs. No
wall-clock sleep — if the autopilot is slow, the sim is slow too. If
the autopilot crashes, the sim blocks on recv (with 100 ms socket
timeout to stay responsive to SIGINT).

**Free-run** (default for standalone, no autopilot):

```
┌─ poll input engine + handle engine signals ─────────────┐
└─────────────────────────────────────────────────────────┘
                     ↓
┌─ drain UDP non-blocking (apply whatever arrived) ───────┐
└─────────────────────────────────────────────────────────┘
                     ↓
┌─ stepper.step(dt) ──────────────────────────────────────┐
└─────────────────────────────────────────────────────────┘
                     ↓
┌─ apply [signals.stepper_inputs] (standalone) ───────────┐
│   build + send SignalFrame (if coupled)                 │
│   push viewer JSON                                      │
└─────────────────────────────────────────────────────────┘
                     ↓
┌─ realtime pacing: sleep until dt has elapsed ───────────┐
└─────────────────────────────────────────────────────────┘
```

Physics advances every `dt` regardless of what the autopilot is doing.
Good for the rover (no autopilot) and "just keep the viewer responsive"
modes. Not deterministic with an autopilot attached.

### Sub-stepping

Inside `stepper.step(dt)`, a `MAX_SUB_DT = 0.002s` guards the numerical
integrator. If `dt > MAX_SUB_DT`, physics is split into `ceil(dt / 0.002)`
internal sub-steps. This lets the outer loop run slowly (autopilot's
~50 Hz = 20 ms) while keeping physics resolution fine. The user sets
`[sim].dt` to match the autopilot's control period.

### The axes

Each axis lives in its own crate family and can be swapped independently.
The CLI (`rumoca`) composes them from the TOML:

```
         ┌──────────────────── rumoca (CLI) ─────────────────────┐
         │   loads SimulationConfig, wires pieces, runs loop     │
         └───────────────────────────────────────────────────────┘
              │          │         │         │         │
      ┌───────▼──┐ ┌─────▼───┐ ┌──▼──────┐ ┌▼───────┐ ┌▼───────────┐
      │  input   │ │transport│ │  codec  │ │ solver │ │  viz-web   │
      ├──────────┤ ├─────────┤ ├─────────┤ ├────────┤ ├────────────┤
      │ -gamepad │ │  -udp   │ │  -flat  │ │-diffsol│ │ HTTP shell │
      │ -keyboard│ │  -ws    │ │  buffers│ │ -rk45  │ │ + viewer   │
      └──────────┘ └─────────┘ └─────────┘ └────────┘ └────────────┘
```

**Codec** (`rumoca-codec` + implementations)
Converts between a logical `SignalFrame` (a map of string → f64) and
concrete wire bytes. `rumoca-codec-flatbuffers` is the only
implementation today; adding `rumoca-codec-protobuf` is a matter of
writing one crate and no changes to the framework.

**Transport** (`rumoca-transport-udp`, `-websocket`)
Moves bytes. `UdpTransport::drain` and `::send` are the whole surface.
Browser WebSocket server lives in `rumoca-transport-websocket`. Adding
a `rumoca-transport-zenoh` or `rumoca-transport-ros2` is again one
crate.

**Solver** (`rumoca-solver-diffsol`, `-rk45`)
Numerical DAE integrators. Selection is currently build-time. Runtime
swap via config key is a future addition.

**Input** (`rumoca-input` + `-gamepad` + `-keyboard`)
`rumoca-input` owns the state machine: local store, action dispatcher
(precondition + debounce + fire), derive rules, signal mapper. Device
crates own native polling deps (`gilrs`, `crossterm`) and translate
concrete events into `rumoca-input`'s abstract snapshots/events.

**Viz** (`rumoca-viz-web`)
HTTP server serves `viewer.html` (Three.js shell). `viewer.html` loads
a scene script (`--scene <path>` or `[transport.http].scene`) that
renders from WebSocket state. Wasm-clean for the compile parts; the
HTTP server is native-only.

### Composition: where does glue live?

- **Rust composition**: `crates/rumoca/src/sim/` in the CLI binary.
  - `mod.rs` — `SimArgs` + `run()` (setup: compile model, build schema
    set, spawn HTTP server)
  - `executor.rs` — `FrameCtx` + `run_sim_loop` (per-frame orchestration)
- **TOML composition**: the user's `.toml` file chooses which axes to
  instantiate and how to wire them
- **No intermediate crate**: the old `rumoca-sim-fb` that existed to
  glue things together is gone; composition happens directly in the
  CLI, TOML-driven

### Signal references

The TOML has a mini reference language for signal values:

| Reference | Meaning |
|---|---|
| `"stepper:foo"` | Read `foo` from the compiled Modelica model |
| `"stepper:time"` | Current simulation time |
| `"local:foo"` | Read from the input engine's local store |
| `"local:rc.2"` | Element 2 of array local `rc` |
| `"runtime:frame_num"` | Integer frame counter |
| `"runtime:wall_ms"` | Wall-clock ms since epoch |
| `"runtime:input_connected"` | Bool, device present |
| `"runtime:input_mode"` | `"gamepad"` or `"keyboard"` |
| `{ const = 1.0 }` | Constant |
| `{ from = "...", default = 1.0 }` | Fallback if source returns None |
| `{ from = "...", when_true = X, when_false = Y }` | Bool branch |

These show up in `[signals.send]`, `[signals.viewer]`,
`[signals.stepper_inputs]`.

### Input engine primitives

Defined in the TOML with a small vocabulary:

| Primitive | TOML section |
|---|---|
| Axis (continuous value) | `[input.gamepad.axes.*]` |
| Integrator (accumulate source × rate × dt) | `[input.*.integrators.*]` |
| Toggle button (bool flip + debounce + precondition) | `[input.gamepad.buttons.arm]` |
| Signal button (one-shot named signal) | `[input.gamepad.buttons.reset]` |
| Key binding (set/toggle/signal) | `[input.keyboard.keys]` |
| Decay (exponential falloff per frame) | `[input.keyboard.decay]` |

Preconditions use a small string expression parser — `"rc.2 <= 1050"`,
where operators are `<`, `<=`, `==`, `!=`, `>=`, `>`.

### Mode semantics (gamepad + keyboard)

`[input].mode`: `"auto"`, `"gamepad"`, or `"keyboard"`.

- **Gamepad primary** (auto with gamepad detected, or explicit):
  gamepad polled fully; keyboard polled fully but decay is skipped.
  Poll order is keyboard-then-gamepad, so gamepad wins any write
  conflicts.
- **Keyboard primary**: gamepad not polled (even if plugged in);
  keyboard polled fully with decay.

This lets keyboard hotkeys (reset, log, quit) work alongside a gamepad
without the user having to duplicate those bindings under both sections.

---

## 3. What still needs to be done

### Open items (prioritized)

**1. End-to-end rover test** — *not yet run*. The quadrotor has been
verified running with Cerebri (800 Hz loop, real packet exchange). The
rover example hasn't been driven end-to-end; the model may or may not
compile through rumoca's pipeline on first try. Fix is probably a one-
or two-line Modelica tweak if anything.

**2. Quadrotor camera follow** — *cosmetic bug*. The scene script
updates the quadrotor mesh position correctly, but never updates
`api.cam.target`. As soon as the drone moves significantly, it flies
out of the camera's field of view. Fix is a one-line addition to
`quadrotor_scene.js`'s `onFrame` — set `api.cam.target` to the drone's
world-frame position.

**3. Optional hover-default mode** — *feature*. Today if the autopilot
isn't arming, the drone falls because motor inputs are zero. A config
flag like `[sim].initial_hover = true` could pre-load the stepper
inputs to hover-omega (~759.5 rad/s for the default physics) so a
browser demo works without Cerebri needing to arm. Useful for "just
show me the viewer is alive" checks.

**4. Input device split** — *implemented*. `rumoca-input` defines the
abstract input identifiers, config compilation, local state machine, and
signal mapper. `rumoca-input-gamepad` and `rumoca-input-keyboard` own
native polling and convert device-specific events into those abstract
snapshots/events. A formal trait is only worth adding when a second caller
needs dynamic device polymorphism.

**5. Wasm package shape** — *updated from proposal item 8*.
`rumoca-bind-wasm` remains one Rust crate with compile/template exports
enabled by default and simulation exports behind optional Cargo features.
If npm needs separate install surfaces, publish separate npm artifacts from
feature-selected builds instead of splitting the Rust binding crate.

**6. Docs polish**
- `README.md` at repo root doesn't mention `rumoca lockstep run` anywhere
- `docs/user-guide` book doesn't cover simulation at all
- A top-level "how to simulate any vehicle" tutorial (rover → custom)
  would be helpful

### Testing plan

What's been validated:

| Level | Status |
|---|---|
| Unit tests (`cargo test --workspace`) | ✓ passing (minus pre-existing ball_example_* drift) |
| Clippy (`cargo clippy ... -- -D warnings`) | ✓ clean |
| Wasm build (`cargo check --target wasm32-unknown-unknown`) | ✓ clean |
| Architecture hardening (`cargo test architecture_hardening_test`) | ✓ 29/29 |
| `lockstep check` on both example configs | ✓ |
| Quadrotor end-to-end (real Cerebri) | ✓ 800 Hz, 52 pkts/sec steady state |
| Rover end-to-end | ✗ not attempted |

What still needs manual testing:

- **Rover standalone flight** — does `Rover.mo` compile? Does the
  gamepad drive `forward_cmd` / `turn_cmd` correctly? Does the rover
  show up in the viewer and move?
- **Keyboard mode on bare terminal** — gamepad has been the primary
  driver in verified runs; keyboard raw mode needs a real TTY (IDE
  integrated terminals vary; `--debug` helps diagnose)
- **`lockstep init` output roundtrip** — edit the template output, fill in
  real values, does `lockstep check` pass and `lockstep run` succeed?
- **Reset across autopilot lifecycle** — does `[reset].restart_autopilot`
  actually kill and respawn Cerebri?
- **Config path portability** — configs reference `.mo`, `.js`, `.bfbs`
  by path. Relative paths are resolved relative to the TOML's
  directory; confirm this works when run from different cwds.

### Pre-existing test failures (not our problem)

Three `ball_example_*` tests fail due to a duplicate `RigidBodyQuat`
class between `examples/QuadrotorAttitude.mo` and another example — a
pre-existing issue on `pr_arch_fix` unrelated to this refactor.

### Known working-tree noise

These files appear in `git status` but should not be committed:

- `crates/rumoca-phase-parse/src/generated/modelica_{grammar_trait,parser}.rs`
  — regenerated by parol every build; header-comment drift between
  parol patch versions. See the prior `Refresh generated parser file
  headers` commit for precedent.
- `crates/rumoca-bind-wasm/web/rumoca_bind_wasm.{js,wasm}` — wasm-pack
  build artifacts.

### Useful references

- Full naming scheme: [`rumoca_naming_scheme_architecture_proposal.md`](../../Downloads/rumoca_naming_scheme_architecture_proposal.md) (in user's Downloads)
- PR review checklist: [`spec/SPEC_0025_PR_REVIEW_PROCESS.md`](../spec/SPEC_0025_PR_REVIEW_PROCESS.md)
- Complexity limits: [`spec/SPEC_0021_CODE_COMPLEXITY.md`](../spec/SPEC_0021_CODE_COMPLEXITY.md)
- Crate boundary rules: [`spec/SPEC_0023_CRATE_ARCHITECTURE.md`](../spec/SPEC_0023_CRATE_ARCHITECTURE.md), [`SPEC_0029_CRATE_BOUNDARIES.md`](../spec/SPEC_0029_CRATE_BOUNDARIES.md)
- Worked quadrotor config: [`examples/quadrotor_sil/`](../examples/quadrotor_sil/)
- Worked rover config: [`examples/rover_sil/`](../examples/rover_sil/)
