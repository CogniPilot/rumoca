# rumoca sim-fb

FlatBuffer-based Software-in-the-Loop (SIL) simulation with 3D browser visualization.

Runs a Modelica plant model in a lockstep loop with an external autopilot (e.g., CogniPilot Cerebri), communicating via FlatBuffers over UDP. A 3D browser viewer shows the simulation state in real time.

## Architecture

```
Autopilot (e.g. Cerebri)
    |  UDP / FlatBuffers
    v
rumoca sim -c rum.sil.toml (Rust)
    |  - Compiles Modelica model at startup
    |  - Runs physics in lockstep with autopilot:
    |      drain motor commands -> step physics -> send sensors -> repeat
    |  - Packs/unpacks FlatBuffer messages via .bfbs schema reflection
    |  - Manages autopilot process lifecycle (auto-start, restart on reset, kill on exit)
    |  - Keyboard/gamepad input for RC channels
    |
    |  WebSocket / JSON (state broadcast, ~60 fps)
    v
Browser (Three.js)
    - 3D visualization with pluggable scene scripts
    - Velocity-based position extrapolation for smooth rendering
    - State display and keyboard forwarding
    - Passive observer (no physics in browser)
```

## Quick Start

```sh
# Build or use the Rumoca checkout that contains the interactive runner.
cd /home/prady/purdue/research/rumoca
cargo build -p rumoca --release

# Run this SIL scenario through Rumoca's normal interactive viewer runner.
cargo run -p rumoca --release -- \
  sim -c examples/interactive/sim_fb_demo/rum.sil.toml

# Open http://localhost:8080 in a browser
```

This repo is not meant to be copied into the Rumoca source tree. The
`rum.sil.toml` scenario points Rumoca at `QuadrotorSIL.mo`, starts the external
binary configured in `[external_interface]`, exchanges FlatBuffers over UDP, and
serves the same external web viewer used by `examples/interactive`.

Before running, compile the C/Cerebri side and update these paths in
`rum.sil.toml` if your build output differs:

- `[external_interface].command`
- `[schema].bfbs`

## Controls

Input is handled by Rumoca's interactive input engine. Keyboard controls work
from the terminal when raw terminal input is available and from the browser when
the viewer page has focus.

### Keyboard

| Key | Action |
|-----|--------|
| W / S | Throttle |
| Up / Down | Roll |
| Left / Right | Pitch |
| A / D | Yaw |
| Space | Arm / Disarm (throttle must be low) |
| R | Reset simulation + restart autopilot |
| Q | Quit |

### Gamepad

| Button/Stick | Action |
|-------------|--------|
| Left stick Y | Throttle (ramp) |
| Left stick X | Yaw |
| Right stick Y | Pitch |
| Right stick X | Roll |
| Start | Arm / Disarm (throttle must be low) |
| A / South | Reset |

### Browser

| Control | Action |
|---------|--------|
| Mouse drag | Orbit camera |
| Scroll wheel | Zoom in/out |

## Configuration

Current Rumoca uses `rum.sil.toml` for this interactive SIL scenario. The older
`sil_config.toml` is kept as a reference for the previous `sim-fb` command, but
the native viewer runner expects `rum.toml`-style sections:

```toml
[rumoca]
version = "1"

[model]
file = "QuadrotorSIL.mo"
name = "QuadrotorSIL"

[sim]
dt = 0.00125
mode = "lockstep"
solver = "rk-like"

[external_interface]
command = "/path/to/autopilot/binary"

[transport.udp]
listen = "0.0.0.0:4244"    # Receive motor commands from autopilot
send = "127.0.0.1:4242"    # Send sensor data to autopilot

[schema]
bfbs = [                    # Binary FlatBuffer Schema files
    "/path/to/schema1.bfbs",
    "/path/to/schema2.bfbs",
]

[receive]
root_type = "namespace.MotorOutput"
[receive.route]
"motors.m0" = { to = "stepper:omega_m1", scale = 1100.0 }
"motors.m1" = { to = "stepper:omega_m2", scale = 1100.0 }
"armed"     = { to = "local:armed" }

[send]
root_type = "namespace.SimInput"
[send.route]
"gyro.x"  = { key = "gyro_x" }
"accel.x" = { key = "accel_x" }
"rc.ch0"  = { key = "rc_0" }

[signals.send]
gyro_x = "stepper:gyro_x"
accel_x = "stepper:accel_x"
rc_0 = "local:rc.0"
```

### Simulation Rate

The `dt` parameter controls both the physics timestep and the sensor update rate. The simulation runs in lockstep with the autopilot: drain motor commands, step physics, send sensors, repeat.

| dt | Rate | Notes |
|----|------|-------|
| 0.004 | 250 Hz | Conservative, works with any autopilot |
| 0.002 | 500 Hz | Higher fidelity |
| 0.00125 | 800 Hz | Matches high-rate controllers (e.g. Cerebri) |

### Lockstep Pacing

This SIL scenario uses `mode = "lockstep"`. Each physics step waits for one
incoming motor command packet from the external binary, then steps the Modelica
plant and sends sensors/RC back out.

## Custom 3D Scenes

The default scene is a quadrotor in a desert environment. Provide a custom scene script for different aircraft or environments:

```sh
cargo run -p rumoca --release -- \
  sim -c examples/interactive/sim_fb_demo/rum.sil.toml
```

### Scene Script API

A scene script is a JavaScript file that defines `onInit` and `onFrame` callbacks:

```js
ctx.onInit = function(api) {
  // api.THREE  - Three.js library
  // api.scene  - THREE.Scene (add meshes here)
  // api.state  - persistent object (store references across frames)
  // api.camera - THREE.Camera
  // api.cam    - { target, dist, angle, elev } (mutable camera orbit state)

  const mesh = new api.THREE.Mesh(geometry, material);
  api.scene.add(mesh);
  api.state.mesh = mesh;
};

ctx.onFrame = function(api) {
  // api.get(name) - read state variable (extrapolated for smooth rendering)
  //   Position: px, py, pz (NED frame)
  //   Velocity: vx, vy, vz
  //   Attitude: q0, q1, q2, q3 (quaternion)
  //   Motors: omega_m1, omega_m2, omega_m3, omega_m4
  //   Sensors: accel_x/y/z, gyro_x/y/z
  // api.motors  - full state object
  // api.camera  - THREE.Camera
  // api.cam     - mutable camera orbit state

  const px = api.get("px") ?? 0;
  const pz = api.get("pz") ?? 0;

  // NED to Three.js: x_three = py, y_three = -pz, z_three = px
  const tx = (api.get("py") ?? 0);
  const ty = -(api.get("pz") ?? 0);
  const tz = (api.get("px") ?? 0);

  api.state.mesh.position.set(tx, ty, tz);

  // Camera follow
  api.cam.target.lerp(new api.THREE.Vector3(tx, ty, tz), 0.05);
  api.camera.position.set(
    api.cam.target.x + api.cam.dist * Math.sin(api.cam.angle) * Math.cos(api.cam.elev),
    api.cam.target.y + api.cam.dist * Math.sin(api.cam.elev),
    api.cam.target.z + api.cam.dist * Math.cos(api.cam.angle) * Math.cos(api.cam.elev)
  );
  api.camera.lookAt(api.cam.target);
};
```

## CLI Options

```
rumoca sim -c <CONFIG>

Arguments:
  -c, --config <CONFIG>  Rumoca task file, for example rum.sil.toml

Validate:
  rumoca sim check -c examples/interactive/sim_fb_demo/rum.sil.toml
```

## Debug Mode

```sh
cargo run -p rumoca --release -- \
  sim -c examples/interactive/sim_fb_demo/rum.sil.toml --trace=viewer
```

Enables additional diagnostics:

| Feature | Location | Trigger |
|---------|----------|---------|
| Debug overlay | Browser (bottom-left) | Always visible in debug mode |
| FPS / WS Hz counters | Browser HUD | Always visible in debug mode |
| Trace capture trigger | Terminal file system | **L** key (keyboard) or **Y** button (gamepad) |
| Render position log | Browser Downloads | **P** key (browser must have focus) |

The configured trace capture path is `rumoca_sil_trace.csv`.
