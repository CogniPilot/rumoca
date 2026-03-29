# Quadrotor SIL Plant Simulator

Software-in-the-loop plant model for a quadrotor. Communicates with Cerebri
flight controller via UDP using the cerebri flatbuffer protocol (48-byte
motor_output, 164-byte flight_snapshot).

## Quick Start

```bash
# Self-test mode (hover, no UDP)
cargo run --example quadrotor_sil -p rumoca

# UDP mode (connected to Cerebri)
SIL_UDP_LISTEN=0.0.0.0:4243 SIL_UDP_SEND=192.0.2.1:4242 \
  cargo run --example quadrotor_sil -p rumoca
```

Open http://localhost:8080 for the Three.js visualization.

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `SIL_UDP_LISTEN` | (none) | UDP address to listen for motor_output packets |
| `SIL_UDP_SEND` | (none) | UDP address to send flight_snapshot packets |
| `SIL_DT` | `0.004` | Simulation timestep in seconds (250 Hz) |

Both `SIL_UDP_LISTEN` and `SIL_UDP_SEND` must be set to enable UDP mode.
Without them, the simulator runs in self-test mode (constant hover RPMs).

## Protocol

### Receive: motor_output (48 bytes)

Cerebri sends this flatbuffer packet containing:
- `motors[4]`: float, normalised 0..1 motor commands
- `raw[4]`: uint16, PWM microseconds
- `armed`: bool
- `test_mode`: bool

The normalised motor values are converted to rad/s:
`omega = motors[i] * OMEGA_MAX` (default OMEGA_MAX = 1100 rad/s)

### Send: flight_snapshot (164 bytes)

Rumoca sends back this flatbuffer packet containing:
- `imu.gyro_rad_s[3]`: body-frame angular velocity [rad/s]
- `imu.accel_m_s2[3]`: body-frame specific force [m/s^2]
- `rc_channels[16]`: zeros (plant sim has no RC)
- `control_status`: armed state, imu_ok=true, timestamps
- `rate_desired[3]`, `rate_cmd[3]`: current body rates

## Data Flow

```
Cerebri                        Rumoca SIL                    Browser
  |                               |                             |
  |-- motor_output (48B, UDP) --> |                             |
  |                               |-- step physics (dt)         |
  |                               |-- compute sensors           |
  |                               |-- stream JSON -----------> | (WS:8081)
  |<- flight_snapshot (164B) ---- |                             |
```

## Conventions

- **World frame:** NED (North-East-Down)
- **Body frame:** FRD (Forward-Right-Down)
- **Quaternion:** `{w, x, y, z}` scalar-first, body-to-world
- **Motor layout:** X-config matching Cerebri MixQuadX
  - Motor 1: front-right, CW
  - Motor 2: rear-right, CCW
  - Motor 3: rear-left, CW
  - Motor 4: front-left, CCW

## Rust API (for custom integration)

```rust
use quadrotor_sil::{QuadrotorSil, SensorOutput};

let mut sil = QuadrotorSil::new()?;

// From your UDP/protocol layer:
let motor_rpms = [omega1, omega2, omega3, omega4]; // rad/s
let sensors = sil.receive_motors(motor_rpms, target_clock_sec)?;

// sensors.accel  — body-frame accelerometer [m/s^2]
// sensors.gyro   — body-frame gyroscope [rad/s]
// sensors.mag    — body-frame magnetometer [Gauss]

// Convert to flatbuffer for sending:
let snap = sil.sensor_to_snapshot(&sensors, armed);
let buf = cerebri_fb::pack_flight_snapshot(&snap);
```

## Physical Parameters

| Parameter | Value | Unit |
|-----------|-------|------|
| mass | 2.0 | kg |
| Ixx | 0.020 | kg*m^2 |
| Iyy | 0.020 | kg*m^2 |
| Izz | 0.040 | kg*m^2 |
| Ct (thrust coeff) | 8.5e-6 | N/(rad/s)^2 |
| Cm (torque coeff) | 1.36e-7 | N*m/(rad/s)^2 |
| arm_length | 0.2 | m |
| OMEGA_MAX | 1100 | rad/s |
| hover omega | ~759.5 | rad/s |

## Ports

- HTTP: 8080 (Three.js viewer)
- WebSocket: 8081 (state streaming)
- UDP: configurable via env vars
