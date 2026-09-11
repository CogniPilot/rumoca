"""One closed-loop Sport Cub model for the pure-Python mission study.

``SportCubFirmwareModel`` owns the identified 6-DOF physics, the HobbyZone SAFE
receiver behavior, and the controller state update generated into the Zephyr
flight application. The controller equations mirror
``cerebri_cubs2/src/FixedWingOuterLoop.mo`` at commit
e25c108fc2c4762fda8303594b82eb8ddccee0ad (source SHA-256
5737ebe5f1fc8d9cca9e28c22a29ed6c4caf0388cd18aabcded9026d8da5368f).
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
from numpy.typing import NDArray

FloatArray = NDArray[np.float64]

MISSION_WAYPOINTS = np.array(
    [
        [-4.0, -5.0, 3.0],
        [-3.0, 2.0, 3.0],
        [16.20, 2.0, 3.0],
        [16.0, -4.22, 3.0],
        [-4.0, -5.0, 3.0],
    ],
    dtype=float,
)
MISSION_LEG_COUNT = len(MISSION_WAYPOINTS) - 1


@dataclass(frozen=True)
class AirframeParameters:
    mass: float = 0.065
    gravity: float = 9.81
    jx: float = 8.0e-4
    jy: float = 1.2e-3
    jz: float = 1.8e-3
    jxz: float = 1.0e-4
    rho: float = 1.225
    area: float = 0.055
    span: float = 0.617
    chord: float = 0.09
    wing_incidence: float = np.deg2rad(6.0)
    thrust_max: float = 0.30
    cl0: float = 0.5
    cla: float = 4.7
    cd0: float = 0.06
    induced_drag: float = 0.09
    flat_plate_cd0: float = 0.30
    flat_plate_cy: float = 0.50
    cm0: float = 0.0
    cma: float = -0.8
    cmq: float = -12.0
    cmde: float = 0.3
    cyb: float = -0.50
    cyda: float = 0.004
    cydr: float = -0.015
    cyp: float = -0.15
    cyr: float = 0.20
    clb: float = -0.25
    clp: float = -0.50
    clr: float = 0.15
    clda: float = 0.05
    cldr: float = 0.006
    cnb: float = 0.06
    cnp: float = 0.010
    cnr: float = -0.15
    cndr: float = 0.015
    cnda: float = 0.006
    alpha_stall: float = np.deg2rad(20.0)
    blend_width: float = np.deg2rad(5.0)
    max_aileron: float = np.deg2rad(30.0)
    max_elevator: float = np.deg2rad(24.0)
    max_rudder: float = np.deg2rad(20.0)


@dataclass(frozen=True)
class ReceiverParameters:
    max_roll: float = 0.90
    max_pitch: float = 0.45
    max_yaw_rate: float = 1.0
    attitude_kp: float = 5.0
    max_roll_rate: float = 4.0
    max_pitch_rate: float = 2.5
    rate_kp: tuple[float, float, float] = (0.45, 0.55, 0.40)
    rate_ki: tuple[float, float, float] = (0.30, 0.40, 0.10)
    integral_limit: tuple[float, float, float] = (1.0, 1.0, 0.6)
    protection_low: float = 2.6
    protection_high: float = 3.6
    dive_slope: float = 0.06


@dataclass(frozen=True)
class FirmwareParameters:
    dt: float = 0.02
    gravity: float = 9.81
    filter_cutoff_hz: float = 10.0
    cruise_speed: float = 4.0
    altitude_gain: float = 2.0
    speed_gain: float = 1.0
    lookahead_time: float = 2.0
    lookahead_min: float = 3.0
    lookahead_max: float = 8.0
    waypoint_guard: float = 3.0
    turn_elevator_gain: float = 1.5
    course_gain: float = 1.20
    bank_limit: float = np.deg2rad(30.0)
    bank_rate_limit: float = np.deg2rad(90.0)
    course_deadband: float = np.deg2rad(1.0)
    takeoff_altitude: float = 0.4
    takeoff_elevator: float = 0.15
    tecs_mass: float = 0.063
    tecs_thrust_max: float = 0.30
    trim_thrust: float = 0.1
    thrust_kp: float = 0.01
    thrust_ki: float = 0.25
    energy_integral_limit: float = 3.0
    pitch_kp: float = 0.075
    pitch_ki: float = 0.216
    distance_integral_limit: float = 7.5
    envelope_drag: float = 0.07
    pitch_limit: float = np.deg2rad(12.0)
    pitch_attitude_kp: float = 0.4
    pitch_attitude_ki: float = 0.4
    pitch_attitude_integral_limit: float = 0.5
    course_attitude_kp: float = 1.2
    course_attitude_ki: float = 0.05
    course_attitude_kd: float = 0.35
    course_attitude_integral_limit: float = 0.4


@dataclass
class FirmwareState:
    started: bool = False
    airborne: bool = False
    current_waypoint: int = 0
    position: FloatArray = field(default_factory=lambda: np.zeros(3))
    euler: FloatArray = field(default_factory=lambda: np.zeros(3))
    velocity: FloatArray = field(default_factory=lambda: np.zeros(3))
    euler_rate: FloatArray = field(default_factory=lambda: np.zeros(3))
    speed: float = 0.0
    gamma: float = 0.0
    acceleration: float = 0.0
    previous_position: FloatArray = field(default_factory=lambda: np.zeros(3))
    previous_euler: FloatArray = field(default_factory=lambda: np.zeros(3))
    previous_speed: float = 0.0
    bank_command: float = 0.0
    tecs_energy_integral: float = 0.0
    tecs_distance_integral: float = 0.0
    pid_error: FloatArray = field(default_factory=lambda: np.zeros(2))
    pid_integral: FloatArray = field(default_factory=lambda: np.zeros(2))


@dataclass(frozen=True)
class FirmwareCommand:
    aileron: float
    elevator: float
    throttle: float
    rudder: float
    desired_speed: float


@dataclass(frozen=True)
class MissionTrace:
    time: FloatArray
    position: FloatArray
    airspeed: FloatArray
    desired_speed: FloatArray
    waypoint: NDArray[np.int64]
    throttle: FloatArray
    euler: FloatArray
    angle_of_attack: FloatArray
    aileron: FloatArray
    elevator: FloatArray


def _rotation_matrix(q: FloatArray) -> FloatArray:
    a, b, c, d = q
    return np.array(
        [
            [a * a + b * b - c * c - d * d, 2 * (b * c - a * d), 2 * (b * d + a * c)],
            [2 * (b * c + a * d), a * a - b * b + c * c - d * d, 2 * (c * d - a * b)],
            [2 * (b * d - a * c), 2 * (c * d + a * b), a * a - b * b - c * c + d * d],
        ]
    )


def _euler_from_quaternion(q: FloatArray) -> FloatArray:
    a, b, c, d = q
    pitch_sine = np.clip(2.0 * (a * c - d * b), -1.0, 1.0)
    yaw = np.arctan2(2.0 * (a * d + b * c), 1.0 - 2.0 * (c * c + d * d))
    pitch = np.arcsin(pitch_sine)
    roll = np.arctan2(2.0 * (a * b + c * d), 1.0 - 2.0 * (b * b + c * c))
    return np.array([roll, pitch, yaw])


def _wrap(angle: float) -> float:
    return float(np.arctan2(np.sin(angle), np.cos(angle)))


class SportCubFirmwareModel:
    """Sport Cub physics, SAFE receiver, and Zephyr controller in one model."""

    def __init__(
        self,
        airframe: AirframeParameters | None = None,
        receiver: ReceiverParameters | None = None,
        firmware: FirmwareParameters | None = None,
        *,
        initial_speed: float = 4.0,
        initial_heading: float | None = None,
    ) -> None:
        self.airframe = airframe or AirframeParameters()
        self.receiver = receiver or ReceiverParameters()
        self.firmware = firmware or FirmwareParameters()
        heading = initial_heading
        if heading is None:
            first_leg = MISSION_WAYPOINTS[1] - MISSION_WAYPOINTS[0]
            heading = float(np.arctan2(first_leg[1], first_leg[0]))
        self.state = np.zeros(16)
        self.state[:3] = MISSION_WAYPOINTS[0]
        self.state[3:6] = [initial_speed, 0.0, 0.0]
        self.state[6:10] = [np.cos(heading / 2.0), 0.0, 0.0, np.sin(heading / 2.0)]
        self.controller = FirmwareState()

    def simulate(self, duration: float) -> MissionTrace:
        dt = self.firmware.dt
        count = int(round(duration / dt))
        time = np.arange(count + 1, dtype=float) * dt
        buffers = {
            "state": np.empty((count + 1, len(self.state))),
            "desired_speed": np.empty(count + 1),
            "waypoint": np.empty(count + 1, dtype=np.int64),
            "command": np.empty((count + 1, 3)),
        }
        command = FirmwareCommand(0.0, 0.0, 0.0, 0.0, self.firmware.cruise_speed)
        self._record(0, buffers, command)
        end_index = count
        for index in range(1, count + 1):
            command = self._firmware_step()
            self.state = self._rk4_step(self.state, command, dt)
            self.state[6:10] /= np.linalg.norm(self.state[6:10])
            self._record(index, buffers, command)
            if self.state[2] <= 0.0:
                end_index = index
                break
        used = slice(0, end_index + 1)
        state = buffers["state"][used]
        command = buffers["command"][used]
        euler = np.array([_euler_from_quaternion(row[6:10]) for row in state])
        angle_of_attack = (
            np.arctan2(-state[:, 5], state[:, 3]) + self.airframe.wing_incidence
        )
        return MissionTrace(
            time=time[used],
            position=state[:, :3],
            airspeed=np.linalg.norm(state[:, 3:6], axis=1),
            desired_speed=buffers["desired_speed"][used],
            waypoint=buffers["waypoint"][used],
            throttle=command[:, 2],
            euler=euler,
            angle_of_attack=angle_of_attack,
            aileron=command[:, 0],
            elevator=command[:, 1],
        )

    def _record(
        self,
        index: int,
        buffers: dict[str, FloatArray],
        command: FirmwareCommand,
    ) -> None:
        buffers["state"][index] = self.state
        buffers["desired_speed"][index] = command.desired_speed
        buffers["waypoint"][index] = self.controller.current_waypoint
        buffers["command"][index] = [command.aileron, command.elevator, command.throttle]

    def _firmware_step(self) -> FirmwareCommand:
        state = self.controller
        params = self.firmware
        position = self.state[:3].copy()
        euler = _euler_from_quaternion(self.state[6:10])
        alpha = np.exp(-2.0 * np.pi * params.filter_cutoff_hz * params.dt)
        if not state.started:
            self._initialize_estimator(position, euler)
        else:
            velocity = (position - state.previous_position) / params.dt
            euler_rate = np.array([_wrap(value) for value in euler - state.previous_euler]) / params.dt
            speed = float(np.linalg.norm(velocity))
            gamma = float(np.arcsin(np.clip(velocity[2] / max(speed, 1e-5), -1.0, 1.0)))
            state.position = alpha * position + (1.0 - alpha) * state.position
            state.euler = alpha * euler + (1.0 - alpha) * state.euler
            state.velocity = alpha * velocity + (1.0 - alpha) * state.velocity
            state.euler_rate = alpha * euler_rate + (1.0 - alpha) * state.euler_rate
            state.speed = alpha * speed + (1.0 - alpha) * state.speed
            state.gamma = alpha * gamma + (1.0 - alpha) * state.gamma
            state.acceleration = alpha * (speed - state.previous_speed) + (1.0 - alpha) * state.acceleration
        guidance = self._guidance()
        state.airborne = state.airborne or position[2] > params.takeoff_altitude
        command = self._control(guidance) if state.airborne else self._takeoff_command()
        state.previous_position = position
        state.previous_euler = euler
        state.previous_speed = state.speed
        return command

    def _initialize_estimator(self, position: FloatArray, euler: FloatArray) -> None:
        state = self.controller
        state.position = position.copy()
        state.euler = euler.copy()
        state.previous_position = position.copy()
        state.previous_euler = euler.copy()
        state.started = True

    def _guidance(self) -> tuple[float, float, float, float, float]:
        state = self.controller
        params = self.firmware
        start = MISSION_WAYPOINTS[state.current_waypoint]
        end = MISSION_WAYPOINTS[state.current_waypoint + 1]
        error = end - state.position
        path = end - start
        length = max(float(np.linalg.norm(path)), 1e-6)
        unit = path[:2] / length
        normal = np.array([-unit[1], unit[0]])
        displacement = state.position[:2] - start[:2]
        remaining = max(0.0, length - np.clip(float(displacement @ unit), 0.0, length))
        cross_track = float(displacement @ normal)
        horizontal_speed = float(np.linalg.norm(state.velocity[:2]))
        lookahead = np.clip(horizontal_speed * params.lookahead_time, params.lookahead_min, params.lookahead_max)
        lookahead = max(params.lookahead_min, min(lookahead, remaining))
        heading = _wrap(float(np.arctan2(path[1], path[0]) + np.arctan2(-cross_track, lookahead)))
        horizontal_error = float(np.linalg.norm(error[:2]))
        gamma = np.clip(params.altitude_gain * error[2] / max(horizontal_error, params.lookahead_min), -0.12, 0.12)
        acceleration = params.speed_gain * (params.cruise_speed - abs(state.speed))
        return params.cruise_speed, float(gamma), heading, float(acceleration), remaining

    def _control(self, guidance: tuple[float, float, float, float, float]) -> FirmwareCommand:
        desired_speed, desired_gamma, desired_heading, desired_acceleration, remaining = guidance
        throttle, pitch_command = self._tecs(desired_gamma, desired_acceleration)
        aileron, elevator = self._attitude(pitch_command, desired_heading)
        if remaining < self.firmware.waypoint_guard:
            self.controller.current_waypoint = (
                self.controller.current_waypoint + 1
            ) % MISSION_LEG_COUNT
        return FirmwareCommand(aileron, elevator, throttle, 0.0, desired_speed)

    def _tecs(self, desired_gamma: float, desired_acceleration: float) -> tuple[float, float]:
        state = self.controller
        params = self.firmware
        weight = params.tecs_mass * params.gravity
        desired_acceleration = np.clip(
            desired_acceleration,
            -params.envelope_drag / weight,
            (params.tecs_thrust_max - params.envelope_drag) / weight,
        )
        energy_error = (desired_gamma - state.gamma) + (desired_acceleration - state.acceleration) / params.gravity
        thrust = params.trim_thrust + weight * (
            params.thrust_kp * (state.gamma + state.acceleration / params.gravity)
            + params.thrust_ki * state.tecs_energy_integral
        )
        thrust_command = float(np.clip(thrust, 0.0, params.tecs_thrust_max))
        if not ((thrust_command >= params.tecs_thrust_max - 1e-9 and energy_error > 0.0)
                or (thrust_command <= 1e-9 and energy_error < 0.0)):
            state.tecs_energy_integral = float(np.clip(
                state.tecs_energy_integral + energy_error * params.dt,
                -params.energy_integral_limit,
                params.energy_integral_limit,
            ))
        distance_error = (desired_gamma - state.gamma) - (desired_acceleration - state.acceleration) / params.gravity
        pitch = params.pitch_ki * state.tecs_distance_integral - params.pitch_kp * (
            state.gamma - state.acceleration / params.gravity
        )
        pitch_command = float(np.clip(pitch, -params.pitch_limit, params.pitch_limit))
        if not ((pitch_command >= params.pitch_limit - 1e-9 and distance_error > 0.0)
                or (pitch_command <= -params.pitch_limit + 1e-9 and distance_error < 0.0)):
            state.tecs_distance_integral = float(np.clip(
                state.tecs_distance_integral + distance_error * params.dt,
                -params.distance_integral_limit,
                params.distance_integral_limit,
            ))
        return thrust_command / params.tecs_thrust_max, pitch_command

    def _attitude(self, pitch_command: float, desired_heading: float) -> tuple[float, float]:
        state = self.controller
        params = self.firmware
        pitch_ned = -state.euler[1]
        pitch_error = _wrap(pitch_command - pitch_ned)
        turn_rate = np.sin(state.euler[0]) * np.cos(pitch_ned) * np.tan(state.euler[0]) * params.gravity / max(state.speed, 1e-5)
        pitch_rate_error = _wrap(turn_rate - state.euler_rate[1])
        elevator_feedforward = params.turn_elevator_gain * (1.0 / max(np.cos(state.euler[0]), 1e-5) - 1.0)
        course = float(np.arctan2(state.velocity[1], state.velocity[0]))
        course_error = -_wrap(desired_heading - course)
        if abs(course_error) < params.course_deadband:
            course_error = 0.0
        desired_course_rate = params.course_gain * course_error
        bank = np.clip(np.arctan2(max(state.speed, 0.05) * desired_course_rate, params.gravity), -params.bank_limit, params.bank_limit)
        delta = np.clip(bank - state.bank_command, -params.bank_rate_limit * params.dt, params.bank_rate_limit * params.dt)
        state.bank_command = float(np.clip(state.bank_command + delta, -params.bank_limit, params.bank_limit))
        yaw_error = _wrap(desired_heading - state.euler[2])
        error = np.array([pitch_error, yaw_error])
        derivative = np.array([pitch_rate_error, (yaw_error - state.pid_error[1]) / params.dt])
        limits = np.array([
            params.pitch_attitude_integral_limit,
            params.course_attitude_integral_limit,
        ])
        state.pid_integral = np.clip(state.pid_integral + error * params.dt, -limits, limits)
        command = np.clip(
            np.array([params.pitch_attitude_kp, params.course_attitude_kp]) * error
            + np.array([params.pitch_attitude_ki, params.course_attitude_ki]) * state.pid_integral
            + np.array([0.0, params.course_attitude_kd]) * derivative
            + np.array([elevator_feedforward, 0.0]),
            -1.0,
            1.0,
        )
        state.pid_error = error
        return float(command[1]), float(command[0])

    def _takeoff_command(self) -> FirmwareCommand:
        return FirmwareCommand(0.0, self.firmware.takeoff_elevator, 1.0, 0.0, 0.0)

    def _rk4_step(self, state: FloatArray, command: FirmwareCommand, dt: float) -> FloatArray:
        k1 = self._closed_loop_rhs(state, command)
        k2 = self._closed_loop_rhs(state + 0.5 * dt * k1, command)
        k3 = self._closed_loop_rhs(state + 0.5 * dt * k2, command)
        k4 = self._closed_loop_rhs(state + dt * k3, command)
        return state + dt * (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0

    def _closed_loop_rhs(self, state: FloatArray, command: FirmwareCommand) -> FloatArray:
        surfaces, integral_rate = self._receiver_control(state, command)
        derivative = self._airframe_rhs(state, surfaces)
        derivative[13:16] = integral_rate
        return derivative

    def _receiver_control(self, state: FloatArray, command: FirmwareCommand) -> tuple[FloatArray, FloatArray]:
        params = self.receiver
        up_body = _rotation_matrix(state[6:10])[2]
        phi = float(np.arctan2(up_body[1], up_body[2]))
        theta = float(np.arctan2(up_body[0], up_body[2]))
        speed = float(np.linalg.norm(state[3:6]))
        authority = np.clip((speed - params.protection_low) / (params.protection_high - params.protection_low), 0.0, 1.0)
        # src/main.c reverses the aileron PWM axis; the receiver's elevator
        # channel reversal maps the firmware value back to nose-up-positive.
        phi_target = np.clip(-command.aileron * params.max_roll, -params.max_roll, params.max_roll)
        pitch_stick = command.elevator * params.max_pitch
        theta_target = np.clip(pitch_stick * authority if pitch_stick > 0.0 else pitch_stick, -params.max_pitch, params.max_pitch)
        if speed < params.protection_low:
            theta_target = min(theta_target, -params.dive_slope * (params.protection_low - speed))
        rate_target = np.array(
            [
                np.clip(params.attitude_kp * (phi_target - phi), -params.max_roll_rate, params.max_roll_rate),
                np.clip(params.attitude_kp * (theta_target - theta), -params.max_pitch_rate, params.max_pitch_rate),
                command.rudder * params.max_yaw_rate,
            ]
        )
        rate_error = rate_target - np.array([state[10], -state[11], state[12]])
        integrals = np.clip(state[13:16], -np.array(params.integral_limit), np.array(params.integral_limit))
        surfaces = np.array(params.rate_kp) * rate_error + np.array(params.rate_ki) * integrals
        return np.array([surfaces[0], surfaces[1], surfaces[2], command.throttle]), rate_error

    def _airframe_rhs(self, state: FloatArray, control: FloatArray) -> FloatArray:
        params = self.airframe
        velocity, quat, omega = state[3:6], state[6:10], state[10:13]
        force, moment = self._aerodynamics(velocity, omega, control)
        rotation = _rotation_matrix(quat)
        gravity_body = rotation.T @ np.array([0.0, 0.0, -params.gravity])
        inertia = np.array([[params.jx, 0.0, params.jxz], [0.0, params.jy, 0.0], [params.jxz, 0.0, params.jz]])
        derivative = np.zeros_like(state)
        derivative[:3] = rotation @ velocity
        derivative[3:6] = force / params.mass + gravity_body - np.cross(omega, velocity)
        derivative[6:10] = 0.5 * np.array(
            [
                -quat[1] * omega[0] - quat[2] * omega[1] - quat[3] * omega[2],
                quat[0] * omega[0] - quat[3] * omega[1] + quat[2] * omega[2],
                quat[3] * omega[0] + quat[0] * omega[1] - quat[1] * omega[2],
                -quat[2] * omega[0] + quat[1] * omega[1] + quat[0] * omega[2],
            ]
        )
        derivative[6:10] -= (quat @ quat - 1.0) * quat
        derivative[10:13] = np.linalg.solve(inertia, moment - np.cross(omega, inertia @ omega))
        return derivative

    def _aerodynamics(self, velocity: FloatArray, omega: FloatArray, control: FloatArray) -> tuple[FloatArray, FloatArray]:
        p = self.airframe
        u, v_frd, w_frd = velocity[0], -velocity[1], -velocity[2]
        speed = float(np.linalg.norm([u, v_frd, w_frd])) + 1e-6
        alpha = float(np.arctan2(w_frd, u) + p.wing_incidence)
        beta = float(np.arctan2(v_frd, np.hypot(u, w_frd) + 1e-6))
        dynamic_pressure = 0.5 * p.rho * speed * speed
        wind_x = np.array([u, v_frd, w_frd]) / speed
        reference = np.array([0.0, 0.0, 1.0]) if wind_x[2] ** 2 < wind_x[0] ** 2 else np.array([1.0, 0.0, 0.0])
        wind_z = reference - (reference @ wind_x) * wind_x
        wind_z /= np.linalg.norm(wind_z) + 1e-6
        wind_y = np.cross(wind_z, wind_x)
        aileron = np.clip(control[0], -1.0, 1.0) * p.max_aileron
        elevator = np.clip(control[1], -1.0, 1.0) * p.max_elevator
        rudder = -np.clip(control[2], -1.0, 1.0) * p.max_rudder
        roll_rate, pitch_rate, yaw_rate = omega[0], -omega[1], -omega[2]
        blend = 0.5 * (1.0 + np.tanh((alpha - p.alpha_stall) / p.blend_width))
        cl_linear = p.cl0 + p.cla * alpha
        lift = (1.0 - blend) * cl_linear + blend * 2.0 * np.sin(alpha) * np.cos(alpha)
        drag = (1.0 - blend) * (p.cd0 + p.induced_drag * cl_linear**2) + blend * (p.flat_plate_cd0 + 2.0 * np.sin(alpha) ** 2)
        side_linear = p.cyb * beta + p.cyda * aileron + p.cydr * rudder + p.cyp * p.span * roll_rate / (2.0 * speed) + p.cyr * p.span * yaw_rate / (2.0 * speed)
        side = (1.0 - blend) * side_linear + blend * p.flat_plate_cy * np.sin(beta) * np.cos(alpha)
        roll = p.clda * aileron + p.cldr * rudder + p.clb * beta + p.clp * p.span * roll_rate / (2.0 * speed) + p.clr * p.span * yaw_rate / (2.0 * speed)
        pitch = p.cm0 + p.cma * alpha + p.cmde * elevator + p.cmq * p.chord * pitch_rate / (2.0 * speed)
        yaw = p.cnb * beta + p.cndr * rudder + p.cnda * aileron + p.cnp * p.span * roll_rate / (2.0 * speed) + p.cnr * p.span * yaw_rate / (2.0 * speed)
        force_frd = dynamic_pressure * p.area * (-drag * wind_x + side * wind_y - lift * wind_z)
        force_flu = np.array([force_frd[0], -force_frd[1], -force_frd[2]])
        force_flu[0] += p.thrust_max * np.clip(control[3], 0.0, 1.0)
        moment_frd = dynamic_pressure * p.area * np.array([p.span * roll, p.chord * pitch, p.span * yaw])
        return force_flu, np.array([moment_frd[0], -moment_frd[1], -moment_frd[2]])
