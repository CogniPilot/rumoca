#!/usr/bin/env python3
"""Replay a Zephyr SIL pose trace through the original Python controller."""

from __future__ import annotations

import argparse
import contextlib
import csv
import math
import os
import sys
from pathlib import Path


WAYPOINTS = [
    (-4.0, -5.0, 3.0),
    (-3.0, 2.0, 3.0),
    (16.20, 2.0, 3.0),
    (16.0, -4.22, 3.0),
    (6.88, -5.1, 3.0),
    (-4.0, -5.0, 3.0),
]


def wrap_pi(value: float) -> float:
    return math.atan2(math.sin(value), math.cos(value))


class StateEstimator:
    def __init__(self) -> None:
        self.prev_t: float | None = None
        self.prev_x: float | None = None
        self.prev_y: float | None = None
        self.prev_z: float | None = None
        self.prev_roll: float | None = None
        self.prev_pitch: float | None = None
        self.prev_yaw: float | None = None
        self.prev_speed = 0.0
        self.last: dict[str, float] = {}

    def update(self, row: dict[str, str]) -> tuple[float, dict[str, float]]:
        timestamp_ns = float(row.get("timestamp_ns") or 0.0)
        t = timestamp_ns * 1e-9
        if self.prev_t is None:
            self.prev_t = t - 0.01
        dt = max(t - self.prev_t, 0.01)
        self.prev_t = t

        x = float(row["x"])
        y = float(row["y"])
        z = float(row["z"])
        roll = float(row["roll"])
        pitch = float(row["pitch"])
        yaw = float(row["yaw"])

        if self.prev_x is None:
            self.prev_x, self.prev_y, self.prev_z = x, y, z
            self.prev_roll, self.prev_pitch, self.prev_yaw = roll, pitch, yaw
            self.prev_speed = math.sqrt(x * x + y * y + z * z)

        vx_new = (x - self.prev_x) / dt
        vy_new = (y - self.prev_y) / dt
        vz_new = (z - self.prev_z) / dt
        speed_new = math.sqrt(vx_new * vx_new + vy_new * vy_new + vz_new * vz_new)
        gamma_new = math.asin(max(-1.0, min(1.0, vz_new / max(speed_new, 1e-5))))
        vdot_new = speed_new - self.prev_speed
        p_new = wrap_pi(roll - self.prev_roll) / dt
        q_new = wrap_pi(pitch - self.prev_pitch) / dt
        r_new = wrap_pi(yaw - self.prev_yaw) / dt

        alpha = math.exp(-2.0 * math.pi * 10.0 * dt)
        raw = {
            "x": x,
            "y": y,
            "z": z,
            "roll": roll,
            "pitch": pitch,
            "yaw": yaw,
            "vx": vx_new,
            "vy": vy_new,
            "vz": vz_new,
            "v": speed_new,
            "gamma": gamma_new,
            "vdot": vdot_new,
            "p": p_new,
            "q": q_new,
            "r": r_new,
        }
        for name, value in raw.items():
            previous = self.last.get(name, value)
            self.last[name] = alpha * value + (1.0 - alpha) * previous

        self.prev_x, self.prev_y, self.prev_z = x, y, z
        self.prev_roll, self.prev_pitch, self.prev_yaw = roll, pitch, yaw
        self.prev_speed = self.last["v"]

        actual = {
            "x_est": self.last["x"],
            "y_est": self.last["y"],
            "z_est": self.last["z"],
            "roll_est": self.last["roll"],
            "pitch_est": self.last["pitch"],
            "yaw_est": self.last["yaw"],
            "vx_est": self.last["vx"],
            "vy_est": self.last["vy"],
            "vz_est": self.last["vz"],
            "v_est": self.last["v"],
            "gamma_est": self.last["gamma"],
            "vdot_est": self.last["vdot"],
            "p_est": self.last["p"],
            "q_est": self.last["q"],
            "r_est": self.last["r"],
        }
        return dt, actual


def cub_control_root() -> Path:
    return Path(os.environ.get("CUB_CONTROL_DEV", "/home/prady/code/purt/cub_control-dev"))


def add_cub_control_imports() -> None:
    root = cub_control_root()
    sys.path.insert(0, str(root))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", default="/tmp/fixed_wing_zephyr_trace.csv")
    parser.add_argument("--output", default="/tmp/fixed_wing_python_replay.csv")
    args = parser.parse_args()

    add_cub_control_imports()
    from controller_cub.tecs_controller_xtrack import TECSControl_cub
    from navigation.cross_tracker_lookAhead import XTrack_NAV_lookAhead

    estimator = StateEstimator()
    planner = XTrack_NAV_lookAhead(0.01, WAYPOINTS, 0, 3.0)
    planner.path_distance_buf = 5.0
    planner.wpt_switching_distance = 4.0
    controller = TECSControl_cub(0.01, "cub1")
    controller.roll_mode = "direct"

    current_wp = 0
    throttle = 0.0
    elevator = 0.0
    time_s = 0.0

    fields = [
        "seq", "dt", "x", "y", "z", "roll", "pitch", "yaw", "current_wp",
        "airborne", "des_v", "des_gamma", "des_heading", "des_a", "phi_cmd",
        "chi_err", "aileron", "elevator", "throttle", "rudder",
    ]
    with open(args.input, newline="") as f_in, open(args.output, "w", newline="") as f_out:
        reader = csv.DictReader(f_in)
        writer = csv.DictWriter(f_out, fieldnames=fields)
        writer.writeheader()
        for row in reader:
            dt, actual = estimator.update(row)
            controller.dt = dt
            planner.dt = dt
            time_s += dt
            airborne = actual["z_est"] > 0.4
            rudder = 0.0
            aileron = 0.0
            des_v = des_gamma = des_heading = des_a = 0.0
            chi_err = 0.0

            if not airborne:
                throttle = min(1.0, max(0.7, throttle + 2.0 * dt))
                if actual["v_est"] < 0.5:
                    elevator = -0.02
                else:
                    elevator = min(0.15, elevator + 0.40 * dt)
            else:
                if current_wp >= len(WAYPOINTS) - 1:
                    current_wp = 0
                    planner = XTrack_NAV_lookAhead(dt, WAYPOINTS, current_wp, 3.0)
                    planner.path_distance_buf = 5.0
                    planner.wpt_switching_distance = 4.0
                planner.current_WP_ind = current_wp
                v_array = [actual["vx_est"], actual["vy_est"], actual["vz_est"]]
                des_v, des_gamma, des_heading, along_track_err, _cross_track_err = planner.wp_tracker(
                    WAYPOINTS, actual["x_est"], actual["y_est"], actual["z_est"], v_array
                )
                des_a = des_v - abs(actual["v_est"])
                chi = math.atan2(actual["vy_est"], actual["vx_est"])
                chi_err = -wrap_pi(des_heading - chi)
                if abs(chi_err) < controller.chi_deadband:
                    chi_err = 0.0
                ref = {
                    "des_v": des_v,
                    "des_gamma": des_gamma,
                    "des_heading": des_heading,
                    "des_a": des_a,
                }
                # SIL provides FLU convention (nose-up-positive, yaw CCW+),
                # but Python controller expects NED (nose-up-negative, yaw CW+).
                # Pre-negate pitch so the Python negation cancels out.
                # Negate aileron output because FLU yaw sign (CCW=positive) is
                # opposite to NED (CW=positive) — positive heading error in
                # FLU means turn left, but Python's aileron formula assumes
                # positive heading error = turn right.
                sil_actual = dict(actual)
                sil_actual["pitch_est"] = -sil_actual["pitch_est"]
                with open(os.devnull, "w") as devnull, contextlib.redirect_stdout(devnull):
                    aileron, elevator, throttle, rudder = controller.compute_control(
                        int(time_s / dt), ref, sil_actual
                    )
                    aileron = -aileron
                    current_wp = planner.check_arrived(along_track_err, v_array)

            writer.writerow({
                "seq": row["seq"],
                "dt": dt,
                "x": actual["x_est"],
                "y": actual["y_est"],
                "z": actual["z_est"],
                "roll": actual["roll_est"],
                "pitch": actual["pitch_est"],
                "yaw": actual["yaw_est"],
                "current_wp": current_wp,
                "airborne": int(airborne),
                "des_v": des_v,
                "des_gamma": des_gamma,
                "des_heading": des_heading,
                "des_a": des_a,
                "phi_cmd": controller._phi_cmd,
                "chi_err": chi_err,
                "aileron": aileron,
                "elevator": elevator,
                "throttle": throttle,
                "rudder": rudder,
            })

    print(f"wrote {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
