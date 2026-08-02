#!/usr/bin/env python3
"""Monte Carlo mission sweep for the consolidated Sport Cub firmware model."""

from __future__ import annotations

import argparse
import csv
import os
from concurrent.futures import ProcessPoolExecutor
from dataclasses import replace
from pathlib import Path
from typing import Any, NamedTuple

import matplotlib
import numpy as np

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
from matplotlib.colors import Normalize  # noqa: E402

from sport_cub_firmware import (  # noqa: E402
    AirframeParameters,
    FirmwareParameters,
    MISSION_LEG_COUNT,
    MISSION_WAYPOINTS,
    MissionTrace,
    SportCubFirmwareModel,
)

CRUISE_VELOCITIES = np.linspace(1.0, 5.0, 9)
FIELD_NAMES = [
    "cruise_velocity_m_s",
    "trial",
    "completed_duration_s",
    "waypoints_completed",
    "laps_completed",
    "success",
    "cross_track_rmse_m",
    "altitude_rmse_m",
    "airspeed_rmse_m_s",
    "mean_airspeed_m_s",
    "minimum_altitude_m",
    "mean_throttle",
    "mass_kg",
    "cla",
    "cd0",
    "thrust_max_n",
    "initial_speed_m_s",
    "initial_heading_rad",
]


class TrialInput(NamedTuple):
    velocity: float
    trial: int
    duration: float
    mass_scale: float
    lift_scale: float
    drag_scale: float
    thrust_scale: float
    initial_speed: float
    heading_offset: float


def make_trials(samples: int, duration: float, seed: int) -> list[TrialInput]:
    rng = np.random.default_rng(seed)
    draws = np.column_stack(
        [
            rng.normal(1.0, 0.04, samples),
            rng.normal(1.0, 0.06, samples),
            rng.normal(1.0, 0.10, samples),
            rng.normal(1.0, 0.05, samples),
            rng.normal(4.0, 0.15, samples),
            rng.normal(0.0, np.deg2rad(2.0), samples),
        ]
    )
    draws[0] = [1.0, 1.0, 1.0, 1.0, 4.0, 0.0]
    return [
        TrialInput(float(velocity), trial, duration, *map(float, draws[trial]))
        for velocity in CRUISE_VELOCITIES
        for trial in range(samples)
    ]


def run_trial(task: TrialInput) -> tuple[dict[str, float | int], dict[str, Any] | None]:
    nominal = AirframeParameters()
    airframe = replace(
        nominal,
        mass=nominal.mass * task.mass_scale,
        cla=nominal.cla * task.lift_scale,
        cd0=nominal.cd0 * task.drag_scale,
        thrust_max=nominal.thrust_max * task.thrust_scale,
    )
    firmware = replace(FirmwareParameters(), cruise_speed=task.velocity)
    first_leg = MISSION_WAYPOINTS[1] - MISSION_WAYPOINTS[0]
    heading = float(np.arctan2(first_leg[1], first_leg[0]) + task.heading_offset)
    model = SportCubFirmwareModel(
        airframe=airframe,
        firmware=firmware,
        initial_speed=task.initial_speed,
        initial_heading=heading,
    )
    trace = model.simulate(task.duration)
    record = mission_metrics(task, trace, airframe, heading)
    trajectory = None
    if task.trial == 0:
        trajectory = {
            "velocity": task.velocity,
            "time": trace.time,
            "airspeed": trace.airspeed,
            "desired_speed": trace.desired_speed,
            "x": trace.position[:, 0],
            "y": trace.position[:, 1],
            "altitude": trace.position[:, 2],
            "laps": record["laps_completed"],
        }
    return record, trajectory


def mission_metrics(
    task: TrialInput,
    trace: MissionTrace,
    airframe: AirframeParameters,
    heading: float,
) -> dict[str, float | int]:
    transitions = int(np.count_nonzero(np.diff(trace.waypoint)))
    cross_track = cross_track_error(trace.position, trace.waypoint)
    steady = trace.time >= min(5.0, 0.25 * task.duration)
    if not np.any(steady):
        steady = np.ones_like(trace.time, dtype=bool)
    altitude_error = trace.position[steady, 2] - 3.0
    speed_error = trace.airspeed[steady] - task.velocity
    speed_rmse = rms(speed_error)
    completed = float(trace.time[-1]) >= task.duration - 0.5 * FirmwareParameters().dt
    minimum_altitude = float(np.min(trace.position[:, 2]))
    speed_tolerance = max(0.5, 0.25 * task.velocity)
    success = int(
        completed and transitions >= MISSION_LEG_COUNT and minimum_altitude > 0.0
        and speed_rmse <= speed_tolerance
    )
    return {
        "cruise_velocity_m_s": task.velocity,
        "trial": task.trial,
        "completed_duration_s": float(trace.time[-1]),
        "waypoints_completed": transitions,
        "laps_completed": transitions / MISSION_LEG_COUNT,
        "success": success,
        "cross_track_rmse_m": rms(cross_track[steady]),
        "altitude_rmse_m": rms(altitude_error),
        "airspeed_rmse_m_s": speed_rmse,
        "mean_airspeed_m_s": float(np.mean(trace.airspeed[steady])),
        "minimum_altitude_m": minimum_altitude,
        "mean_throttle": float(np.mean(trace.throttle[steady])),
        "mass_kg": airframe.mass,
        "cla": airframe.cla,
        "cd0": airframe.cd0,
        "thrust_max_n": airframe.thrust_max,
        "initial_speed_m_s": task.initial_speed,
        "initial_heading_rad": heading,
    }


def cross_track_error(position: np.ndarray, waypoint: np.ndarray) -> np.ndarray:
    errors = np.empty(len(position))
    for index in range(MISSION_LEG_COUNT):
        selected = waypoint == index
        start = MISSION_WAYPOINTS[index, :2]
        path = MISSION_WAYPOINTS[index + 1, :2] - start
        unit = path / max(float(np.linalg.norm(path)), 1e-9)
        normal = np.array([-unit[1], unit[0]])
        errors[selected] = (position[selected, :2] - start) @ normal
    return errors


def rms(values: np.ndarray) -> float:
    return float(np.sqrt(np.mean(np.square(values))))


def write_csv(records: list[dict[str, float | int]], path: Path) -> None:
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=FIELD_NAMES)
        writer.writeheader()
        writer.writerows(records)


def percentile_band(
    records: list[dict[str, float | int]], field: str
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    groups = [
        np.array([float(row[field]) for row in records if row["cruise_velocity_m_s"] == velocity])
        for velocity in CRUISE_VELOCITIES
    ]
    return tuple(np.array([np.percentile(group, q) for group in groups]) for q in (10, 50, 90))  # type: ignore[return-value]


def plot_trajectories(trajectories: list[dict[str, Any]], path: Path) -> None:
    figure, axes = plt.subplots(3, 3, figsize=(12, 10), sharex=True, sharey=True)
    route = MISSION_WAYPOINTS[:, :2]
    norm = Normalize(CRUISE_VELOCITIES.min(), CRUISE_VELOCITIES.max())
    cmap = plt.get_cmap("viridis")
    for axis, trajectory in zip(axes.flat, sorted(trajectories, key=lambda item: item["velocity"])):
        velocity = trajectory["velocity"]
        axis.plot(route[:, 0], route[:, 1], "k--o", linewidth=1.0, markersize=3, label="firmware route")
        axis.plot(trajectory["x"], trajectory["y"], color=cmap(norm(velocity)), linewidth=1.5)
        axis.set_title(f"{velocity:.1f} m/s · {trajectory['laps']:.1f} laps")
        axis.grid(alpha=0.25)
        axis.set_aspect("equal", adjustable="box")
    figure.supxlabel("east x [m]")
    figure.supylabel("north y [m]")
    figure.suptitle("Nominal Sport Cub mission trajectories")
    figure.tight_layout()
    figure.savefig(path, dpi=180)
    plt.close(figure)


def plot_airspeed_tracking(trajectories: list[dict[str, Any]], path: Path) -> None:
    figure, axes = plt.subplots(3, 3, figsize=(12, 9), sharex=True, sharey=True)
    norm = Normalize(CRUISE_VELOCITIES.min(), CRUISE_VELOCITIES.max())
    cmap = plt.get_cmap("viridis")
    ordered = sorted(trajectories, key=lambda item: item["velocity"])
    for axis, trajectory in zip(axes.flat, ordered):
        velocity = trajectory["velocity"]
        axis.plot(
            trajectory["time"],
            trajectory["airspeed"],
            color=cmap(norm(velocity)),
            linewidth=1.5,
            label="actual",
        )
        axis.plot(
            trajectory["time"],
            trajectory["desired_speed"],
            color="black",
            linestyle="--",
            linewidth=1.2,
            label="command",
        )
        axis.set_title(f"{velocity:.1f} m/s cruise command")
        axis.grid(alpha=0.25)
    for axis in axes[-1]:
        axis.set_xlabel("mission time [s]")
    for axis in axes[:, 0]:
        axis.set_ylabel("airspeed [m/s]")
    axes.flat[0].legend(loc="best")
    figure.suptitle("Nominal Sport Cub commanded and actual airspeed")
    figure.tight_layout()
    figure.savefig(path, dpi=180)
    plt.close(figure)


def plot_performance(records: list[dict[str, float | int]], path: Path) -> None:
    figure, axes = plt.subplots(2, 3, figsize=(13, 7.5), sharex=True)
    panels = [
        ("laps_completed", "completed laps"),
        ("cross_track_rmse_m", "cross-track RMSE [m]"),
        ("altitude_rmse_m", "altitude RMSE [m]"),
        ("airspeed_rmse_m_s", "airspeed RMSE [m/s]"),
        ("minimum_altitude_m", "minimum altitude [m]"),
    ]
    for axis, (field, label) in zip(axes.flat, panels):
        low, median, high = percentile_band(records, field)
        axis.fill_between(CRUISE_VELOCITIES, low, high, color="tab:blue", alpha=0.2, label="10–90%")
        axis.plot(CRUISE_VELOCITIES, median, "o-", color="tab:blue", label="median")
        axis.set_ylabel(label)
        axis.grid(alpha=0.25)
    success = np.array(
        [
            np.mean([row["success"] for row in records if row["cruise_velocity_m_s"] == velocity])
            for velocity in CRUISE_VELOCITIES
        ]
    )
    axes.flat[5].plot(CRUISE_VELOCITIES, 100.0 * success, "o-", color="tab:green")
    axes.flat[5].set(ylabel="mission success [%]", ylim=(-5.0, 105.0))
    axes.flat[5].grid(alpha=0.25)
    for axis in axes[-1]:
        axis.set_xlabel("commanded cruise velocity [m/s]")
    axes.flat[0].legend(loc="best")
    figure.suptitle("Sport Cub mission performance under airframe uncertainty")
    figure.tight_layout()
    figure.savefig(path, dpi=180)
    plt.close(figure)


def print_summary(records: list[dict[str, float | int]]) -> None:
    print("command  success  median actual  median throttle  median laps")
    for velocity in CRUISE_VELOCITIES:
        group = [row for row in records if row["cruise_velocity_m_s"] == velocity]
        success = 100.0 * np.mean([row["success"] for row in group])
        actual = np.median([row["mean_airspeed_m_s"] for row in group])
        throttle = np.median([row["mean_throttle"] for row in group])
        laps = np.median([row["laps_completed"] for row in group])
        print(
            f" {velocity:4.1f}     {success:5.1f}%"
            f"       {actual:5.2f} m/s        {throttle:5.3f}         {laps:5.2f}"
        )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=16, help="trials per cruise velocity")
    parser.add_argument("--duration", type=float, default=60.0, help="mission duration [s]")
    parser.add_argument("--seed", type=int, default=7, help="Monte Carlo seed")
    parser.add_argument("--workers", type=int, default=0, help="worker processes; 0 selects automatically")
    parser.add_argument("--output-dir", type=Path, default=Path("figures"))
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.samples < 1 or args.duration <= 0.0:
        raise SystemExit("--samples and --duration must be positive")
    tasks = make_trials(args.samples, args.duration, args.seed)
    workers = args.workers or min(len(CRUISE_VELOCITIES), os.cpu_count() or 1)
    if workers == 1:
        results = list(map(run_trial, tasks))
    else:
        with ProcessPoolExecutor(max_workers=workers) as executor:
            results = list(executor.map(run_trial, tasks))
    records = [record for record, _ in results]
    trajectories = [trajectory for _, trajectory in results if trajectory is not None]
    records.sort(key=lambda row: (row["cruise_velocity_m_s"], row["trial"]))
    args.output_dir.mkdir(parents=True, exist_ok=True)
    write_csv(records, args.output_dir / "mission_performance.csv")
    plot_trajectories(trajectories, args.output_dir / "mission_trajectories.png")
    plot_airspeed_tracking(trajectories, args.output_dir / "airspeed_tracking.png")
    plot_performance(records, args.output_dir / "mission_performance.png")
    print_summary(records)
    print(f"\nWrote {args.output_dir.resolve()}")


if __name__ == "__main__":
    main()
