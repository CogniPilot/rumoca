#!/usr/bin/env python3
"""Visualize the velocity-dependent crash boundary with trajectory bands."""

from __future__ import annotations

import argparse
import os
from concurrent.futures import ProcessPoolExecutor
from dataclasses import replace
from pathlib import Path
from typing import NamedTuple

import matplotlib
import numpy as np

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
from matplotlib.colors import Normalize  # noqa: E402

from monte_carlo import TrialInput, make_trials
from sport_cub_firmware import (
    AirframeParameters,
    FirmwareParameters,
    MISSION_WAYPOINTS,
    SportCubFirmwareModel,
)

CRUISE_SPEEDS = (2.0, 3.0, 4.0, 5.0, 6.0)


class CrashTrace(NamedTuple):
    velocity: float
    trial: int
    altitude: np.ndarray
    impact_time: float | None


def make_tasks(samples: int, duration: float, seed: int) -> list[TrialInput]:
    nominal_speed = FirmwareParameters().cruise_speed
    draws = [
        trial for trial in make_trials(samples, duration, seed)
        if trial.velocity == nominal_speed
    ]
    return [
        baseline._replace(velocity=velocity)
        for velocity in CRUISE_SPEEDS
        for baseline in draws
    ]


def run_trace(task: TrialInput) -> CrashTrace:
    nominal = AirframeParameters()
    airframe = replace(
        nominal,
        mass=nominal.mass * task.mass_scale,
        cla=nominal.cla * task.lift_scale,
        cd0=nominal.cd0 * task.drag_scale,
        thrust_max=nominal.thrust_max * task.thrust_scale,
    )
    firmware = replace(
        FirmwareParameters(),
        cruise_speed=task.velocity,
        altitude_gain=0.0,
    )
    first_leg = MISSION_WAYPOINTS[1] - MISSION_WAYPOINTS[0]
    heading = float(np.arctan2(first_leg[1], first_leg[0]) + task.heading_offset)
    model = SportCubFirmwareModel(
        airframe=airframe,
        firmware=firmware,
        initial_speed=task.initial_speed,
        initial_heading=heading,
    )
    trace = model.simulate(task.duration)
    impact_time = (
        float(trace.time[-1]) if trace.position[-1, 2] <= 0.0 else None
    )
    return CrashTrace(task.velocity, task.trial, trace.position[:, 2], impact_time)


def altitude_matrix(
    traces: list[CrashTrace], velocity: float, count: int
) -> np.ndarray:
    selected = sorted(
        (trace for trace in traces if trace.velocity == velocity),
        key=lambda trace: trace.trial,
    )
    matrix = np.empty((len(selected), count))
    for row, trace in enumerate(selected):
        used = min(len(trace.altitude), count)
        matrix[row, :used] = np.maximum(trace.altitude[:used], 0.0)
        fill = 0.0 if trace.impact_time is not None else trace.altitude[-1]
        matrix[row, used:] = fill
    return matrix


def survival_curve(
    traces: list[CrashTrace], velocity: float, time: np.ndarray
) -> np.ndarray:
    selected = [trace for trace in traces if trace.velocity == velocity]
    impact_times = np.array([
        trace.impact_time if trace.impact_time is not None else np.inf
        for trace in selected
    ])
    return 100.0 * np.mean(impact_times[:, None] > time[None, :], axis=0)


def plot_crash_boundary(
    traces: list[CrashTrace], duration: float, dt: float, path: Path
) -> None:
    count = int(round(duration / dt)) + 1
    time = np.arange(count) * dt
    colors = plt.get_cmap("viridis")(
        Normalize(min(CRUISE_SPEEDS), max(CRUISE_SPEEDS))(CRUISE_SPEEDS)
    )
    plt.rcParams.update({
        "axes.facecolor": "#f8fafc",
        "axes.edgecolor": "#344054",
        "axes.titleweight": "bold",
        "figure.facecolor": "white",
        "font.size": 11,
        "grid.color": "#d0d5dd",
        "grid.alpha": 0.6,
        "legend.frameon": False,
        "text.color": "#101828",
    })
    figure, axes = plt.subplots(1, 2, figsize=(15, 6), constrained_layout=True)
    for velocity, color in zip(CRUISE_SPEEDS, colors):
        altitude = altitude_matrix(traces, velocity, count)
        low, median, high = np.percentile(altitude, (10, 50, 90), axis=0)
        axes[0].fill_between(time, low, high, color=color, alpha=0.14)
        axes[0].plot(
            time, median, color=color, linewidth=2.2,
            label=f"{velocity:g} m/s",
        )
        survival = survival_curve(traces, velocity, time)
        axes[1].step(
            time, survival, where="post", color=color, linewidth=2.4,
            label=f"{velocity:g} m/s",
        )
    axes[0].axhline(0.0, color="#b42318", linestyle="--", linewidth=1.2)
    axes[0].set(
        title="Altitude evolution after altitude feedback is disabled",
        xlabel="Mission time [s]",
        ylabel="Altitude above ground [m]",
        xlim=(0.0, duration),
        ylim=(-0.08, 4.3),
    )
    axes[0].text(
        1.0, 0.08, "ground-impact boundary", color="#b42318", fontsize=9,
    )
    axes[1].set(
        title="Monte Carlo survival probability",
        xlabel="Mission time [s]",
        ylabel="Aircraft still airborne [%]",
        xlim=(0.0, duration),
        ylim=(-3.0, 103.0),
    )
    for axis in axes:
        axis.grid(True)
    axes[1].legend(title="Cruise command", loc="lower left", ncol=2)
    figure.suptitle(
        "Velocity-dependent ground-impact boundary with altitude gain = 0",
        fontsize=18,
        fontweight="bold",
    )
    figure.text(
        0.5, -0.015,
        "Lines show medians; shading shows the 10–90% Monte Carlo range. "
        "Post-impact altitude is held at ground level.",
        ha="center", fontsize=10, color="#475467",
    )
    figure.savefig(path, dpi=200, bbox_inches="tight")
    plt.close(figure)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=16)
    parser.add_argument("--duration", type=float, default=60.0)
    parser.add_argument("--seed", type=int, default=7)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument(
        "--output", type=Path,
        default=Path("figures/velocity_crash_altitude_survival.png"),
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.samples < 1 or args.duration <= 0.0 or args.workers < 1:
        raise SystemExit("--samples, --duration, and --workers must be positive")
    tasks = make_tasks(args.samples, args.duration, args.seed)
    if args.workers == 1:
        traces = list(map(run_trace, tasks))
    else:
        with ProcessPoolExecutor(max_workers=min(args.workers, os.cpu_count() or 1)) as executor:
            traces = list(executor.map(run_trace, tasks))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    plot_crash_boundary(traces, args.duration, FirmwareParameters().dt, args.output)
    failures = sum(trace.impact_time is not None for trace in traces)
    print(f"disabled altitude feedback: {failures}/{len(traces)} ground impacts")
    print(f"Wrote {args.output.resolve()}")


if __name__ == "__main__":
    main()
