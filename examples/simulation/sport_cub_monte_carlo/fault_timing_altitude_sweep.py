#!/usr/bin/env python3
"""Monte Carlo fault-onset timing and initial-altitude envelope study."""

from __future__ import annotations

import argparse
import csv
import os
from concurrent.futures import ProcessPoolExecutor
from dataclasses import fields, replace
from pathlib import Path
from typing import Any, NamedTuple

import matplotlib
import numpy as np

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from autopilot_gain_sweep import classify_failure, safety_metrics
from monte_carlo import FIELD_NAMES, TrialInput, make_trials, mission_metrics
from sport_cub_firmware import (
    AirframeParameters,
    FirmwareParameters,
    MISSION_WAYPOINTS,
    MissionTrace,
    ReceiverParameters,
    SportCubFirmwareModel,
)

FAULT_TIMES_S = (0.0, 10.0, 20.0, 30.0, 40.0, 50.0)
INITIAL_ALTITUDES_M = (1.0, 3.0, 5.0, 10.0, 20.0)
FAULTS = ("disabled_altitude_feedback", "safe_forced_dive")
EXTRA_FIELDS = [
    "random_seed", "requested_duration_s", "fault",
    "fault_activation_time_s", "initial_altitude_m",
    "altitude_at_activation_m", "airspeed_at_activation_m_s",
    "roll_at_activation_deg", "pitch_at_activation_deg",
    "waypoint_at_activation", "failure_mode", "time_to_failure_s",
    "time_from_fault_to_failure_s", "maximum_abs_roll_deg",
    "maximum_abs_pitch_deg", "maximum_abs_angle_of_attack_deg",
    "stall_fraction", "aileron_saturation_fraction",
    "elevator_saturation_fraction", "throttle_saturation_fraction",
]


class TimingTrial(NamedTuple):
    baseline: TrialInput
    fault: str
    activation_time: float
    initial_altitude: float
    seed: int


def make_timing_trials(
    samples: int, duration: float, seed: int
) -> list[TimingTrial]:
    nominal_speed = FirmwareParameters().cruise_speed
    draws = [
        trial for trial in make_trials(samples, duration, seed)
        if trial.velocity == nominal_speed
    ]
    return [
        TimingTrial(baseline, fault, activation, altitude, seed)
        for fault in FAULTS
        for altitude in INITIAL_ALTITUDES_M
        for activation in FAULT_TIMES_S
        for baseline in draws
    ]


def run_timing_trial(task: TimingTrial) -> dict[str, Any]:
    baseline, fault, activation, initial_altitude, seed = task
    nominal = AirframeParameters()
    airframe = replace(
        nominal,
        mass=nominal.mass * baseline.mass_scale,
        cla=nominal.cla * baseline.lift_scale,
        cd0=nominal.cd0 * baseline.drag_scale,
        thrust_max=nominal.thrust_max * baseline.thrust_scale,
    )
    first_leg = MISSION_WAYPOINTS[1] - MISSION_WAYPOINTS[0]
    heading = float(np.arctan2(first_leg[1], first_leg[0]) + baseline.heading_offset)
    model = SportCubFirmwareModel(
        airframe=airframe,
        initial_speed=baseline.initial_speed,
        initial_heading=heading,
    )
    model.state[2] = initial_altitude
    before = model.simulate(activation) if activation > 0.0 else None
    activation_state = activation_metrics(model)
    inject_fault(model, fault)
    remaining = max(0.0, baseline.duration - activation)
    after = model.simulate(remaining)
    trace = concatenate_traces(before, after, activation)
    record = mission_metrics(baseline, trace, airframe, heading)
    dt = model.firmware.dt
    failure_mode, failure_time = classify_failure(
        baseline.duration, trace, airframe, dt
    )
    delay = (
        float(failure_time) - activation if failure_time != "" else ""
    )
    record.update(
        random_seed=seed,
        requested_duration_s=baseline.duration,
        fault=fault,
        fault_activation_time_s=activation,
        initial_altitude_m=initial_altitude,
        failure_mode=failure_mode,
        time_to_failure_s=failure_time,
        time_from_fault_to_failure_s=delay,
        **activation_state,
        **safety_metrics(trace, airframe, dt),
    )
    return record


def activation_metrics(model: SportCubFirmwareModel) -> dict[str, float | int]:
    state = model.state
    euler = np.array([
        np.arctan2(
            2.0 * (state[6] * state[7] + state[8] * state[9]),
            1.0 - 2.0 * (state[7] ** 2 + state[8] ** 2),
        ),
        np.arcsin(np.clip(2.0 * (state[6] * state[8] - state[9] * state[7]), -1.0, 1.0)),
    ])
    return {
        "altitude_at_activation_m": float(state[2]),
        "airspeed_at_activation_m_s": float(np.linalg.norm(state[3:6])),
        "roll_at_activation_deg": float(np.rad2deg(euler[0])),
        "pitch_at_activation_deg": float(np.rad2deg(euler[1])),
        "waypoint_at_activation": model.controller.current_waypoint,
    }


def inject_fault(model: SportCubFirmwareModel, fault: str) -> None:
    if fault == "disabled_altitude_feedback":
        model.firmware = replace(model.firmware, altitude_gain=0.0)
        return
    model.receiver = replace(
        model.receiver,
        protection_low=8.0,
        protection_high=9.0,
        dive_slope=0.4,
    )


def concatenate_traces(
    before: MissionTrace | None, after: MissionTrace, offset: float
) -> MissionTrace:
    if before is None:
        return after
    values: dict[str, np.ndarray] = {}
    for item in fields(MissionTrace):
        first = getattr(before, item.name)
        second = getattr(after, item.name)[1:]
        if item.name == "time":
            second = second + offset
        values[item.name] = np.concatenate((first, second), axis=0)
    return MissionTrace(**values)


def write_csv(records: list[dict[str, Any]], path: Path) -> None:
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=EXTRA_FIELDS + FIELD_NAMES)
        writer.writeheader()
        writer.writerows(records)


def aggregate(
    records: list[dict[str, Any]], fault: str, field: str, method: str
) -> np.ndarray:
    matrix = np.full((len(INITIAL_ALTITUDES_M), len(FAULT_TIMES_S)), np.nan)
    for row_index, altitude in enumerate(INITIAL_ALTITUDES_M):
        for column_index, activation in enumerate(FAULT_TIMES_S):
            group = [
                row for row in records
                if row["fault"] == fault
                and float(row["initial_altitude_m"]) == altitude
                and float(row["fault_activation_time_s"]) == activation
            ]
            if method == "failure":
                value = 100.0 * np.mean([row["failure_mode"] != "none" for row in group])
            elif method == "delay":
                values = [float(row[field]) for row in group if row[field] != ""]
                value = float(np.median(values)) if values else np.nan
            elif method == "success":
                value = 100.0 * np.mean([int(row["success"]) for row in group])
            else:
                value = float(np.median([float(row[field]) for row in group]))
            matrix[row_index, column_index] = value
    return matrix


def plot_fault(records: list[dict[str, Any]], fault: str, path: Path) -> None:
    title = (
        "Disabled altitude-feedback timing and recovery envelope"
        if fault == "disabled_altitude_feedback"
        else "SAFE forced-dive timing and recovery envelope"
    )
    panels = [
        ("failure_mode", "Hard-failure probability", "%", "Reds", "failure"),
        ("time_from_fault_to_failure_s", "Median delay from fault to impact", "s", "viridis", "delay"),
        ("success", "Legacy mission success", "%", "YlGn", "success"),
        ("altitude_at_activation_m", "Altitude when fault activates", "m", "viridis", "median"),
        ("airspeed_at_activation_m_s", "Airspeed when fault activates", "m/s", "magma_r", "median"),
        ("minimum_altitude_m", "Minimum mission altitude", "m", "viridis", "median"),
    ]
    plt.rcParams.update({
        "axes.facecolor": "#f8fafc", "axes.titleweight": "bold",
        "figure.facecolor": "white", "font.size": 10,
    })
    figure, axes = plt.subplots(2, 3, figsize=(15, 9), constrained_layout=True)
    for axis, (field, subtitle, unit, cmap, method) in zip(axes.flat, panels):
        matrix = aggregate(records, fault, field, method)
        limits = {"vmin": 0.0, "vmax": 100.0} if unit == "%" else {}
        image = axis.imshow(matrix, origin="lower", aspect="auto", cmap=cmap, **limits)
        annotate(axis, matrix, unit)
        axis.set(
            xticks=np.arange(len(FAULT_TIMES_S)),
            xticklabels=[f"{value:g}" for value in FAULT_TIMES_S],
            yticks=np.arange(len(INITIAL_ALTITUDES_M)),
            yticklabels=[f"{value:g}" for value in INITIAL_ALTITUDES_M],
            xlabel="Fault activation time [s]",
            ylabel="Initial altitude [m]",
            title=subtitle,
        )
        colorbar = figure.colorbar(image, ax=axis, shrink=0.86, pad=0.02)
        colorbar.set_label(unit)
    figure.suptitle(title, fontsize=18, fontweight="bold")
    figure.savefig(path, dpi=200, bbox_inches="tight")
    plt.close(figure)


def annotate(axis: Any, matrix: np.ndarray, unit: str) -> None:
    finite = matrix[np.isfinite(matrix)]
    midpoint = 0.5 * (float(np.min(finite)) + float(np.max(finite))) if len(finite) else 0.0
    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            value = matrix[row_index, column_index]
            label = "—" if not np.isfinite(value) else (
                f"{value:.0f}" if unit == "%" else f"{value:.2f}"
            )
            color = "white" if np.isfinite(value) and value > midpoint else "#101828"
            axis.text(
                column_index, row_index, label,
                ha="center", va="center", color=color, fontsize=8, fontweight="bold",
            )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=16)
    parser.add_argument("--duration", type=float, default=60.0)
    parser.add_argument("--seed", type=int, default=7)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument(
        "--output", type=Path,
        default=Path("figures/fault_timing_altitude_sweep.csv"),
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.samples < 1 or args.duration <= 0.0 or args.workers < 1:
        raise SystemExit("--samples, --duration, and --workers must be positive")
    tasks = make_timing_trials(args.samples, args.duration, args.seed)
    if args.workers == 1:
        records = list(map(run_timing_trial, tasks))
    else:
        with ProcessPoolExecutor(max_workers=min(args.workers, os.cpu_count() or 1)) as executor:
            records = list(executor.map(run_timing_trial, tasks))
    records.sort(key=lambda row: (
        row["fault"], row["initial_altitude_m"],
        row["fault_activation_time_s"], row["trial"],
    ))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    write_csv(records, args.output)
    for fault in FAULTS:
        plot_path = args.output.with_name(f"{args.output.stem}_{fault}.png")
        plot_fault(records, fault, plot_path)
        print(f"Wrote {plot_path.resolve()}")
    failures = sum(row["failure_mode"] != "none" for row in records)
    print(f"timing/altitude study: {failures}/{len(records)} hard failures")
    print(f"Wrote {args.output.resolve()}")


if __name__ == "__main__":
    main()
