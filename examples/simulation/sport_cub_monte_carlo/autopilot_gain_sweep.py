#!/usr/bin/env python3
"""One-at-a-time autopilot gain sweeps for the Sport Cub mission model."""

from __future__ import annotations

import argparse
import csv
import os
from concurrent.futures import ProcessPoolExecutor
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Any, NamedTuple

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from monte_carlo import FIELD_NAMES, TrialInput, make_trials, mission_metrics
from sport_cub_firmware import (
    AirframeParameters,
    FirmwareParameters,
    MISSION_WAYPOINTS,
    MissionTrace,
    SportCubFirmwareModel,
)

DEFAULT_SCALES = (
    -1.0, -0.5, -0.25, 0.0, 0.1, 0.25, 0.5,
    0.75, 1.0, 1.5, 2.0, 4.0, 10.0,
)
POLARITY_SCALES = (
    -10.0, -4.0, -2.0, -1.0, -0.5, -0.25,
    0.0, 0.25, 0.5, 1.0, 2.0, 4.0,
)
SWEEP_FIELDS = [
    "random_seed",
    "requested_duration_s",
    "sweep_parameter",
    "sweep_nominal_value",
    "sweep_scale",
    "sweep_applied_value",
    "failure_mode",
    "time_to_failure_s",
    "maximum_abs_roll_deg",
    "maximum_abs_pitch_deg",
    "maximum_abs_angle_of_attack_deg",
    "stall_fraction",
    "aileron_saturation_fraction",
    "elevator_saturation_fraction",
    "throttle_saturation_fraction",
]

UNSAFE_ROLL_RAD = np.deg2rad(90.0)
UNSAFE_PITCH_RAD = np.deg2rad(60.0)
STALL_DURATION_S = 1.0
UNSAFE_ATTITUDE_DURATION_S = 0.5


@dataclass(frozen=True)
class SweepDefinition:
    parameter: str
    scales: tuple[float, ...] = DEFAULT_SCALES

    @property
    def nominal_value(self) -> float:
        return float(getattr(FirmwareParameters(), self.parameter))


class GainTrial(NamedTuple):
    baseline: TrialInput
    sweep: SweepDefinition
    scale: float
    seed: int


SWEEPS = {
    definition.parameter: definition
    for definition in (
        SweepDefinition("altitude_gain", POLARITY_SCALES),
        SweepDefinition("speed_gain"),
        SweepDefinition("thrust_ki"),
        SweepDefinition("pitch_ki", POLARITY_SCALES),
        SweepDefinition("course_gain"),
        SweepDefinition("turn_elevator_gain"),
        SweepDefinition("pitch_attitude_kp"),
        SweepDefinition("pitch_attitude_ki"),
        SweepDefinition("course_attitude_kp"),
        SweepDefinition("course_attitude_ki"),
        SweepDefinition("course_attitude_kd"),
    )
}


def make_gain_trials(
    parameter: str, samples: int, duration: float, seed: int
) -> list[GainTrial]:
    """Reuse identical physical draws at every gain scale."""
    sweep = SWEEPS[parameter]
    baselines = [
        trial
        for trial in make_trials(samples, duration, seed)
        if trial.velocity == FirmwareParameters().cruise_speed
    ]
    return [
        GainTrial(baseline, sweep, scale, seed)
        for scale in sweep.scales
        for baseline in baselines
    ]


def run_gain_trial(task: GainTrial) -> dict[str, Any]:
    baseline, sweep, scale, seed = task
    nominal_airframe = AirframeParameters()
    airframe = replace(
        nominal_airframe,
        mass=nominal_airframe.mass * baseline.mass_scale,
        cla=nominal_airframe.cla * baseline.lift_scale,
        cd0=nominal_airframe.cd0 * baseline.drag_scale,
        thrust_max=nominal_airframe.thrust_max * baseline.thrust_scale,
    )
    applied_value = sweep.nominal_value * scale
    firmware = replace(FirmwareParameters(), **{sweep.parameter: applied_value})
    first_leg = MISSION_WAYPOINTS[1] - MISSION_WAYPOINTS[0]
    heading = float(np.arctan2(first_leg[1], first_leg[0]) + baseline.heading_offset)
    model = SportCubFirmwareModel(
        airframe=airframe,
        firmware=firmware,
        initial_speed=baseline.initial_speed,
        initial_heading=heading,
    )
    trace = model.simulate(baseline.duration)
    record = mission_metrics(baseline, trace, airframe, heading)
    safety = safety_metrics(trace, airframe, firmware.dt)
    failure_mode, failure_time = classify_failure(
        baseline.duration, trace, airframe, firmware.dt
    )
    record.update(
        random_seed=seed,
        requested_duration_s=baseline.duration,
        sweep_parameter=sweep.parameter,
        sweep_nominal_value=sweep.nominal_value,
        sweep_scale=scale,
        sweep_applied_value=applied_value,
        failure_mode=failure_mode,
        time_to_failure_s=failure_time,
        **safety,
    )
    return record


def safety_metrics(
    trace: MissionTrace, airframe: AirframeParameters, dt: float
) -> dict[str, float]:
    steady = trace.time >= min(5.0, 0.25 * float(trace.time[-1]))
    if not np.any(steady):
        steady = np.ones_like(trace.time, dtype=bool)
    alpha = trace.angle_of_attack[steady]
    return {
        "maximum_abs_roll_deg": float(np.rad2deg(np.max(np.abs(trace.euler[:, 0])))),
        "maximum_abs_pitch_deg": float(np.rad2deg(np.max(np.abs(trace.euler[:, 1])))),
        "maximum_abs_angle_of_attack_deg": float(np.rad2deg(np.max(np.abs(alpha)))),
        "stall_fraction": float(np.mean(alpha >= airframe.alpha_stall)),
        "aileron_saturation_fraction": float(np.mean(np.abs(trace.aileron[steady]) >= 0.99)),
        "elevator_saturation_fraction": float(np.mean(np.abs(trace.elevator[steady]) >= 0.99)),
        "throttle_saturation_fraction": float(
            np.mean((trace.throttle[steady] <= 0.01) | (trace.throttle[steady] >= 0.99))
        ),
    }


def classify_failure(
    duration: float,
    trace: MissionTrace,
    airframe: AirframeParameters,
    dt: float,
) -> tuple[str, float | str]:
    """Classify only hard failures directly evidenced by the trace."""
    finite_rows = (
        np.isfinite(trace.position).all(axis=1)
        & np.isfinite(trace.airspeed)
        & np.isfinite(trace.throttle)
        & np.isfinite(trace.euler).all(axis=1)
        & np.isfinite(trace.angle_of_attack)
    )
    invalid = np.flatnonzero(~finite_rows)
    if len(invalid):
        return "non_finite_state", float(trace.time[int(invalid[0])])
    candidates: list[tuple[float, str]] = []
    impact = first_sustained(trace.position[:, 2] <= 0.0, 1)
    stall = first_sustained(
        trace.angle_of_attack >= airframe.alpha_stall,
        max(1, int(round(STALL_DURATION_S / dt))),
    )
    unsafe = first_sustained(
        (np.abs(trace.euler[:, 0]) >= UNSAFE_ROLL_RAD)
        | (np.abs(trace.euler[:, 1]) >= UNSAFE_PITCH_RAD),
        max(1, int(round(UNSAFE_ATTITUDE_DURATION_S / dt))),
    )
    for index, mode in (
        (impact, "ground_impact"),
        (stall, "sustained_stall"),
        (unsafe, "unsafe_attitude"),
    ):
        if index is not None:
            candidates.append((float(trace.time[index]), mode))
    if candidates:
        failure_time, failure_mode = min(candidates)
        return failure_mode, failure_time
    if float(trace.time[-1]) < duration - 0.5 * dt:
        return "early_termination", float(trace.time[-1])
    return "none", ""


def first_sustained(mask: np.ndarray, count: int) -> int | None:
    if len(mask) < count:
        return None
    hits = np.convolve(mask.astype(np.int64), np.ones(count, dtype=np.int64), mode="valid")
    indices = np.flatnonzero(hits == count)
    return int(indices[0]) if len(indices) else None


def write_csv(records: list[dict[str, Any]], path: Path) -> None:
    field_names = SWEEP_FIELDS + FIELD_NAMES
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=field_names)
        writer.writeheader()
        writer.writerows(records)


def plot_sweep(records: list[dict[str, Any]], path: Path) -> None:
    scales = np.array(sorted({float(row["sweep_scale"]) for row in records}))
    trials = sorted({int(row["trial"]) for row in records})
    figure, axes = plt.subplots(3, 3, figsize=(14, 11), sharex=True)
    panels = [
        ("airspeed_rmse_m_s", "airspeed RMSE [m/s]"),
        ("altitude_rmse_m", "altitude RMSE [m]"),
        ("cross_track_rmse_m", "cross-track RMSE [m]"),
        ("minimum_altitude_m", "minimum altitude [m]"),
        ("maximum_abs_pitch_deg", "maximum |pitch| [deg]"),
        ("maximum_abs_roll_deg", "maximum |roll| [deg]"),
        ("maximum_abs_angle_of_attack_deg", "maximum |AoA| [deg]"),
        ("elevator_saturation_fraction", "elevator saturation [%]"),
    ]
    for axis, (field, label) in zip(axes.flat, panels):
        values = metric_matrix(records, trials, scales, field)
        if field.endswith("_fraction"):
            values *= 100.0
        for row in values:
            axis.plot(scales, row, color="tab:blue", alpha=0.12, linewidth=0.7)
        low, median, high = np.percentile(values, (10, 50, 90), axis=0)
        axis.fill_between(scales, low, high, color="tab:blue", alpha=0.22, label="10–90%")
        axis.plot(scales, median, "o-", color="tab:blue", linewidth=1.8, label="median")
        axis.axvline(1.0, color="black", linestyle="--", linewidth=0.9)
        configure_scale_axis(axis, scales)
        axis.set_ylabel(label)
        axis.grid(alpha=0.25)
    outcome_axis = axes.flat[8]
    success = group_percent(records, scales, lambda row: int(row["success"]) == 1)
    hard_failure = group_percent(records, scales, lambda row: row["failure_mode"] != "none")
    outcome_axis.plot(scales, success, "o-", color="tab:green", label="mission-tracking success")
    outcome_axis.plot(scales, hard_failure, "o-", color="tab:red", label="hard failure")
    outcome_axis.set(ylabel="trials [%]", ylim=(-5.0, 105.0))
    outcome_axis.axvline(1.0, color="black", linestyle="--", linewidth=0.9)
    configure_scale_axis(outcome_axis, scales)
    outcome_axis.grid(alpha=0.25)
    outcome_axis.legend(loc="best")
    for axis in axes[-1]:
        axis.set_xlabel("gain scale relative to nominal")
    axes.flat[0].legend(loc="best")
    parameter = records[0]["sweep_parameter"]
    figure.suptitle(f"Sport Cub {parameter} sweep under airframe uncertainty")
    figure.tight_layout()
    figure.savefig(path, dpi=180)
    plt.close(figure)


def configure_scale_axis(axis: Any, scales: np.ndarray) -> None:
    if np.any(scales < 0.0):
        axis.set_xscale("symlog", linthresh=0.25)
        axis.set_xticks(scales)
        axis.set_xticklabels([f"{scale:g}" for scale in scales], rotation=45)


def plot_failure_envelope(records: list[dict[str, Any]], path: Path) -> None:
    scales = np.array(sorted({float(row["sweep_scale"]) for row in records}))
    duration = float(records[0]["requested_duration_s"])
    figure, axes = plt.subplots(1, 2, figsize=(13, 5.5), constrained_layout=True)
    failure_rate = group_percent(
        records, scales, lambda row: row["failure_mode"] != "none"
    )
    axes[0].plot(scales, failure_rate, "o-", color="tab:red", linewidth=2.2)
    axes[0].fill_between(scales, 0.0, failure_rate, color="tab:red", alpha=0.16)
    axes[0].set(ylabel="hard-failure probability [%]", ylim=(-5.0, 105.0))
    for scale, rate in zip(scales, failure_rate):
        axes[0].annotate(
            f"{rate:.0f}%", (scale, rate), xytext=(0, 7),
            textcoords="offset points", ha="center", fontsize=8,
        )
    for scale in scales:
        group = [row for row in records if float(row["sweep_scale"]) == scale]
        failures = np.array([
            float(row["time_to_failure_s"])
            for row in group if row["time_to_failure_s"] != ""
        ])
        if len(failures):
            axes[1].scatter(
                np.full(len(failures), scale),
                failures,
                color="tab:red",
                alpha=0.32,
                s=18,
            )
            low, median, high = np.percentile(failures, (10, 50, 90))
            axes[1].errorbar(
                scale, median,
                yerr=[[median - low], [high - median]],
                fmt="o", color="#b42318", capsize=4, linewidth=1.8,
            )
        survivors = sum(row["failure_mode"] == "none" for row in group)
        if survivors:
            axes[1].scatter(
                [scale], [duration], marker="^", color="tab:green", s=55,
            )
    axes[1].set(
        ylabel="time to hard failure [s]",
        ylim=(0.0, 1.08 * duration),
        title="Failure timing and completed missions",
    )
    axes[1].plot([], [], "o", color="#b42318", label="median and 10–90%")
    axes[1].plot([], [], "^", color="tab:green", label="survived duration")
    axes[1].legend(loc="best")
    for axis in axes:
        configure_scale_axis(axis, scales)
        axis.axvline(1.0, color="black", linestyle="--", linewidth=0.9)
        axis.set_xlabel("gain scale relative to nominal")
        axis.grid(alpha=0.25)
    axes[0].set_title("Monte Carlo hard-failure probability")
    parameter = records[0]["sweep_parameter"]
    figure.suptitle(
        f"Sport Cub {parameter} failure envelope",
        fontsize=16,
        fontweight="bold",
    )
    figure.savefig(path, dpi=200, bbox_inches="tight")
    plt.close(figure)


def metric_matrix(
    records: list[dict[str, Any]], trials: list[int], scales: np.ndarray, field: str
) -> np.ndarray:
    lookup = {
        (int(row["trial"]), float(row["sweep_scale"])): float(row[field])
        for row in records
    }
    return np.array([[lookup[(trial, scale)] for scale in scales] for trial in trials])


def group_percent(
    records: list[dict[str, Any]], scales: np.ndarray, predicate: Any
) -> np.ndarray:
    return np.array([
        100.0 * np.mean([predicate(row) for row in records if float(row["sweep_scale"]) == scale])
        for scale in scales
    ])


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--parameter", choices=tuple(SWEEPS), default="pitch_attitude_kp")
    parser.add_argument("--samples", type=int, default=16, help="trials per gain scale")
    parser.add_argument("--duration", type=float, default=60.0, help="mission duration [s]")
    parser.add_argument("--seed", type=int, default=7, help="Monte Carlo seed")
    parser.add_argument(
        "--workers", type=int, default=0,
        help="worker processes; 0 selects automatically",
    )
    parser.add_argument(
        "--output", type=Path,
        default=Path("figures/autopilot_gain_sweep.csv"),
    )
    parser.add_argument("--plot", type=Path, help="PNG path; defaults beside CSV")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.samples < 1 or args.duration <= 0.0:
        raise SystemExit("--samples and --duration must be positive")
    tasks = make_gain_trials(args.parameter, args.samples, args.duration, args.seed)
    workers = args.workers or min(
        len(SWEEPS[args.parameter].scales), os.cpu_count() or 1
    )
    if workers == 1:
        records = list(map(run_gain_trial, tasks))
    else:
        with ProcessPoolExecutor(max_workers=workers) as executor:
            records = list(executor.map(run_gain_trial, tasks))
    records.sort(key=lambda row: (row["sweep_scale"], row["trial"]))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    write_csv(records, args.output)
    plot_path = args.plot or args.output.with_suffix(".png")
    plot_path.parent.mkdir(parents=True, exist_ok=True)
    plot_sweep(records, plot_path)
    failures = sum(row["failure_mode"] != "none" for row in records)
    failure_plot = args.output.with_name(f"{args.output.stem}_failure_envelope.png")
    if failures:
        plot_failure_envelope(records, failure_plot)
    print(f"{args.parameter}: {failures}/{len(records)} hard failures")
    print(f"Wrote {args.output.resolve()}")
    print(f"Wrote {plot_path.resolve()}")
    if failures:
        print(f"Wrote {failure_plot.resolve()}")


if __name__ == "__main__":
    main()
