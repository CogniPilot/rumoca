#!/usr/bin/env python3
"""Pitch-attitude Kp x Ki interaction sweep for the Sport Cub mission."""

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

from autopilot_gain_sweep import classify_failure, safety_metrics
from monte_carlo import FIELD_NAMES, TrialInput, make_trials, mission_metrics
from sport_cub_firmware import (
    AirframeParameters,
    FirmwareParameters,
    MISSION_WAYPOINTS,
    SportCubFirmwareModel,
)

KP_SCALES = (1.0, 2.0, 4.0, 10.0)
KI_SCALES = (1.0, 2.0, 4.0, 6.0, 8.0, 10.0)
INTERACTION_FIELDS = [
    "random_seed",
    "requested_duration_s",
    "pitch_attitude_kp_nominal",
    "pitch_attitude_kp_scale",
    "pitch_attitude_kp_applied",
    "pitch_attitude_ki_nominal",
    "pitch_attitude_ki_scale",
    "pitch_attitude_ki_applied",
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


class InteractionTrial(NamedTuple):
    baseline: TrialInput
    kp_scale: float
    ki_scale: float
    seed: int


def make_interaction_trials(
    samples: int, duration: float, seed: int
) -> list[InteractionTrial]:
    nominal_speed = FirmwareParameters().cruise_speed
    baselines = [
        trial
        for trial in make_trials(samples, duration, seed)
        if trial.velocity == nominal_speed
    ]
    return [
        InteractionTrial(baseline, kp_scale, ki_scale, seed)
        for kp_scale in KP_SCALES
        for ki_scale in KI_SCALES
        for baseline in baselines
    ]


def run_interaction_trial(task: InteractionTrial) -> dict[str, Any]:
    baseline, kp_scale, ki_scale, seed = task
    nominal_airframe = AirframeParameters()
    airframe = replace(
        nominal_airframe,
        mass=nominal_airframe.mass * baseline.mass_scale,
        cla=nominal_airframe.cla * baseline.lift_scale,
        cd0=nominal_airframe.cd0 * baseline.drag_scale,
        thrust_max=nominal_airframe.thrust_max * baseline.thrust_scale,
    )
    nominal_firmware = FirmwareParameters()
    firmware = replace(
        nominal_firmware,
        pitch_attitude_kp=nominal_firmware.pitch_attitude_kp * kp_scale,
        pitch_attitude_ki=nominal_firmware.pitch_attitude_ki * ki_scale,
    )
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
        pitch_attitude_kp_nominal=nominal_firmware.pitch_attitude_kp,
        pitch_attitude_kp_scale=kp_scale,
        pitch_attitude_kp_applied=firmware.pitch_attitude_kp,
        pitch_attitude_ki_nominal=nominal_firmware.pitch_attitude_ki,
        pitch_attitude_ki_scale=ki_scale,
        pitch_attitude_ki_applied=firmware.pitch_attitude_ki,
        failure_mode=failure_mode,
        time_to_failure_s=failure_time,
        **safety,
    )
    return record


def write_csv(records: list[dict[str, Any]], path: Path) -> None:
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=INTERACTION_FIELDS + FIELD_NAMES)
        writer.writeheader()
        writer.writerows(records)


def configure_plot_style() -> None:
    plt.rcParams.update({
        "axes.facecolor": "#f7f9fc",
        "axes.edgecolor": "#344054",
        "axes.labelcolor": "#1d2939",
        "axes.titleweight": "bold",
        "figure.facecolor": "white",
        "font.size": 10,
        "grid.color": "#d0d5dd",
        "grid.alpha": 0.55,
        "legend.frameon": False,
        "text.color": "#101828",
    })


def plot_response_surfaces(records: list[dict[str, Any]], path: Path) -> None:
    configure_plot_style()
    figure, axes = plt.subplots(2, 3, figsize=(15, 9), constrained_layout=True)
    panels = [
        ("success", "Legacy mission success", "%", "YlGn", "mean"),
        ("failure_mode", "Screened hard failures", "%", "Reds", "failure"),
        ("airspeed_rmse_m_s", "Airspeed tracking RMSE", "m/s", "magma_r", "median"),
        ("altitude_rmse_m", "Altitude tracking RMSE", "m", "magma_r", "median"),
        ("elevator_saturation_fraction", "Elevator saturation", "%", "YlOrRd", "percent"),
        ("maximum_abs_angle_of_attack_deg", "Maximum angle of attack", "deg", "YlOrRd", "median"),
    ]
    for axis, (field, title, unit, cmap, method) in zip(axes.flat, panels):
        matrix = aggregate_matrix(records, field, method)
        limits = {"vmin": 0.0, "vmax": 100.0} if unit == "%" else {}
        image = axis.imshow(
            matrix, origin="lower", aspect="auto", cmap=cmap, **limits
        )
        annotate_heatmap(axis, matrix, unit)
        axis.set(
            xticks=np.arange(len(KI_SCALES)),
            xticklabels=[f"{scale:g}×" for scale in KI_SCALES],
            yticks=np.arange(len(KP_SCALES)),
            yticklabels=[f"{scale:g}×" for scale in KP_SCALES],
            xlabel="Pitch attitude Ki scale",
            ylabel="Pitch attitude Kp scale",
            title=title,
        )
        colorbar = figure.colorbar(image, ax=axis, shrink=0.86, pad=0.02)
        colorbar.set_label(unit)
    figure.suptitle(
        "Sport Cub pitch-controller interaction envelope",
        fontsize=18,
        fontweight="bold",
    )
    figure.savefig(path, dpi=200, bbox_inches="tight")
    plt.close(figure)


def aggregate_matrix(
    records: list[dict[str, Any]], field: str, method: str
) -> np.ndarray:
    matrix = np.empty((len(KP_SCALES), len(KI_SCALES)))
    for row_index, kp_scale in enumerate(KP_SCALES):
        for column_index, ki_scale in enumerate(KI_SCALES):
            group = [
                row for row in records
                if float(row["pitch_attitude_kp_scale"]) == kp_scale
                and float(row["pitch_attitude_ki_scale"]) == ki_scale
            ]
            if method == "failure":
                value = 100.0 * np.mean([row["failure_mode"] != "none" for row in group])
            else:
                values = np.array([float(row[field]) for row in group])
                value = np.mean(values) if method == "mean" else np.median(values)
                if method == "percent" or field == "success":
                    value *= 100.0
            matrix[row_index, column_index] = value
    return matrix


def annotate_heatmap(axis: Any, matrix: np.ndarray, unit: str) -> None:
    midpoint = 0.5 * (float(np.min(matrix)) + float(np.max(matrix)))
    for row_index in range(matrix.shape[0]):
        for column_index in range(matrix.shape[1]):
            value = matrix[row_index, column_index]
            label = f"{value:.0f}" if unit == "%" else f"{value:.2f}"
            color = "white" if value > midpoint else "#101828"
            axis.text(
                column_index, row_index, label,
                ha="center", va="center", color=color, fontsize=8, fontweight="bold",
            )


def plot_interaction_profiles(records: list[dict[str, Any]], path: Path) -> None:
    configure_plot_style()
    figure, axes = plt.subplots(2, 2, figsize=(13, 9), sharex=True, constrained_layout=True)
    panels = [
        ("airspeed_rmse_m_s", "Airspeed tracking", "RMSE [m/s]"),
        ("altitude_rmse_m", "Altitude tracking", "RMSE [m]"),
        ("maximum_abs_angle_of_attack_deg", "Angle-of-attack excursion", "Maximum |AoA| [deg]"),
        ("elevator_saturation_fraction", "Elevator authority usage", "Saturation time [%]"),
    ]
    colors = plt.get_cmap("viridis")(Normalize(min(KP_SCALES), max(KP_SCALES))(KP_SCALES))
    for axis, (field, title, label) in zip(axes.flat, panels):
        for kp_scale, color in zip(KP_SCALES, colors):
            matrix = profile_matrix(records, kp_scale, field)
            if field.endswith("_fraction"):
                matrix *= 100.0
            low, median, high = np.percentile(matrix, (10, 50, 90), axis=0)
            axis.fill_between(KI_SCALES, low, high, color=color, alpha=0.13)
            axis.plot(
                KI_SCALES, median, "o-", color=color, linewidth=2,
                markersize=5, label=f"Kp {kp_scale:g}×",
            )
        axis.axvline(1.0, color="#667085", linestyle="--", linewidth=1)
        axis.set(title=title, ylabel=label)
        axis.grid(True)
    for axis in axes[-1]:
        axis.set_xlabel("Pitch attitude Ki scale")
    axes.flat[0].legend(ncol=2, loc="best")
    figure.suptitle(
        "Pitch Kp–Ki interaction under Monte Carlo airframe uncertainty",
        fontsize=17,
        fontweight="bold",
    )
    figure.savefig(path, dpi=200, bbox_inches="tight")
    plt.close(figure)


def profile_matrix(
    records: list[dict[str, Any]], kp_scale: float, field: str
) -> np.ndarray:
    trials = sorted({int(row["trial"]) for row in records})
    lookup = {
        (
            float(row["pitch_attitude_kp_scale"]),
            float(row["pitch_attitude_ki_scale"]),
            int(row["trial"]),
        ): float(row[field])
        for row in records
    }
    return np.array([
        [lookup[(kp_scale, ki_scale, trial)] for ki_scale in KI_SCALES]
        for trial in trials
    ])


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=16, help="trials per gain pair")
    parser.add_argument("--duration", type=float, default=60.0, help="mission duration [s]")
    parser.add_argument("--seed", type=int, default=7, help="Monte Carlo seed")
    parser.add_argument("--workers", type=int, default=4, help="CPU worker processes")
    parser.add_argument(
        "--output", type=Path,
        default=Path("figures/pitch_attitude_kp_ki_interaction.csv"),
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.samples < 1 or args.duration <= 0.0 or args.workers < 1:
        raise SystemExit("--samples, --duration, and --workers must be positive")
    tasks = make_interaction_trials(args.samples, args.duration, args.seed)
    if args.workers == 1:
        records = list(map(run_interaction_trial, tasks))
    else:
        with ProcessPoolExecutor(max_workers=min(args.workers, os.cpu_count() or 1)) as executor:
            records = list(executor.map(run_interaction_trial, tasks))
    records.sort(key=lambda row: (
        row["pitch_attitude_kp_scale"], row["pitch_attitude_ki_scale"], row["trial"]
    ))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    write_csv(records, args.output)
    surface_path = args.output.with_name(f"{args.output.stem}_surfaces.png")
    profile_path = args.output.with_name(f"{args.output.stem}_profiles.png")
    plot_response_surfaces(records, surface_path)
    plot_interaction_profiles(records, profile_path)
    failures = sum(row["failure_mode"] != "none" for row in records)
    print(f"pitch Kp x Ki: {failures}/{len(records)} hard failures")
    print(f"Wrote {args.output.resolve()}")
    print(f"Wrote {surface_path.resolve()}")
    print(f"Wrote {profile_path.resolve()}")


if __name__ == "__main__":
    main()
