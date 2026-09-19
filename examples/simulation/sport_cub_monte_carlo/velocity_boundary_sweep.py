#!/usr/bin/env python3
"""Cruise-velocity sweeps across selected controller and physical boundaries."""

from __future__ import annotations

import argparse
import csv
import os
from concurrent.futures import ProcessPoolExecutor
from dataclasses import dataclass, replace
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
    ReceiverParameters,
    SportCubFirmwareModel,
)

CRUISE_SPEEDS = (2.0, 3.0, 4.0, 5.0, 6.0)
EXTRA_FIELDS = [
    "random_seed", "requested_duration_s", "campaign", "boundary_label",
    "altitude_gain_scale", "protection_low_m_s", "dive_slope",
    "physical_mass_scale", "actual_thrust_scale",
    "failure_mode", "time_to_failure_s",
    "maximum_abs_roll_deg", "maximum_abs_pitch_deg",
    "maximum_abs_angle_of_attack_deg", "stall_fraction",
    "aileron_saturation_fraction", "elevator_saturation_fraction",
    "throttle_saturation_fraction",
]


@dataclass(frozen=True)
class BoundaryCondition:
    campaign: str
    label: str
    altitude_gain_scale: float = 1.0
    protection_low: float = 2.6
    dive_slope: float = 0.06
    physical_mass_scale: float = 1.0
    actual_thrust_scale: float = 1.0


BOUNDARIES = (
    BoundaryCondition("altitude_feedback", "gain 0×", altitude_gain_scale=0.0),
    BoundaryCondition("altitude_feedback", "gain 0.1×", altitude_gain_scale=0.1),
    BoundaryCondition("altitude_feedback", "gain 0.25×", altitude_gain_scale=0.25),
    BoundaryCondition("altitude_feedback", "gain 1×", altitude_gain_scale=1.0),
    BoundaryCondition("safe_protection", "low 6 / dive 0.2", protection_low=6.0, dive_slope=0.2),
    BoundaryCondition("safe_protection", "low 6 / dive 0.4", protection_low=6.0, dive_slope=0.4),
    BoundaryCondition("safe_protection", "low 8 / dive 0.2", protection_low=8.0, dive_slope=0.2),
    BoundaryCondition("safe_protection", "low 8 / dive 0.4", protection_low=8.0, dive_slope=0.4),
    *tuple(
        BoundaryCondition(
            "thrust_mass",
            f"mass {mass:g}× / thrust {thrust:g}×",
            physical_mass_scale=mass,
            actual_thrust_scale=thrust,
        )
        for mass in (1.0, 1.5, 2.0)
        for thrust in (0.25, 0.5, 0.75, 1.0)
    ),
)


CAMPAIGN_TITLES = {
    "altitude_feedback": "Cruise-speed sensitivity of altitude feedback",
    "safe_protection": "Cruise-speed sensitivity of SAFE protection settings",
    "thrust_mass": "Cruise-speed sensitivity of the physical thrust-to-mass envelope",
}


class VelocityTrial(NamedTuple):
    baseline: TrialInput
    boundary: BoundaryCondition
    seed: int


def make_velocity_trials(
    samples: int, duration: float, seed: int
) -> list[VelocityTrial]:
    nominal = FirmwareParameters().cruise_speed
    draws = [
        trial for trial in make_trials(samples, duration, seed)
        if trial.velocity == nominal
    ]
    return [
        VelocityTrial(baseline._replace(velocity=velocity), boundary, seed)
        for boundary in BOUNDARIES
        for velocity in CRUISE_SPEEDS
        for baseline in draws
    ]


def run_velocity_trial(task: VelocityTrial) -> dict[str, Any]:
    baseline, boundary, seed = task
    nominal_airframe = AirframeParameters()
    airframe = replace(
        nominal_airframe,
        mass=nominal_airframe.mass * baseline.mass_scale * boundary.physical_mass_scale,
        cla=nominal_airframe.cla * baseline.lift_scale,
        cd0=nominal_airframe.cd0 * baseline.drag_scale,
        thrust_max=(
            nominal_airframe.thrust_max * baseline.thrust_scale
            * boundary.actual_thrust_scale
        ),
    )
    nominal_firmware = FirmwareParameters()
    firmware = replace(
        nominal_firmware,
        cruise_speed=baseline.velocity,
        altitude_gain=(
            nominal_firmware.altitude_gain * boundary.altitude_gain_scale
        ),
    )
    receiver = replace(
        ReceiverParameters(),
        protection_low=boundary.protection_low,
        protection_high=boundary.protection_low + 1.0,
        dive_slope=boundary.dive_slope,
    )
    first_leg = MISSION_WAYPOINTS[1] - MISSION_WAYPOINTS[0]
    heading = float(np.arctan2(first_leg[1], first_leg[0]) + baseline.heading_offset)
    model = SportCubFirmwareModel(
        airframe=airframe,
        receiver=receiver,
        firmware=firmware,
        initial_speed=baseline.initial_speed,
        initial_heading=heading,
    )
    trace = model.simulate(baseline.duration)
    record = mission_metrics(baseline, trace, airframe, heading)
    failure_mode, failure_time = classify_failure(
        baseline.duration, trace, airframe, firmware.dt
    )
    record.update(
        random_seed=seed,
        requested_duration_s=baseline.duration,
        campaign=boundary.campaign,
        boundary_label=boundary.label,
        altitude_gain_scale=boundary.altitude_gain_scale,
        protection_low_m_s=boundary.protection_low,
        dive_slope=boundary.dive_slope,
        physical_mass_scale=boundary.physical_mass_scale,
        actual_thrust_scale=boundary.actual_thrust_scale,
        failure_mode=failure_mode,
        time_to_failure_s=failure_time,
        **safety_metrics(trace, airframe, firmware.dt),
    )
    return record


def write_csv(records: list[dict[str, Any]], path: Path) -> None:
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=EXTRA_FIELDS + FIELD_NAMES)
        writer.writeheader()
        writer.writerows(records)


def campaign_boundaries(campaign: str) -> list[BoundaryCondition]:
    return [boundary for boundary in BOUNDARIES if boundary.campaign == campaign]


def aggregate(
    records: list[dict[str, Any]],
    boundaries: list[BoundaryCondition],
    field: str,
    method: str,
) -> np.ndarray:
    matrix = np.full((len(boundaries), len(CRUISE_SPEEDS)), np.nan)
    for row_index, boundary in enumerate(boundaries):
        for column_index, velocity in enumerate(CRUISE_SPEEDS):
            group = [
                row for row in records
                if row["boundary_label"] == boundary.label
                and float(row["cruise_velocity_m_s"]) == velocity
            ]
            if method == "failure":
                value = 100.0 * np.mean([row["failure_mode"] != "none" for row in group])
            elif method == "failure_time":
                times = [float(row[field]) for row in group if row[field] != ""]
                value = float(np.median(times)) if times else np.nan
            elif method == "success":
                value = 100.0 * np.mean([int(row["success"]) for row in group])
            else:
                value = float(np.median([float(row[field]) for row in group]))
                if method == "percent":
                    value *= 100.0
            matrix[row_index, column_index] = value
    return matrix


def plot_campaign(
    records: list[dict[str, Any]], campaign: str, path: Path
) -> None:
    boundaries = campaign_boundaries(campaign)
    selected = [row for row in records if row["campaign"] == campaign]
    plt.rcParams.update({
        "axes.facecolor": "#f8fafc", "axes.titleweight": "bold",
        "figure.facecolor": "white", "font.size": 10,
    })
    figure, axes = plt.subplots(2, 3, figsize=(15, 9), constrained_layout=True)
    panels = [
        ("failure_mode", "Hard-failure probability", "%", "Reds", "failure"),
        ("time_to_failure_s", "Median time to failure", "s", "viridis", "failure_time"),
        ("success", "Legacy mission success", "%", "YlGn", "success"),
        ("minimum_altitude_m", "Minimum altitude", "m", "viridis", "median"),
        ("airspeed_rmse_m_s", "Airspeed tracking RMSE", "m/s", "magma_r", "median"),
        ("maximum_abs_angle_of_attack_deg", "Maximum angle of attack", "deg", "YlOrRd", "median"),
    ]
    for axis, (field, title, unit, cmap, method) in zip(axes.flat, panels):
        matrix = aggregate(selected, boundaries, field, method)
        limits = {"vmin": 0.0, "vmax": 100.0} if unit == "%" else {}
        image = axis.imshow(matrix, origin="lower", aspect="auto", cmap=cmap, **limits)
        annotate(axis, matrix, unit)
        axis.set(
            xticks=np.arange(len(CRUISE_SPEEDS)),
            xticklabels=[f"{velocity:g}" for velocity in CRUISE_SPEEDS],
            yticks=np.arange(len(boundaries)),
            yticklabels=[boundary.label for boundary in boundaries],
            xlabel="Commanded cruise velocity [m/s]",
            title=title,
        )
        colorbar = figure.colorbar(image, ax=axis, shrink=0.86, pad=0.02)
        colorbar.set_label(unit)
    figure.suptitle(CAMPAIGN_TITLES[campaign], fontsize=17, fontweight="bold")
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
        default=Path("figures/velocity_boundary_sweep.csv"),
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.samples < 1 or args.duration <= 0.0 or args.workers < 1:
        raise SystemExit("--samples, --duration, and --workers must be positive")
    tasks = make_velocity_trials(args.samples, args.duration, args.seed)
    if args.workers == 1:
        records = list(map(run_velocity_trial, tasks))
    else:
        with ProcessPoolExecutor(max_workers=min(args.workers, os.cpu_count() or 1)) as executor:
            records = list(executor.map(run_velocity_trial, tasks))
    records.sort(key=lambda row: (
        row["campaign"], row["boundary_label"],
        row["cruise_velocity_m_s"], row["trial"],
    ))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    write_csv(records, args.output)
    for campaign in CAMPAIGN_TITLES:
        plot_path = args.output.with_name(f"{args.output.stem}_{campaign}.png")
        plot_campaign(records, campaign, plot_path)
        print(f"Wrote {plot_path.resolve()}")
    failures = sum(row["failure_mode"] != "none" for row in records)
    print(f"velocity boundaries: {failures}/{len(records)} hard failures")
    print(f"Wrote {args.output.resolve()}")


if __name__ == "__main__":
    main()
