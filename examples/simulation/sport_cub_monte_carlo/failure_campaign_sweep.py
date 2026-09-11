#!/usr/bin/env python3
"""SAFE-protection and physical thrust-to-mass failure-envelope studies."""

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

EXTRA_FIELDS = [
    "random_seed", "requested_duration_s", "campaign",
    "x_parameter", "x_value", "y_parameter", "y_value",
    "failure_mode", "time_to_failure_s",
    "maximum_abs_roll_deg", "maximum_abs_pitch_deg",
    "maximum_abs_angle_of_attack_deg", "stall_fraction",
    "aileron_saturation_fraction", "elevator_saturation_fraction",
    "throttle_saturation_fraction",
]


@dataclass(frozen=True)
class Campaign:
    name: str
    title: str
    x_parameter: str
    x_label: str
    x_values: tuple[float, ...]
    y_parameter: str
    y_label: str
    y_values: tuple[float, ...]


CAMPAIGNS = {
    campaign.name: campaign
    for campaign in (
        Campaign(
            "safe_protection",
            "SAFE low-speed protection failure envelope",
            "protection_low_m_s",
            "Protection threshold [m/s]",
            (2.6, 4.0, 5.0, 6.0, 8.0),
            "dive_slope",
            "Dive slope",
            (0.0, 0.06, 0.1, 0.2, 0.4),
        ),
        Campaign(
            "thrust_mass",
            "Physical thrust-to-mass failure envelope",
            "actual_thrust_scale",
            "Actual thrust scale",
            (0.0, 0.1, 0.25, 0.5, 0.75, 1.0),
            "physical_mass_scale",
            "Physical mass scale",
            (1.0, 1.25, 1.5, 2.0, 3.0),
        ),
    )
}


class CampaignTrial(NamedTuple):
    baseline: TrialInput
    campaign: Campaign
    x_value: float
    y_value: float
    seed: int


def make_campaign_trials(
    campaign: Campaign, samples: int, duration: float, seed: int
) -> list[CampaignTrial]:
    baselines = [
        trial for trial in make_trials(samples, duration, seed)
        if trial.velocity == FirmwareParameters().cruise_speed
    ]
    return [
        CampaignTrial(baseline, campaign, x_value, y_value, seed)
        for y_value in campaign.y_values
        for x_value in campaign.x_values
        for baseline in baselines
    ]


def run_campaign_trial(task: CampaignTrial) -> dict[str, Any]:
    baseline, campaign, x_value, y_value, seed = task
    nominal_airframe = AirframeParameters()
    airframe = replace(
        nominal_airframe,
        mass=nominal_airframe.mass * baseline.mass_scale,
        cla=nominal_airframe.cla * baseline.lift_scale,
        cd0=nominal_airframe.cd0 * baseline.drag_scale,
        thrust_max=nominal_airframe.thrust_max * baseline.thrust_scale,
    )
    receiver = ReceiverParameters()
    if campaign.name == "safe_protection":
        receiver = replace(
            receiver,
            protection_low=x_value,
            protection_high=x_value + 1.0,
            dive_slope=y_value,
        )
    else:
        airframe = replace(
            airframe,
            mass=airframe.mass * y_value,
            thrust_max=airframe.thrust_max * x_value,
        )
    first_leg = MISSION_WAYPOINTS[1] - MISSION_WAYPOINTS[0]
    heading = float(np.arctan2(first_leg[1], first_leg[0]) + baseline.heading_offset)
    firmware = FirmwareParameters()
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
        campaign=campaign.name,
        x_parameter=campaign.x_parameter,
        x_value=x_value,
        y_parameter=campaign.y_parameter,
        y_value=y_value,
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


def aggregate(
    records: list[dict[str, Any]], campaign: Campaign, field: str, method: str
) -> np.ndarray:
    matrix = np.full((len(campaign.y_values), len(campaign.x_values)), np.nan)
    for row_index, y_value in enumerate(campaign.y_values):
        for column_index, x_value in enumerate(campaign.x_values):
            group = [
                row for row in records
                if float(row["x_value"]) == x_value and float(row["y_value"]) == y_value
            ]
            if method == "failure":
                value = 100.0 * np.mean([row["failure_mode"] != "none" for row in group])
            elif method == "failure_time":
                times = [float(row[field]) for row in group if row[field] != ""]
                value = float(np.median(times)) if times else np.nan
            else:
                value = float(np.median([float(row[field]) for row in group]))
                if method == "percent":
                    value *= 100.0
            matrix[row_index, column_index] = value
    return matrix


def plot_campaign(records: list[dict[str, Any]], campaign: Campaign, path: Path) -> None:
    plt.rcParams.update({
        "axes.facecolor": "#f8fafc", "axes.titleweight": "bold",
        "figure.facecolor": "white", "font.size": 10,
    })
    figure, axes = plt.subplots(2, 3, figsize=(15, 9), constrained_layout=True)
    panels = [
        ("failure_mode", "Hard-failure probability", "%", "Reds", "failure"),
        ("time_to_failure_s", "Median time to failure", "s", "viridis", "failure_time"),
        ("minimum_altitude_m", "Minimum altitude", "m", "viridis", "median"),
        ("airspeed_rmse_m_s", "Airspeed tracking RMSE", "m/s", "magma_r", "median"),
        ("maximum_abs_angle_of_attack_deg", "Maximum angle of attack", "deg", "YlOrRd", "median"),
        ("elevator_saturation_fraction", "Elevator saturation", "%", "YlOrRd", "percent"),
    ]
    for axis, (field, title, unit, cmap, method) in zip(axes.flat, panels):
        matrix = aggregate(records, campaign, field, method)
        limits = {"vmin": 0.0, "vmax": 100.0} if unit == "%" else {}
        image = axis.imshow(matrix, origin="lower", aspect="auto", cmap=cmap, **limits)
        annotate(axis, matrix, unit)
        axis.set(
            xticks=np.arange(len(campaign.x_values)),
            xticklabels=[f"{value:g}" for value in campaign.x_values],
            yticks=np.arange(len(campaign.y_values)),
            yticklabels=[f"{value:g}" for value in campaign.y_values],
            xlabel=campaign.x_label,
            ylabel=campaign.y_label,
            title=title,
        )
        colorbar = figure.colorbar(image, ax=axis, shrink=0.86, pad=0.02)
        colorbar.set_label(unit)
    figure.suptitle(campaign.title, fontsize=18, fontweight="bold")
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
    parser.add_argument("--campaign", choices=tuple(CAMPAIGNS), required=True)
    parser.add_argument("--samples", type=int, default=16)
    parser.add_argument("--duration", type=float, default=60.0)
    parser.add_argument("--seed", type=int, default=7)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--output", type=Path)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.samples < 1 or args.duration <= 0.0 or args.workers < 1:
        raise SystemExit("--samples, --duration, and --workers must be positive")
    campaign = CAMPAIGNS[args.campaign]
    output = args.output or Path(f"figures/{campaign.name}_sweep.csv")
    tasks = make_campaign_trials(campaign, args.samples, args.duration, args.seed)
    if args.workers == 1:
        records = list(map(run_campaign_trial, tasks))
    else:
        with ProcessPoolExecutor(max_workers=min(args.workers, os.cpu_count() or 1)) as executor:
            records = list(executor.map(run_campaign_trial, tasks))
    records.sort(key=lambda row: (row["y_value"], row["x_value"], row["trial"]))
    output.parent.mkdir(parents=True, exist_ok=True)
    write_csv(records, output)
    plot_path = output.with_suffix(".png")
    plot_campaign(records, campaign, plot_path)
    failures = sum(row["failure_mode"] != "none" for row in records)
    print(f"{campaign.name}: {failures}/{len(records)} hard failures")
    print(f"Wrote {output.resolve()}")
    print(f"Wrote {plot_path.resolve()}")


if __name__ == "__main__":
    main()
