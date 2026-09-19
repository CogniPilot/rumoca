#!/usr/bin/env python3
"""Confirm ground-impact boundaries found by influential_parameter_screen.py."""

from __future__ import annotations

import argparse
import csv
import os
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
from typing import Any

import matplotlib
import numpy as np

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from influential_parameter_screen import (
    ScreenDefinition,
    ScreenTask,
    run_screen_task,
    write_csv,
)
from monte_carlo import make_trials
from sport_cub_firmware import FirmwareParameters


BOUNDARIES = (
    ScreenDefinition(
        "firmware", "thrust_ki",
        (-1.0, -0.5, -0.25, 0.0, 0.1, 0.25, 0.5, 1.0),
    ),
    ScreenDefinition(
        "firmware", "trim_thrust",
        (-1.0, -0.5, 0.0, 0.25, 0.5, 1.0),
    ),
    ScreenDefinition(
        "receiver", "attitude_kp",
        (-1.0, -0.5, -0.25, 0.0, 0.25, 1.0),
    ),
    ScreenDefinition(
        "receiver", "rate_kp",
        (-1.0, -0.5, -0.25, 0.0, 0.25, 1.0), 1,
    ),
    ScreenDefinition(
        "receiver", "rate_ki",
        (0.0, 1.0, 2.0, 4.0, 6.0, 8.0, 10.0), 1,
    ),
    ScreenDefinition(
        "airframe", "max_elevator",
        (0.05, 0.1, 0.15, 0.25, 0.5, 1.0),
    ),
)

DISPLAY_NAMES = {
    "firmware.thrust_ki": "TECS thrust integral gain",
    "firmware.trim_thrust": "TECS trim-thrust command",
    "receiver.attitude_kp": "Receiver attitude gain",
    "receiver.rate_kp[1]": "Receiver pitch-rate Kp",
    "receiver.rate_ki[1]": "Receiver pitch-rate Ki",
    "airframe.max_elevator": "Physical elevator authority",
}


def make_tasks(samples: int, duration: float, seed: int) -> list[ScreenTask]:
    baselines = [
        trial for trial in make_trials(samples, duration, seed)
        if trial.velocity == FirmwareParameters().cruise_speed
    ]
    return [
        ScreenTask(baseline, definition, scale, seed)
        for definition in BOUNDARIES
        for scale in definition.scales
        for baseline in baselines
    ]


def summarize(records: list[dict[str, Any]]) -> list[dict[str, Any]]:
    summary: list[dict[str, Any]] = []
    for definition in BOUNDARIES:
        for scale in definition.scales:
            group = [
                row for row in records
                if row["screen_parameter"] == definition.key
                and float(row["scale"]) == scale
            ]
            impacts = [row for row in group if row["failure_mode"] == "ground_impact"]
            times = [float(row["time_to_failure_s"]) for row in impacts]
            summary.append({
                "screen_parameter": definition.key,
                "nominal_value": definition.nominal,
                "scale": scale,
                "applied_value": definition.nominal * scale,
                "trials": len(group),
                "ground_impacts": len(impacts),
                "ground_impact_percent": 100.0 * len(impacts) / len(group),
                "median_time_to_impact_s": np.median(times) if times else "",
                "p10_time_to_impact_s": np.percentile(times, 10) if times else "",
                "p90_time_to_impact_s": np.percentile(times, 90) if times else "",
            })
    return summary


def plot_boundaries(summary: list[dict[str, Any]], path: Path) -> None:
    plt.rcParams.update({
        "font.size": 10,
        "axes.titleweight": "bold",
        "axes.spines.top": False,
        "legend.frameon": False,
    })
    figure, axes = plt.subplots(3, 2, figsize=(14, 12), constrained_layout=True)
    for axis, definition in zip(axes.flat, BOUNDARIES, strict=True):
        rows = [row for row in summary if row["screen_parameter"] == definition.key]
        scales = np.array([float(row["scale"]) for row in rows])
        probability = np.array([float(row["ground_impact_percent"]) for row in rows])
        axis.plot(scales, probability, "o-", color="#b42318", linewidth=2.2)
        axis.fill_between(scales, 0.0, probability, color="#f04438", alpha=0.13)
        axis.axvline(1.0, color="#667085", linestyle="--", linewidth=1.0)
        axis.set_title(DISPLAY_NAMES[definition.key])
        axis.set_xlabel("scale relative to nominal")
        axis.set_ylabel("ground-impact probability [%]", color="#b42318")
        axis.set_ylim(-4.0, 104.0)
        axis.grid(alpha=0.22)
        axis.tick_params(axis="y", labelcolor="#b42318")
        for x, y in zip(scales, probability, strict=True):
            axis.annotate(f"{y:.0f}%", (x, y), xytext=(0, 6),
                          textcoords="offset points", ha="center", fontsize=8)

        time_axis = axis.twinx()
        medians = np.array([
            np.nan if row["median_time_to_impact_s"] == ""
            else float(row["median_time_to_impact_s"])
            for row in rows
        ])
        time_axis.plot(scales, medians, "D--", color="#175cd3", linewidth=1.5,
                       markersize=5)
        time_axis.set_ylabel("median time to impact [s]", color="#175cd3")
        time_axis.set_ylim(0.0, 63.0)
        time_axis.tick_params(axis="y", labelcolor="#175cd3")
        time_axis.spines["right"].set_color("#175cd3")

    figure.suptitle(
        "Confirmed additional ground-impact boundaries",
        fontsize=19, fontweight="bold",
    )
    figure.text(
        0.5, -0.015,
        "Red circles: impact probability across 16 matched Monte Carlo trials. "
        "Blue diamonds: median impact time among crashed trials. "
        "Vertical dashed line: nominal value (1×); missing blue marker: no impact.",
        ha="center", color="#475467", fontsize=10,
    )
    figure.savefig(path, dpi=190, bbox_inches="tight")
    plt.close(figure)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=16)
    parser.add_argument("--duration", type=float, default=60.0)
    parser.add_argument("--seed", type=int, default=7)
    parser.add_argument("--workers", type=int, default=0)
    parser.add_argument(
        "--output", type=Path,
        default=Path("figures/crash_parameter_boundary_sweep.csv"),
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    tasks = make_tasks(args.samples, args.duration, args.seed)
    workers = args.workers or min(os.cpu_count() or 1, 8)
    if workers == 1:
        records = list(map(run_screen_task, tasks))
    else:
        with ProcessPoolExecutor(max_workers=workers) as executor:
            records = list(executor.map(run_screen_task, tasks, chunksize=2))
    records.sort(key=lambda row: (row["screen_parameter"], row["scale"], row["trial"]))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    write_csv(records, args.output)
    summary = summarize(records)
    summary_path = args.output.with_name(f"{args.output.stem}_summary.csv")
    with summary_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(summary[0]))
        writer.writeheader()
        writer.writerows(summary)
    plot_path = args.output.with_suffix(".png")
    plot_boundaries(summary, plot_path)
    impacts = sum(row["failure_mode"] == "ground_impact" for row in records)
    print(f"Confirmed {impacts}/{len(records)} ground impacts")
    print(f"Wrote {args.output.resolve()}")
    print(f"Wrote {summary_path.resolve()}")
    print(f"Wrote {plot_path.resolve()}")


if __name__ == "__main__":
    main()
