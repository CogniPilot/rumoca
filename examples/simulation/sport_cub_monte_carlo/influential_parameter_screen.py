#!/usr/bin/env python3
"""Screen additional Sport Cub parameters for Monte Carlo hard failures."""

from __future__ import annotations

import argparse
import csv
import os
from concurrent.futures import ProcessPoolExecutor
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Any

import matplotlib
import numpy as np

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from autopilot_gain_sweep import classify_failure, safety_metrics
from monte_carlo import TrialInput, make_trials, mission_metrics
from sport_cub_firmware import (
    AirframeParameters,
    FirmwareParameters,
    MISSION_WAYPOINTS,
    ReceiverParameters,
    SportCubFirmwareModel,
)


SIGNED_SCALES = (-1.0, 0.0, 0.25, 1.0, 4.0, 10.0)
POSITIVE_SCALES = (0.0, 0.25, 0.5, 1.0, 2.0, 4.0)
LIMIT_SCALES = (0.1, 0.25, 0.5, 1.0, 1.5, 2.0)


@dataclass(frozen=True)
class ScreenDefinition:
    component: str
    parameter: str
    scales: tuple[float, ...]
    tuple_index: int | None = None

    @property
    def nominal(self) -> float:
        owner: Any = {
            "firmware": FirmwareParameters(),
            "receiver": ReceiverParameters(),
            "airframe": AirframeParameters(),
        }[self.component]
        value = getattr(owner, self.parameter)
        return float(value if self.tuple_index is None else value[self.tuple_index])

    @property
    def key(self) -> str:
        suffix = "" if self.tuple_index is None else f"[{self.tuple_index}]"
        return f"{self.component}.{self.parameter}{suffix}"


DEFINITIONS = (
    ScreenDefinition("firmware", "speed_gain", SIGNED_SCALES),
    ScreenDefinition("firmware", "thrust_kp", SIGNED_SCALES),
    ScreenDefinition("firmware", "thrust_ki", SIGNED_SCALES),
    ScreenDefinition("firmware", "pitch_kp", SIGNED_SCALES),
    ScreenDefinition("firmware", "pitch_ki", SIGNED_SCALES),
    ScreenDefinition("firmware", "tecs_mass", LIMIT_SCALES),
    ScreenDefinition("firmware", "tecs_thrust_max", LIMIT_SCALES),
    ScreenDefinition("firmware", "trim_thrust", SIGNED_SCALES),
    ScreenDefinition("firmware", "pitch_limit", LIMIT_SCALES),
    ScreenDefinition("firmware", "course_gain", SIGNED_SCALES),
    ScreenDefinition("firmware", "bank_limit", LIMIT_SCALES),
    ScreenDefinition("firmware", "turn_elevator_gain", SIGNED_SCALES),
    ScreenDefinition("firmware", "course_attitude_kp", SIGNED_SCALES),
    ScreenDefinition("firmware", "course_attitude_ki", SIGNED_SCALES),
    ScreenDefinition("firmware", "course_attitude_kd", SIGNED_SCALES),
    ScreenDefinition("receiver", "max_pitch", LIMIT_SCALES),
    ScreenDefinition("receiver", "attitude_kp", SIGNED_SCALES),
    ScreenDefinition("receiver", "rate_kp", SIGNED_SCALES, 1),
    ScreenDefinition("receiver", "rate_ki", SIGNED_SCALES, 1),
    ScreenDefinition("receiver", "integral_limit", LIMIT_SCALES, 1),
    ScreenDefinition("airframe", "cla", POSITIVE_SCALES),
    ScreenDefinition("airframe", "cd0", POSITIVE_SCALES),
    ScreenDefinition("airframe", "alpha_stall", LIMIT_SCALES),
    ScreenDefinition("airframe", "max_elevator", LIMIT_SCALES),
)


@dataclass(frozen=True)
class ScreenTask:
    baseline: TrialInput
    definition: ScreenDefinition
    scale: float
    seed: int


def replace_parameter(owner: Any, definition: ScreenDefinition, value: float) -> Any:
    if definition.tuple_index is None:
        return replace(owner, **{definition.parameter: value})
    values = list(getattr(owner, definition.parameter))
    values[definition.tuple_index] = value
    return replace(owner, **{definition.parameter: tuple(values)})


def make_screen_tasks(samples: int, duration: float, seed: int) -> list[ScreenTask]:
    baselines = [
        trial for trial in make_trials(samples, duration, seed)
        if trial.velocity == FirmwareParameters().cruise_speed
    ]
    return [
        ScreenTask(baseline, definition, scale, seed)
        for definition in DEFINITIONS
        for scale in definition.scales
        for baseline in baselines
    ]


def run_screen_task(task: ScreenTask) -> dict[str, Any]:
    baseline, definition, scale, seed = (
        task.baseline, task.definition, task.scale, task.seed
    )
    nominal_airframe = AirframeParameters()
    airframe = replace(
        nominal_airframe,
        mass=nominal_airframe.mass * baseline.mass_scale,
        cla=nominal_airframe.cla * baseline.lift_scale,
        cd0=nominal_airframe.cd0 * baseline.drag_scale,
        thrust_max=nominal_airframe.thrust_max * baseline.thrust_scale,
    )
    firmware = FirmwareParameters()
    receiver = ReceiverParameters()
    applied = definition.nominal * scale
    if definition.component == "firmware":
        firmware = replace_parameter(firmware, definition, applied)
    elif definition.component == "receiver":
        receiver = replace_parameter(receiver, definition, applied)
    else:
        airframe = replace_parameter(airframe, definition, applied)

    first_leg = MISSION_WAYPOINTS[1] - MISSION_WAYPOINTS[0]
    heading = float(np.arctan2(first_leg[1], first_leg[0]) + baseline.heading_offset)
    model = SportCubFirmwareModel(
        airframe=airframe,
        firmware=firmware,
        receiver=receiver,
        initial_speed=baseline.initial_speed,
        initial_heading=heading,
    )
    trace = model.simulate(baseline.duration)
    failure_mode, failure_time = classify_failure(
        baseline.duration, trace, airframe, firmware.dt
    )
    record = mission_metrics(baseline, trace, airframe, heading)
    record.update(
        random_seed=seed,
        requested_duration_s=baseline.duration,
        screen_parameter=definition.key,
        component=definition.component,
        nominal_value=definition.nominal,
        scale=scale,
        applied_value=applied,
        failure_mode=failure_mode,
        time_to_failure_s=failure_time,
        **safety_metrics(trace, airframe, firmware.dt),
    )
    return record


def write_csv(records: list[dict[str, Any]], path: Path) -> None:
    fields = list(records[0])
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        writer.writerows(records)


def summarize(records: list[dict[str, Any]]) -> list[dict[str, Any]]:
    rows: list[dict[str, Any]] = []
    for definition in DEFINITIONS:
        for scale in definition.scales:
            group = [
                row for row in records
                if row["screen_parameter"] == definition.key
                and float(row["scale"]) == scale
            ]
            failures = [row for row in group if row["failure_mode"] != "none"]
            times = [float(row["time_to_failure_s"]) for row in failures]
            rows.append({
                "screen_parameter": definition.key,
                "component": definition.component,
                "nominal_value": definition.nominal,
                "scale": scale,
                "applied_value": definition.nominal * scale,
                "trials": len(group),
                "hard_failures": len(failures),
                "hard_failure_percent": 100.0 * len(failures) / len(group),
                "median_time_to_failure_s": np.median(times) if times else "",
            })
    return rows


def plot_summary(summary: list[dict[str, Any]], path: Path) -> None:
    keys = [definition.key for definition in DEFINITIONS]
    worst = [
        max(float(row["hard_failure_percent"]) for row in summary
            if row["screen_parameter"] == key)
        for key in keys
    ]
    order = np.argsort(worst)
    figure, axis = plt.subplots(figsize=(11, 9))
    colors = ["#b42318" if worst[index] > 0.0 else "#98a2b3" for index in order]
    axis.barh(np.array(keys)[order], np.array(worst)[order], color=colors)
    axis.set(xlabel="Maximum observed hard-failure probability [%]",
             title="Additional-parameter Monte Carlo crash screening")
    axis.set_xlim(0.0, 105.0)
    axis.grid(axis="x", alpha=0.25)
    for y, index in enumerate(order):
        axis.text(worst[index] + 1.0, y, f"{worst[index]:.0f}%", va="center")
    figure.text(
        0.01, 0.01,
        "Red bars identify parameters with at least one hard failure; gray bars produced none. "
        "Screening establishes candidates, not final boundaries.",
        fontsize=9, color="#475467",
    )
    figure.tight_layout(rect=(0, 0.04, 1, 1))
    figure.savefig(path, dpi=180, bbox_inches="tight")
    plt.close(figure)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=8)
    parser.add_argument("--duration", type=float, default=60.0)
    parser.add_argument("--seed", type=int, default=7)
    parser.add_argument("--workers", type=int, default=0)
    parser.add_argument(
        "--output", type=Path,
        default=Path("figures/influential_parameter_screen.csv"),
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.samples < 1 or args.duration <= 0.0:
        raise SystemExit("--samples and --duration must be positive")
    tasks = make_screen_tasks(args.samples, args.duration, args.seed)
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
    write_csv(summary, summary_path)
    plot_path = args.output.with_suffix(".png")
    plot_summary(summary, plot_path)
    candidates = sorted({
        row["screen_parameter"] for row in summary
        if float(row["hard_failure_percent"]) > 0.0
    })
    print(f"Completed {len(records)} trials across {len(DEFINITIONS)} parameters")
    print(f"Crash-relevant screening candidates: {', '.join(candidates) or 'none'}")
    print(f"Wrote {args.output.resolve()}")
    print(f"Wrote {summary_path.resolve()}")
    print(f"Wrote {plot_path.resolve()}")


if __name__ == "__main__":
    main()
