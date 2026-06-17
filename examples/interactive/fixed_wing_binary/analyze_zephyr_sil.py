#!/usr/bin/env python3
"""Analyze Zephyr-backed fixed-wing SIL traces against controller waypoints."""

from __future__ import annotations

import argparse
import csv
import math
import sys
from pathlib import Path

from probe_zephyr_waypoints import read_fwdbg_log


WAYPOINTS = [
    (-4.0, -5.0, 3.0),
    (-3.0, 2.0, 3.0),
    (16.20, 2.0, 3.0),
    (16.0, -4.22, 3.0),
    (6.88, -5.1, 3.0),
    (-4.0, -5.0, 3.0),
]


def read_csv(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def value(row: dict[str, str], key: str) -> float:
    raw = row.get(key, "")
    return float(raw) if raw not in ("", None) else math.nan


def finite_values(rows: list[dict[str, str]], key: str) -> list[float]:
    vals = [value(row, key) for row in rows if row.get(key, "") not in ("", None)]
    return [val for val in vals if math.isfinite(val)]


def print_range(label: str, vals: list[float]) -> None:
    if vals:
        print(f"{label}: {min(vals):.3f}..{max(vals):.3f} first={vals[0]:.3f} last={vals[-1]:.3f}")


def summarize_path(name: str, rows: list[dict[str, str]], keys: tuple[str, str, str]) -> None:
    if not rows:
        print(f"{name}: no rows")
        return
    xs = finite_values(rows, keys[0])
    ys = finite_values(rows, keys[1])
    zs = finite_values(rows, keys[2])
    ts = finite_values(rows, "t") if "t" in rows[0] else [i * 0.01 for i in range(len(rows))]
    print(f"{name}: rows={len(rows)} t={min(ts):.2f}..{max(ts):.2f}")
    print_range(f"{name} x", xs)
    print_range(f"{name} y", ys)
    print_range(f"{name} z", zs)
    for index, waypoint in enumerate(WAYPOINTS, start=1):
        best = min(
            (
                (math.dist((x, y, z), waypoint), t, x, y, z)
                for t, x, y, z in zip(ts, xs, ys, zs)
            ),
            key=lambda item: item[0],
        )
        print(
            f"{name} waypoint {index}: min_dist={best[0]:.2f} "
            f"at t={best[1]:.2f} pos=({best[2]:.2f},{best[3]:.2f},{best[4]:.2f})"
        )


def summarize_commands(name: str, rows: list[dict[str, str]]) -> None:
    if not rows:
        return
    for key in ("aileron", "elevator", "throttle", "rudder", "stabilizer"):
        print_range(f"{name} {key}", finite_values(rows, key))


def summarize_fwdbg(rows: list[dict[str, float]]) -> None:
    if not rows:
        print("fwdbg: no rows")
        return
    print(f"fwdbg: rows={len(rows)} time={rows[0]['fw_time']:.2f}..{rows[-1]['fw_time']:.2f}")
    for key in (
        "dbg_current_wp",
        "dbg_airborne",
        "dbg_des_heading",
        "dbg_des_v",
        "dbg_des_gamma",
        "dbg_aileron",
        "dbg_elevator",
        "dbg_throttle",
    ):
        vals = [row[key] for row in rows]
        print_range(f"fwdbg {key}", vals)
    changes: list[dict[str, float]] = []
    last_wp: int | None = None
    for row in rows:
        wp = int(row["dbg_current_wp"])
        if wp != last_wp:
            changes.append(row)
            last_wp = wp
    print(f"fwdbg waypoint_changes={len(changes)}")
    for row in changes[:12]:
        print(
            "  change "
            f"t={row['fw_time']:.2f} pos=({row['dbg_x']:.2f},{row['dbg_y']:.2f},{row['dbg_z']:.2f}) "
            f"wp={int(row['dbg_current_wp'])} des_heading={row['dbg_des_heading']:.3f}"
        )
    if len(changes) > 12:
        print("  ...")
        for row in changes[-6:]:
            print(
                "  change "
                f"t={row['fw_time']:.2f} pos=({row['dbg_x']:.2f},{row['dbg_y']:.2f},{row['dbg_z']:.2f}) "
                f"wp={int(row['dbg_current_wp'])} des_heading={row['dbg_des_heading']:.3f}"
            )


def compare_replay(trace: list[dict[str, str]], replay: list[dict[str, str]]) -> None:
    if not trace or not replay:
        return
    n = min(len(trace), len(replay))
    print(f"python_replay: rows={len(replay)} compared={n}")
    summarize_commands("python_replay", replay)
    for key in ("current_wp", "des_gamma", "des_heading", "phi_cmd", "chi_err"):
        print_range(f"python_replay {key}", finite_values(replay, key))
    for key in ("aileron", "elevator", "throttle", "rudder"):
        diffs = [
            abs(value(trace_row, key) - value(replay_row, key))
            for trace_row, replay_row in zip(trace[:n], replay[:n])
            if math.isfinite(value(trace_row, key)) and math.isfinite(value(replay_row, key))
        ]
        if diffs:
            print(f"zephyr_vs_python {key}: mean_abs={sum(diffs) / len(diffs):.3f} max_abs={max(diffs):.3f}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--telemetry", default="/tmp/fixed_wing_zephyr_sil_telemetry_fresh.csv")
    parser.add_argument("--trace", default="/tmp/fixed_wing_zephyr_trace_sil_fresh.csv")
    parser.add_argument("--zephyr-log", default="/tmp/zephyr_bridge_zephyr.log")
    parser.add_argument("--python-replay", default="")
    args = parser.parse_args()

    telemetry = read_csv(Path(args.telemetry))
    trace = read_csv(Path(args.trace))
    fwdbg = read_fwdbg_log(Path(args.zephyr_log))
    replay = read_csv(Path(args.python_replay)) if args.python_replay else []

    summarize_path("telemetry", telemetry, ("px", "py", "pz"))
    summarize_path("bridge_trace", trace, ("x", "y", "z"))
    summarize_commands("zephyr_command", trace)
    summarize_fwdbg(fwdbg)
    compare_replay(trace, replay)
    return 0


if __name__ == "__main__":
    sys.exit(main())
