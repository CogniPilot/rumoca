#!/usr/bin/env python3
"""Inject deterministic fixed-wing states into zephyr.exe and log waypoint output."""

from __future__ import annotations

import argparse
import csv
import math
import os
import re
import signal
import socket
import subprocess
import time
from pathlib import Path

from fixed_wing_zephyr_bridge import (
    CUBS2_INPUT,
    ZEPHYR_EXE_DEFAULT,
    ZEPHYR_FLIGHT,
    ZEPHYR_INPUT,
    command_from_flight_snapshot,
    pack_sim_input,
)


WAYPOINTS = [
    (-4.0, -5.0, 3.0),
    (-3.0, 2.0, 3.0),
    (16.20, 2.0, 3.0),
    (16.0, -4.22, 3.0),
    (6.88, -5.1, 3.0),
    (-4.0, -5.0, 3.0),
]

FWDBG_COLUMNS = [
    "fw_time", "dbg_x", "dbg_y", "dbg_z", "dbg_roll", "dbg_pitch", "dbg_yaw",
    "dbg_aileron", "dbg_elevator", "dbg_throttle", "dbg_rudder", "dbg_stabilizer",
    "dbg_des_heading", "dbg_phi_cmd", "dbg_chi_err", "dbg_current_wp",
    "dbg_airborne", "dbg_des_v", "dbg_des_gamma",
]
ANSI_RE = re.compile(r"\x1b\[[0-9;?]*[A-Za-z]")


def path_samples(dt: float, speed: float, loops: int) -> list[dict[str, float]]:
    samples: list[dict[str, float]] = []
    t = 0.0
    previous = (0.0, 0.0, 3.0)
    for _ in range(loops):
        for waypoint in WAYPOINTS:
            dx = waypoint[0] - previous[0]
            dy = waypoint[1] - previous[1]
            dz = waypoint[2] - previous[2]
            distance = math.sqrt(dx * dx + dy * dy + dz * dz)
            steps = max(1, int(math.ceil(distance / max(speed * dt, 1e-6))))
            yaw = math.atan2(dy, dx)
            for step in range(steps):
                ratio = min(1.0, (step + 1) / steps)
                samples.append({
                    "t": t,
                    "x": previous[0] + dx * ratio,
                    "y": previous[1] + dy * ratio,
                    "z": previous[2] + dz * ratio,
                    "roll": 0.0,
                    "pitch": 0.0,
                    "yaw": yaw,
                })
                t += dt
            previous = waypoint
    return samples


def parse_fwdbg(line: str) -> dict[str, float]:
    line = ANSI_RE.sub("", line)
    marker = "FWDBG,"
    index = line.find(marker)
    if index < 0:
        return {}
    parts = line[index + len(marker):].strip().split(",")
    if len(parts) != len(FWDBG_COLUMNS):
        return {}
    return {name: float(value) for name, value in zip(FWDBG_COLUMNS, parts)}


def read_fwdbg_log(path: Path) -> list[dict[str, float]]:
    rows: list[dict[str, float]] = []
    if not path.exists():
        return rows
    with open(path) as f:
        for line in f:
            parsed = parse_fwdbg(line)
            if parsed and (not rows or parsed != rows[-1]):
                rows.append(parsed)
    return rows


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--zephyr", default=os.environ.get("CUBS2_ZEPHYR_EXE", ZEPHYR_EXE_DEFAULT))
    parser.add_argument("--output", default="/tmp/fixed_wing_zephyr_direct_probe.csv")
    parser.add_argument("--zephyr-log", default="/tmp/fixed_wing_zephyr_direct_probe.log")
    parser.add_argument("--dt", type=float, default=0.01)
    parser.add_argument("--speed", type=float, default=3.0)
    parser.add_argument("--loops", type=int, default=1)
    args = parser.parse_args()

    zephyr = Path(args.zephyr)
    if not zephyr.exists():
        raise SystemExit(f"missing zephyr executable: {zephyr}")

    rx = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    rx.bind(ZEPHYR_FLIGHT)
    rx.setblocking(False)
    tx = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

    zephyr_log_path = Path(args.zephyr_log)
    zephyr_log = open(zephyr_log_path, "w", buffering=1)
    proc = subprocess.Popen([str(zephyr)], stdout=zephyr_log, stderr=zephyr_log, start_new_session=True)

    rows: list[dict[str, float | str]] = []
    last_command: dict[str, float] = {}
    try:
        for seq, sample in enumerate(path_samples(args.dt, args.speed, args.loops), start=1):
            sim_input = pack_sim_input(sample)
            tx.sendto(sim_input, ZEPHYR_INPUT)
            tx.sendto(sim_input, CUBS2_INPUT)
            deadline = time.monotonic() + args.dt
            while time.monotonic() < deadline:
                try:
                    packet, _ = rx.recvfrom(2048)
                except BlockingIOError:
                    break
                decoded = command_from_flight_snapshot(packet)
                if decoded:
                    last_command = decoded
            row: dict[str, float | str] = {"seq": seq, **sample}
            for name in ("aileron", "elevator", "throttle", "rudder", "stabilizer"):
                row[name] = last_command.get(name, "")
            rows.append(row)
            if proc.poll() is not None:
                raise SystemExit(f"zephyr exited early with code {proc.returncode}")
            time.sleep(args.dt)
    finally:
        if proc.poll() is None:
            os.killpg(proc.pid, signal.SIGTERM)
            try:
                proc.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                os.killpg(proc.pid, signal.SIGKILL)
                proc.wait(timeout=2.0)
        zephyr_log.close()

    dbg_rows = read_fwdbg_log(zephyr_log_path)
    for row, dbg in zip(rows, dbg_rows):
        row.update(dbg)

    fieldnames = [
        "seq", "t", "x", "y", "z", "roll", "pitch", "yaw",
        "aileron", "elevator", "throttle", "rudder", "stabilizer",
        *FWDBG_COLUMNS,
    ]
    with open(args.output, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    current_wps = [
        float(row["dbg_current_wp"])
        for row in rows
        if row.get("dbg_current_wp") not in ("", None)
    ]
    print(f"wrote {args.output} ({len(rows)} rows)")
    print(f"wrote {zephyr_log_path}")
    print(f"parsed {len(dbg_rows)} FWDBG rows")
    if current_wps:
        print(f"current_wp range: {min(current_wps):.0f}..{max(current_wps):.0f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
