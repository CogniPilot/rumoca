#!/usr/bin/env python3
"""Headless telemetry capture from the fixed-wing SIL WebSocket.

Connects to ws://127.0.0.1:8083, records state to CSV, and runs sanity
checks. Works with any profile (manual, binary, zephyr).
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import sys
import time

try:
    from websocket import WebSocketApp
except ImportError:
    print("pip install websocket-client", file=sys.stderr)
    sys.exit(1)


WS_URL = "ws://127.0.0.1:8083"
SANITY_FIELDS = [
    ("px", "pos_x", -1e6, 1e6),
    ("py", "pos_y", -1e6, 1e6),
    ("pz", "altitude", -5, 50),
    ("airspeed", "airspeed", 0, 30),
    ("ail_rad", "ail_rad", -0.53, 0.53),
    ("elev_rad", "elev_rad", -0.42, 0.42),
    ("rud_rad", "rud_rad", -0.35, 0.35),
    ("throttle", "throttle", 0, 1),
]


def main() -> None:
    parser = argparse.ArgumentParser(description="Verify fixed-wing SIL telemetry")
    parser.add_argument("--csv", default=None, help="CSV output path")
    parser.add_argument("--duration", type=float, default=8.0, help="Capture duration (s)")
    parser.add_argument("--ws", default=WS_URL, help="WebSocket URL")
    args = parser.parse_args()

    records: list[dict[str, float]] = []
    errors: list[str] = []
    start = time.monotonic()
    connected = False

    def on_message(_ws: WebSocketApp, message: str) -> None:
        nonlocal connected
        connected = True
        try:
            data = json.loads(message)
        except json.JSONDecodeError:
            return
        records.append(data)

        # Sanity checks per packet
        t = data.get("t", 0)
        for key, label, lo, hi in SANITY_FIELDS:
            val = data.get(key)
            if val is None:
                continue
            if not math.isfinite(val):
                errors.append(f"t={t}: {label}={val} (NaN/Inf)")
            elif val < lo or val > hi:
                errors.append(f"t={t}: {label}={val} out of [{lo},{hi}]")

    ws = WebSocketApp(args.ws, on_message=on_message)
    ws.run_forever()

    # Wait for connection
    deadline = time.monotonic() + 3.0
    while not connected and time.monotonic() < deadline:
        time.sleep(0.05)

    if not connected:
        print("ERROR: no WebSocket connection after 3s", file=sys.stderr)
        sys.exit(1)

    print(f"Connected to {args.ws}, capturing for {args.duration}s...", flush=True)
    time.sleep(args.duration)

    packet_count = len(records)
    duration = time.monotonic() - start
    rate = packet_count / duration if duration > 0 else 0
    print(f"\nCaptured {packet_count} packets in {duration:.1f}s ({rate:.0f} Hz)")

    # CSV output
    if args.csv and records:
        fieldnames = sorted(set().union(*(r.keys() for r in records)))
        with open(args.csv, "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=fieldnames)
            w.writeheader()
            w.writerows(records)
        print(f"Wrote {args.csv} ({len(records)} rows)")

    # Report errors
    if errors:
        for e in errors[:20]:
            print(f"  FAIL: {e}")
        if len(errors) > 20:
            print(f"  ... and {len(errors)-20} more errors")
        sys.exit(1)
    else:
        print("All sanity checks PASSED")


if __name__ == "__main__":
    main()
