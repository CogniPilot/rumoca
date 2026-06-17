#!/usr/bin/env python3
"""Headless telemetry capture from the fixed-wing SIL WebSocket.

Connects to ws://127.0.0.1:8083, records state to CSV, and runs sanity
checks. Works with any profile (manual, binary, zephyr).
"""

from __future__ import annotations

import argparse
import base64
import csv
import hashlib
import json
import math
import os
import socket
import sys
import threading
import time
from urllib.parse import urlparse

try:
    from websocket import WebSocketApp
except ImportError:
    WebSocketApp = None


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


class StdlibWebSocket:
    def __init__(self, url: str):
        parsed = urlparse(url)
        if parsed.scheme != "ws":
            raise ValueError(f"unsupported WebSocket URL: {url}")
        self.host = parsed.hostname or "127.0.0.1"
        self.port = parsed.port or 80
        self.path = parsed.path or "/"
        if parsed.query:
            self.path += "?" + parsed.query
        self.sock: socket.socket | None = None

    def connect(self) -> None:
        key = base64.b64encode(os.urandom(16)).decode("ascii")
        sock = socket.create_connection((self.host, self.port), timeout=3.0)
        request = (
            f"GET {self.path} HTTP/1.1\r\n"
            f"Host: {self.host}:{self.port}\r\n"
            "Upgrade: websocket\r\n"
            "Connection: Upgrade\r\n"
            f"Sec-WebSocket-Key: {key}\r\n"
            "Sec-WebSocket-Version: 13\r\n"
            "\r\n"
        )
        sock.sendall(request.encode("ascii"))
        response = b""
        while b"\r\n\r\n" not in response:
            chunk = sock.recv(4096)
            if not chunk:
                raise ConnectionError("WebSocket handshake closed")
            response += chunk
        if not response.startswith(b"HTTP/1.1 101"):
            raise ConnectionError(response.decode("utf-8", errors="replace"))
        accept = base64.b64encode(
            hashlib.sha1((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").encode("ascii")).digest()
        ).decode("ascii")
        if f"sec-websocket-accept: {accept}".lower() not in response.decode(
            "ascii", errors="ignore"
        ).lower():
            raise ConnectionError("WebSocket accept key mismatch")
        sock.settimeout(0.5)
        self.sock = sock

    def recv_text(self) -> str | None:
        sock = self.sock
        if sock is None:
            return None
        header = self._recv_exact(2)
        if not header:
            return None
        opcode = header[0] & 0x0F
        length = header[1] & 0x7F
        if length == 126:
            length = int.from_bytes(self._recv_exact(2), "big")
        elif length == 127:
            length = int.from_bytes(self._recv_exact(8), "big")
        payload = self._recv_exact(length)
        if opcode == 8:
            return None
        if opcode == 1:
            return payload.decode("utf-8", errors="replace")
        return ""

    def close(self) -> None:
        if self.sock is not None:
            self.sock.close()
            self.sock = None

    def _recv_exact(self, length: int) -> bytes:
        sock = self.sock
        if sock is None:
            return b""
        data = bytearray()
        while len(data) < length:
            chunk = sock.recv(length - len(data))
            if not chunk:
                return b""
            data.extend(chunk)
        return bytes(data)


def read_stdlib_ws(ws: StdlibWebSocket, on_message) -> None:
    while True:
        try:
            message = ws.recv_text()
        except (OSError, TimeoutError):
            continue
        if message is None:
            return
        if message:
            on_message(ws, message)


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

    def on_message(_ws: object, message: str) -> None:
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

    if WebSocketApp is not None:
        ws = WebSocketApp(args.ws, on_message=on_message)
        ws_thread = threading.Thread(target=ws.run_forever, daemon=True)
        ws_thread.start()

        deadline = time.monotonic() + 3.0
        while not connected and time.monotonic() < deadline:
            time.sleep(0.05)
    else:
        ws = StdlibWebSocket(args.ws)
        ws.connect()
        ws_thread = threading.Thread(target=read_stdlib_ws, args=(ws, on_message), daemon=True)
        ws_thread.start()

        deadline = time.monotonic() + 3.0
        while not connected and time.monotonic() < deadline:
            time.sleep(0.05)

    if not connected:
        print("ERROR: no WebSocket connection after 3s", file=sys.stderr)
        sys.exit(1)

    print(f"Connected to {args.ws}, capturing for {args.duration}s...", flush=True)
    time.sleep(args.duration)
    ws.close()
    ws_thread.join(timeout=2.0)

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
