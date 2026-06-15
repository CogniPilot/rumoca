#!/usr/bin/env python3
"""Reference external-process controller for fixed-wing binary SIL.

Sends stick commands (aileron→stick_roll, elevator→stick_pitch,
throttle→stick_throttle, rudder→stick_yaw) via CubControl.ActuatorCommand
over UDP. The receiving FixedWingFBW controller interprets these as
rate-command/attitude-hold: holding a nonzero roll stick keeps rolling;
releasing holds the bank.

The controller flies a simple waypoint pattern. It includes a timed takeoff
sequence since the plant starts at rest on the runway.
"""

from __future__ import annotations

import math
import socket
import struct
import time


LISTEN = ("127.0.0.1", 4250)
SEND = ("127.0.0.1", 4251)

PLANT_FIELDS = [
    ("timestamp_ns", "Q"),
    ("x", "d"),
    ("y", "d"),
    ("z", "d"),
    ("roll", "d"),
    ("pitch", "d"),
    ("yaw", "d"),
]

COMMAND_FIELDS = [
    ("timestamp_ns", "Q"),
    ("aileron", "d"),
    ("elevator", "d"),
    ("throttle", "d"),
    ("rudder", "d"),
    ("stabilizer", "d"),
]

# Waypoints in the same NWU coordinate frame as the plant.
WAYPOINTS = [
    (60.0, 0.0, 25.0),
    (60.0, 60.0, 25.0),
    (-40.0, 60.0, 25.0),
    (-40.0, -30.0, 25.0),
]

CRUISE_ALT = 25.0
TO_THROTTLE = 0.85
CRUISE_THROTTLE = 0.55
CLIMB_PITCH = 0.20


def clamp(value: float, lo: float, hi: float) -> float:
    return min(max(value, lo), hi)


def wrap_pi(value: float) -> float:
    return math.atan2(math.sin(value), math.cos(value))


def field_offsets(field_count: int, formats: list[str]) -> tuple[int, int, list[int]]:
    vtable_size = 4 + 2 * field_count
    table_off = (4 + vtable_size + 3) & ~3
    cursor = 4
    offsets: list[int] = []
    for fmt in formats:
        size = struct.calcsize("<" + fmt)
        align = max(size, 1)
        cursor = (cursor + align - 1) & ~(align - 1)
        offsets.append(cursor)
        cursor += size
    max_align = max(struct.calcsize("<" + fmt) for fmt in formats)
    object_size = (cursor + max_align - 1) & ~(max_align - 1)
    return table_off, object_size, offsets


def unpack_table(buf: bytes, fields: list[tuple[str, str]]) -> dict[str, float]:
    if len(buf) < 8:
        return {}
    table_off = struct.unpack_from("<I", buf, 0)[0]
    if table_off >= len(buf):
        return {}
    soff = struct.unpack_from("<i", buf, table_off)[0]
    vtable_off = table_off - soff
    if vtable_off < 0 or vtable_off + 4 > len(buf):
        return {}
    vtable_size = struct.unpack_from("<H", buf, vtable_off)[0]
    out: dict[str, float] = {}
    for field_id, (name, fmt) in enumerate(fields):
        slot = vtable_off + 4 + 2 * field_id
        if slot + 2 > vtable_off + vtable_size:
            continue
        rel = struct.unpack_from("<H", buf, slot)[0]
        if rel == 0:
            continue
        off = table_off + rel
        size = struct.calcsize("<" + fmt)
        if off + size <= len(buf):
            out[name] = float(struct.unpack_from("<" + fmt, buf, off)[0])
    return out


def pack_table(fields: list[tuple[str, str]], values: dict[str, float]) -> bytes:
    formats = [fmt for _, fmt in fields]
    table_off, object_size, offsets = field_offsets(len(fields), formats)
    vtable_off = 4
    vtable_size = 4 + 2 * len(fields)
    buf = bytearray(table_off + object_size)
    struct.pack_into("<I", buf, 0, table_off)
    struct.pack_into("<H", buf, vtable_off, vtable_size)
    struct.pack_into("<H", buf, vtable_off + 2, object_size)
    for field_id, rel in enumerate(offsets):
        struct.pack_into("<H", buf, vtable_off + 4 + 2 * field_id, rel)
    struct.pack_into("<I", buf, table_off, table_off - vtable_off)
    for (name, fmt), rel in zip(fields, offsets):
        value = values.get(name, 0.0)
        if fmt == "Q":
            value = int(max(0, value))
        struct.pack_into("<" + fmt, buf, table_off + rel, value)
    return bytes(buf)


class WaypointController:
    def __init__(self) -> None:
        self.index = 0
        self.t0 = time.monotonic()
        self.state = "takeoff"  # takeoff → climb → cruise

    def step(self, state: dict[str, float]) -> dict[str, float]:
        x = state.get("x", 0.0)
        y = state.get("y", 0.0)
        z = state.get("z", 0.0)
        roll = state.get("roll", 0.0)
        pitch = state.get("pitch", 0.0)
        yaw = state.get("yaw", 0.0)
        elapsed = time.monotonic() - self.t0

        # Crude speed estimate from position change (not computed here directly).
        # Instead use a timed takeoff sequence.

        # Phase transitions
        if self.state == "takeoff" and elapsed > 3.0:
            self.state = "climb"
        if self.state == "climb" and z >= CRUISE_ALT - 1.0:
            self.state = "cruise"

        if self.state == "takeoff":
            # Full throttle, no pitch/roll input — accelerate straight
            return self._cmd(0.0, 0.0, TO_THROTTLE, 0.0, 1.0)

        if self.state == "climb":
            # Climb pitch + full throttle, heading hold
            return self._cmd(0.0, CLIMB_PITCH, TO_THROTTLE, 0.0, 1.0)

        # Cruise — navigate waypoints
        wx, wy, wz = WAYPOINTS[self.index]
        dx = wx - x
        dy = wy - y
        dist_xy = math.hypot(dx, dy)
        if dist_xy < 12.0:
            self.index = (self.index + 1) % len(WAYPOINTS)
            wx, wy, wz = WAYPOINTS[self.index]
            dx = wx - x
            dy = wy - y

        desired_heading = math.atan2(dy, dx)
        heading_error = wrap_pi(desired_heading - yaw)
        altitude_error = wz - z

        aileron = clamp(0.5 * heading_error - 0.15 * roll, -1.0, 1.0)
        elevator = clamp(0.06 * altitude_error - 0.4 * pitch, -1.0, 1.0)
        throttle = CRUISE_THROTTLE

        return self._cmd(aileron, elevator, throttle, 0.0, 1.0)

    @staticmethod
    def _cmd(ail: float, ele: float, thr: float, rud: float, stb: float) -> dict[str, float]:
        return {
            "timestamp_ns": time.time_ns(),
            "aileron": ail,
            "elevator": ele,
            "throttle": thr,
            "rudder": rud,
            "stabilizer": stb,
        }


def main() -> None:
    controller = WaypointController()
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(LISTEN)
    print(f"fixed-wing stub listening on {LISTEN[0]}:{LISTEN[1]}", flush=True)
    while True:
        data, _addr = sock.recvfrom(2048)
        state = unpack_table(data, PLANT_FIELDS)
        if not state:
            continue
        command = controller.step(state)
        sock.sendto(pack_table(COMMAND_FIELDS, command), SEND)


if __name__ == "__main__":
    main()
