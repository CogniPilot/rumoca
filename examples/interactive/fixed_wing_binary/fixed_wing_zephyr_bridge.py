#!/usr/bin/env python3
"""Bridge fixed-wing CubControl SIL packets to CUBS2 native-sim zephyr.exe."""

from __future__ import annotations

import os
import signal
import socket
import struct
import subprocess
import sys
import time


RUMOCA_STATE = ("127.0.0.1", 4250)
RUMOCA_COMMAND = ("127.0.0.1", 4251)
ZEPHYR_INPUT = ("127.0.0.1", 4242)
ZEPHYR_FLIGHT = ("127.0.0.1", 4243)

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

ZEPHYR_EXE_DEFAULT = (
    "/home/prady/code/purt/cognipilot/ws/cerebri/cubs2/build-native_sim/"
    "zephyr/zephyr.exe"
)

child_proc: subprocess.Popen[bytes] | None = None


def field_offsets(field_count: int, formats: list[str]) -> tuple[int, int, list[int]]:
    vtable_size = 4 + 2 * field_count
    table_off = (4 + vtable_size + 3) & ~3
    cursor = 4
    offsets: list[int] = []
    for fmt in formats:
        size = struct.calcsize("<" + fmt)
        align = min(max(size, 1), 8)
        cursor = (cursor + align - 1) & ~(align - 1)
        offsets.append(cursor)
        cursor += size
    max_align = min(max(struct.calcsize("<" + fmt) for fmt in formats), 8)
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


def pack_sim_input(state: dict[str, float]) -> bytes:
    vtable_size = 16
    table_off = 24
    object_size = 96
    buf = bytearray(table_off + object_size)
    struct.pack_into("<I", buf, 0, table_off)
    buf[4:8] = b"SYSI"
    struct.pack_into("<H", buf, 8, vtable_size)
    struct.pack_into("<H", buf, 10, object_size)
    for field_id, rel in enumerate((4, 16, 28, 92, 93, 94)):
        struct.pack_into("<H", buf, 12 + 2 * field_id, rel)
    struct.pack_into("<I", buf, table_off, table_off - 8)
    struct.pack_into(
        "<fff", buf, table_off + 4,
        float(state.get("x", 0.0)), float(state.get("y", 0.0)), float(state.get("z", 0.0)),
    )
    struct.pack_into(
        "<fff", buf, table_off + 16,
        float(state.get("roll", 0.0)), float(state.get("pitch", 0.0)), float(state.get("yaw", 0.0)),
    )
    rc = [1500] * 16
    rc[4] = 2000
    struct.pack_into("<16i", buf, table_off + 28, *rc)
    struct.pack_into("<BBB", buf, table_off + 92, 100, 1, 1)
    return bytes(buf)


def command_from_flight_snapshot(buf: bytes) -> dict[str, float]:
    if len(buf) < 192 or struct.unpack_from("<I", buf, 0)[0] != 24:
        return {}
    table_off = 24
    rc_off = table_off + 28
    rc = struct.unpack_from("<16i", buf, rc_off)
    return {
        "timestamp_ns": time.monotonic_ns(),
        "aileron": max(-1.0, min(1.0, (rc[0] - 1500) / 500.0)),
        "elevator": max(-1.0, min(1.0, (1500 - rc[1]) / 500.0)),
        "throttle": max(0.0, min(1.0, (rc[2] - 1000) / 1000.0)),
        "rudder": max(-1.0, min(1.0, (rc[3] - 1500) / 500.0)),
        "stabilizer": float(rc[4]),
    }


def is_uninitialized_command(command: dict[str, float]) -> bool:
    return (
        command.get("aileron") == 0.0
        and command.get("elevator") == 0.0
        and command.get("throttle") == 0.0
        and command.get("rudder") == 0.0
        and command.get("stabilizer") == 1000.0
    )


def main() -> int:
    global child_proc
    debug = os.environ.get("CUBS2_ZEPHYR_BRIDGE_DEBUG") == "1"

    log_path = "/tmp/bridge_debug.log"
    with open(log_path, "w", buffering=1) as log:
        log.write(f"Bridge starting at {__import__('datetime').datetime.now()}\n")
        log.write(f"CUBS2_ZEPHYR_EXE={os.environ.get('CUBS2_ZEPHYR_EXE', '')}\n")
        log.write(f"debug={debug}\n")
        log.flush()

        def stop_child(_signum: int, _frame: object) -> None:
            if child_proc is not None and child_proc.poll() is None:
                os.killpg(child_proc.pid, signal.SIGTERM)
            raise KeyboardInterrupt

        signal.signal(signal.SIGTERM, stop_child)
        signal.signal(signal.SIGINT, stop_child)

        zephyr_exe = os.environ.get("CUBS2_ZEPHYR_EXE", ZEPHYR_EXE_DEFAULT)
        log.write(f"Using zephyr_exe={zephyr_exe}\n")

        zephyr_log = open("/tmp/zephyr_bridge_zephyr.log", "w", buffering=1) if debug else subprocess.DEVNULL
        proc = subprocess.Popen(
            [zephyr_exe],
            stdout=zephyr_log,
            stderr=zephyr_log,
            start_new_session=True,
        )
        child_proc = proc
        log.write(f"Started zephyr PID {proc.pid}\n")

        rumoca_rx = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        rumoca_rx.bind(RUMOCA_STATE)
        rumoca_rx.settimeout(0.01)
        log.write(f"Bound {RUMOCA_STATE}\n")

        zephyr_rx = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        zephyr_rx.bind(ZEPHYR_FLIGHT)
        zephyr_rx.setblocking(False)
        log.write(f"Bound {ZEPHYR_FLIGHT}\n")

        tx = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

        states_seen = 0
        commands_sent = 0
        last_command: dict[str, float] | None = None
        try:
            while proc.poll() is None:
                try:
                    data, _ = rumoca_rx.recvfrom(2048)
                except socket.timeout:
                    continue
                state = unpack_table(data, PLANT_FIELDS)
                if state:
                    states_seen += 1
                    if states_seen <= 3:
                        log.write(f"state#{states_seen}: {state}\n")
                    tx.sendto(pack_sim_input(state), ZEPHYR_INPUT)

                command = None
                zephyr_packets = 0
                while True:
                    try:
                        packet, _ = zephyr_rx.recvfrom(2048)
                    except BlockingIOError:
                        break
                    zephyr_packets += 1
                    decoded = command_from_flight_snapshot(packet)
                    if decoded:
                        uninit = is_uninitialized_command(decoded)
                        if zephyr_packets <= 3:
                            log.write(f"  zephyr_rx#{zephyr_packets}: decoded={decoded} uninit={uninit}\n")
                    if decoded and not is_uninitialized_command(decoded):
                        command = decoded
                        last_command = decoded
                if command is None and last_command is not None:
                    command = last_command
                if command is not None:
                    commands_sent += 1
                    if commands_sent <= 3:
                        log.write(f"cmd#{commands_sent}: {command}\n")
                    tx.sendto(pack_table(COMMAND_FIELDS, command), RUMOCA_COMMAND)
        except KeyboardInterrupt:
            pass
        finally:
            log.write(f"Exiting. States seen: {states_seen}, Commands sent: {commands_sent}\n")
            if proc.poll() is None:
                os.killpg(proc.pid, signal.SIGTERM)
            try:
                proc.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                os.killpg(proc.pid, signal.SIGKILL)
                proc.wait(timeout=2.0)
            child_proc = None
    return 0


if __name__ == "__main__":
    sys.exit(main())
