"""Execute exact guide source; observe real replies without replacing its API."""

import json
import math
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import threading

from inventory_probe import load_inventory, read_span


def source_context(root):
    inventory = load_inventory(root)
    context = inventory["contexts"]["stdio-plant"]
    assert context["language"] == "modelica"
    plant = read_span(root, context)
    entries = [entry for entry in inventory["entries"] if entry["page"] == "simulation/stdio-session.md"]
    controller = [entry for entry in entries if entry["language"] == "python"]
    transcript = [entry for entry in entries if entry["language"] == "text"]
    assert len(controller) == len(transcript) == 1
    return plant, read_span(root, controller[0]), read_span(root, transcript[0])


def controller_trace(source):
    frames, hellos, namespace = [], [], {"__name__": "__main__"}

    def observe(frame, event, result):
        if event != "return":
            return
        if frame.f_code.co_filename == "guide-controller.py" and frame.f_code.co_name == "call":
            frames.append((dict(frame.f_locals["command"]), result))
        if frame.f_code.co_name == "loads" and isinstance(result, dict) and result.get("event") == "hello":
            hellos.append(result)

    def stop_stalled_child():
        child = namespace.get("child")
        if child is not None and child.poll() is None:
            child.kill()

    watchdog = threading.Timer(90, stop_stalled_child)
    watchdog.start()
    sys.setprofile(observe)
    try:
        exec(compile(source, "guide-controller.py", "exec"), namespace)
        child = namespace["child"]
        assert child.wait(timeout=15) == 0
        assert child.stdout.readline() == "", "unexpected extra protocol reply"
    finally:
        watchdog.cancel()
        sys.setprofile(None)
        child = namespace.get("child")
        if child is not None and child.poll() is None:
            child.kill()
            child.wait(timeout=15)
    assert len(hellos) == 1 and hellos[0]["protocol_version"] == 1, hellos
    assert hellos[0]["engine"]["engine"] in {"interpreter", "cranelift", "wasm_program"}
    assert len(frames) == 32, len(frames)
    verify_curve(frames)
    print(json.dumps({"actual_engine": hellos[0]["engine"], "sampled_steps": 10, "protocol_replies": 33}))


def verify_curve(frames):
    rho = 2.6 * math.exp(-0.2) - 1.6
    for step in range(10):
        get, set_input, advance = frames[3 * step : 3 * step + 3]
        expected = 8 / 13 * (1 - rho**step)
        assert get[0] == {"command": "get", "name": "y"}, get
        assert get[1]["event"] == "value" and get[1]["name"] == "y", get
        assert math.isclose(get[1]["time"], step * 0.1, abs_tol=1e-12)
        # The unmodified CLI uses 1e-6 absolute and relative solver tolerances.
        assert math.isclose(get[1]["value"], expected, rel_tol=0, abs_tol=1e-5), (step, get, expected)
        assert set_input[0] == {"command": "set_input", "name": "u", "value": 0.8 * (1 - get[1]["value"])}
        assert set_input[1]["event"] == "ok"
        assert advance[0] == {"command": "step", "dt": 0.1}
        assert advance[1]["event"] == "ok"
        assert math.isclose(advance[1]["time"], (step + 1) * 0.1, abs_tol=1e-12)
    assert frames[30][0] == {"command": "get", "name": "y"}
    assert math.isclose(frames[30][1]["value"], 8 / 13 * (1 - rho**10), rel_tol=0, abs_tol=1e-5)
    assert frames[31][0] == {"command": "close"} and frames[31][1]["event"] == "closed"
    assert math.isclose(frames[31][1]["time"], 1.0, abs_tol=1e-12)


def transcript_trace(source, binary):
    lines = [line for line in source.splitlines() if line.startswith(("-> ", "<- "))]
    assert len(lines) == 8
    child = subprocess.Popen([binary, "sim", "--serve-stdio", "Plant.mo", "-m", "Plant", "--input", "u=0"],
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    watchdog = threading.Timer(90, child.kill)
    watchdog.start()
    try:
        assert json.loads(child.stdout.readline())["event"] == "hello"
        for command, display in zip(lines[::2], lines[1::2], strict=True):
            child.stdin.write(command[3:] + "\n")
            child.stdin.flush()
            actual, shown = json.loads(child.stdout.readline()), json.loads(display[3:])
            print(json.dumps({"shown": shown, "actual": actual}))
            assert actual["event"] == shown["event"]
            assert math.isclose(actual["time"], shown["time"], abs_tol=1e-12)
            if actual["event"] == "state":
                assert math.isclose(actual["values"]["y"], -2 * math.expm1(-0.002), rel_tol=0, abs_tol=1e-7)
                assert actual["values"].keys() == shown["values"].keys(), (actual, shown)
                for name, value in shown["values"].items():
                    assert math.isclose(actual["values"][name], value, rel_tol=0, abs_tol=5e-5), (actual, shown)
        assert child.wait(timeout=15) == 0
    finally:
        watchdog.cancel()
        if child.poll() is None:
            child.kill()
            child.wait(timeout=15)


def main():
    root, binary = Path(sys.argv[1]).resolve(), str(Path(sys.argv[2]).resolve())
    plant, controller, transcript = source_context(root)
    os.environ["PATH"] = str(Path(binary).parent) + os.pathsep + os.environ["PATH"]
    with tempfile.TemporaryDirectory() as directory:
        previous = Path.cwd()
        try:
            os.chdir(directory)
            os.environ["XDG_CACHE_HOME"] = str(Path(directory) / "cache")
            os.environ["LOCALAPPDATA"] = str(Path(directory) / "cache")
            Path("Plant.mo").write_text(plant, encoding="utf-8")
            controller_trace(controller)
            transcript_trace(transcript, binary)
        finally:
            os.chdir(previous)


if __name__ == "__main__":
    main()
