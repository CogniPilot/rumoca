"""Fixed single-model CI experiment; raw execution is not a parity verdict."""

import hashlib
import json
import math
import os
from pathlib import Path
import platform
import signal
import subprocess
import sys
import time

SOURCES = {
    "old": "da9d7ca4708e6b5d694a0163d07d5007d61a86eb",
    "current": "4b475aee41ac6b0ca811d19297780c14824a2150",
}
MODEL = "Modelica.Mechanics.MultiBody.Examples.Loops.Engine1b_analytic"
MSL_SHA = "5409cfa17797c52d866dc9d2675badc1378b4bbf88066e60899a489cb1cb8a2d"


def sha(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def write(path, value):
    Path(path).write_text(json.dumps(value, indent=2, allow_nan=False) + "\n")


def git(source, *args):
    return subprocess.check_output(["git", "-C", str(source), *args], text=True).strip()


def bind(source, artifacts, output):
    label = os.environ["SOURCE_LABEL"]
    revision = git(source, "rev-parse", "HEAD")
    if revision != SOURCES[label] or git(source, "status", "--porcelain"):
        raise ValueError("producer source identity or cleanliness mismatch")
    root = Path(artifacts).resolve(strict=True)
    write(output, {
        "label": label, "source": revision,
        "tree": git(source, "rev-parse", "HEAD^{tree}"),
        "flake_lock_sha256": sha(Path(source) / "flake.lock"),
        "artifact_root": str(root),
        "worker_sha256": sha(root / "bin/rumoca-worker"),
        "workflow_source": os.environ["GITHUB_SHA"],
        "run_id": os.environ["GITHUB_RUN_ID"],
        "run_attempt": os.environ["GITHUB_RUN_ATTEMPT"],
    })


def verified_worker(manifest, label):
    if manifest["label"] != label or manifest["source"] != SOURCES[label]:
        raise ValueError("wrong producer source")
    worker = Path(manifest["artifact_root"]) / "bin/rumoca-worker"
    if not worker.is_absolute() or not str(worker.resolve()).startswith("/nix/store/"):
        raise ValueError("worker is outside the imported Nix closure")
    if sha(worker) != manifest["worker_sha256"]:
        raise ValueError("worker content mismatch")
    return worker


def snapshot():
    result = {"utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
              "monotonic": time.monotonic(), "uname": platform.uname()._asdict(),
              "affinity": sorted(os.sched_getaffinity(0)), "loadavg": os.getloadavg()}
    for name in ("cpuinfo", "meminfo", "stat", "pressure/cpu", "pressure/memory"):
        path = Path("/proc") / name
        result[name] = path.read_text() if path.exists() else None
    for name in ("GITHUB_SHA", "GITHUB_WORKFLOW_SHA", "GITHUB_RUN_ID",
                 "GITHUB_RUN_ATTEMPT", "RUNNER_NAME", "ImageOS", "ImageVersion"):
        result[name] = os.environ.get(name)
    return result


def timing_row(raw):
    if raw.get("protocol_version") != 5:
        raise ValueError("unexpected worker protocol")
    row = raw["result"]
    if row.get("model_name") != MODEL:
        raise ValueError("unexpected model")
    if row.get("ic_status") != "ic_ok" or row.get("sim_status") != "sim_ok":
        raise ValueError("worker did not complete initialization and simulation")
    keys = ("ir_solve_seconds", "ir_solve_structural_dae_seconds",
            "ir_solve_lower_seconds", "sim_backend_build_seconds", "sim_run_seconds")
    for key in keys:
        value = row.get(key)
        if isinstance(value, bool) or not isinstance(value, (int, float)):
            raise ValueError("missing timing: " + key)
        if not math.isfinite(value) or value < 0:
            raise ValueError("invalid timing: " + key)
    return {key: row[key] for key in keys}


def attempt(worker, root, directory):
    directory.mkdir()
    request = {"protocol_version": 5, "model_name": MODEL,
               "run_simulation": True, "selected_for_simulation": True,
               "explicit_sim_target": True, "sim_timeout_secs": 45.0,
               "emit_json": False, "nan_trace": False, "emit_modelica": False,
               "source_root_path": str(root.resolve()), "output_dir": str(directory.resolve())}
    write(directory / "request.json", request)
    command = [str(worker), "--request-json", str((directory / "request.json").resolve()),
               "--jobs", "1", "--memory-limit-mb", "4096"]
    write(directory / "before.json", snapshot())
    started = time.monotonic()
    outcome = {"command": command, "timeout_seconds": 90, "timed_out": False}
    process = None
    try:
        with (directory / "stdout.log").open("wb") as stdout, (directory / "stderr.log").open("wb") as stderr:
            process = subprocess.Popen(command, stdout=stdout, stderr=stderr, start_new_session=True)
            try:
                outcome["returncode"] = process.wait(timeout=90)
            except subprocess.TimeoutExpired:
                outcome["timed_out"] = True
    finally:
        if process is not None:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()
        outcome["wall_seconds"] = time.monotonic() - started
        write(directory / "outcome.json", outcome)
        write(directory / "after.json", snapshot())
    if outcome["timed_out"] or outcome.get("returncode") != 0:
        raise ValueError("worker timeout or nonzero exit; raw attempt retained")
    metrics = timing_row(json.loads((directory / "result.json").read_text()))
    trace = directory / "sim-trace.json"
    if not trace.is_file() or trace.stat().st_size == 0:
        raise ValueError("missing raw simulation trace")
    metrics["trace_sha256"] = sha(trace)
    return metrics


def measure(inputs, archive, root, output, pair):
    if pair not in ("1", "2", "3") or sha(archive) != MSL_SHA:
        raise ValueError("invalid pair or MSL archive identity")
    output = Path(output)
    output.mkdir()
    order = ["old", "current"] if pair != "2" else ["current", "old"]
    write(output / "plan.json", {"pair": pair, "order": order, "sources": SOURCES,
                                  "msl_sha256": MSL_SHA, "model": MODEL,
                                  "parity": "unmeasured", "host": snapshot()})
    results = {}
    for label in order:
        try:
            manifest = json.loads((Path(inputs) / label / "manifest.json").read_text())
            write(output / (label + "-producer.json"), manifest)
            worker = verified_worker(manifest, label)
            results[label] = attempt(worker, Path(root), output / label)
        except Exception as error:
            results[label] = {"error": str(error)}
        write(output / "summary.json", results)
    if any("error" in row for row in results.values()):
        raise ValueError("one or more attempts failed; no timing comparison")


if __name__ == "__main__":
    if sys.argv[1] == "bind" and len(sys.argv) == 5:
        bind(*sys.argv[2:])
    elif sys.argv[1] == "measure" and len(sys.argv) == 7:
        measure(*sys.argv[2:])
    else:
        raise SystemExit("expected bind SOURCE ARTIFACTS OUTPUT or measure INPUTS ZIP ROOT OUTPUT PAIR")
