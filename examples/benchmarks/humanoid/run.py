"""Run the frozen native/MuJoCo benchmark on one CPU; speed is informational."""

import argparse
import ctypes
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

import fidelity

HERE = Path(__file__).resolve().parent


def digest(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def execute(argv, output, *, cwd=None, timeout=180):
    """Retain raw output and terminate the whole owned process group on timeout."""
    start = time.monotonic()
    with output.with_suffix(".stdout").open("wb") as stdout, \
            output.with_suffix(".stderr").open("wb") as stderr:
        process = subprocess.Popen(argv, cwd=cwd, stdout=stdout, stderr=stderr,
                                   start_new_session=True)
        failure = None
        try:
            code = process.wait(timeout=timeout)
        except BaseException as error:
            failure = error
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            code = process.wait()
    record = {"argv": list(map(str, argv)), "returncode": code,
              "wall_seconds": time.monotonic() - start,
              "failure": type(failure).__name__ if failure else None,
              "affinity": sorted(os.sched_getaffinity(0))}
    output.with_suffix(".command.json").write_text(json.dumps(record, indent=2) + "\n")
    if failure:
        raise failure
    if code:
        raise RuntimeError(f"command failed ({code}); see {output}.stderr")
    return output.with_suffix(".stdout")


def verify_inputs():
    for line in (HERE / "SHA256SUMS").read_text().splitlines():
        expected, name = line.split(maxsplit=1)
        if digest(HERE / name) != expected:
            raise ValueError(f"fixture hash mismatch: {name}")


def rumoca_command(binary, msl, output):
    return [str(binary), "--cache-dir", str(output / "cache"), "sim",
            str(HERE / "HumanoidPhysical.mo"), "-m", "HumanoidPhysical",
            "--source-root", str(HERE / "MultiBodyLie"),
            "--source-root", str(HERE / "GroundContact.mo"),
            "--source-root", str(msl), "--dt", "0.01", "--solver", "velimplicit"]


def prepare_twin(args, output):
    execute([sys.executable, str(HERE / "generate.py"),
             str(HERE / "HumanoidPhysical.mo"), str(output)], output / "generate")
    library = args.mujoco / "lib/libmujoco.so.3.8.1"
    execute([args.cc, "-O2", "-o", str(output / "twin"), str(HERE / "twin.c"),
             "-I" + str(output), "-I" + str(args.mujoco / "include"), str(library),
             "-Wl,-rpath," + str(library.parent), "-lm"], output / "build-twin")
    return library


def timing(args, output):
    # One warmup + three hot runs, as in the frozen chart protocol. Rumoca
    # reuses its prepared model; each twin's timer excludes XML load and reset.
    command = rumoca_command(args.rumoca, args.msl, output)
    command.insert(command.index("sim") + 1, "bench")
    raw = execute(command + ["--t-end", "2", "--iterations", "3", "--warmups", "1", "--json"],
                  output / "rumoca-speed")
    rumoca = json.loads(raw.read_text())
    if (rumoca["iterations"] != 3 or rumoca["warmups"] != 1
            or rumoca["t_end"] != 2 or rumoca["dt"] != 0.01
            or rumoca["solver"] != "velimplicit" or rumoca["last_final_time"] < 1.998):
        raise ValueError("incomplete Rumoca speed attempt")
    twin = [str(output / "twin"), str(output / "twin_native_act.xml"),
            "native_act", "2", "0.001", "-", "1"]
    attempts = []
    for index in range(4):
        raw = execute(twin, output / f"mujoco-speed-{index}")
        measured = json.loads(raw.read_text())
        if measured["t_end"] < 1.998 or measured["repeats"] != 1:
            raise ValueError("incomplete MuJoCo speed attempt")
        attempts.append(measured)
    hot = [row["wall_best_s"] for row in attempts[1:]]
    native_best, twin_best = rumoca["hot_best_seconds"], min(hot)
    if not all(value > 0 and math.isfinite(value)
               for value in [native_best, *hot]):
        raise ValueError("invalid speed measurement")
    return {"rumoca": rumoca, "mujoco_hot_seconds": hot,
            "rumoca_s_per_sim_s": native_best / 2,
            "mujoco_s_per_sim_s": twin_best / 2,
            "ratio_rumoca_over_mujoco": native_best / twin_best}


def trajectories(args, output):
    execute(rumoca_command(args.rumoca, args.msl, output)
            + ["--t-end", "4", "--output", str(output / "rumoca.csv")],
            output / "rumoca-fidelity")
    execute([str(output / "twin"), str(output / "twin_native_act.xml"),
             "native_act", "4", "0.001", str(output / "mujoco.csv"), "1"],
            output / "mujoco-fidelity")
    return {name: fidelity.check(output / f"{name}.csv") for name in ("rumoca", "mujoco")}


def report(result, output):
    (output / "result.json").write_text(json.dumps(result, indent=2) + "\n")
    speed = result["speed"]
    text = ("### Humanoid native / MuJoCo\n\n"
            "2 s speed window, one warmup + three hot runs, best of three; one pinned CPU.\n\n"
            f"Rumoca: {speed['rumoca_s_per_sim_s']:.6f} s/sim-s; "
            f"MuJoCo: {speed['mujoco_s_per_sim_s']:.6f} s/sim-s.\n\n"
            f"Rumoca / MuJoCo hot-time ratio: **{speed['ratio_rumoca_over_mujoco']:.3f}x** "
            "(lower is better; informational, no speed threshold).\n\n"
            f"Rumoca compile / prepare: {speed['rumoca']['compile_seconds']:.3f} / "
            f"{speed['rumoca']['prepare_seconds']:.3f} s, separate from hot timing.\n\n"
            "4 s task fidelity: " + ", ".join(
                name + " " + ("PASS" if value["pass"] else "FAIL")
                for name, value in result["fidelity"].items()) + ".\n\n"
            "Matched robot/controller task; MuJoCo uses its own contact and actuator "
            "physics. This is not equation or trajectory parity. Raw attempts and identities "
            "are in the humanoid-comparison artifact.\n")
    (output / "summary.md").write_text(text)
    if os.environ.get("GITHUB_STEP_SUMMARY"):
        with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as stream:
            stream.write(text)
    print(text)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("rumoca", "msl", "mujoco", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    parser.add_argument("--cc", default="cc")
    args = parser.parse_args()
    for name in ("rumoca", "msl", "mujoco", "output"):
        setattr(args, name, getattr(args, name).resolve())
    args.output.mkdir(parents=True, exist_ok=False)
    verify_inputs()
    available = sorted(os.sched_getaffinity(0))
    os.sched_setaffinity(0, {available[0]})
    for name in ("RAYON_NUM_THREADS", "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS",
                 "MKL_NUM_THREADS", "CARGO_BUILD_JOBS"):
        os.environ[name] = "1"
    identities = {"source": os.environ.get("CI_SOURCE_SHA"),
                  "workflow": os.environ.get("CI_WORKFLOW_SHA"),
                  "runner": {key: os.environ.get(key) for key in
                             ("RUNNER_NAME", "RUNNER_OS", "ImageOS", "ImageVersion")},
                  "platform": platform.platform(), "available_cpus": available,
                  "selected_cpu": available[0], "rumoca_sha256": digest(args.rumoca),
                  "threads": {name: os.environ[name] for name in
                              ("RAYON_NUM_THREADS", "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS",
                               "MKL_NUM_THREADS", "CARGO_BUILD_JOBS")},
                  "fixture_manifest_sha256": digest(HERE / "SHA256SUMS")}
    (args.output / "identity.json").write_text(json.dumps(identities, indent=2) + "\n")
    library = prepare_twin(args, args.output)
    identities["mujoco_library_sha256"] = digest(library)
    version = ctypes.CDLL(str(library)).mj_versionString
    version.restype = ctypes.c_char_p
    identities["mujoco_version"] = version().decode("ascii")
    if identities["mujoco_version"] != "3.8.1":
        raise ValueError("unexpected MuJoCo library version")
    execute([str(args.rumoca), "build-info"], args.output / "rumoca-build-info")
    execute([args.cc, "--version"], args.output / "compiler-version")
    identities["twin_sha256"] = digest(args.output / "twin")
    for name in ("twin_native_act.xml", "params.h", "params.json"):
        identities[name + "_sha256"] = digest(args.output / name)
    (args.output / "identity.json").write_text(json.dumps(identities, indent=2) + "\n")
    speed = timing(args, args.output)
    result = {"identity": identities, "speed": speed,
              "fidelity": trajectories(args, args.output)}
    report(result, args.output)
    return 0 if all(row["pass"] for row in result["fidelity"].values()) else 1


if __name__ == "__main__":
    sys.exit(main())
