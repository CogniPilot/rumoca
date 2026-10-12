"""Run both original HangingMass CLI examples and check every reported sample."""

import json
import math
from pathlib import Path
import subprocess
import sys
import tempfile

from inventory_probe import commands, load_inventory, read_span


def report_curve(path):
    html = path.read_text(encoding="utf-8")
    marker = "payload: "
    start = html.index(marker, html.index("globalThis.__rumocaResultsReportMount({")) + len(marker)
    payload, _ = json.JSONDecoder().raw_decode(html[start:])
    assert payload["version"] == 1
    requested = payload["simDetails"]["requested"]
    assert requested["atol"] == requested["rtol"] == 1e-6
    times, *series = payload["allData"]
    data = dict(zip(payload["names"], series, strict=True))
    assert len(data) == len(payload["names"]) and {"x", "v", "f_spring"} <= data.keys()
    assert times[0] == 0 and times[-1] == 5 and len(times) > 10
    assert all(a < b for a, b in zip(times, times[1:]))
    for name in ["x", "v", "f_spring"]:
        assert len(data[name]) == len(times) and all(math.isfinite(x) for x in data[name])
    omega, equilibrium = math.sqrt(39.75), 9.81 / 40
    for row, time in enumerate(times):
        x = equilibrium * (1 - math.exp(-time / 2) * (math.cos(omega * time) + math.sin(omega * time) / (2 * omega)))
        v = equilibrium * 40 / omega * math.exp(-time / 2) * math.sin(omega * time)
        assert math.isclose(data["f_spring"][row], -20 * data["x"][row], rel_tol=0, abs_tol=1e-10)
        for name, expected in [("x", x), ("v", v), ("f_spring", -20 * x)]:
            # Use displacement units for all channels: force/k and velocity/
            # natural angular frequency. Solver options remain the exact guide
            # defaults; the three channels share one physical error bound.
            scale = {"x": 1, "v": math.sqrt(40), "f_spring": 20}[name]
            # Same accumulated-error budget as tracked_models::assert_curve,
            # using the actual request's unchanged default solver tolerances.
            bound = 20 * (requested["atol"] + requested["rtol"] * abs(expected / scale))
            assert abs((data[name][row] - expected) / scale) <= bound, (name, time, data[name][row], expected, bound)
    print(json.dumps({"actual_report": path.name, "samples": len(times), "analytic_channels": 3, "start": 0, "end": 5}))


def main():
    root, binary = Path(sys.argv[1]).resolve(), str(Path(sys.argv[2]).resolve())
    inventory = load_inventory(root)
    entries = [entry for entry in inventory["entries"] if entry["page"] == "getting-started/first-model.md"]
    scenarios = [entry for entry in entries if entry["language"] == "toml"]
    bash = [entry for entry in entries if entry["language"] == "bash"]
    assert len(scenarios) == 1 and len(bash) == 2
    plant = read_span(root, inventory["contexts"]["first-hanging-mass"])
    with tempfile.TemporaryDirectory() as directory:
        directory = Path(directory)
        (directory / "HangingMass.mo").write_text(plant, encoding="utf-8")
        (directory / "rumoca-scenario.toml").write_text(read_span(root, scenarios[0]), encoding="utf-8")
        for entry, report in zip(bash, ["HangingMass_results.html", "hanging_mass.html"], strict=True):
            argv = commands(read_span(root, entry))
            assert len(argv) == 1
            result = subprocess.run([binary, *argv[0], "--cache-dir", str(directory / "cache")],
                                    cwd=directory, capture_output=True, text=True, timeout=90)
            assert result.returncode == 0, (result.stdout, result.stderr)
            report_curve(directory / report)


if __name__ == "__main__":
    main()
