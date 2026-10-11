"""Fail-closed checks for the guide's actual neural benchmark owner."""

import argparse
import importlib.util
import json
import math
from pathlib import Path
import subprocess
import sys
from tempfile import TemporaryDirectory

import numpy as np

REPOSITORY = Path(__file__).resolve().parents[3]
SCRIPT = REPOSITORY / "examples/benchmarks/neural_ode_jax_parity.py"


def benchmark_module():
    spec = importlib.util.spec_from_file_location("guide_neural_benchmark", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_nested_cli_report(module):
    expected = {"ready_seconds": 1.0, "prepare": {"backend_seconds": 0.5}}
    assert module.parse_last_json("Cargo build diagnostics\n" + json.dumps(expected)) == expected


def require_rejection(call):
    try:
        call()
    except RuntimeError:
        return
    raise AssertionError("invalid parity evidence must be rejected")


def test_missing_channel(module):
    require_rejection(lambda: module.max_abs_diff_from_row({}, ["loss"], np.array([1.0])))
    error, _ = module.max_abs_diff_from_row({"loss": 2.0}, ["loss"], np.array([1.5]))
    assert error == 0.5


def test_nonfinite_channel(module):
    for native, jax in [(float("nan"), 1.0), (1.0, float("nan")), (1.0, float("inf"))]:
        require_rejection(lambda: module.max_abs_diff_from_row(
            {"loss": native}, ["loss"], np.array([jax])))


def run_guard_controls(module):
    failures = []
    for test in (test_nested_cli_report, test_missing_channel, test_nonfinite_channel):
        try:
            test(module)
            print(f"benchmark_test: {test.__name__} PASS", flush=True)
        except Exception as error:
            failures.append(f"{test.__name__}: {type(error).__name__}: {error}")
    assert not failures, "\n".join(failures)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--guards-only", action="store_true",
                        help="Run the parser and invalid-evidence controls without compiling.")
    args = parser.parse_args()
    module = benchmark_module()
    run_guard_controls(module)
    if not args.guards_only:
        run_actual_benchmark(module)


def require_finite_json(value):
    if isinstance(value, dict):
        for item in value.values():
            require_finite_json(item)
    elif isinstance(value, list):
        for item in value:
            require_finite_json(item)
    elif isinstance(value, float):
        assert math.isfinite(value), "nonfinite benchmark report"


def run_actual_benchmark(module):
    # Preserve the guide command's model, dimensions, time window, warmups and
    # iterations. Only place artifacts in the process-owned scratch directory.
    with TemporaryDirectory(prefix="rumoca-guide-neural-benchmark-") as temporary:
        directory = Path(temporary)
        csv_path, report_path = directory / "neural.csv", directory / "report.json"
        subprocess.run([sys.executable, str(SCRIPT), "--platform", "cpu",
                        "--output", str(csv_path), "--json-output", str(report_path)],
                       cwd=REPOSITORY, check=True, timeout=600)
        report = json.loads(report_path.read_text(encoding="utf-8"))
        require_finite_json(report)
        assert (report["model"], report["hidden"], report["batch"], report["trainable_params"]) == (
            "NeuralODEBackprop1k", 30, 8, 1082)
        assert report["t_end"] == 0.2 and report["dt"] == 0.02
        assert report["jax"]["platform"] == "cpu"
        bench = report["rumoca"]["bench"]
        assert bench["iterations"] == 3 and bench["warmups"] == 1
        assert bench["last_points"] == 11 and bench["last_final_time"] == 0.2
        accuracy = report["accuracy"]
        assert accuracy["rumoca_observable_count"] == 6
        assert accuracy["jax_gradient_count"] == 1082
        for key in ("jax_manual_vs_reverse_grad_max_abs_error",
                    "jax_final_manual_vs_reverse_grad_max_abs_error"):
            assert accuracy[key] <= 1e-10, (key, accuracy[key])
        config = argparse.Namespace(t_end=0.2, dt=0.02)
        rows = module.load_rumoca_rows(csv_path, config)
        expected = np.asarray(report["jax"]["observable_trajectory"])
        assert expected.shape == (11, 6)
        for index, (row, values) in enumerate(zip(rows, expected, strict=True)):
            for name, value in zip(module.observable_names(), values, strict=True):
                # Initial formulas are algebraic; integrated values use the
                # documented default RK atol=rtol=1e-6 error scale.
                bound = 1e-10 if index == 0 else 20 * (1e-6 + 1e-6 * abs(value))
                assert abs(row[name] - value) <= bound, (index, name, row[name], value, bound)
    print("benchmark_test: actual Backprop1k 30/8, 11x6 observables and 1082 JAX gradients PASS")


if __name__ == "__main__":
    main()
