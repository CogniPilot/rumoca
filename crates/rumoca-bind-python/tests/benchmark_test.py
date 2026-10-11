"""Fail-closed checks for the guide's actual neural benchmark owner."""

import argparse
import importlib.util
import json
from pathlib import Path
import sys

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
    parser.parse_args()
    run_guard_controls(benchmark_module())


if __name__ == "__main__":
    main()
