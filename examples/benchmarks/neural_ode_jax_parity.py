#!/usr/bin/env python3
"""Compare Rumoca's native NeuralODEBackprop example with a JAX equivalent.

The script checks three things:

1. Rumoca prepare/hot simulation timings from `rumoca sim bench`.
2. JAX JIT compile-plus-first-execute and hot execution timings.
3. Numeric parity for initial loss/gradient values and final integrated values.

It intentionally keeps the JAX implementation in this file so the formulas can
be audited against examples/models/NeuralODEBackprop.mo.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import os
import pathlib
import statistics
import subprocess
import sys
import time
from dataclasses import dataclass
from typing import Any

import numpy as np


REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]


@dataclass(frozen=True)
class ModelConfig:
    hidden: int
    batch: int
    state: int = 2
    learning_rate: float = 0.35
    weight_decay: float = 1.0e-4
    omega: float = 2.2
    damping: float = 0.18
    data_rate: float = 0.55
    pi: float = math.pi

    @property
    def trainable_params(self) -> int:
        return (
            self.hidden * self.state
            + self.hidden
            + self.hidden * self.hidden
            + self.hidden
            + self.state * self.hidden
            + self.state
        )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Benchmark NeuralODEBackprop against an equivalent JAX model."
    )
    parser.add_argument("--hidden", type=int, default=30)
    parser.add_argument("--batch", type=int, default=8)
    parser.add_argument("--model-file", default="examples/models/NeuralODEBackprop.mo")
    parser.add_argument("--model", default="NeuralODEBackprop1k")
    parser.add_argument("--solver", default="rk-like")
    parser.add_argument("--t-end", type=float, default=0.2)
    parser.add_argument("--dt", type=float, default=0.02)
    parser.add_argument("--iterations", type=int, default=3)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--platform", choices=("auto", "cpu", "gpu"), default="auto")
    parser.add_argument("--output", default="target/neural_ode_backprop1k_rumoca.csv")
    parser.add_argument("--skip-rumoca", action="store_true")
    parser.add_argument("--skip-jax", action="store_true")
    parser.add_argument(
        "--json-output",
        default=None,
        help="Optional path for the JSON report. The report is always printed.",
    )
    return parser.parse_args()


def run_command(command: list[str], *, cwd: pathlib.Path) -> str:
    completed = subprocess.run(
        command,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=True,
    )
    return completed.stdout


def parse_last_json(stdout: str) -> dict[str, Any]:
    start = stdout.rfind("{")
    if start < 0:
        raise RuntimeError(f"command did not print a JSON object:\n{stdout}")
    return json.loads(stdout[start:])


def run_rumoca(args: argparse.Namespace) -> dict[str, Any]:
    model_file = str(REPO_ROOT / args.model_file)
    bench_output = run_command(
        [
            "cargo",
            "run",
            "--release",
            "-p",
            "rumoca",
            "--",
            "sim",
            "bench",
            model_file,
            "--model",
            args.model,
            "--solver",
            args.solver,
            "--t-end",
            str(args.t_end),
            "--dt",
            str(args.dt),
            "--iterations",
            str(args.iterations),
            "--warmups",
            str(args.warmups),
            "--json",
        ],
        cwd=REPO_ROOT,
    )
    bench = parse_last_json(bench_output)
    output_path = REPO_ROOT / args.output
    output_path.parent.mkdir(parents=True, exist_ok=True)
    run_command(
        [
            "cargo",
            "run",
            "--release",
            "-p",
            "rumoca",
            "--",
            "sim",
            model_file,
            "--model",
            args.model,
            "--solver",
            args.solver,
            "--t-end",
            str(args.t_end),
            "--dt",
            str(args.dt),
            "--output",
            str(output_path),
        ],
        cwd=REPO_ROOT,
    )
    return {
        "bench": bench,
        "csv": str(output_path.relative_to(REPO_ROOT)),
    }


def load_rumoca_rows(csv_path: pathlib.Path) -> tuple[dict[str, float], dict[str, float]]:
    with csv_path.open(newline="") as handle:
        rows = list(csv.DictReader(handle))
    if not rows:
        raise RuntimeError(f"no samples found in {csv_path}")
    return row_to_float_map(rows[0]), row_to_float_map(rows[-1])


def row_to_float_map(row: dict[str, str]) -> dict[str, float]:
    return {name: float(value) for name, value in row.items()}


def initial_state_np(cfg: ModelConfig) -> np.ndarray:
    hidden = cfg.hidden
    state = cfg.state
    input_scale = 0.6 / math.sqrt(state)
    recurrent_scale = 0.22 / math.sqrt(hidden)
    output_scale = 0.12 / math.sqrt(hidden)

    i = np.arange(1, hidden + 1, dtype=np.float64)[:, None]
    j = np.arange(1, state + 1, dtype=np.float64)[None, :]
    w1 = np.where(
        i == j,
        input_scale,
        0.10
        * input_scale
        * (np.sin(0.17 * i + 0.31 * j) + 0.5 * np.cos(0.11 * i - 0.23 * j)),
    )
    b1 = 0.02 * np.sin(0.19 * np.arange(1, hidden + 1, dtype=np.float64))

    row = np.arange(1, hidden + 1, dtype=np.float64)[:, None]
    col = np.arange(1, hidden + 1, dtype=np.float64)[None, :]
    wmid = np.where(
        row == col,
        recurrent_scale,
        0.04
        * recurrent_scale
        * (
            np.sin(0.07 * row + 0.13 * col)
            - 0.3 * np.cos(0.05 * row - 0.17 * col)
        ),
    )
    bmid = 0.01 * np.cos(0.17 * np.arange(1, hidden + 1, dtype=np.float64))

    out = np.arange(1, state + 1, dtype=np.float64)[:, None]
    hid = np.arange(1, hidden + 1, dtype=np.float64)[None, :]
    w2 = output_scale * (
        np.sin(0.23 * out + 0.07 * hid) - 0.4 * np.cos(0.13 * out - 0.11 * hid)
    )
    b2 = np.zeros(state, dtype=np.float64)
    return pack_np((w1, b1, wmid, bmid, w2, b2))


def pack_np(parts: tuple[np.ndarray, ...]) -> np.ndarray:
    return np.concatenate([part.reshape(-1) for part in parts])


def state_names(cfg: ModelConfig) -> list[str]:
    names: list[str] = []
    names.extend(
        f"W1[{i},{j}]"
        for i in range(1, cfg.hidden + 1)
        for j in range(1, cfg.state + 1)
    )
    names.extend(f"b1[{i}]" for i in range(1, cfg.hidden + 1))
    names.extend(
        f"Wmid[{i},{j}]"
        for i in range(1, cfg.hidden + 1)
        for j in range(1, cfg.hidden + 1)
    )
    names.extend(f"bmid[{i}]" for i in range(1, cfg.hidden + 1))
    names.extend(
        f"W2[{i},{j}]"
        for i in range(1, cfg.state + 1)
        for j in range(1, cfg.hidden + 1)
    )
    names.extend(f"b2[{i}]" for i in range(1, cfg.state + 1))
    return names


def grad_names(cfg: ModelConfig) -> list[str]:
    names: list[str] = []
    names.extend(
        f"gradW1[{i},{j}]"
        for i in range(1, cfg.hidden + 1)
        for j in range(1, cfg.state + 1)
    )
    names.extend(f"gradb1[{i}]" for i in range(1, cfg.hidden + 1))
    names.extend(
        f"gradWmid[{i},{j}]"
        for i in range(1, cfg.hidden + 1)
        for j in range(1, cfg.hidden + 1)
    )
    names.extend(f"gradbmid[{i}]" for i in range(1, cfg.hidden + 1))
    names.extend(
        f"gradW2[{i},{j}]"
        for i in range(1, cfg.state + 1)
        for j in range(1, cfg.hidden + 1)
    )
    names.extend(f"gradb2[{i}]" for i in range(1, cfg.state + 1))
    return names


def observable_names() -> list[str]:
    return [
        "loss",
        "rmsError",
        "learnedDX[1]",
        "learnedDX[2]",
        "targetFirst[1]",
        "targetFirst[2]",
    ]


def max_abs_diff_from_row(
    row: dict[str, float], names: list[str], values: np.ndarray
) -> tuple[float, list[dict[str, float]]]:
    diffs = []
    max_abs = 0.0
    for name, actual in zip(names, values, strict=True):
        if name not in row:
            continue
        expected = row[name]
        abs_error = abs(expected - float(actual))
        max_abs = max(max_abs, abs_error)
        diffs.append(
            {
                "name": name,
                "rumoca": expected,
                "jax": float(actual),
                "abs_error": abs_error,
            }
        )
    diffs.sort(key=lambda item: item["abs_error"], reverse=True)
    return max_abs, diffs[:10]


def import_jax(platform: str):
    if platform == "cpu":
        os.environ["JAX_PLATFORMS"] = "cpu"
        os.environ["JAX_PLATFORM_NAME"] = "cpu"
    elif platform == "gpu":
        os.environ["JAX_PLATFORMS"] = "cuda"
        os.environ["JAX_PLATFORM_NAME"] = "gpu"
    import jax

    jax.config.update("jax_enable_x64", True)
    import jax.numpy as jnp

    return jax, jnp


def run_jax(
    args: argparse.Namespace,
    cfg: ModelConfig,
    initial_state: np.ndarray,
) -> dict[str, Any]:
    jax, jnp = import_jax(args.platform)

    hidden = cfg.hidden
    state = cfg.state
    batch = cfg.batch

    def unpack(y):
        offset = 0
        w1 = y[offset : offset + hidden * state].reshape((hidden, state))
        offset += hidden * state
        b1 = y[offset : offset + hidden]
        offset += hidden
        wmid = y[offset : offset + hidden * hidden].reshape((hidden, hidden))
        offset += hidden * hidden
        bmid = y[offset : offset + hidden]
        offset += hidden
        w2 = y[offset : offset + state * hidden].reshape((state, hidden))
        offset += state * hidden
        b2 = y[offset : offset + state]
        return w1, b1, wmid, bmid, w2, b2

    def pack(parts):
        return jnp.concatenate([part.reshape(-1) for part in parts])

    def batch_samples(t):
        b = jnp.arange(batch, dtype=jnp.float64)
        angle = cfg.data_rate * t + 2.0 * cfg.pi * b / batch
        sample = jnp.stack((jnp.cos(angle), jnp.sin(angle)), axis=0)
        target = jnp.stack(
            (
                -cfg.damping * sample[0] - cfg.omega * sample[1],
                cfg.omega * sample[0] - cfg.damping * sample[1],
            ),
            axis=0,
        )
        return sample, target

    def forward(parts, t):
        w1, b1, wmid, bmid, w2, b2 = parts
        sample, target = batch_samples(t)
        z1 = w1 @ sample
        a1 = jnp.tanh(z1 + b1[:, None])
        z2 = wmid @ a1
        a2 = jnp.tanh(z2 + bmid[:, None])
        pred = w2 @ a2
        prediction = pred + b2[:, None]
        error = prediction - target
        loss = 0.5 * jnp.sum(error * error) / batch
        rms_error = jnp.sqrt(2.0 * loss / state)
        observables = jnp.concatenate(
            (
                jnp.array([loss, rms_error], dtype=jnp.float64),
                prediction[:, 0],
                target[:, 0],
            )
        )
        return loss, observables, (sample, a1, a2, error)

    def manual_grads(parts, t):
        w1, _b1, wmid, _bmid, w2, _b2 = parts
        _loss, observables, (sample, a1, a2, error) = forward(parts, t)
        gradb2 = jnp.sum(error, axis=1) / batch
        gradw2 = error @ a2.T / batch
        delta2 = (w2.T @ error) * (1.0 - a2 * a2)
        gradbmid = jnp.sum(delta2, axis=1) / batch
        gradwmid = delta2 @ a1.T / batch
        delta1 = (wmid.T @ delta2) * (1.0 - a1 * a1)
        gradb1 = jnp.sum(delta1, axis=1) / batch
        gradw1 = delta1 @ sample.T / batch
        grads = (gradw1, gradb1, gradwmid, gradbmid, gradw2, gradb2)
        return observables, grads

    def derivatives_from_grads(parts, grads):
        return tuple(
            -cfg.learning_rate * (grad + cfg.weight_decay * value)
            for value, grad in zip(parts, grads, strict=True)
        )

    def reverse_rhs(y, t):
        parts = unpack(y)

        def loss_from_parts(candidate_parts):
            loss, _observables, _aux = forward(candidate_parts, t)
            return loss

        _loss, grads = jax.value_and_grad(loss_from_parts)(parts)
        return pack(derivatives_from_grads(parts, grads))

    def manual_rhs(y, t):
        parts = unpack(y)
        _observables, grads = manual_grads(parts, t)
        return pack(derivatives_from_grads(parts, grads))

    def diagnostic_values(y, t):
        parts = unpack(y)
        observables, grads = manual_grads(parts, t)

        def loss_from_parts(candidate_parts):
            loss, _observables, _aux = forward(candidate_parts, t)
            return loss

        _loss, reverse_grads = jax.value_and_grad(loss_from_parts)(parts)
        return observables, pack(grads), pack(reverse_grads)

    def dopri5_step(y, t, h):
        k1 = reverse_rhs(y, t)
        k2 = reverse_rhs(y + h * (1.0 / 5.0) * k1, t + h * (1.0 / 5.0))
        k3 = reverse_rhs(
            y + h * ((3.0 / 40.0) * k1 + (9.0 / 40.0) * k2),
            t + h * (3.0 / 10.0),
        )
        k4 = reverse_rhs(
            y + h * ((44.0 / 45.0) * k1 + (-56.0 / 15.0) * k2 + (32.0 / 9.0) * k3),
            t + h * (4.0 / 5.0),
        )
        k5 = reverse_rhs(
            y
            + h
            * (
                (19372.0 / 6561.0) * k1
                + (-25360.0 / 2187.0) * k2
                + (64448.0 / 6561.0) * k3
                + (-212.0 / 729.0) * k4
            ),
            t + h * (8.0 / 9.0),
        )
        k6 = reverse_rhs(
            y
            + h
            * (
                (9017.0 / 3168.0) * k1
                + (-355.0 / 33.0) * k2
                + (46732.0 / 5247.0) * k3
                + (49.0 / 176.0) * k4
                + (-5103.0 / 18656.0) * k5
            ),
            t + h,
        )
        y5 = y + h * (
            (35.0 / 384.0) * k1
            + (500.0 / 1113.0) * k3
            + (125.0 / 192.0) * k4
            + (-2187.0 / 6784.0) * k5
            + (11.0 / 84.0) * k6
        )
        k7 = reverse_rhs(y5, t + h)
        y4 = y + h * (
            (5179.0 / 57600.0) * k1
            + (7571.0 / 16695.0) * k3
            + (393.0 / 640.0) * k4
            + (-92097.0 / 339200.0) * k5
            + (187.0 / 2100.0) * k6
            + (1.0 / 40.0) * k7
        )
        scale = 1.0e-6 + 1.0e-6 * jnp.maximum(jnp.abs(y), jnp.abs(y5))
        error_norm = jnp.max(jnp.abs(y5 - y4) / jnp.maximum(scale, 1.0e-30))
        return y5, error_norm

    def adapt_step(h, error_norm):
        factor = jnp.where(
            error_norm <= 0.0,
            5.0,
            jnp.clip(0.9 * error_norm ** -0.2, 0.2, 5.0),
        )
        return jnp.maximum(h * factor, 1.0e-12)

    def advance_to(carry, target_t):
        def cond(inner):
            t, _y, _h = inner
            return t < target_t - 1.0e-15

        def body(inner):
            t, y, h = inner
            step_h = jnp.minimum(jnp.maximum(h, 1.0e-12), target_t - t)
            y_next, error_norm = dopri5_step(y, t, step_h)
            accepted = error_norm <= 1.0
            next_t = jnp.where(accepted, t + step_h, t)
            next_y = jnp.where(accepted, y_next, y)
            next_h = adapt_step(step_h, error_norm)
            return next_t, next_y, next_h

        return jax.lax.while_loop(cond, body, carry)

    def integrate(y0):
        sample_times = jnp.linspace(0.0, args.t_end, int(round(args.t_end / args.dt)) + 1)
        initial_h = min(args.dt, 0.01)

        def scan_step(carry, target_t):
            next_carry = advance_to(carry, target_t)
            _t, y, _h = next_carry
            return next_carry, y

        carry0 = (jnp.array(0.0, dtype=jnp.float64), y0, jnp.array(initial_h, dtype=jnp.float64))
        final_carry, trajectory = jax.lax.scan(scan_step, carry0, sample_times[1:])
        _t, final_y, _h = final_carry
        return final_y, trajectory

    def block_until_ready(value):
        return jax.block_until_ready(value)

    y0 = jnp.asarray(initial_state, dtype=jnp.float64)
    t0 = jnp.array(0.0, dtype=jnp.float64)
    jit_reverse_rhs = jax.jit(reverse_rhs)
    jit_manual_rhs = jax.jit(manual_rhs)
    jit_diagnostics = jax.jit(diagnostic_values)
    jit_integrate = jax.jit(integrate)

    reverse_rhs_compile_seconds, _ = time_first_call(
        lambda: block_until_ready(jit_reverse_rhs(y0, t0))
    )
    manual_rhs_compile_seconds, _ = time_first_call(
        lambda: block_until_ready(jit_manual_rhs(y0, t0))
    )
    diagnostics_compile_seconds, diagnostic_result = time_first_call(
        lambda: block_until_ready(jit_diagnostics(y0, t0))
    )
    integrate_compile_seconds, integrate_result = time_first_call(
        lambda: block_until_ready(jit_integrate(y0))
    )

    reverse_rhs_hot = time_hot_loop(
        lambda: block_until_ready(jit_reverse_rhs(y0, t0)),
        args.warmups,
        args.iterations,
    )
    integrate_hot = time_hot_loop(
        lambda: block_until_ready(jit_integrate(y0)),
        args.warmups,
        args.iterations,
    )

    observables, manual_grad_flat, reverse_grad_flat = diagnostic_result
    final_y, _trajectory = integrate_result
    final_observables, final_manual_grad_flat, _final_reverse_grad_flat = block_until_ready(
        jit_diagnostics(final_y, jnp.asarray(args.t_end, dtype=jnp.float64))
    )
    manual_reverse_error = float(
        np.max(np.abs(np.asarray(manual_grad_flat) - np.asarray(reverse_grad_flat)))
    )
    final_manual_reverse_error = float(
        np.max(np.abs(np.asarray(final_manual_grad_flat) - np.asarray(_final_reverse_grad_flat)))
    )

    return {
        "platform": jax.default_backend(),
        "devices": [str(device) for device in jax.devices()],
        "trainable_params": cfg.trainable_params,
        "reverse_rhs_compile_seconds": reverse_rhs_compile_seconds,
        "manual_rhs_compile_seconds": manual_rhs_compile_seconds,
        "diagnostics_compile_seconds": diagnostics_compile_seconds,
        "integrate_compile_seconds": integrate_compile_seconds,
        "reverse_rhs_hot_average_seconds": reverse_rhs_hot["average_seconds"],
        "reverse_rhs_hot_best_seconds": reverse_rhs_hot["best_seconds"],
        "integrate_hot_average_seconds": integrate_hot["average_seconds"],
        "integrate_hot_best_seconds": integrate_hot["best_seconds"],
        "initial_observables": np.asarray(observables).tolist(),
        "initial_manual_gradients": np.asarray(manual_grad_flat).tolist(),
        "initial_reverse_gradients": np.asarray(reverse_grad_flat).tolist(),
        "final_observables": np.asarray(final_observables).tolist(),
        "final_state": np.asarray(final_y).tolist(),
        "manual_vs_reverse_grad_max_abs_error": manual_reverse_error,
        "final_manual_vs_reverse_grad_max_abs_error": final_manual_reverse_error,
    }


def time_first_call(func):
    start = time.perf_counter()
    value = func()
    return time.perf_counter() - start, value


def time_hot_loop(func, warmups: int, iterations: int) -> dict[str, float]:
    for _ in range(warmups):
        func()
    samples: list[float] = []
    for _ in range(iterations):
        start = time.perf_counter()
        func()
        samples.append(time.perf_counter() - start)
    return {
        "average_seconds": statistics.mean(samples),
        "best_seconds": min(samples),
        "worst_seconds": max(samples),
    }


def build_accuracy_report(
    cfg: ModelConfig,
    initial_state: np.ndarray,
    rumoca_initial: dict[str, float],
    rumoca_final: dict[str, float],
    jax_report: dict[str, Any],
) -> dict[str, Any]:
    state_error, state_diffs = max_abs_diff_from_row(
        rumoca_initial, state_names(cfg), initial_state
    )
    observable_error, observable_diffs = max_abs_diff_from_row(
        rumoca_initial,
        observable_names(),
        np.asarray(jax_report["initial_observables"], dtype=np.float64),
    )
    grad_error, grad_diffs = max_abs_diff_from_row(
        rumoca_initial,
        grad_names(cfg),
        np.asarray(jax_report["initial_manual_gradients"], dtype=np.float64),
    )
    final_observable_error, final_observable_diffs = max_abs_diff_from_row(
        rumoca_final,
        observable_names(),
        np.asarray(jax_report["final_observables"], dtype=np.float64),
    )
    final_state_error, final_state_diffs = max_abs_diff_from_row(
        rumoca_final,
        state_names(cfg),
        np.asarray(jax_report["final_state"], dtype=np.float64),
    )
    return {
        "initial_state_max_abs_error": state_error,
        "initial_state_worst": state_diffs,
        "initial_observable_max_abs_error": observable_error,
        "initial_observable_worst": observable_diffs,
        "initial_gradient_max_abs_error": grad_error,
        "initial_gradient_worst": grad_diffs,
        "final_observable_max_abs_error": final_observable_error,
        "final_observable_worst": final_observable_diffs,
        "final_state_max_abs_error": final_state_error,
        "final_state_worst": final_state_diffs,
        "jax_manual_vs_reverse_grad_max_abs_error": jax_report[
            "manual_vs_reverse_grad_max_abs_error"
        ],
        "jax_final_manual_vs_reverse_grad_max_abs_error": jax_report[
            "final_manual_vs_reverse_grad_max_abs_error"
        ],
    }


def slim_jax_report(report: dict[str, Any]) -> dict[str, Any]:
    hidden_keys = {
        "initial_observables",
        "initial_manual_gradients",
        "initial_reverse_gradients",
        "final_observables",
        "final_state",
    }
    return {key: value for key, value in report.items() if key not in hidden_keys}


def build_comparison_report(
    rumoca_report: dict[str, Any],
    jax_report: dict[str, Any],
    accuracy_report: dict[str, Any],
) -> dict[str, Any]:
    bench = rumoca_report["bench"]
    ready_seconds = float(bench["ready_seconds"])
    prepare_seconds = float(bench.get("prepare_seconds", 0.0))
    structural_seconds = float(bench.get("prepare_ir_solve_structural_dae_seconds", 0.0))
    jax_integrate_compile = float(jax_report["integrate_compile_seconds"])
    rumoca_hot = float(bench["hot_average_seconds"])
    jax_hot = float(jax_report["integrate_hot_average_seconds"])
    accuracy_keys = [
        "initial_state_max_abs_error",
        "initial_observable_max_abs_error",
        "initial_gradient_max_abs_error",
        "final_state_max_abs_error",
        "final_observable_max_abs_error",
        "jax_manual_vs_reverse_grad_max_abs_error",
        "jax_final_manual_vs_reverse_grad_max_abs_error",
    ]
    max_accuracy = max(float(accuracy_report[key]) for key in accuracy_keys)
    return {
        "rumoca_ready_seconds": ready_seconds,
        "jax_integrate_compile_seconds": jax_integrate_compile,
        "rumoca_ready_over_jax_integrate_compile": ready_seconds
        / max(jax_integrate_compile, 1.0e-30),
        "rumoca_hot_average_seconds": rumoca_hot,
        "jax_integrate_hot_average_seconds": jax_hot,
        "rumoca_hot_average_over_jax_integrate_hot_average": rumoca_hot
        / max(jax_hot, 1.0e-30),
        "rumoca_prepare_seconds": prepare_seconds,
        "rumoca_structural_prepare_seconds": structural_seconds,
        "rumoca_structural_prepare_fraction": structural_seconds
        / max(prepare_seconds, 1.0e-30),
        "max_accuracy_abs_error": max_accuracy,
    }


def main() -> int:
    args = parse_args()
    cfg = ModelConfig(hidden=args.hidden, batch=args.batch)
    initial_state = initial_state_np(cfg)
    report: dict[str, Any] = {
        "model": args.model,
        "hidden": cfg.hidden,
        "batch": cfg.batch,
        "trainable_params": cfg.trainable_params,
        "t_end": args.t_end,
        "dt": args.dt,
        "notes": {
            "rumoca_ready_seconds": "compile_seconds + prepare_seconds from sim bench.",
            "rumoca_prepare_seconds": "DAE-to-Solve lowering plus runtime preparation paid before hot stepping.",
            "jax_compile_seconds": "JIT compile plus first execute for the named JAX function.",
            "jax_backprop": "JAX reverse-mode value_and_grad RHS; Rumoca model uses explicit Modelica backprop equations.",
        },
    }

    rumoca_report = None
    if not args.skip_rumoca:
        rumoca_report = run_rumoca(args)
        bench = rumoca_report["bench"]
        bench["ready_seconds"] = bench.get("compile_seconds", 0.0) + bench.get(
            "prepare_seconds", 0.0
        )
        report["rumoca"] = rumoca_report

    jax_report = None
    if not args.skip_jax:
        jax_report = run_jax(args, cfg, initial_state)
        report["jax"] = slim_jax_report(jax_report)

    if rumoca_report is not None and jax_report is not None:
        initial_row, final_row = load_rumoca_rows(REPO_ROOT / rumoca_report["csv"])
        accuracy_report = build_accuracy_report(
            cfg, initial_state, initial_row, final_row, jax_report
        )
        report["accuracy"] = accuracy_report
        report["comparison"] = build_comparison_report(
            rumoca_report, jax_report, accuracy_report
        )

    rendered = json.dumps(report, indent=2, sort_keys=True)
    print(rendered)
    if args.json_output:
        output = pathlib.Path(args.json_output)
        if not output.is_absolute():
            output = REPO_ROOT / output
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(rendered + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
