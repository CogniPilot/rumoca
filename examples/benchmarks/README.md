# Benchmark Examples

This directory contains reproducible comparison scripts for the example models.

## Neural ODE JAX Parity

`neural_ode_jax_parity.py` compares `examples/models/NeuralODEBackprop.mo`
with an equivalent JAX implementation. It reports:

- Rumoca front-end compile, Solve IR/runtime prepare, and hot stepping time.
- JAX JIT compile-plus-first-execute and hot execution time.
- Initial loss/gradient parity against Rumoca CSV output.
- JAX manual backpropagation equations versus `jax.value_and_grad`.

Use a disposable virtualenv under `target/`. For CPU-only runs, install the
plain JAX package:

```bash
python3 -m venv target/jax-bench-venv
target/jax-bench-venv/bin/python -m pip install --upgrade pip
target/jax-bench-venv/bin/python -m pip install jax numpy
```

Run the CPU comparison:

```bash
target/jax-bench-venv/bin/python examples/benchmarks/neural_ode_jax_parity.py --platform cpu
```

For GPU runs, install a CUDA-enabled JAX wheel instead:

```bash
target/jax-bench-venv/bin/python -m pip install --upgrade "jax[cuda12]" numpy
target/jax-bench-venv/bin/python examples/benchmarks/neural_ode_jax_parity.py --platform gpu
```

The JSON report distinguishes Rumoca `compile_seconds`, Rumoca
`prepare_seconds`, and JAX JIT compile time. For the user's first-run wait,
compare Rumoca `ready_seconds` with JAX `integrate_compile_seconds`. When both
runtimes are enabled, the `comparison` block reports the ready-time ratio,
hot-loop ratio, structural-prepare fraction, and the maximum accuracy error
across the parity checks.
