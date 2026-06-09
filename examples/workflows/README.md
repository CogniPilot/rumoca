# End-to-end workflows

Four self-contained demonstrations of the **same** Rumoca pipeline:

> hand-written Modelica model → `rumoca compile --target <backend>` →
> machine-generated artifact → a notebook that *uses* it.

Each folder is independent: it carries its own `model/*.mo` source, its own
`_generated/<backend>/` artifact (already exported for you), and one notebook
that re-states the export command, then runs the actual control / analysis /
estimation / learning task with plots inline.

| Folder | Verb | Backend | What the notebook does |
|--------|------|---------|------------------------|
| [`1_jax_paramid/`](1_jax_paramid/) | **Learn** | JAX + diffrax | Recover `(m, J)` of a PVTOL by gradient descent through a differentiable rollout — 50%/40% wrong → <0.5% in 60 Adam steps |
| [`2_sympy_linearize/`](2_sympy_linearize/) | **Understand** | SymPy (+ CasADi) | Symbolic A, B at hover, controllability rank, and cross-backend Jacobian agreement to machine precision |
| [`3_casadi_perch/`](3_casadi_perch/) | **Control** | CasADi → IPOPT | Nonlinear MPC flying a fixed-wing UAV into a post-stall perched landing |
| [`4_symforce_ekf/`](4_symforce_ekf/) | **Estimate** | SymForce | On-manifold IMU+GPS EKF; analytical Jacobians + C++ codegen; ~500 m drift → ~2 m |

## The one invariant

Everything under each `_generated/` directory is produced by Rumoca and is
**never hand-edited**. The notebook imports it as an ordinary Python module.
Change the `.mo` and re-export, and the downstream task picks up the new
physics with no hand-translation — that is the point the paper argues.

## Running one

```bash
cd 3_casadi_perch        # for example
jupyter lab perch_nmpc.ipynb
```

The artifacts are checked in, so the notebooks run as-is. To regenerate an
artifact from its source, run the `rumoca compile ...` command shown near the
top of that folder's notebook (and in its README). Backend prerequisites:

| Folder | `pip install` |
|--------|---------------|
| `1_jax_paramid` | `jax jaxlib diffrax numpy matplotlib` |
| `2_sympy_linearize` | `sympy casadi numpy` |
| `3_casadi_perch` | `casadi numpy matplotlib` |
| `4_symforce_ekf` | `symforce numpy matplotlib` |
