"""Rumoca — a first-class Python API for the Rumoca Modelica compiler.

The surface is small and typed. Compile a model and everything hangs off it::

    import rumoca as rm

    m = rm.load("Quadrotor.mo", model="Quadrotor", roots=["libs/CMM"])
    m.parameters["mass"].value        # typed, autocompletes
    r = m.simulate(t=(0, 10), dt=0.01)
    r.plot("body.v[1]"); df = r.to_dataframe()

No ``json.loads`` anywhere: returned objects are real typed classes backed
directly by the compiler/solver. JSON appears only when you ask for it
(``m.to_json()`` / ``r.to_json()``).

The compiled extension lives at :mod:`rumoca._native`; this package re-exports
the public surface and registers the ``%%modelica`` Jupyter cell magic when
imported inside IPython.
"""

from __future__ import annotations

from . import _native
from ._export import CasadiModel, JaxModel, SolveExport, SympyModel
from ._magic import load_ipython_extension, unload_ipython_extension

# ── module-level functions ──────────────────────────────────────────────────
load = _native.load
loads = _native.loads
validate = _native.validate
validate_source = _native.validate_source
format = _native.format  # noqa: A001 — mirrors `rumoca format`, intentional
version = _native.version
targets = _native.targets
solvers = _native.solvers

# ── the hub & session ───────────────────────────────────────────────────────
Model = _native.Model
Session = _native.Session
SimConfig = _native.SimConfig

# ── views & metadata ────────────────────────────────────────────────────────
VarView = _native.VarView
ParamView = _native.ParamView
VariableInfo = _native.VariableInfo
ParameterInfo = _native.ParameterInfo
StructuralInfo = _native.StructuralInfo

# ── results & codegen ───────────────────────────────────────────────────────
Result = _native.Result
CodegenResult = _native.CodegenResult
GeneratedFile = _native.GeneratedFile
Target = _native.Target
SolverInfo = _native.SolverInfo

# ── live symbolic exports (returned by Model.to_casadi/to_jax/to_sympy) ──────
# (defined in Python; imported above from ._export)

# ── diagnostics & errors ────────────────────────────────────────────────────
Diagnostic = _native.Diagnostic
RumocaError = _native.RumocaError
ParseError = _native.ParseError
CompileError = _native.CompileError
SimulationError = _native.SimulationError
StructuralParamError = _native.StructuralParamError

__all__ = [
    # functions
    "load",
    "loads",
    "validate",
    "validate_source",
    "format",
    "version",
    "targets",
    "solvers",
    # hub & session
    "Model",
    "Session",
    "SimConfig",
    # views & metadata
    "VarView",
    "ParamView",
    "VariableInfo",
    "ParameterInfo",
    "StructuralInfo",
    # results & codegen
    "Result",
    "CodegenResult",
    "GeneratedFile",
    "Target",
    "SolverInfo",
    # live symbolic exports
    "CasadiModel",
    "JaxModel",
    "SympyModel",
    "SolveExport",
    # diagnostics & errors
    "Diagnostic",
    "RumocaError",
    "ParseError",
    "CompileError",
    "SimulationError",
    "StructuralParamError",
    # jupyter magic hooks
    "load_ipython_extension",
    "unload_ipython_extension",
]


def _register_magic_if_in_ipython() -> None:
    """Auto-register ``%%modelica`` so it works after a bare ``import rumoca``.

    No-op outside IPython/Jupyter (e.g. a plain ``python`` process), so importing
    the package never fails just because IPython is absent.
    """
    try:
        from IPython import get_ipython

        shell = get_ipython()
    except Exception:
        return
    if shell is not None:
        load_ipython_extension(shell)


_register_magic_if_in_ipython()
