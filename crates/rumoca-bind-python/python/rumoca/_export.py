"""Live exports built from checked Solve programs.

Generated modules are trusted in-repository artifacts. Optional CasADi or JAX
dependencies are imported by those modules only when an export is requested.
"""

from __future__ import annotations

import types


def _exec_module(content: str, name: str) -> types.ModuleType:
    module = types.ModuleType(name)
    module.__dict__["__name__"] = name
    code = compile(content, f"<rumoca {name}>", "exec")
    exec(code, module.__dict__)  # noqa: S102 — trusted generated source
    return module


class SolveExport:
    """Explicit ``xdot = rhs(t, x, u, p)`` from the checked Solve IR."""

    def __init__(self, module: types.ModuleType, name: str, target: str):
        self.module = module
        self.name = name
        self.target = target
        self.rhs = module.rhs
        self.state_names = list(module.STATE_NAMES)
        self.input_names = list(module.INPUT_NAMES)
        self.parameter_names = list(module.PARAM_NAMES)
        self.default_states = list(module.DEFAULT_X)
        self.default_parameters = list(module.DEFAULT_P)
        self.n_states = module.N_Y
        self.n_inputs = module.N_U
        self.n_parameters = module.N_P

    def __repr__(self) -> str:
        return (
            f"SolveExport(target={self.target!r}, name={self.name!r}, "
            f"states={self.n_states}, parameters={self.n_parameters})"
        )


def build_casadi(content: str, name: str, _form: str) -> SolveExport:
    module = _exec_module(content, f"{name}_casadi")
    return SolveExport(module, name, "casadi-solve")


def build_jax(content: str, name: str, _form: str) -> SolveExport:
    module = _exec_module(content, f"{name}_jax")
    return SolveExport(module, name, "jax-solve")
