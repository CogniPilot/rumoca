"""Live symbolic exports: turn a rendered codegen target into a live object.

`Model.to_casadi()` / `.to_jax()` / `.to_sympy()` render the *existing* codegen
target in memory (no file dance, same tested templates as `codegen()`), then this
module ``exec``s the rendered source into a throwaway module and wraps the result
in a typed object. The heavy optional deps (casadi/jax/sympy) are imported only
by the generated module's own ``import`` lines, so importing rumoca never needs
them — a missing dep surfaces as a clear ``ImportError`` when you call the export.

The Rust ``Model`` methods do the rendering and call the ``build_*`` functions
here; users never touch this module directly.
"""

from __future__ import annotations

import types
from typing import Any


def _exec_module(content: str, name: str) -> types.ModuleType:
    """Exec rendered source into a fresh, named module namespace.

    A real module object (not bare ``exec`` into a dict) lets the generated
    helper functions resolve each other and gives readable tracebacks via the
    ``<rumoca ...>`` code filename.
    """
    module = types.ModuleType(name)
    module.__dict__["__name__"] = name
    code = compile(content, f"<rumoca {name}>", "exec")
    exec(code, module.__dict__)  # noqa: S102 — trusted, in-repo codegen templates
    return module


class SolveExport:
    """A live solve-IR export: the explicit ODE right-hand side.

    The solve form is the scalarized, causalized explicit system — ``xdot =
    rhs(x, u, p)`` — the same source the C/FMI/Rust backends use, with no DAE
    residual or rootfinder. Both ``casadi-solve`` (where ``rhs`` is a
    differentiable ``ca.Function``) and ``jax-solve`` (where ``rhs`` is a
    jit/grad/vmap-able ``jnp`` function) share this shape.
    """

    def __init__(self, module: types.ModuleType, name: str, target: str):
        self.module = module
        self.name = name
        self.target = target
        self.rhs = module.rhs
        self.state_names = list(module.STATE_NAMES)
        self.input_names = list(module.INPUT_NAMES)
        self.parameter_names = list(module.PARAM_NAMES)
        self.n_states = module.N_Y
        self.n_inputs = module.N_U
        self.n_parameters = module.N_P

    def __repr__(self) -> str:
        return (
            f"SolveExport(target={self.target!r}, name={self.name!r}, "
            f"states={self.n_states}, parameters={self.n_parameters})"
        )


class CasadiModel:
    """A live CasADi model built from the ``casadi-mx``/``casadi-sx`` target.

    Mirrors the generated ``create_model()`` dict: symbolic vectors
    ``x``/``xdot``/``z``/``u``/``p``; the implicit residual ``f_x``; CasADi's
    *native* semi-explicit DAE — the explicit RHS ``ode``, the algebraic block
    ``alg``, and the integrator-ready dict ``dae`` (``{x, z, p, t, ode, alg}``);
    the residual ``ca.Function`` ``dae_fn``; default vectors ``x0``/``p0``; and
    the ``*_names`` lists. ``integrator()`` and ``jacobian()`` are conveniences.
    """

    _FIELD_ALIASES = {
        "ode": "ode",
        "alg": "alg",
        "f_x": "f_x",
        "residual": "f_x",
        "x": "x",
        "xdot": "xdot",
        "z": "z",
        "u": "u",
        "p": "p",
        "params": "p",
    }

    def __init__(self, raw: dict[str, Any], module: types.ModuleType, name: str):
        self._raw = raw
        self.module = module
        self.name = name
        self.x = raw["x"]
        self.xdot = raw.get("xdot")
        self.z = raw["z"]
        self.u = raw["u"]
        self.p = raw["p"]
        self.f_x = raw["f_x"]
        # Native CasADi DAE structure. Older templates expose only the implicit
        # residual; fall back to it so the wrapper stays robust across targets.
        self.ode = raw.get("ode", raw["f_x"])
        self.alg = raw.get("alg")
        self.dae = raw.get("dae")
        self.dae_fn = raw["dae_fn"]
        self.x0 = raw["x0"]
        self.p0 = raw["p0"]
        self.state_names = raw.get("state_names", [])
        self.algebraic_names = raw.get("algebraic_names", [])
        self.input_names = raw.get("input_names", [])
        self.parameter_names = raw.get("param_names", [])
        self.functions: dict[str, Any] = raw.get("functions", {})

    def integrator(self, dt: Any = 1.0, *, method: str = "idas", **opts: Any) -> Any:
        """Build a CasADi integrator over the time grid/step ``dt``."""
        return self._raw["build_integrator"](dt, opts or None, method)

    def jacobian(self, of: str, wrt: str) -> Any:
        """Exact AD Jacobian of one field w.r.t. another, e.g. ``("ode", "p")``.

        ``"ode"`` is the explicit state RHS (CasADi's native DAE form), so
        ``jacobian("ode", "p")`` is the true parameter sensitivity of the
        dynamics, not of the implicit residual.
        """
        import casadi as ca

        try:
            lhs = getattr(self, self._FIELD_ALIASES[of])
            rhs = getattr(self, self._FIELD_ALIASES[wrt])
        except KeyError as error:
            valid = ", ".join(sorted(self._FIELD_ALIASES))
            raise KeyError(f"unknown field {error.args[0]!r}; choose from: {valid}") from None
        return ca.jacobian(lhs, rhs)

    def __repr__(self) -> str:
        return (
            f"CasadiModel(name={self.name!r}, states={len(self.state_names)}, "
            f"parameters={len(self.parameter_names)})"
        )


class JaxModel:
    """A live JAX model built from the ``jax`` (diffrax) target.

    Exposes the explicit ODE ``ode_fn(t, y, args)``, default ``x0``/``p0``, the
    name lists, and a ready ``simulate()`` helper.
    """

    def __init__(self, module: types.ModuleType, name: str):
        self.module = module
        self.name = name
        defaults = module.get_default_values()
        self.x0 = defaults["x0"]
        self.p0 = defaults["p0"]
        self.ode_fn = module.ode_fn
        self.state_names = module.get_state_names()
        self.parameter_names = module.get_param_names()
        self.input_names = module.get_input_names()

    def simulate(self, *args: Any, **kwargs: Any) -> Any:
        """Roll out the model with diffrax (see the generated ``simulate``)."""
        return self.module.simulate(*args, **kwargs)

    def __repr__(self) -> str:
        return (
            f"JaxModel(name={self.name!r}, states={len(self.state_names)}, "
            f"parameters={len(self.parameter_names)})"
        )


class SympyModel:
    """A live SymPy model built from the ``sympy`` target.

    Wraps the generated ``Model`` instance: symbolic vectors ``x``/``y``/``u``/
    ``w``/``p``, residuals ``f_x``, and ``solve_explicit()`` for an explicit
    derivative/algebraic solve where possible.
    """

    def __init__(self, model: Any, module: types.ModuleType, name: str):
        self.model = model
        self.module = module
        self.name = name
        self.x = model.x
        self.y = model.y
        self.u = model.u
        self.w = model.w
        self.p = model.p
        self.f_x = model.f_x

    def solve_explicit(self) -> Any:
        """Best-effort symbolic solve for explicit derivatives/algebraics."""
        return self.model.solve_explicit()

    def summary(self) -> dict[str, Any]:
        return self.model.summary()

    def __repr__(self) -> str:
        return f"SympyModel(name={self.name!r}, states={len(self.x)}, parameters={len(self.p)})"


# ── builders called from the Rust `Model` methods ───────────────────────────


def build_casadi(content: str, name: str, form: str) -> Any:
    module = _exec_module(content, f"{name}_casadi")
    if form == "dae":
        return CasadiModel(module.create_model(), module, name)
    return SolveExport(module, name, "casadi-solve")


def build_jax(content: str, name: str, form: str) -> Any:
    module = _exec_module(content, f"{name}_jax")
    if form == "dae":
        return JaxModel(module, name)
    return SolveExport(module, name, "jax-solve")


def build_sympy(content: str, name: str, _form: str) -> Any:
    module = _exec_module(content, f"{name}_sympy")
    return SympyModel(module.Model(), module, name)
