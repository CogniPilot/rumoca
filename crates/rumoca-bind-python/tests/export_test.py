"""Live exports from the valid-by-construction Solve representation."""

from pathlib import Path
import argparse
import builtins
import importlib
from unittest.mock import patch

import rumoca as rm

FIXTURE_ROOT = Path(__file__).resolve().parent / "fixtures"
MODEL_FILE = FIXTURE_ROOT / "UsesLib.mo"
SOURCE_ROOT = FIXTURE_ROOT / "Lib"


def _model() -> rm.Model:
    return rm.Session(roots=[str(SOURCE_ROOT)]).load(MODEL_FILE)


def _have(module: str) -> bool:
    import importlib.util

    return importlib.util.find_spec(module) is not None


def test_to_casadi() -> None:
    if not _have("casadi"):
        print("  (casadi not installed; skipping)")
        return
    import casadi as ca

    exported = _model().to_casadi()
    assert isinstance(exported, rm.SolveExport)
    assert exported.target == "casadi-ode"
    assert exported.state_names == ["x"]
    assert exported.parameter_names == ["gain"]
    assert isinstance(exported.rhs, ca.Function)
    arguments = exported.rhs.sx_in()
    expression = exported.rhs(*arguments)
    sensitivity = ca.Function("sensitivity", arguments, [
        ca.jacobian(expression, arguments[1]),
        ca.jacobian(expression, arguments[3]),
    ])
    assert sensitivity.size_out(0) == sensitivity.size_out(1) == (1, 1)
    for state, gain in [(1.0, 2.0), (3.0, 4.0)]:
        inputs = (0.0, ca.DM([state]), ca.DM.zeros(0), ca.DM([gain]))
        assert float(exported.rhs(*inputs)) == -gain * state
        state_jacobian, parameter_jacobian = sensitivity(*inputs)
        assert float(state_jacobian) == -gain
        assert float(parameter_jacobian) == -state


def test_to_jax() -> None:
    if not _have("jax"):
        print("  (jax not installed; skipping)")
        return
    import jax
    import jax.numpy as jnp

    exported = _model().to_jax()
    assert isinstance(exported, rm.SolveExport)
    assert exported.target == "jax-ode"
    assert exported.state_names == ["x"]
    assert exported.parameter_names == ["gain"]
    for state, gain in [(1.0, 2.0), (3.0, 4.0)]:
        inputs = (0.0, jnp.array([state]), jnp.zeros(0), jnp.array([gain]))
        assert float(jax.jit(exported.rhs)(*inputs)[0]) == -gain * state
        for differentiate in [jax.jacfwd, jax.jacrev]:
            state_jacobian, parameter_jacobian = differentiate(
                exported.rhs, argnums=(1, 3)
            )(*inputs)
            assert state_jacobian.shape == parameter_jacobian.shape == (1, 1)
            assert float(state_jacobian[0, 0]) == -gain
            assert float(parameter_jacobian[0, 0]) == -state


def test_magic_export_returns_checked_solve_object() -> None:
    if not _have("casadi"):
        print("  (casadi not installed; skipping magic live-export)")
        return
    from rumoca._magic import run_modelica_cell

    namespace: dict = {}
    source = "model Decay Real x(start=1); equation der(x)=-x; end Decay;"
    exported = run_modelica_cell(
        namespace, "export casadi -m Decay --name model", source
    )
    assert isinstance(exported, rm.SolveExport)
    assert namespace["model"] is exported


def test_live_export_initializes_dependent_defaults() -> None:
    model = rm.Session().loads(
        "model Initialized parameter Real a=2; parameter Real b=3*a; "
        "Real x(start=b, fixed=true); equation der(x)=-b*x; end Initialized;",
        model="Initialized",
    )
    for backend, export in [("casadi", model.to_casadi), ("jax", model.to_jax)]:
        if not _have(backend):
            print(f"  ({backend} not installed; skipping initialized live-export)")
            continue
        exported = export()
        assert exported.default_states == [6.0], backend
        parameters = dict(zip(exported.parameter_names, exported.default_parameters))
        assert parameters["a"] == 2.0 and parameters["b"] == 6.0, backend
        if backend == "casadi":
            import casadi as ca

            actual = exported.rhs(0.0, ca.DM(exported.default_states),
                                  ca.DM.zeros(exported.n_inputs), ca.DM(exported.default_parameters))
            assert float(actual) == -36.0
        else:
            import jax
            import jax.numpy as jnp

            actual = jax.jit(exported.rhs)(0.0, jnp.array(exported.default_states),
                                         jnp.zeros(exported.n_inputs), jnp.array(exported.default_parameters))
            assert float(actual[0]) == -36.0


def test_missing_dependency_raises_importerror() -> None:
    checks = [
        ("casadi", lambda model: model.to_casadi()),
        ("jax", lambda model: model.to_jax()),
    ]
    model = _model()
    for dependency, export in checks:
        def without_dependency(name, globals=None, locals=None, fromlist=(), level=0):
            if name == dependency or name.startswith(dependency + "."):
                raise ImportError(f"{dependency} deliberately unavailable")
            return builtins_import(name, globals, locals, fromlist, level)

        builtins_import = builtins.__import__
        with patch("builtins.__import__", without_dependency):
            try:
                export(model)
            except ImportError as error:
                assert dependency in str(error)
            else:
                raise AssertionError(
                    f"{dependency} export should raise ImportError when absent"
                )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--require-backend", choices=("jax", "casadi"), action="append", default=[])
    for backend in parser.parse_args().require_backend:
        dependency = importlib.import_module(backend)
        print(f"required {backend}: {dependency.__version__}")
    test_to_casadi()
    test_to_jax()
    test_magic_export_returns_checked_solve_object()
    test_live_export_initializes_dependent_defaults()
    test_missing_dependency_raises_importerror()
    print("export_test: OK")


if __name__ == "__main__":
    main()
