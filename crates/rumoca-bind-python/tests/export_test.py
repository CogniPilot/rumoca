"""Live exports from the valid-by-construction Solve representation."""

from pathlib import Path

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
    xdot = exported.rhs(0.0, ca.DM([1.0]), ca.DM.zeros(0), ca.DM([2.0]))
    assert float(xdot) == -2.0
    sensitivity = ca.Function(
        "sensitivity",
        exported.rhs.sx_in(),
        [ca.jacobian(exported.rhs.sx_out(0), exported.rhs.sx_in(3))],
    )
    assert sensitivity.size_out(0) == (1, 1)


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
    xdot = jax.jit(exported.rhs)(
        0.0, jnp.array([1.0]), jnp.zeros(0), jnp.array([2.0])
    )
    assert float(xdot[0]) == -2.0
    sensitivity = jax.jacfwd(exported.rhs, argnums=3)(
        0.0, jnp.array([1.0]), jnp.zeros(0), jnp.array([2.0])
    )
    assert sensitivity.shape == (1, 1)


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


def test_missing_dependency_raises_importerror() -> None:
    checks = [
        ("casadi", lambda model: model.to_casadi()),
        ("jax", lambda model: model.to_jax()),
    ]
    model = _model()
    for dependency, export in checks:
        if _have(dependency):
            continue
        try:
            export(model)
        except ImportError:
            pass
        else:
            raise AssertionError(
                f"{dependency} export should raise ImportError when absent"
            )


def main() -> None:
    test_to_casadi()
    test_to_jax()
    test_magic_export_returns_checked_solve_object()
    test_missing_dependency_raises_importerror()
    print("export_test: OK")


if __name__ == "__main__":
    main()
