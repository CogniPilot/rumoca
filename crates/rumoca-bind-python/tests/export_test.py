"""Live symbolic export tests (roadmap P4).

Each export needs an optional dependency (casadi / sympy / jax); the test skips
gracefully when it is absent, so this runs everywhere while still exercising the
real path wherever the dep is installed.
"""

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


def test_to_sympy() -> None:
    if not _have("sympy"):
        print("  (sympy not installed; skipping)")
        return
    sm = _model().to_sympy()
    assert isinstance(sm, rm.SympyModel)
    sol = sm.solve_explicit()
    # der(x) = -gain*x  ->  x_dot solved symbolically.
    assert any("gain" in str(v) for v in sol.values())


def test_to_casadi_native_dae() -> None:
    if not _have("casadi"):
        print("  (casadi not installed; skipping)")
        return
    import casadi as ca

    cm = _model().to_casadi()
    assert isinstance(cm, rm.CasadiModel)
    assert cm.state_names == ["x"]
    assert cm.parameter_names == ["gain"]

    # CasADi's native semi-explicit DAE dict is exposed and integrator-ready.
    assert cm.dae is not None
    assert "ode" in cm.dae and "x" in cm.dae

    # jacobian("ode", "p") is the true dynamics sensitivity d(dx/dt)/d(gain).
    sens = cm.jacobian("ode", "p")
    assert sens.shape == (1, 1)

    # Integrate straight off the native dict and check the analytic decay.
    F = ca.integrator("F", "cvodes", cm.dae, 0, 1.0)
    xf = float(F(x0=cm.x0, p=ca.vertcat(cm.p0))["xf"])
    assert abs(xf - 1.0 * 2.718281828459045 ** (-2.0)) < 1e-4


def test_to_casadi_solve() -> None:
    if not _have("casadi"):
        print("  (casadi not installed; skipping)")
        return
    import casadi as ca

    s = _model().to_casadi(form="solve")
    assert isinstance(s, rm.SolveExport)
    assert s.target == "casadi-solve"
    assert s.state_names == ["x"]
    assert s.parameter_names == ["gain"]
    # rhs is a differentiable ca.Function: xdot = rhs(x, u, p).
    assert isinstance(s.rhs, ca.Function)
    xdot = s.rhs(ca.DM([1.0]), ca.DM.zeros(0), ca.DM([2.0]))
    assert float(xdot) == -2.0  # der(x) = -gain*x at x=1, gain=2


def test_to_jax() -> None:
    if not _have("jax") or not _have("diffrax"):
        print("  (jax/diffrax not installed; skipping)")
        return
    jm = _model().to_jax()
    assert isinstance(jm, rm.JaxModel)
    assert jm.state_names == ["x"]
    assert jm.parameter_names == ["gain"]


def test_magic_export_returns_live_object() -> None:
    if not _have("sympy"):
        print("  (sympy not installed; skipping magic live-export)")
        return
    from rumoca._magic import run_modelica_cell

    ns: dict = {}
    src = "model Decay Real x(start=1); equation der(x)=-x; end Decay;"
    out = run_modelica_cell(ns, "export sympy -m Decay --name sm", src)
    assert isinstance(out, rm.SympyModel)
    assert ns["sm"] is out


def test_missing_dep_raises_importerror() -> None:
    # Whichever export deps are absent must raise a clean ImportError naming the
    # install, never a NameError/AttributeError from a half-built object.
    checks = [("casadi", lambda m: m.to_casadi()), ("sympy", lambda m: m.to_sympy())]
    m = _model()
    for dep, call in checks:
        if _have(dep):
            continue
        try:
            call(m)
        except ImportError:
            pass
        else:
            raise AssertionError(f"{dep} export should raise ImportError when {dep} is absent")


def main() -> None:
    test_to_sympy()
    test_to_casadi_native_dae()
    test_to_casadi_solve()
    test_to_jax()
    test_magic_export_returns_live_object()
    test_missing_dep_raises_importerror()
    print("export_test: OK")


if __name__ == "__main__":
    main()
