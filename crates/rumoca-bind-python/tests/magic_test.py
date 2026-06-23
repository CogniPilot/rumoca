"""Smoke test for the `%%modelica` Jupyter cell magic (typed API).

Run directly (`python tests/magic_test.py`) against an installed / `maturin
develop` build. The magic compiles the cell to a typed object — a `Model` by
default, a `Result` for `sim`, a `CodegenResult` for `export`. The IPython
portion is skipped when IPython is unavailable so the core dispatch still runs.
"""

import rumoca as rm
from rumoca._magic import ModelicaMagicError, run_modelica_cell

BALL = "model Ball\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Ball;\n"


def test_core_compile_returns_model() -> None:
    ns: dict = {}
    out = run_modelica_cell(ns, "-m Ball --name m", BALL)
    assert isinstance(out, rm.Model)
    assert out.name == "Ball"
    assert ns["m"] is out


def test_core_sim_returns_result() -> None:
    ns: dict = {}
    out = run_modelica_cell(ns, "sim -m Ball --t-end 1 --dt 0.5", BALL)
    assert isinstance(out, rm.Result)
    assert out.model == "Ball"
    assert "x" in out


def test_core_export_returns_codegen() -> None:
    # A non-symbolic target returns generated files (no optional deps needed);
    # `export casadi|jax|sympy` instead return live objects (see export_test).
    ns: dict = {}
    out = run_modelica_cell(ns, "export dae-modelica -m Ball", BALL)
    assert isinstance(out, rm.CodegenResult)
    assert out.target == "dae-modelica"


def test_core_quiet_returns_none_but_binds() -> None:
    ns: dict = {}
    out = run_modelica_cell(ns, "-m Ball --name m --quiet", BALL)
    assert out is None
    assert isinstance(ns["m"], rm.Model)


def test_core_bad_flag_errors() -> None:
    try:
        run_modelica_cell({}, "-m Ball --nope", BALL)
    except ModelicaMagicError as error:
        assert "nope" in str(error)
    else:
        raise AssertionError("unknown flag should raise")


def _ipython_shell():
    try:
        from IPython.core.interactiveshell import InteractiveShell
    except ModuleNotFoundError:
        return None
    shell = InteractiveShell.instance()
    rm.load_ipython_extension(shell)
    return shell


def test_magic_paths() -> None:
    ip = _ipython_shell()
    if ip is None:
        print("  (IPython not installed; skipping %%modelica magic checks)")
        return

    out = ip.run_cell_magic("modelica", "-m Ball --name m", BALL)
    assert isinstance(out, rm.Model) and ip.user_ns["m"] is out

    sim = ip.run_cell_magic("modelica", "sim -m Ball --t-end 1", BALL)
    assert isinstance(sim, rm.Result) and sim.model == "Ball"

    from IPython.core.error import UsageError

    try:
        ip.run_cell_magic("modelica", "-m Ball --nope", BALL)
    except UsageError:
        pass
    else:
        raise AssertionError("bad flag should raise UsageError")


def main() -> None:
    test_core_compile_returns_model()
    test_core_sim_returns_result()
    test_core_export_returns_codegen()
    test_core_quiet_returns_none_but_binds()
    test_core_bad_flag_errors()
    test_magic_paths()
    print("magic_test: OK")


if __name__ == "__main__":
    main()
