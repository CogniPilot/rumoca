"""Smoke test for the first-class typed Rumoca Python API.

Run directly (`python tests/smoke_test.py`) against an installed / `maturin
develop` build. Exercises the happy path end-to-end: load a model, inspect its
typed metadata, simulate, and generate code — with no `json.loads` anywhere.
"""

from pathlib import Path

import rumoca as rm

FIXTURE_ROOT = Path(__file__).resolve().parent / "fixtures"
MODEL_FILE = FIXTURE_ROOT / "UsesLib.mo"
SOURCE_ROOT = FIXTURE_ROOT / "Lib"


def test_load_and_metadata() -> None:
    m = rm.Session(roots=[str(SOURCE_ROOT)]).load(MODEL_FILE)
    assert m.name == "UsesLib"
    assert m.states.names == ["x"]
    assert m.parameters.names == ["gain"]

    # Typed parameter metadata, no magic-key dict access.
    gain = m.parameters["gain"]
    assert isinstance(gain, rm.ParameterInfo)
    assert gain.kind in ("tunable", "structural")

    # By-name and by-index variable lookup.
    assert m.states["x"].name == "x"
    assert m.states[0].name == "x"

    struct = m.structure()
    assert struct.n_states == 1
    assert struct.is_balanced


def test_loads_inline() -> None:
    src = "model Decay Real x(start=1); equation der(x) = -x; end Decay;"
    m = rm.Session().loads(src, model="Decay")
    assert m.name == "Decay"
    assert m.states.names == ["x"]


def test_simulate() -> None:
    m = rm.Session(roots=[str(SOURCE_ROOT)]).load(MODEL_FILE)
    r = m.simulate(t=(0.0, 0.2), dt=0.1)
    assert isinstance(r, rm.Result)
    assert r.model == "UsesLib"
    assert "x" in r
    x = r["x"]
    assert len(list(x)) == len(list(r.time)) >= 2
    assert r.metrics["points"] >= 2


def test_codegen() -> None:
    m = rm.Session(roots=[str(SOURCE_ROOT)]).load(MODEL_FILE)
    cg = m.codegen("sympy")
    assert cg.target == "sympy"
    assert any(p.endswith(".py") for p in cg.paths)
    # Iterable of (path, content).
    for path, content in cg:
        assert isinstance(path, str) and isinstance(content, str)


def test_render() -> None:
    m = rm.Session(roots=[str(SOURCE_ROOT)]).load(MODEL_FILE)
    rendered = m.render("dae-modelica")
    assert "UsesLib" in rendered


def test_session_reuse() -> None:
    s = rm.Session(roots=[str(SOURCE_ROOT)])
    m1 = s.load(str(MODEL_FILE))
    m2 = s.load(str(MODEL_FILE))
    assert m1.name == m2.name == "UsesLib"


def test_targets_and_solvers() -> None:
    ids = [t.id for t in rm.targets()]
    assert "sympy" in ids
    sympy = next(t for t in rm.targets() if t.id == "sympy")
    assert sympy.ir in ("ast", "flat", "dae", "solve")
    solver_ids = [s.id for s in rm.solvers()]
    assert "rk-like" in solver_ids


def test_validate() -> None:
    diags = rm.validate_source("model M Real x end M;")
    assert any(d.level == "error" for d in diags)
    clean = rm.validate_source("model M Real x; end M;")
    assert all(d.rule != "syntax-error" for d in clean)


def main() -> None:
    test_load_and_metadata()
    test_loads_inline()
    test_simulate()
    test_codegen()
    test_render()
    test_session_reuse()
    test_targets_and_solvers()
    test_validate()
    print("smoke_test: OK")


if __name__ == "__main__":
    main()
