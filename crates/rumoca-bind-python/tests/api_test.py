"""API contract tests for the first-class typed surface.

Locks the public namespace shape (and the *absence* of the old JSON-soup names),
typed returns, and typed error hierarchy. Run directly or under pytest.
"""

from pathlib import Path

import rumoca as rm

FIXTURE_ROOT = Path(__file__).resolve().parent / "fixtures"
MODEL_FILE = FIXTURE_ROOT / "UsesLib.mo"
SOURCE_ROOT = FIXTURE_ROOT / "Lib"

EXPECTED_PUBLIC = {
    "validate",
    "validate_source",
    "format",
    "version",
    "targets",
    "solvers",
    "Model",
    "Session",
    "SimConfig",
    "VarView",
    "ParamView",
    "VariableInfo",
    "ParameterInfo",
    "StructuralInfo",
    "Result",
    "ScenarioResult",
    "GradientResult",
    "CodegenResult",
    "GeneratedFile",
    "Target",
    "SolverInfo",
    "CasadiModel",
    "JaxModel",
    "SympyModel",
    "SolveExport",
    "Diagnostic",
    "RumocaError",
    "ParseError",
    "CompileError",
    "SimulationError",
    "StructuralParamError",
    "load_ipython_extension",
    "unload_ipython_extension",
}

# Names that must NOT be public anymore, including old JSON-returning helpers
# and convenience functions that created throwaway compiler sessions.
REMOVED_NAMES = {
    "load",
    "loads",
    "run_scenario",
    "codegen_file",
    "compile",
    "compile_source",
    "compile_file",
    "compile_to_json",
    "compile_file_to_json",
    "simulate",
    "simulate_file",
    "render_model",
    "render_model_file",
    "render_target_model",
    "render_target_file",
    "cli",
    "parse",
    "lint",
    "check",
    "get_builtin_targets",
    "Project",
    "ProjectSession",
    "SimulationOptions",
    "ParseResult",
    "LintMessage",
}


def _load_fixture() -> rm.Model:
    return rm.Session(roots=[str(SOURCE_ROOT)]).load(MODEL_FILE)


def _loads(source: str, *, model: str) -> rm.Model:
    return rm.Session().loads(source, model=model)


def test_public_namespace_is_clean() -> None:
    assert set(rm.__all__) == EXPECTED_PUBLIC
    public = {n for n in dir(rm) if not n.startswith("_")}
    leaked = public & REMOVED_NAMES
    assert not leaked, f"removed public names leaked: {sorted(leaked)}"


def test_no_removed_attributes() -> None:
    for name in REMOVED_NAMES:
        assert not hasattr(rm, name), f"removed attribute still present: {name}"


def test_typed_returns_not_json() -> None:
    m = _load_fixture()
    assert isinstance(m, rm.Model)
    assert isinstance(m.parameters["gain"], rm.ParameterInfo)
    assert isinstance(m.parameters["gain"].value, float)
    r = m.simulate(t=0.2, dt=0.1)
    assert isinstance(r, rm.Result)
    # to_json/to_dict are the only JSON, and only on request.
    assert isinstance(m.to_json(), str)
    assert isinstance(r.to_dict(), dict)


def test_error_hierarchy() -> None:
    assert issubclass(rm.CompileError, rm.RumocaError)
    assert issubclass(rm.StructuralParamError, rm.RumocaError)
    # A bad model raises a typed CompileError, not a bare RuntimeError.
    try:
        _loads("model Bad Real x = ; end Bad;", model="Bad")
    except rm.RumocaError:
        pass
    except Exception as error:  # noqa: BLE001
        raise AssertionError(f"expected RumocaError, got {type(error).__name__}") from error
    else:
        raise AssertionError("malformed model should raise")


DECAY = "model Decay parameter Real k=1.0; Real x(start=1); equation der(x)=-k*x; end Decay;"

DEPS = (
    "model M\n"
    "  parameter Integer n = 2 annotation(Evaluate=true);\n"
    "  parameter Real a = 1.0;\n"
    "  parameter Real b = 2*a;\n"
    "  parameter Real k = 1.0;\n"
    "  Real x[n](each start=1);\n"
    "equation\n"
    "  for i in 1:n loop der(x[i]) = -k*x[i] + b; end for;\n"
    "end M;\n"
)


def test_tunable_param_override_changes_trajectory() -> None:
    import math

    m = _loads(DECAY, model="Decay")
    base = m.simulate(t=1.0)["x"][-1]
    over = m.simulate(t=1.0, params={"k": 4.0})["x"][-1]
    assert abs(base - math.exp(-1)) < 1e-3
    assert abs(over - math.exp(-4)) < 1e-3


def test_cheap_sweep_compiles_once() -> None:
    # with_params shares the compiled artifact; each handle simulates from it.
    m = _loads(DECAY, model="Decay")
    results = [m.with_params(k=k).simulate(t=1.0)["x"][-1] for k in (0.5, 2.0, 8.0)]
    assert results[0] > results[1] > results[2]  # faster decay with larger k


def test_start_override() -> None:
    import math

    m = _loads(DECAY, model="Decay")
    out = m.with_start(x=3.0).simulate(t=1.0)["x"][-1]
    assert abs(out - 3.0 * math.exp(-1)) < 1e-3


def test_overrides_are_solver_independent() -> None:
    # Overrides apply in the solver-neutral lowering funnel, so the explicit
    # rk45 backend honors them just like the default (diffsol) path.
    import math

    m = _loads(DECAY, model="Decay")
    out = m.simulate(t=1.0, params={"k": 3.0}, config=rm.SimConfig(solver="rk-like"))["x"][-1]
    assert abs(out - math.exp(-3)) < 1e-2


def test_unknown_start_override_is_keyerror() -> None:
    m = _loads(DECAY, model="Decay")
    try:
        m.simulate(t=0.1, start={"nope": 1.0})
    except KeyError:
        pass
    else:
        raise AssertionError("unknown start name should raise KeyError")


def test_override_rejections_are_typed() -> None:
    m = _loads(DEPS, model="M")
    assert m.parameters["n"].kind == "structural"
    assert m.parameters["k"].kind == "tunable"

    def rejects(call, exc):
        try:
            call()
        except exc:
            return True
        except Exception as error:  # noqa: BLE001
            raise AssertionError(f"wrong error type {type(error).__name__}") from error
        raise AssertionError("expected rejection")

    # structural parameter -> StructuralParamError (recompile needed)
    assert rejects(lambda: m.simulate(t=0.1, params={"n": 3.0}), rm.StructuralParamError)
    # unknown name -> KeyError
    assert rejects(lambda: m.simulate(t=0.1, params={"zzz": 1.0}), KeyError)
    # a depended-upon leaf (b = 2*a) now PROPAGATES instead of being rejected;
    # overriding the dependent leaf `b` directly is also fine.
    m.simulate(t=0.1, params={"a": 5.0})
    m.simulate(t=0.1, params={"b": 9.0})


def test_dependent_param_propagation() -> None:
    # `b = 2*a` is folded at lowering; overriding `a` must re-derive `b` rather
    # than run with the stale value. der(x)=b so x(1)=b.
    src = (
        "model Dep\n"
        "  parameter Real a = 1.0;\n"
        "  parameter Real b = 2*a;\n"
        "  Real x(start=0);\n"
        "equation\n"
        "  der(x) = b;\n"
        "end Dep;\n"
    )
    m = _loads(src, model="Dep")
    assert abs(m.simulate(t=1.0, dt=0.5)["x"][-1] - 2.0) < 1e-6
    assert abs(m.simulate(t=1.0, dt=0.5, params={"a": 5.0})["x"][-1] - 10.0) < 1e-6
    # A chain c = 3*b = 6*a propagates transitively.
    chain = src.replace(
        "  parameter Real b = 2*a;\n",
        "  parameter Real b = 2*a;\n  parameter Real c = 3*b;\n",
    ).replace("der(x) = b;", "der(x) = c;").replace("model Dep", "model Chain").replace(
        "end Dep;", "end Chain;"
    )
    mc = _loads(chain, model="Chain")
    assert abs(mc.simulate(t=1.0, dt=0.5, params={"a": 2.0})["x"][-1] - 12.0) < 1e-6


def test_array_dependent_param_propagation() -> None:
    # Array-valued dependent parameters are re-derived during override-aware
    # lowering. This is required for models such as the airfoil mask, where a
    # scalar input/parameter changes a whole parameter field.
    src = (
        "model Arr\n"
        "  parameter Real a = 1.0;\n"
        "  parameter Real arr[3] = {a, 2*a, 3*a};\n"
        "  Real x[3](each start=0);\n"
        "equation\n"
        "  for i in 1:3 loop der(x[i]) = arr[i]; end for;\n"
        "end Arr;\n"
    )
    m = _loads(src, model="Arr")
    base = m.simulate(t=0.5, dt=0.5)
    over = m.simulate(t=0.5, dt=0.5, params={"a": 10.0})
    for i, (base_expected, over_expected) in enumerate(
        [(0.5, 5.0), (1.0, 10.0), (1.5, 15.0)],
        start=1,
    ):
        assert abs(base[f"x[{i}]"][-1] - base_expected) < 1e-6
        assert abs(over[f"x[{i}]"][-1] - over_expected) < 1e-6


def test_structural_recompile() -> None:
    # `with_params(recompile=True)` re-instantiates: an Integer dimension param
    # changes the array size (§9 DoD). The original handle is untouched.
    m = _loads(DEPS, model="M")
    assert m.states["x"].dims == [2]
    m4 = m.with_params(n=4, recompile=True)
    assert m4.states["x"].dims == [4]
    assert m.states["x"].dims == [2]  # original unchanged
    # The recompiled model simulates and still accepts tunable overrides.
    r = m4.with_params(k=2.0).simulate(t=0.2, dt=0.1)
    assert len([n for n in r.names if n.startswith("x[")]) == 4


def test_structural_recompile_boolean() -> None:
    # A Boolean structural override (the `bool`-before-`int` path) flips a folded
    # `if` branch on re-instantiation: x decays when off, grows when on.
    src = (
        "model Gate\n"
        "  parameter Boolean on = false annotation(Evaluate=true);\n"
        "  Real x(start=1);\n"
        "equation\n"
        "  der(x) = if on then 1.0 else -x;\n"
        "end Gate;\n"
    )
    m = _loads(src, model="Gate")
    assert m.parameters["on"].kind == "structural"
    off = m.simulate(t=0.5, dt=0.25)["x"][-1]
    on = m.with_params(on=True, recompile=True).simulate(t=0.5, dt=0.25)["x"][-1]
    assert off < 1.0 < on


# der(x) = -k*x + u  ⇒  steady state x_ss = u/k, with the known analytic gradient
#   d(x_ss)/dk = -u/k²,   d(x_ss)/du = 1/k.
# At k=2, u=3: x_ss = 1.5, d/dk = -0.75, d/du = 0.5.
STEADY = (
    "model Steady\n"
    "  parameter Real k = 2.0;\n"
    "  parameter Real u = 3.0;\n"
    "  Real x(start=0);\n"
    "equation\n"
    "  der(x) = -k*x + u;\n"
    "end Steady;\n"
)


def test_objective_gradient_forward_matches_analytic() -> None:
    m = _loads(STEADY, model="Steady")
    grad = m.objective_gradient("x", state={"x": 1.5})
    assert isinstance(grad, rm.GradientResult)
    assert grad.objective == "x"
    assert grad.mode == "forward"
    assert set(grad.names) == {"k", "u"}
    assert abs(grad["k"] - (-0.75)) < 1e-6
    assert abs(grad["u"] - 0.5) < 1e-6


def test_objective_gradient_adjoint_matches_forward() -> None:
    m = _loads(STEADY, model="Steady")
    fwd = m.objective_gradient("x", state={"x": 1.5}, mode="forward")
    adj = m.objective_gradient("x", state={"x": 1.5}, mode="adjoint")
    assert adj.mode == "adjoint"
    for name in fwd.names:
        assert abs(fwd[name] - adj[name]) < 1e-6
    # "reverse" is an accepted alias for the adjoint mode.
    rev = m.objective_gradient("x", state={"x": 1.5}, mode="reverse")
    assert abs(rev["k"] - adj["k"]) < 1e-9


def test_objective_gradient_typed_views() -> None:
    m = _loads(STEADY, model="Steady")
    grad = m.objective_gradient("x", state={"x": 1.5})
    # dict view and membership.
    d = grad.to_dict()
    assert isinstance(d, dict) and abs(d["u"] - 0.5) < 1e-6
    assert "k" in grad and "nope" not in grad
    assert len(grad) == 2
    # numpy/list vector in `names` order (numpy optional; list fallback otherwise).
    vec = grad.to_numpy()
    assert [round(float(v), 6) for v in list(vec)] == [
        round(grad[n], 6) for n in grad.names
    ]


def test_objective_gradient_honors_param_override() -> None:
    # with_params linearizes the gradient at the overridden value: at k=4, u=3 the
    # steady state is 0.75 and d(x_ss)/dk = -u/k² = -3/16.
    m = _loads(STEADY, model="Steady").with_params(k=4.0)
    grad = m.objective_gradient("x", state={"x": 0.75})
    assert abs(grad["k"] - (-3.0 / 16.0)) < 1e-6
    assert abs(grad["u"] - 0.25) < 1e-6


def test_objective_gradient_errors_are_typed() -> None:
    m = _loads(STEADY, model="Steady")
    # An objective that is not a solver-y variable surfaces as SimulationError,
    # never an empty/zero gradient.
    try:
        m.objective_gradient("does_not_exist", state={"x": 1.5})
    except rm.RumocaError:
        pass
    else:
        raise AssertionError("unknown objective should raise")
    # An unknown mode is rejected up front.
    try:
        m.objective_gradient("x", mode="sideways")
    except rm.RumocaError:
        pass
    else:
        raise AssertionError("unknown mode should raise")


def test_session_from_scenario() -> None:
    # One call reads roots/model/solver from a rumoca-scenario.toml and returns
    # a ready (session, model, config) triple, paths resolved relative to it.
    scenario = FIXTURE_ROOT / "scenario.toml"
    session, model, config = rm.Session.from_scenario(scenario)
    assert isinstance(session, rm.Session)
    assert isinstance(model, rm.Model) and model.name == "UsesLib"
    assert isinstance(config, rm.SimConfig)
    assert config.solver == "auto" and config.dt == 0.1
    r = model.simulate(t=0.2, config=config)
    assert r.model == "UsesLib" and len(r) >= 2
    # The scenario-loaded model is sweepable like any other.
    model.with_params(gain=4.0).simulate(t=0.2, config=config)


def test_run_scenario_batch_with_output_override() -> None:
    import tempfile

    scenario = FIXTURE_ROOT / "scenario.toml"
    session = rm.Session(roots=[str(SOURCE_ROOT)])
    with tempfile.TemporaryDirectory() as tmp:
        output = Path(tmp) / "scenario.csv"
        run = session.run_scenario(scenario, overrides={"output": output})
        assert isinstance(run, rm.ScenarioResult)
        assert run.task == "simulate"
        assert run.status == "completed"
        assert run.model == "UsesLib"
        assert run.schedule in ("as_fast_as_possible", "realtime", "lockstep")
        assert output.exists()
        assert str(output) in run.output_paths
        assert isinstance(run.result, rm.Result)
        assert run.result.model == "UsesLib"
        assert run.codegen is None


def test_session_run_scenario_reuses_session_surface() -> None:
    import tempfile

    scenario = FIXTURE_ROOT / "scenario.toml"
    session = rm.Session(roots=[str(SOURCE_ROOT)])
    with tempfile.TemporaryDirectory() as tmp:
        output = Path(tmp) / "session-scenario.csv"
        run = session.run_scenario(scenario, overrides={"output": output})
        assert isinstance(run, rm.ScenarioResult)
        assert run.task == "simulate"
        assert output.exists()


def test_codegen_file_helper() -> None:
    import tempfile

    with tempfile.TemporaryDirectory() as tmp:
        written = rm.Session(roots=[str(SOURCE_ROOT)]).codegen_file(
            MODEL_FILE,
            "UsesLib",
            "sympy",
            tmp,
        )
        assert written
        assert any(path.endswith(".py") for path in written)
        for path in written:
            assert Path(path).exists()


def test_run_codegen_scenario() -> None:
    import tempfile

    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        scenario = root / "rumoca-scenario.codegen.toml"
        scenario.write_text(
            "\n".join(
                [
                    f'source_roots = ["{SOURCE_ROOT}"]',
                    "",
                    "[rumoca]",
                    'version = "1"',
                    'task = "codegen"',
                    "",
                    "[model]",
                    f'file = "{MODEL_FILE}"',
                    'name = "UsesLib"',
                    "",
                    "[codegen]",
                    'target = "sympy"',
                    'output_dir = "generated"',
                    "",
                ]
            ),
            encoding="utf-8",
        )
        run = rm.Session().run_scenario(scenario)
        assert isinstance(run, rm.ScenarioResult)
        assert run.task == "codegen"
        assert run.result is None
        assert isinstance(run.codegen, rm.CodegenResult)
        assert run.output_paths
        assert all(Path(path).exists() for path in run.output_paths)


def test_structure_blt() -> None:
    # A plain ODE is one sequential block with no algebraic loops.
    m = _loads(DECAY, model="Decay")
    s = m.structure()
    assert s.is_matched and s.n_blocks >= 1
    assert s.n_algebraic_loops == 0 and s.largest_algebraic_loop == 0

    # A coupled algebraic pair forms one algebraic-loop block.
    loop = (
        "model Loop\n"
        "  Real x(start=1); Real a; Real b;\n"
        "equation\n"
        "  der(x) = -a;\n"
        "  a + b = 3;\n"
        "  a - b = x;\n"
        "end Loop;\n"
    )
    sl = _loads(loop, model="Loop").structure()
    assert sl.n_algebraic_loops == 1
    assert sl.largest_algebraic_loop >= 2

    # Balanced but structurally singular (a is determined twice, b never): no
    # full matching, so the BLT can't be built — reported honestly, not crashed.
    singular = _loads("model S Real a; Real b; equation a=1; a=2; end S;", model="S")
    ss = singular.structure()
    assert not ss.is_matched
    assert ss.n_blocks == 0 and ss.n_algebraic_loops == 0


def test_errors_teach_did_you_mean() -> None:
    # Unknown names suggest the closest match (§1 "errors that teach").
    m = _loads(DECAY, model="Decay")
    try:
        m.parameters["kk"]  # typo for "k"
    except KeyError as error:
        assert "did you mean" in str(error) and "k" in str(error)
    else:
        raise AssertionError("expected KeyError for typo'd parameter")

    try:
        m.states["xx"]  # VarView typo for "x"
    except KeyError as error:
        assert "did you mean" in str(error) and '"x"' in str(error)
    else:
        raise AssertionError("expected KeyError for typo'd state")

    try:
        m.with_start(xx=1.0)  # start-override typo for "x"
    except KeyError as error:
        assert "did you mean" in str(error)
    else:
        raise AssertionError("expected KeyError for typo'd start name")

    r = m.simulate(t=0.2)
    try:
        r["xx"]  # typo for "x"
    except KeyError as error:
        assert "did you mean" in str(error)
    else:
        raise AssertionError("expected KeyError for typo'd result variable")


def test_subscripted_lookup_falls_back_to_base() -> None:
    # Scalar `x` resolves both exactly and via a subscripted spelling.
    m = _load_fixture()
    assert m.states["x"].name == "x"
    assert "x" in m.states


def main() -> None:
    test_public_namespace_is_clean()
    test_no_removed_attributes()
    test_typed_returns_not_json()
    test_error_hierarchy()
    test_tunable_param_override_changes_trajectory()
    test_cheap_sweep_compiles_once()
    test_start_override()
    test_overrides_are_solver_independent()
    test_unknown_start_override_is_keyerror()
    test_override_rejections_are_typed()
    test_dependent_param_propagation()
    test_array_dependent_param_propagation()
    test_structural_recompile()
    test_structural_recompile_boolean()
    test_session_from_scenario()
    test_run_scenario_batch_with_output_override()
    test_session_run_scenario_reuses_session_surface()
    test_codegen_file_helper()
    test_run_codegen_scenario()
    test_structure_blt()
    test_errors_teach_did_you_mean()
    test_subscripted_lookup_falls_back_to_base()
    print("api_test: OK")


if __name__ == "__main__":
    main()
