"""Execute the Python guide's repository examples through an installed wheel.

Run from any directory with the data, plot, notebook, JAX and CasADi extras.
The companion scenario test also executes the guide's CMake build steps.
"""

from contextlib import chdir
from pathlib import Path
import re
from tempfile import TemporaryDirectory

import casadi as ca
import jax
import jax.numpy as jnp
import matplotlib
import numpy as np
import rumoca as rm

from guide_fixture import stage_examples

REPOSITORY = Path(__file__).resolve().parents[3]
GUIDE = REPOSITORY / "docs/user-guide/src/tools/python.md"


def assert_decay(result, name, rate, end):
    times = np.asarray(result.time)
    values = np.asarray(result[name])
    assert times.ndim == 1 and values.shape == times.shape
    assert len(times) >= 2 and times[0] == 0.0 and times[-1] == end
    assert np.all(np.isfinite(times)) and np.all(np.diff(times) >= 0)
    assert np.all(np.isfinite(values))
    np.testing.assert_allclose(values, np.exp(-rate * times), rtol=0, atol=1e-3)


def assert_live_exports(namespace):
    model = namespace["m"]
    assert model.parameters["k"].value == 0.5
    assert model.parameters["k"].kind == "tunable"
    assert model.states.names == ["x"]
    assert isinstance(model.structure(), rm.StructuralInfo)
    assert isinstance(model.to_dict("dae"), dict)
    assert isinstance(model.to_json("dae"), str)
    for name in ("jax_model", "casadi_model"):
        exported = namespace[name]
        assert isinstance(exported, rm.SolveExport)
        assert exported.state_names == ["x"] and exported.parameter_names == ["k"]
        assert exported.default_states == [1.0] and exported.default_parameters == [0.5]
    exported = namespace["jax_model"]
    for state, rate in [(1.0, 0.5), (3.0, 2.0)]:
        inputs = (0.0, jnp.array([state]), jnp.zeros(0), jnp.array([rate]))
        assert float(jax.jit(exported.rhs)(*inputs)[0]) == -rate * state
        for differentiate in (jax.jacfwd, jax.jacrev):
            dx, dp = differentiate(exported.rhs, argnums=(1, 3))(*inputs)
            assert float(dx[0, 0]) == -rate and float(dp[0, 0]) == -state
    exported = namespace["casadi_model"]
    arguments = exported.rhs.sx_in()
    expression = exported.rhs(*arguments)
    jacobian = ca.Function("guide_jacobian", arguments, [
        ca.jacobian(expression, arguments[1]),
        ca.jacobian(expression, arguments[3]),
    ])
    for state, rate in [(1.0, 0.5), (3.0, 2.0)]:
        inputs = (0.0, ca.DM([state]), ca.DM.zeros(0), ca.DM([rate]))
        assert float(exported.rhs(*inputs)) == -rate * state
        dx, dp = jacobian(*inputs)
        assert float(dx) == -rate and float(dp) == -state


def test_guide_python_fences():
    matplotlib.use("Agg")
    text = GUIDE.read_text(encoding="utf-8")
    fences = re.findall(r"```python\n(.*?)\n```", text, flags=re.DOTALL)
    assert len(fences) == 10, "update the guide inventory when its contexts change"
    namespace = {"__name__": "rumoca_python_guide"}
    with TemporaryDirectory(prefix="rumoca-python-guide-") as temporary:
        workspace = Path(temporary)
        # Preserve every authored model/scenario byte and its relative paths.
        stage_examples(workspace)
        with chdir(workspace):
            for index, source in enumerate(fences):
                exec(compile(source, f"{GUIDE}:python-fence-{index + 1}", "exec"), namespace)
                if index == 0:
                    assert_decay(namespace["r"], "x", 0.5, 10.0)
                    assert len(namespace["r"].time) == 1001
                    assert namespace["df"].index.name == "time"
                    assert list(namespace["df"].columns) == ["x"]
                if index in (1, 2):
                    result = namespace["r"] if index == 1 else namespace["run"].result
                    assert_decay(result, "x", 0.5, 10.0)
                if index == 3:
                    run = namespace["run"]
                    assert run.status == "completed" and run.codegen is None
                    assert_decay(run.result, "x", 0.5, 2.0)
                    assert Path("results.csv").is_file()
                    assert all(Path(path).is_file() for path in run.output_paths)
                if index == 4:
                    assert_decay(namespace["r"], "x", 0.1, 10.0)
            assert_live_exports(namespace)
            assert namespace["array_model"].states["x"].dims == [2]
            assert namespace["m3"].states["x"].dims == [3]
            assert namespace["m3"].states["x[3]"].dims == [3]
            assert namespace["r3"].names == ["x[1]", "x[2]", "x[3]"]
            for name in namespace["r3"].names:
                assert_decay(namespace["r3"], name, 0.5, 1.0)
            assert list(Path("out/casadi").glob("*.py"))
            written = namespace["written"]
            assert written and all(Path(path).is_file() for path in written)
            assert any(Path(path).suffix == ".alg" for path in written)
            assert namespace["diagnostics"] == []
            assert namespace["invalid"] and all(
                isinstance(item, rm.Diagnostic) for item in namespace["invalid"]
            )
    print("guide_test: 10 exact Python fences, analytic traces, metadata, JAX/CasADi AD, files OK")


def test_guide_notebook_cell():
    from IPython.core.interactiveshell import InteractiveShell

    text = GUIDE.read_text(encoding="utf-8")
    cells = re.findall(r"```text\n(%%modelica.*?)\n```", text, flags=re.DOTALL)
    assert len(cells) == 1
    header, source = cells[0].split("\n", 1)
    shell = InteractiveShell()
    rm.load_ipython_extension(shell)
    model = shell.run_cell_magic("modelica", header.removeprefix("%%modelica "), source)
    assert isinstance(model, rm.Model) and shell.user_ns["m"] is model
    assert_decay(model.simulate(t=1.0, dt=0.1), "x", 1.0, 1.0)
    exported = shell.run_cell_magic("modelica", "export casadi -m Decay --name exported", source)
    assert isinstance(exported, rm.SolveExport) and shell.user_ns["exported"] is exported
    assert float(exported.rhs(0.0, ca.DM([2.0]), ca.DM.zeros(0), ca.DM.zeros(0))) == -2.0
    print("guide_test: actual notebook Model and SolveExport OK")


if __name__ == "__main__":
    test_guide_python_fences()
    test_guide_notebook_cell()
