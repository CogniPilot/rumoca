"""Required installed-wheel checks for guide-linked scenarios and CMake fences."""

import ctypes
import importlib.util
from pathlib import Path
import re
import shlex
import subprocess
import sys
from tempfile import TemporaryDirectory
import tomllib

import jax
import jax.numpy as jnp
import rumoca as rm

from guide_fixture import stage_examples

REPOSITORY = Path(__file__).resolve().parents[3]
SCENARIOS = (
    ("jax_ode", ["SympyDecay_jax_ode.py"]),
    ("c_ode", ["SympyDecay_ode.h", "SympyDecay_ode.c"]),
    ("checked_dae_report", ["SympyDecay_checked_dae.txt"]),
    ("custom_checked_variables", ["custom_checked_variables"]),
)


def assert_generated_jax(directory):
    path = directory / "SympyDecay_jax_ode.py"
    spec = importlib.util.spec_from_file_location("guide_generated_sympy", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    assert module.STATE_NAMES == ["x"]
    assert module.init_params(module.DEFAULT_P) == [0.5]
    assert module.init_state(module.DEFAULT_P) == [1.0]
    for state, rate in [(1.0, 0.5), (3.0, 2.0)]:
        arguments = (0.0, jnp.array([state]), jnp.zeros(0), jnp.array([rate]))
        assert float(jax.jit(module.rhs)(*arguments)[0]) == -rate * state
        for differentiate in (jax.jacfwd, jax.jacrev):
            dx, dp = differentiate(module.rhs, argnums=(1, 3))(*arguments)
            assert float(dx[0, 0]) == -rate and float(dp[0, 0]) == -state


def assert_generated_c(directory):
    library = directory / "sympy.so"
    # Honor the compiler command supplied by the build environment.
    import os

    command = shlex.split(os.environ.get("CC", "cc"))
    subprocess.run(command + ["-shared", "-fPIC", "-O2", "-std=c11", "-Wall", "-Wextra",
                              "-Werror", str(directory / "SympyDecay_ode.c"), "-lm",
                              "-o", str(library)], check=True, timeout=120)
    compiled = ctypes.CDLL(str(library))
    pointer = ctypes.POINTER(ctypes.c_double)
    compiled.SympyDecay_init_params.argtypes = [pointer]
    compiled.SympyDecay_init_state.argtypes = [pointer, pointer]
    compiled.SympyDecay_derivative_rhs.argtypes = [ctypes.c_double, pointer, pointer, pointer]
    assert list((ctypes.c_double * 1).in_dll(compiled, "SympyDecay_default_p")) == [0.5]
    for state, rate in [(1.0, 0.5), (3.0, 2.0)]:
        y, p, out = [(ctypes.c_double * 1)(value) for value in (state, rate, 987.0)]
        assert compiled.SympyDecay_init_params(p) == 0
        assert compiled.SympyDecay_init_state(p, y) == 0
        assert list(y) == [state]
        assert compiled.SympyDecay_derivative_rhs(0.0, y, p, out) == 0
        assert list(out) == [-rate * state]


def test_authored_codegen_scenarios():
    guide = (REPOSITORY / "docs/user-guide/src/codegen/targets.md").read_text(encoding="utf-8")
    linked = set(re.findall(r"rumoca-scenario\.sympy_decay_[a-z_]+\.toml", guide))
    assert linked == {f"rumoca-scenario.sympy_decay_{profile}.toml" for profile, _ in SCENARIOS}
    failures = []
    session = rm.Session()
    with TemporaryDirectory(prefix="rumoca-guide-scenarios-") as temporary:
        for profile, paths in SCENARIOS:
            scenario = REPOSITORY / f"examples/codegen/rumoca-scenario.sympy_decay_{profile}.toml"
            directory = Path(temporary) / profile
            try:
                config = tomllib.loads(scenario.read_text(encoding="utf-8"))
                run = session.run_scenario(scenario, overrides={"output_dir": directory})
                assert run.task == "codegen" and run.status == "completed"
                assert run.model == "SympyDecay" and run.result is None
                assert run.codegen.target == config["codegen"]["target"]
                assert run.codegen.paths == paths
                assert set(run.output_paths) == {str(directory / path) for path in paths}
                for generated in run.codegen.files:
                    assert (directory / generated.path).read_text(encoding="utf-8") == generated.content
                if profile == "jax_ode":
                    assert_generated_jax(directory)
                elif profile == "c_ode":
                    assert_generated_c(directory)
                else:
                    text = (directory / paths[0]).read_text(encoding="utf-8")
                    assert "state x real" in text and "parameter k real" in text
                    assert "continuous_owners 1" in text
                print(f"guide_scenario_test: {scenario.name} PASS", flush=True)
            except Exception as error:
                failures.append(f"{scenario.name}: {type(error).__name__}: {error}")
                print(f"guide_scenario_test: {failures[-1]}", flush=True)
    assert not failures, "\n".join(failures)


def test_guide_cmake_fences():
    text = (REPOSITORY / "docs/user-guide/src/tools/python.md").read_text(encoding="utf-8")
    fences = re.findall(r"```cmake\n(.*?)\n```", text, flags=re.DOTALL)
    assert len(fences) == 2, "update the guide inventory when its contexts change"
    with TemporaryDirectory(prefix="rumoca-guide-cmake-") as temporary:
        project = Path(temporary)
        stage_examples(project)
        (project / "CMakeLists.txt").write_text("\n".join(fences), encoding="utf-8")
        build = project / "build"
        subprocess.run(["cmake", "-S", str(project), "-B", str(build),
                        f"-DPython3_EXECUTABLE={sys.executable}"], check=True, timeout=120)
        for target in ("generate_rumoca", "run_rumoca_scenario"):
            subprocess.run(["cmake", "--build", str(build), "--target", target],
                           check=True, timeout=120)
        assert (build / "rumoca-codegen.stamp").read_text(encoding="utf-8") == "ok\n"
        assert (build / "generated/galec/AlgorithmCode/GalecCounter.alg").is_file()
        assert (build / "generated/galec/ProductionCode/GalecCounter.c").is_file()
    print("guide_scenario_test: 2 exact CMake fences configured and built OK", flush=True)


if __name__ == "__main__":
    failures = []
    for test in (test_authored_codegen_scenarios, test_guide_cmake_fences):
        try:
            test()
        except Exception as error:
            failures.append(f"{test.__name__}: {type(error).__name__}: {error}")
    assert not failures, "\n".join(failures)
