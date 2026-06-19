import json
from pathlib import Path

import rumoca


FIXTURE_ROOT = Path(__file__).resolve().parent / "fixtures"
MODEL_FILE = FIXTURE_ROOT / "UsesLib.mo"
SOURCE_ROOT = FIXTURE_ROOT / "Lib"


def assert_raw_dae_model(raw_dae: dict) -> None:
    assert raw_dae.get("metadata", {}).get("class_type") == "Model"
    assert "x" in raw_dae["x"]


def main() -> None:
    targets = json.loads(rumoca.get_builtin_targets())
    sympy_targets = [item for item in targets if item["id"] == "sympy"]
    assert sympy_targets
    assert any(path.endswith(".jinja") for path in sympy_targets[0]["templates"])

    raw_dae = json.loads(rumoca.compile_file(str(MODEL_FILE), source_roots=[str(SOURCE_ROOT)]))
    assert_raw_dae_model(raw_dae)

    compiled = json.loads(
        rumoca.compile_file_to_json(str(MODEL_FILE), source_roots=[str(SOURCE_ROOT)])
    )
    assert "x" in compiled
    assert "f_x" in compiled

    rendered_files = json.loads(
        rumoca.render_target_file(
            str(MODEL_FILE),
            "dae-modelica",
            source_roots=[str(SOURCE_ROOT)],
        )
    )
    rendered = rendered_files[0]["content"]
    assert "class UsesLib" in rendered

    session = rumoca.ProjectSession([str(SOURCE_ROOT)])
    session_compile = json.loads(session.compile_file(str(MODEL_FILE)))
    assert_raw_dae_model(session_compile)

    statuses = json.loads(session.source_root_statuses())
    assert statuses

    sim_options = rumoca.SimulationOptions(t_end=0.2, dt=0.1, solver="auto")
    sim = json.loads(session.simulate_file(str(MODEL_FILE), options=sim_options))
    assert sim["model"] == "UsesLib"
    assert sim["metrics"]["points"] >= 2

    project = rumoca.Project.open(
        str(FIXTURE_ROOT),
        model_name="UsesLib",
        model_file=str(MODEL_FILE),
        source_roots=[str(SOURCE_ROOT)],
    )
    project_compile = project.compile_file()
    assert project_compile.model_name == "UsesLib"
    assert_raw_dae_model(project_compile.to_dict())

    project_sim = project.simulate_file(options=sim_options)
    assert project_sim.model_name == "UsesLib"
    assert project_sim.metrics["points"] >= 2

    project_codegen = project.codegen_file(target="dae-modelica")
    assert project_codegen.paths
    assert "class UsesLib" in project_codegen.to_dict()[0]["content"]


if __name__ == "__main__":
    main()
