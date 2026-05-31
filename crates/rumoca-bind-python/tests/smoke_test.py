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

    sim = json.loads(session.simulate_file(str(MODEL_FILE), t_end=0.2, dt=0.1, solver="auto"))
    assert sim["model"] == "UsesLib"
    assert sim["metrics"]["points"] >= 2


if __name__ == "__main__":
    main()
