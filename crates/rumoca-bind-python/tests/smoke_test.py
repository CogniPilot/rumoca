import json
from pathlib import Path

import rumoca


FIXTURE_ROOT = Path(__file__).resolve().parent / "fixtures"
MODEL_FILE = FIXTURE_ROOT / "UsesLib.mo"
SOURCE_ROOT = FIXTURE_ROOT / "Lib"


def main() -> None:
    templates = json.loads(rumoca.get_builtin_templates())
    assert any(item["id"] == "sympy.py.jinja" for item in templates)

    raw_dae = json.loads(rumoca.compile_file(str(MODEL_FILE), source_roots=[str(SOURCE_ROOT)]))
    assert raw_dae["class_type"] == "Model"
    assert "x" in raw_dae["x"]

    compiled = json.loads(
        rumoca.compile_file_to_json(str(MODEL_FILE), source_roots=[str(SOURCE_ROOT)])
    )
    assert "x" in compiled
    assert "f_x" in compiled

    rendered = rumoca.render_builtin_file(
        str(MODEL_FILE),
        "dae_modelica.mo.jinja",
        source_roots=[str(SOURCE_ROOT)],
    )
    assert "class UsesLib" in rendered

    session = rumoca.ProjectSession([str(SOURCE_ROOT)])
    session_compile = json.loads(session.compile_file(str(MODEL_FILE)))
    assert session_compile["class_type"] == "Model"

    statuses = json.loads(session.source_root_statuses())
    assert statuses

    sim = json.loads(session.simulate_file(str(MODEL_FILE), t_end=0.2, dt=0.1, solver="auto"))
    assert sim["model"] == "UsesLib"
    assert sim["metrics"]["points"] >= 2


if __name__ == "__main__":
    main()
