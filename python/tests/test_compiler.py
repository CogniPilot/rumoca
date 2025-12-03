"""Tests for rumoca compiler Python interface."""

import json
import pytest
from pathlib import Path
import sys

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

import rumoca
from rumoca import CompilationError


def get_test_model_path(model_name: str) -> Path:
    """Get path to a test model file."""
    # Try multiple possible locations
    potential_paths = [
        Path(__file__).parent.parent.parent / "tests" / "models" / model_name,
        Path(__file__).parent.parent.parent.parent / "rumoca" / "tests" / "models" / model_name,
    ]

    for path in potential_paths:
        if path.exists():
            return path

    pytest.skip(f"Test model not found: {model_name}")


def test_compile_basic():
    """Test basic compilation of a Modelica model."""
    model_path = get_test_model_path("bouncing_ball.mo")

    result = rumoca.compile(model_path)
    assert result is not None
    assert isinstance(result, rumoca.CompilationResult)


def test_to_base_modelica_json():
    """Test exporting to Base Modelica JSON string."""
    model_path = get_test_model_path("bouncing_ball.mo")

    result = rumoca.compile(model_path)
    json_str = result.to_base_modelica_json()

    assert json_str is not None
    assert isinstance(json_str, str)
    assert len(json_str) > 0

    # Verify it's valid JSON
    data = json.loads(json_str)
    assert "ir_version" in data
    assert data["ir_version"] == "base-0.1.0"
    assert "model_name" in data
    assert "parameters" in data
    assert "variables" in data
    assert "equations" in data


def test_to_base_modelica_dict():
    """Test getting Base Modelica as Python dict."""
    model_path = get_test_model_path("bouncing_ball.mo")

    result = rumoca.compile(model_path)
    model_dict = result.to_base_modelica_dict()

    assert isinstance(model_dict, dict)
    assert model_dict["ir_version"] == "base-0.1.0"
    assert "model_name" in model_dict
    assert isinstance(model_dict["parameters"], list)
    assert isinstance(model_dict["variables"], list)
    assert isinstance(model_dict["equations"], list)


def test_export_base_modelica_json(tmp_path):
    """Test exporting to Base Modelica JSON file."""
    model_path = get_test_model_path("bouncing_ball.mo")
    output_path = tmp_path / "bouncing_ball.json"

    result = rumoca.compile(model_path)
    result.export_base_modelica_json(output_path)

    assert output_path.exists()

    # Verify file contents
    with open(output_path) as f:
        data = json.load(f)

    assert data["ir_version"] == "base-0.1.0"
    assert "model_name" in data


def test_export_casadi(tmp_path):
    """Test exporting to CasADi Python code."""
    model_path = get_test_model_path("bouncing_ball.mo")
    output_path = tmp_path / "bouncing_ball_casadi.py"

    result = rumoca.compile(model_path)
    result.export_casadi(output_path)

    assert output_path.exists()

    # Verify it's Python code
    with open(output_path) as f:
        code = f.read()

    assert "import casadi" in code or "import" in code
    assert len(code) > 0


def test_export_sympy(tmp_path):
    """Test exporting to SymPy Python code."""
    model_path = get_test_model_path("bouncing_ball.mo")
    output_path = tmp_path / "bouncing_ball_sympy.py"

    result = rumoca.compile(model_path)
    result.export_sympy(output_path)

    assert output_path.exists()

    # Verify it's Python code
    with open(output_path) as f:
        code = f.read()

    assert len(code) > 0


def test_file_not_found():
    """Test that FileNotFoundError is raised for missing files."""
    with pytest.raises(FileNotFoundError):
        rumoca.compile("nonexistent_model.mo")


def test_export_with_custom_template(tmp_path):
    """Test using the generic export() method."""
    model_path = get_test_model_path("bouncing_ball.mo")

    result = rumoca.compile(model_path)

    # Test with built-in template name
    json_str = result.export("base_modelica.jinja")
    data = json.loads(json_str)
    assert data["ir_version"] == "base-0.1.0"

    # Test with another built-in template
    casadi_code = result.export("casadi.jinja")
    assert len(casadi_code) > 0


def test_bouncing_ball_structure():
    """Test specific structure of bouncing ball model."""
    model_path = get_test_model_path("bouncing_ball.mo")

    result = rumoca.compile(model_path)
    model_dict = result.to_base_modelica_dict()

    # Check parameters
    params = {p["name"]: p for p in model_dict["parameters"]}
    assert "e" in params  # Coefficient of restitution
    assert params["e"]["vartype"] == "Real"

    # Check variables
    vars_dict = {v["name"]: v for v in model_dict["variables"]}
    assert "h" in vars_dict  # Height
    assert "v" in vars_dict  # Velocity

    # Check equations
    equations = model_dict["equations"]
    assert len(equations) > 0


def test_integration_with_cyecca(tmp_path):
    """Test full pipeline: Rumoca → JSON → Cyecca."""
    pytest.importorskip("cyecca", reason="Cyecca not installed")

    from cyecca.io import import_base_modelica

    model_path = get_test_model_path("bouncing_ball.mo")
    json_path = tmp_path / "bouncing_ball.json"

    # Compile with Rumoca
    result = rumoca.compile(model_path)
    result.export_base_modelica_json(json_path)

    # Import with Cyecca
    model = import_base_modelica(json_path)

    assert model is not None
    assert model.name == "GeneratedModel"
    assert len(model.variables) > 0
    assert len(model.equations) > 0

    # Check that we have parameters and variables
    parameters = [v for v in model.variables if v.is_parameter]
    assert len(parameters) > 0

    variables = [v for v in model.variables if not v.is_parameter]
    assert len(variables) > 0


def test_native_bindings_available():
    """Test that native bindings are available when built with maturin."""
    # This test checks if native bindings are available
    # When installed via pip from source or wheels, this should be True
    # When running without the native extension, it may be False
    print(f"Native bindings available: {rumoca.NATIVE_AVAILABLE}")
    if rumoca.NATIVE_AVAILABLE:
        assert rumoca.compile_str is not None
        assert rumoca.compile_file is not None


def test_compile_source_native():
    """Test compiling Modelica source from string (native bindings only)."""
    if not rumoca.NATIVE_AVAILABLE:
        pytest.skip("Native bindings not available")

    from rumoca import compile_source

    source = """
    model TestModel
        Real x(start=0);
    equation
        der(x) = 1;
    end TestModel;
    """

    result = compile_source(source, "TestModel")
    assert result is not None
    assert result.is_native

    json_str = result.to_base_modelica_json()
    data = json.loads(json_str)
    assert data["model_name"] == "TestModel"


def test_is_native_property():
    """Test the is_native property on CompilationResult."""
    model_path = get_test_model_path("bouncing_ball.mo")
    result = rumoca.compile(model_path)

    # is_native should be True if native bindings are used, False otherwise
    assert isinstance(result.is_native, bool)
    if rumoca.NATIVE_AVAILABLE:
        assert result.is_native is True


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
