"""
Rumoca compiler interface for Python.

This module provides a Python wrapper around the Rumoca Modelica compiler,
enabling compilation of Modelica models and export to Base Modelica JSON format.
"""

import json
import subprocess
import tempfile
from pathlib import Path
from typing import Optional, Union, Dict, Any


class CompilationError(Exception):
    """Raised when Modelica compilation fails."""
    pass


class CompilationResult:
    """
    Result of compiling a Modelica model with Rumoca.

    Attributes:
        model_name: Name of the compiled model
        dae: DAE representation (internal)
        _rumoca_bin: Path to rumoca binary
        _model_file: Path to original Modelica file
    """

    def __init__(self, model_file: Path, rumoca_bin: Optional[Path] = None):
        """
        Initialize compilation result.

        Args:
            model_file: Path to the Modelica source file
            rumoca_bin: Path to rumoca binary (auto-detected if None)
        """
        self._model_file = Path(model_file)
        self._rumoca_bin = rumoca_bin or _find_rumoca_binary()
        self._cached_dict: Optional[Dict[str, Any]] = None

        if not self._model_file.exists():
            raise FileNotFoundError(f"Model file not found: {model_file}")

        if not self._rumoca_bin:
            raise RuntimeError(
                "Rumoca binary not found. Please ensure 'rumoca' is in PATH or "
                "build it with: cd /path/to/rumoca && cargo build --release"
            )

    def __repr__(self) -> str:
        """Return a detailed string representation of the compiled model."""
        try:
            # Get model data (cache it to avoid recompiling)
            if self._cached_dict is None:
                self._cached_dict = self.to_base_modelica_dict()

            data = self._cached_dict
            model_name = data.get("model_name", "Unknown")
            n_params = len(data.get("parameters", []))
            n_vars = len(data.get("variables", []))
            n_eqs = len(data.get("equations", []))

            # Get parameter names
            params = data.get("parameters", [])
            param_names = [p["name"] for p in params[:5]]
            if len(params) > 5:
                param_names.append("...")

            # Get variable names
            variables = data.get("variables", [])
            var_names = [v["name"] for v in variables[:5]]
            if len(variables) > 5:
                var_names.append("...")

            return (
                f"CompilationResult(\n"
                f"  model='{model_name}',\n"
                f"  source={self._model_file.name},\n"
                f"  parameters={n_params}: {param_names},\n"
                f"  variables={n_vars}: {var_names},\n"
                f"  equations={n_eqs}\n"
                f")"
            )
        except Exception as e:
            return f"CompilationResult(model_file={self._model_file}, error={e})"

    def export(self, template: Union[str, Path]) -> str:
        """
        Export model using a template.

        Args:
            template: Template to use for export. Can be either:
                - Name of built-in template (e.g., "base_modelica.jinja")
                - Full path to custom template file

        Returns:
            Generated code as string

        Raises:
            CompilationError: If export fails
            FileNotFoundError: If template not found

        Example:
            >>> result = rumoca.compile("model.mo")
            >>> # Use built-in template
            >>> json_str = result.export("base_modelica.jinja")
            >>> # Use custom template
            >>> code = result.export("/path/to/my_template.jinja")
        """
        template_path = _resolve_template_path(template)
        model_name = _extract_model_name(self._model_file)

        try:
            proc_result = subprocess.run(
                [
                    str(self._rumoca_bin),
                    "-m", model_name,
                    "--template-file",
                    str(template_path),
                    str(self._model_file),
                ],
                capture_output=True,
                text=True,
                check=True,
            )
            return proc_result.stdout
        except subprocess.CalledProcessError as e:
            error_msg = _format_compilation_error(self._model_file, e.stdout, e.stderr)
            raise CompilationError(error_msg) from e

    def to_base_modelica_json(self) -> str:
        """
        Export model to Base Modelica JSON format as a string.

        Uses native Rust serde_json serialization for fast, type-safe export.
        This is the recommended way to export Base Modelica IR.

        Returns:
            JSON string containing Base Modelica representation

        Raises:
            CompilationError: If export fails (requires Rumoca v0.6.0+)
        """
        try:
            # Extract model name from file
            model_name = _extract_model_name(self._model_file)
            proc_result = subprocess.run(
                [str(self._rumoca_bin), "--json", "-m", model_name, str(self._model_file)],
                capture_output=True,
                text=True,
                check=True,
            )
            return proc_result.stdout
        except subprocess.CalledProcessError as e:
            error_msg = _format_compilation_error(self._model_file, e.stdout, e.stderr)
            raise CompilationError(
                f"Native JSON export failed. Please upgrade Rumoca to v0.6.0+.\n\n{error_msg}"
            ) from e

    def export_base_modelica_json(self, output_file: Union[str, Path]) -> None:
        """
        Export model to Base Modelica JSON file.

        Args:
            output_file: Path where JSON file will be written

        Raises:
            CompilationError: If export fails
        """
        json_str = self.to_base_modelica_json()

        output_path = Path(output_file)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        with open(output_path, "w") as f:
            f.write(json_str)

    def to_base_modelica_dict(self) -> Dict[str, Any]:
        """
        Get Base Modelica representation as Python dict.

        Returns:
            Dictionary containing Base Modelica model data

        Raises:
            CompilationError: If export fails
        """
        if self._cached_dict is None:
            json_str = self.to_base_modelica_json()
            self._cached_dict = json.loads(json_str)
        return self._cached_dict



def compile(
    model_file: Union[str, Path],
    rumoca_bin: Optional[Union[str, Path]] = None,
) -> CompilationResult:
    """
    Compile a Modelica model file using Rumoca.

    Args:
        model_file: Path to the Modelica (.mo) file to compile
        rumoca_bin: Optional path to rumoca binary (auto-detected if None)

    Returns:
        CompilationResult object containing the compiled model

    Raises:
        FileNotFoundError: If model file doesn't exist
        RuntimeError: If rumoca binary not found
        CompilationError: If compilation fails

    Example:
        >>> import rumoca
        >>> result = rumoca.compile("bouncing_ball.mo")
        >>> result.export_base_modelica_json("output.json")
    """
    rumoca_path = Path(rumoca_bin) if rumoca_bin else _find_rumoca_binary()

    if not rumoca_path:
        raise RuntimeError(
            "Rumoca binary not found in PATH. Please build it with:\n"
            "  cd /path/to/rumoca\n"
            "  cargo build --release\n"
            "  export PATH=$PATH:$(pwd)/target/release"
        )

    # Test compilation by running rumoca (this validates the model)
    model_path = Path(model_file)
    if not model_path.exists():
        raise FileNotFoundError(f"Model file not found: {model_file}")

    # Extract model name and quick validation
    model_name = _extract_model_name(model_path)
    try:
        subprocess.run(
            [str(rumoca_path), "-m", model_name, str(model_path)],
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as e:
        # Format the error message more nicely
        error_msg = _format_compilation_error(model_path, e.stdout, e.stderr)
        raise CompilationError(error_msg) from e

    return CompilationResult(model_path, rumoca_path)


def _format_compilation_error(model_path: Path, stdout: str, stderr: str) -> str:
    """
    Format a compilation error message from Rumoca output.

    Extracts useful information from panics and error messages, and shows
    the relevant source code context when possible.
    """
    # Read the model source for context
    try:
        with open(model_path, 'r') as f:
            source = f.read()
    except:
        source = None

    # Check if this is a panic
    if "panicked at" in stderr:
        # Extract panic location and message
        panic_msg = _extract_panic_info(stderr)

        # Try to extract file location from panic (e.g., "at src/modelica_grammar.rs:960:21")
        # This doesn't help user much, but we can look for context in the error

        if "not yet implemented" in stderr:
            # This is an unimplemented feature
            feature = _extract_unimplemented_feature(stderr)
            msg = f"Failed to compile {model_path.name}:\n\n"
            msg += f"Rumoca encountered an unimplemented feature: {feature}\n\n"

            if source:
                # Show the whole file with line numbers so user can investigate
                lines = source.split('\n')
                msg += "Model source:\n"
                for i, line in enumerate(lines, 1):
                    msg += f"  {i:3d} | {line}\n"
                msg += "\n"

            msg += "This syntax or feature is not yet supported by the Rumoca parser.\n"
            msg += "Please check your Modelica code for unusual syntax or advanced features.\n"

            if panic_msg:
                msg += f"\nTechnical details: {panic_msg}"

            return msg
        else:
            # Generic panic
            msg = f"Failed to compile {model_path.name}:\n\n"
            msg += "The compiler encountered an internal error (panic).\n\n"

            if panic_msg:
                msg += f"Error: {panic_msg}\n\n"

            if source:
                lines = source.split('\n')
                msg += "Model source:\n"
                for i, line in enumerate(lines, 1):
                    msg += f"  {i:3d} | {line}\n"
                msg += "\n"

            msg += "Full error output:\n"
            if stdout:
                msg += f"stdout: {stdout}\n"
            msg += f"stderr: {stderr}\n"

            return msg

    # Not a panic - check if stderr contains miette diagnostic
    if stderr and ("Error: rumoca::" in stderr or "×" in stderr):
        # This is a miette diagnostic - it's already beautifully formatted
        # Just pass it through without adding extra prefixes
        return stderr.strip()

    # Otherwise, return formatted output
    msg = f"Failed to compile {model_path.name}:\n"
    if stdout:
        msg += f"  stdout: {stdout}\n"
    if stderr:
        msg += f"  stderr: {stderr}"

    return msg


def _extract_panic_info(stderr: str) -> Optional[str]:
    """Extract panic message from stderr."""
    for line in stderr.split('\n'):
        if 'panicked at' in line:
            # Extract the part after "panicked at"
            parts = line.split('panicked at', 1)
            if len(parts) == 2:
                return parts[1].strip()
    return None


def _extract_unimplemented_feature(stderr: str) -> str:
    """Extract the unimplemented feature name from panic message."""
    for line in stderr.split('\n'):
        if 'not yet implemented:' in line:
            # Extract feature name
            parts = line.split('not yet implemented:', 1)
            if len(parts) == 2:
                return parts[1].strip()
    return "unknown feature"


def _extract_model_name(model_file: Path) -> str:
    """
    Extract the model name from a Modelica file.

    Searches for 'model <name>' or 'class <name>' declaration.

    Args:
        model_file: Path to the Modelica file

    Returns:
        Model name extracted from file

    Raises:
        CompilationError: If no model declaration found
    """
    import re

    try:
        with open(model_file, 'r') as f:
            content = f.read()

        # Look for model or class declaration
        match = re.search(r'\b(model|class)\s+(\w+)', content)
        if match:
            return match.group(2)

        raise CompilationError(
            f"Could not find model or class declaration in {model_file}"
        )
    except IOError as e:
        raise CompilationError(f"Could not read model file {model_file}: {e}")


def _find_rumoca_binary() -> Optional[Path]:
    """
    Find the rumoca binary in PATH or common build locations.

    Returns:
        Path to rumoca binary, or None if not found
    """
    import shutil

    # Check PATH first
    rumoca_in_path = shutil.which("rumoca")
    if rumoca_in_path:
        return Path(rumoca_in_path)

    # Check common build locations relative to this file
    package_dir = Path(__file__).parent.parent.parent
    common_locations = [
        package_dir / "target" / "release" / "rumoca",
        package_dir / "target" / "debug" / "rumoca",
        package_dir.parent / "rumoca" / "target" / "release" / "rumoca",
        Path.home() / "ws_fixedwing" / "src" / "rumoca" / "target" / "release" / "rumoca",
    ]

    for location in common_locations:
        if location.exists() and location.is_file():
            return location

    return None


def _resolve_template_path(template: Union[str, Path]) -> Path:
    """
    Resolve a template path.

    Note: Built-in templates are NOT installed with the Python package.
    They exist only in the source repository as examples. For production use,
    export to Base Modelica JSON using to_base_modelica_json() instead.

    Args:
        template: Full path to a custom template file

    Returns:
        Path to the template file

    Raises:
        FileNotFoundError: If template file not found

    Examples:
        >>> _resolve_template_path("/path/to/my_template.jinja")
        Path("/path/to/my_template.jinja")
        >>> _resolve_template_path("./templates/custom.jinja")
        Path("./templates/custom.jinja")
    """
    template_path = Path(template)

    if not template_path.exists():
        raise FileNotFoundError(
            f"Template file not found: {template}\n\n"
            f"Note: Built-in templates are NOT installed with Rumoca.\n"
            f"They exist only in the source repository as educational examples.\n\n"
            f"For production use, please use native JSON export instead:\n"
            f"  result.to_base_modelica_json()  # Python API\n"
            f"  rumoca model.mo --json          # Command line\n\n"
            f"If you need a custom template, provide the full path to your template file."
        )

    return template_path
