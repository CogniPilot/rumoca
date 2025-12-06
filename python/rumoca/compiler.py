"""
Rumoca compiler interface for Python.

This module provides a Python wrapper around the Rumoca Modelica compiler,
enabling compilation of Modelica models and export to Base Modelica JSON format.

When installed with native bindings (pip install rumoca), compilation is done
directly in-process. Otherwise, it falls back to calling the rumoca binary.

To prefer using the system rumoca binary instead of bundled native bindings:
    import rumoca
    rumoca.set_prefer_system_binary(True)

Or per-call:
    result = rumoca.compile("model.mo", prefer_system=True)
"""

import json
import subprocess
import warnings
from pathlib import Path
from typing import Optional, Union, Dict, Any

# Global setting to prefer system binary over native bindings
_prefer_system_binary: bool = False


def get_prefer_system_binary() -> bool:
    """Get the global setting for preferring system binary."""
    return _prefer_system_binary


def set_prefer_system_binary(value: bool) -> None:
    """
    Set whether to prefer the system rumoca binary over bundled native bindings.

    When True, compile() will first try to find 'rumoca' in PATH.
    If not found, falls back to native bindings with a warning.

    Args:
        value: True to prefer system binary, False to prefer native bindings (default)

    Example:
        >>> import rumoca
        >>> rumoca.set_prefer_system_binary(True)
        >>> result = rumoca.compile("model.mo")  # Uses system binary if available
    """
    global _prefer_system_binary
    _prefer_system_binary = value

# Try to import native bindings
try:
    from ._native import compile_str as _native_compile_str
    from ._native import compile_file as _native_compile_file
    from ._native import PyCompilationResult as _NativeResult
    _NATIVE_AVAILABLE = True
except ImportError:
    _NATIVE_AVAILABLE = False
    _native_compile_str = None
    _native_compile_file = None
    _NativeResult = None


class CompilationError(Exception):
    """Raised when Modelica compilation fails."""
    pass


class CompilationResult:
    """
    Result of compiling a Modelica model with Rumoca.

    This class wraps either a native compilation result (when native bindings
    are available) or provides subprocess-based compilation as a fallback.
    """

    def __init__(
        self,
        model_file: Optional[Path] = None,
        rumoca_bin: Optional[Path] = None,
        *,
        _native_result: Optional[Any] = None,
        _cached_json: Optional[str] = None,
    ):
        """
        Initialize compilation result.

        Args:
            model_file: Path to the Modelica source file (for subprocess mode)
            rumoca_bin: Path to rumoca binary (for subprocess mode)
            _native_result: Native compilation result (internal use)
            _cached_json: Pre-cached JSON string (internal use)
        """
        self._native_result = _native_result
        self._model_file = Path(model_file) if model_file else None
        self._rumoca_bin = rumoca_bin
        self._cached_dict: Optional[Dict[str, Any]] = None
        self._cached_json = _cached_json

        # Validate for subprocess mode
        if self._native_result is None and self._model_file is not None:
            if not self._model_file.exists():
                raise FileNotFoundError(f"Model file not found: {model_file}")
            if self._rumoca_bin is None:
                self._rumoca_bin = _find_rumoca_binary()
            if self._rumoca_bin is None:
                raise RuntimeError(
                    "Rumoca binary not found. Please ensure 'rumoca' is in PATH or "
                    "build it with: cd /path/to/rumoca && cargo build --release"
                )

    @property
    def is_native(self) -> bool:
        """Returns True if using native bindings."""
        return self._native_result is not None

    def __repr__(self) -> str:
        """Return a detailed string representation of the compiled model."""
        try:
            if self._native_result is not None:
                return repr(self._native_result)

            # Subprocess mode - get model data
            if self._cached_dict is None:
                self._cached_dict = self.to_base_modelica_dict()

            data = self._cached_dict
            model_name = data.get("model_name", "Unknown")
            n_params = len(data.get("parameters", []))
            n_vars = len(data.get("variables", []))
            n_eqs = len(data.get("equations", []))

            params = data.get("parameters", [])
            param_names = [p["name"] for p in params[:5]]
            if len(params) > 5:
                param_names.append("...")

            variables = data.get("variables", [])
            var_names = [v["name"] for v in variables[:5]]
            if len(variables) > 5:
                var_names.append("...")

            return (
                f"CompilationResult(\n"
                f"  model='{model_name}',\n"
                f"  source={self._model_file.name if self._model_file else 'string'},\n"
                f"  parameters={n_params}: {param_names},\n"
                f"  variables={n_vars}: {var_names},\n"
                f"  equations={n_eqs}\n"
                f")"
            )
        except Exception as e:
            return f"CompilationResult(error={e})"

    def to_base_modelica_json(self) -> str:
        """
        Export model to Base Modelica JSON format as a string.

        Returns:
            JSON string containing Base Modelica representation

        Raises:
            CompilationError: If export fails
        """
        if self._cached_json is not None:
            return self._cached_json

        if self._native_result is not None:
            self._cached_json = self._native_result.json
            return self._cached_json

        # Subprocess mode
        try:
            model_name = _extract_model_name(self._model_file)
            proc_result = subprocess.run(
                [str(self._rumoca_bin), "--json", "-m", model_name, str(self._model_file)],
                capture_output=True,
                text=True,
                check=True,
            )
            self._cached_json = proc_result.stdout
            return self._cached_json
        except subprocess.CalledProcessError as e:
            error_msg = _format_compilation_error(self._model_file, e.stdout, e.stderr)
            raise CompilationError(error_msg) from e

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

    def export(self, template: Union[str, Path]) -> str:
        """
        Export model using a Jinja2 template.

        Note: This requires the rumoca binary for template rendering.

        Args:
            template: Full path to a Jinja2 template file

        Returns:
            Generated code as string

        Raises:
            CompilationError: If export fails
            FileNotFoundError: If template not found
        """
        template_path = _resolve_template_path(template)

        if self._model_file is None:
            raise CompilationError(
                "Template export requires a file path. "
                "Use to_base_modelica_json() for string-based compilation."
            )

        rumoca_bin = self._rumoca_bin or _find_rumoca_binary()
        if rumoca_bin is None:
            raise RuntimeError("Rumoca binary required for template export")

        model_name = _extract_model_name(self._model_file)
        try:
            proc_result = subprocess.run(
                [
                    str(rumoca_bin),
                    "-m", model_name,
                    "--template-file", str(template_path),
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


def compile(
    model_file: Union[str, Path],
    rumoca_bin: Optional[Union[str, Path]] = None,
    *,
    prefer_system: Optional[bool] = None,
) -> CompilationResult:
    """
    Compile a Modelica model file using Rumoca.

    When native bindings are available, compilation is done directly in-process
    for better performance. Otherwise, falls back to calling the rumoca binary.

    Args:
        model_file: Path to the Modelica (.mo) file to compile
        rumoca_bin: Optional path to rumoca binary (forces subprocess mode)
        prefer_system: If True, prefer system rumoca binary over native bindings.
            Falls back to native bindings with a warning if system binary not found.
            Defaults to the global `rumoca.prefer_system_binary` setting.

    Returns:
        CompilationResult object containing the compiled model

    Raises:
        FileNotFoundError: If model file doesn't exist
        RuntimeError: If rumoca binary not found (subprocess mode only)
        CompilationError: If compilation fails

    Example:
        >>> import rumoca
        >>> result = rumoca.compile("bouncing_ball.mo")
        >>> result.export_base_modelica_json("output.json")

        >>> # Use system binary instead of bundled
        >>> result = rumoca.compile("model.mo", prefer_system=True)
    """
    model_path = Path(model_file)
    if not model_path.exists():
        raise FileNotFoundError(f"Model file not found: {model_file}")

    # Determine whether to prefer system binary
    use_system = prefer_system if prefer_system is not None else _prefer_system_binary

    # If rumoca_bin is explicitly provided, always use subprocess mode
    if rumoca_bin is not None:
        return _compile_with_subprocess(model_path, Path(rumoca_bin))

    # If prefer_system is set, try system binary first
    if use_system:
        system_bin = _find_rumoca_binary()
        if system_bin is not None:
            return _compile_with_subprocess(model_path, system_bin)
        else:
            # Fall back to native bindings with a warning
            if _NATIVE_AVAILABLE and _native_compile_file is not None:
                warnings.warn(
                    "System rumoca binary not found in PATH. "
                    "Falling back to bundled native bindings.",
                    RuntimeWarning,
                    stacklevel=2,
                )
                return _compile_with_native(model_path)
            else:
                raise RuntimeError(
                    "System rumoca binary not found and native bindings not available.\n"
                    "Install rumoca with: cargo install rumoca\n"
                    "Or reinstall the Python package: pip install --force-reinstall rumoca"
                )

    # Default behavior: try native bindings first
    if _NATIVE_AVAILABLE and _native_compile_file is not None:
        return _compile_with_native(model_path)

    # Fall back to subprocess
    rumoca_path = _find_rumoca_binary()
    if not rumoca_path:
        raise RuntimeError(
            "Rumoca binary not found in PATH. Please build it with:\n"
            "  cd /path/to/rumoca\n"
            "  cargo build --release\n"
            "  export PATH=$PATH:$(pwd)/target/release\n\n"
            "Or install with native bindings: pip install rumoca"
        )

    return _compile_with_subprocess(model_path, rumoca_path)


def _compile_with_native(model_path: Path) -> CompilationResult:
    """Compile using native bindings."""
    try:
        model_name = _extract_model_name(model_path)
        native_result = _native_compile_file(str(model_path), model_name)
        return CompilationResult(
            model_file=model_path,
            _native_result=native_result,
            _cached_json=native_result.json,
        )
    except Exception as e:
        raise CompilationError(str(e)) from e


def _compile_with_subprocess(model_path: Path, rumoca_bin: Path) -> CompilationResult:
    """Compile using subprocess call to rumoca binary."""
    model_name = _extract_model_name(model_path)
    try:
        subprocess.run(
            [str(rumoca_bin), "-m", model_name, str(model_path)],
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as e:
        error_msg = _format_compilation_error(model_path, e.stdout, e.stderr)
        raise CompilationError(error_msg) from e

    return CompilationResult(model_path, rumoca_bin)


def compile_source(
    source: str,
    model_name: str,
    filename: str = "<string>",
    library_paths: Optional[list] = None,
    use_modelica_path: bool = True,
    threads: Optional[int] = None,
) -> CompilationResult:
    """
    Compile Modelica source code from a string.

    This function requires native bindings to be available.

    Args:
        source: Modelica source code as a string
        model_name: Name of the model to compile
        filename: Optional filename for error messages
        library_paths: Optional list of library paths to include (e.g., ["/path/to/MSL"])
        use_modelica_path: If True, also search MODELICAPATH env var for libraries (default: True)
        threads: Number of threads for parallel parsing (default: 50% of CPU cores)

    Returns:
        CompilationResult object containing the compiled model

    Raises:
        RuntimeError: If native bindings are not available
        CompilationError: If compilation fails

    Example:
        >>> import rumoca
        >>> result = rumoca.compile_source('''
        ...     model Test
        ...         Real x;
        ...     equation
        ...         der(x) = 1;
        ...     end Test;
        ... ''', "Test")
        >>> print(result.to_base_modelica_json())

        >>> # With MSL library:
        >>> result = rumoca.compile_source('''
        ...     model Test
        ...         import Modelica.Blocks.Continuous.PID;
        ...         PID pid;
        ...     end Test;
        ... ''', "Test", library_paths=["/path/to/MSL"])

        >>> # Use all CPU cores for parsing:
        >>> import os
        >>> result = rumoca.compile_source(source, "Test", threads=os.cpu_count())
    """
    if not _NATIVE_AVAILABLE or _native_compile_str is None:
        raise RuntimeError(
            "compile_source() requires native bindings. "
            "Install with: pip install rumoca\n"
            "Or write source to a file and use compile() instead."
        )

    try:
        native_result = _native_compile_str(
            source, model_name, filename, library_paths, use_modelica_path, threads
        )
        return CompilationResult(
            _native_result=native_result,
            _cached_json=native_result.json,
        )
    except Exception as e:
        raise CompilationError(str(e)) from e


# Helper functions

def _format_compilation_error(model_path: Path, stdout: str, stderr: str) -> str:
    """Format a compilation error message from Rumoca output."""
    try:
        with open(model_path, 'r') as f:
            source = f.read()
    except:
        source = None

    if "panicked at" in stderr:
        panic_msg = _extract_panic_info(stderr)
        if "not yet implemented" in stderr:
            feature = _extract_unimplemented_feature(stderr)
            msg = f"Failed to compile {model_path.name}:\n\n"
            msg += f"Rumoca encountered an unimplemented feature: {feature}\n\n"
            if source:
                lines = source.split('\n')
                msg += "Model source:\n"
                for i, line in enumerate(lines, 1):
                    msg += f"  {i:3d} | {line}\n"
                msg += "\n"
            msg += "This syntax or feature is not yet supported by the Rumoca parser.\n"
            if panic_msg:
                msg += f"\nTechnical details: {panic_msg}"
            return msg
        else:
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

    if stderr and ("Error: rumoca::" in stderr or "×" in stderr):
        return stderr.strip()

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
            parts = line.split('panicked at', 1)
            if len(parts) == 2:
                return parts[1].strip()
    return None


def _extract_unimplemented_feature(stderr: str) -> str:
    """Extract the unimplemented feature name from panic message."""
    for line in stderr.split('\n'):
        if 'not yet implemented:' in line:
            parts = line.split('not yet implemented:', 1)
            if len(parts) == 2:
                return parts[1].strip()
    return "unknown feature"


def _extract_model_name(model_file: Path) -> str:
    """Extract the model name from a Modelica file."""
    import re
    try:
        with open(model_file, 'r') as f:
            content = f.read()
        match = re.search(r'\b(model|class)\s+(\w+)', content)
        if match:
            return match.group(2)
        raise CompilationError(
            f"Could not find model or class declaration in {model_file}"
        )
    except IOError as e:
        raise CompilationError(f"Could not read model file {model_file}: {e}")


def _find_rumoca_binary() -> Optional[Path]:
    """Find the rumoca binary in PATH or common build locations."""
    import shutil
    rumoca_in_path = shutil.which("rumoca")
    if rumoca_in_path:
        return Path(rumoca_in_path)

    package_dir = Path(__file__).parent.parent.parent
    common_locations = [
        package_dir / "target" / "release" / "rumoca",
        package_dir / "target" / "debug" / "rumoca",
        package_dir.parent / "rumoca" / "target" / "release" / "rumoca",
    ]
    for location in common_locations:
        if location.exists() and location.is_file():
            return location
    return None


def _resolve_template_path(template: Union[str, Path]) -> Path:
    """Resolve a template path."""
    template_path = Path(template)
    if not template_path.exists():
        raise FileNotFoundError(
            f"Template file not found: {template}\n\n"
            f"For production use, please use native JSON export instead:\n"
            f"  result.to_base_modelica_json()  # Python API\n"
            f"  rumoca model.mo --json          # Command line"
        )
    return template_path
