"""
Rumoca Python Interface

Python wrapper for the Rumoca Modelica compiler, enabling seamless integration
with Cyecca for code generation and simulation.

Example:
    >>> import rumoca
    >>> result = rumoca.compile("bouncing_ball.mo")
    >>>
    >>> # Export to Base Modelica JSON
    >>> result.export_base_modelica_json("output.json")
    >>>
    >>> # Then use Cyecca for backend-specific code generation:
    >>> from cyecca.io import import_base_modelica
    >>> model = import_base_modelica("output.json")
    >>> # Use CasADi backend
    >>> from cyecca.backends import CasadiBackend
    >>> backend = CasadiBackend(model)
"""

from .compiler import (
    compile,
    compile_source,
    CompilationResult,
    CompilationError,
    get_prefer_system_binary,
    set_prefer_system_binary,
)
from .version import __version__

# Try to import native bindings for direct access
try:
    from ._native import compile_str, compile_file, version as native_version
    NATIVE_AVAILABLE = True
except ImportError:
    NATIVE_AVAILABLE = False
    compile_str = None
    compile_file = None
    native_version = None

__all__ = [
    "compile",
    "compile_source",
    "CompilationResult",
    "CompilationError",
    "get_prefer_system_binary",
    "set_prefer_system_binary",
    "__version__",
    "NATIVE_AVAILABLE",
    "compile_str",
    "compile_file",
]
