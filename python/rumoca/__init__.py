"""
Rumoca Python Interface

Python wrapper for the Rumoca Modelica compiler, enabling seamless integration
with Cyecca for code generation and simulation.

Example:
    >>> import rumoca
    >>> result = rumoca.compile("bouncing_ball.mo")
    >>> result.export_base_modelica_json("output.json")
    >>>
    >>> # Or use with cyecca directly:
    >>> from cyecca.backends.casadi import compile_modelica
    >>> model = compile_modelica('''
    ...     model MyModel
    ...         Real x;
    ...     equation
    ...         der(x) = -x;
    ...     end MyModel;
    ... ''')
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

# Check if native bindings are available
def _check_native() -> bool:
    try:
        from . import _native  # noqa: F401
        return True
    except ImportError:
        return False

NATIVE_AVAILABLE = _check_native()
del _check_native  # Remove helper from namespace

__all__ = [
    # Core API
    "compile",
    "compile_source",
    "CompilationResult",
    "CompilationError",
    # Configuration
    "get_prefer_system_binary",
    "set_prefer_system_binary",
    # Metadata
    "__version__",
    "NATIVE_AVAILABLE",
]
