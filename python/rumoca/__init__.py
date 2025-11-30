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

from .compiler import compile, CompilationResult, CompilationError
from .version import __version__

__all__ = ["compile", "CompilationResult", "CompilationError", "__version__"]
