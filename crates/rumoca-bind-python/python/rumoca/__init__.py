"""Python interface for the Rumoca Modelica compiler.

The compiled extension lives at :mod:`rumoca._native`; this package re-exports
its full API so ``import rumoca`` works exactly as before, and additionally
registers the ``%%modelica`` Jupyter cell magic when imported inside IPython.
"""

from __future__ import annotations

# Re-export the entire compiled API (compile, compile_source, simulate,
# render_target_model, cli, the result classes, ...). The accompanying
# ``__init__.pyi`` stub + ``py.typed`` marker give editors full autocomplete on
# these names.
from ._native import *  # noqa: F401,F403
from . import _native

try:
    __all__ = list(_native.__all__)
except AttributeError:
    __all__ = [name for name in dir(_native) if not name.startswith("_")]

from ._magic import load_ipython_extension, unload_ipython_extension

__all__ += ["load_ipython_extension", "unload_ipython_extension"]


def _register_magic_if_in_ipython() -> None:
    """Auto-register ``%%modelica`` so it works after a bare ``import rumoca``.

    No-op outside IPython/Jupyter (e.g. a plain ``python`` process), so importing
    the package never fails just because IPython is absent.
    """
    try:
        from IPython import get_ipython

        shell = get_ipython()
    except Exception:
        # Absent IPython, or any unexpected failure resolving the shell, must
        # never break a plain ``import rumoca``.
        return
    if shell is not None:
        load_ipython_extension(shell)


_register_magic_if_in_ipython()
