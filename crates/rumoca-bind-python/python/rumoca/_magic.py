"""The ``%%modelica`` Jupyter cell magic.

Write Modelica directly in a notebook cell and get back a first-class typed
object — a :class:`rumoca.Model` by default, a :class:`rumoca.Result` for
``sim``, or a :class:`rumoca.CodegenResult` for ``export <target>``. No JSON, no
file dance; the cell body is the model source.

Grammar::

    %%modelica [-m MODEL] [--roots A,B] [-n VAR] [-q]
    <modelica source>                       # -> Model

    %%modelica sim [-m MODEL] [--t-end T] [--dt DT] [--solver S]
                   [--rtol R] [--atol A] [-n VAR] [-q]
    <modelica source>                       # -> Result

    %%modelica export TARGET [-m MODEL] [-n VAR] [-q]
    <modelica source>                       # -> CodegenResult

Two notebook-only flags layer on top:

* ``--name VAR`` / ``-n VAR`` — also bind the result into the notebook namespace.
* ``--quiet`` / ``-q``        — return ``None`` so the cell renders no output;
                                the ``--name`` binding still happens.

IPython is an optional dependency: this module imports without it, and IPython is
only needed when :func:`load_ipython_extension` actually registers the magic.
"""

from __future__ import annotations

import argparse
import shlex
from typing import Any

from . import _native

# The cell body stands in for a real ``.mo`` file so model-name inference and
# diagnostic spans behave like a file on disk.
_CELL_FILENAME = "cell.mo"


class ModelicaMagicError(Exception):
    """A user-facing error from the ``%%modelica`` magic.

    Raised by the IPython-independent core so the IPython wrapper can render it
    as a clean ``UsageError`` without this module importing IPython at all.
    """


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(add_help=False, allow_abbrev=False)
    parser.add_argument("subcommand", nargs="?", default="compile")
    parser.add_argument("export_target", nargs="?", default=None)
    parser.add_argument("--model", "-m", default=None)
    parser.add_argument("--roots", default=None)
    parser.add_argument("--name", "-n", default=None)
    parser.add_argument("--quiet", "-q", action="store_true")
    parser.add_argument("--t-end", dest="t_end", type=float, default=None)
    parser.add_argument("--dt", type=float, default=None)
    parser.add_argument("--solver", default="auto")
    parser.add_argument("--rtol", type=float, default=None)
    parser.add_argument("--atol", type=float, default=None)
    return parser


def _parse_line(line: str) -> argparse.Namespace:
    parser = _build_parser()
    try:
        args, extra = parser.parse_known_args(shlex.split(line))
    except SystemExit as error:  # argparse calls sys.exit on bad input
        raise ModelicaMagicError(f"could not parse magic line: {line!r}") from error
    if extra:
        raise ModelicaMagicError(f"unknown argument(s): {' '.join(extra)}")
    # `export` takes the target as its single positional; for `compile`/`sim`
    # there is no second positional.
    if args.subcommand != "export" and args.export_target is not None:
        raise ModelicaMagicError(
            f"unexpected argument {args.export_target!r} for '{args.subcommand}'"
        )
    return args


def _roots(args: argparse.Namespace) -> list[str] | None:
    if not args.roots:
        return None
    return [part for part in args.roots.split(",") if part]


def run_modelica_cell(user_ns: dict[str, Any], line: str, cell: str) -> Any:
    """Core dispatch for ``%%modelica`` — no IPython required.

    Compiles the cell to a :class:`Model`, then dispatches on the subcommand,
    binding ``--name`` and honouring ``--quiet``. Raises
    :class:`ModelicaMagicError` for user-facing failures.
    """
    args = _parse_line(line)

    try:
        session = _native.Session(roots=_roots(args))
        model = session.loads(cell, model=args.model, filename=_CELL_FILENAME)
        if args.subcommand == "compile":
            result: Any = model
        elif args.subcommand == "sim":
            config = _native.SimConfig(
                solver=args.solver, rtol=args.rtol, atol=args.atol
            )
            result = model.simulate(t=args.t_end, dt=args.dt, config=config)
        elif args.subcommand == "export":
            if not args.export_target:
                raise ModelicaMagicError("export needs a target, e.g. `export casadi`")
            # The symbolic backends return a *live* typed object; any other
            # target returns its generated files (CodegenResult).
            live = {
                "casadi": model.to_casadi,
                "jax": model.to_jax,
                "sympy": model.to_sympy,
            }.get(args.export_target)
            result = live() if live else model.codegen(args.export_target)
        else:
            raise ModelicaMagicError(
                f"unknown subcommand {args.subcommand!r}; expected compile/sim/export"
            )
    except ModelicaMagicError:
        raise
    except Exception as error:  # surface compiler/sim errors as magic errors
        raise ModelicaMagicError(str(error)) from None

    if args.name:
        user_ns[args.name] = result

    if args.quiet:
        return None
    return result


def load_ipython_extension(ipython: Any) -> None:
    """Register the ``%%modelica`` magic (via ``%load_ext rumoca`` or on import).

    IPython is imported lazily here so ``import rumoca`` never requires IPython.
    """
    try:
        from IPython.core.error import UsageError
        from IPython.core.magic import Magics, cell_magic, magics_class
    except ModuleNotFoundError as error:
        raise ModelicaMagicError(
            "the %%modelica magic needs IPython; install it with "
            "`pip install rumoca[notebook]`"
        ) from error

    @magics_class
    class RumocaMagics(Magics):
        """IPython magics for compiling Modelica via the typed rumoca API."""

        @cell_magic
        def modelica(self, line: str, cell: str) -> Any:
            try:
                return run_modelica_cell(self.shell.user_ns, line, cell)
            except ModelicaMagicError as error:
                raise UsageError(str(error)) from None

    ipython.register_magics(RumocaMagics)


def unload_ipython_extension(ipython: Any) -> None:
    """Counterpart to :func:`load_ipython_extension` (best-effort)."""
    registry = getattr(ipython, "magics_manager", None)
    if registry is not None:
        registry.magics.get("cell", {}).pop("modelica", None)
