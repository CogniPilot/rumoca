"""The ``%%modelica`` Jupyter cell magic.

Write Modelica directly in a notebook cell and compile / generate code / simulate
it with the *verbatim* rumoca CLI — the magic line takes the same arguments you
would pass to ``rumoca compile`` / ``rumoca sim`` (the cell body replaces the
``<MODELICA_FILE>`` positional). The compiler runs in-process via
:func:`rumoca._native.cli`, which parses the line with the exact same clap
definitions as the binary, so there is zero drift between the CLI and the magic.

The result is returned as a structured Python ``dict`` (easy to feed into the
next cell), not the raw text the CLI prints.

Two notebook-only flags are layered on top of the CLI flags:

* ``--name VAR`` / ``-n VAR`` — also bind the result into the notebook namespace.
* ``--load``                  — for a ``--target`` Python codegen (sympy, jax, …),
                                write the generated file(s) and ``import`` the
                                module, returning it (real file ⇒ real
                                tracebacks). Binds to ``VAR`` when paired with
                                ``--name``.
* ``--quiet`` / ``-q``        — return ``None`` so the cell renders no output;
                                the ``--name`` binding (and ``--load`` import)
                                still happen as a side effect.

Examples::

    %%modelica -m BouncingBall
    model BouncingBall ... end BouncingBall;

    %%modelica -m Plant --source-root libs/MSL --emit dae-json --name dae
    model Plant ... end Plant;

    %%modelica sim -m Decay --t-end 10 --solver auto --name run
    model Decay ... end Decay;

    %%modelica -m Plant --target sympy --load --name plant
    model Plant ... end Plant;

IPython is an optional dependency: this module imports without it, and IPython is
only needed when :func:`load_ipython_extension` actually registers the magic
(which only happens inside a running IPython/Jupyter kernel).
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import shlex
import sys
import tempfile
from hashlib import blake2b
from pathlib import Path
from types import ModuleType
from typing import Any

from . import _native

# The cell body stands in for the CLI's required <MODELICA_FILE> positional. A
# ``.mo`` name keeps model-name inference and diagnostic spans consistent with a
# real file.
_CELL_FILENAME = "cell.mo"
_CLI_SUBCOMMANDS = ("compile", "sim")

# Per-process root for `--load` codegen, created lazily. `mkdtemp` gives a
# unique, 0700, owner-only directory, so generated modules are never written to
# a predictable shared path that another local user could pre-create.
_GENERATED_ROOT: Path | None = None


class ModelicaMagicError(Exception):
    """A user-facing error from the ``%%modelica`` magic.

    Raised by the IPython-independent core so the IPython wrapper can render it
    as a clean ``UsageError`` without this module importing IPython at all.
    """


def _generated_root() -> Path:
    global _GENERATED_ROOT
    if _GENERATED_ROOT is None:
        _GENERATED_ROOT = Path(tempfile.mkdtemp(prefix="rumoca_generated_"))
    return _GENERATED_ROOT


def _split_notebook_flags(line: str) -> tuple[list[str], argparse.Namespace]:
    """Peel the notebook-only flags off the line, leaving verbatim CLI args.

    Uses ``parse_known_args`` so every flag the magic does not own is passed
    through untouched (and in order) to the rumoca CLI parser.
    """
    parser = argparse.ArgumentParser(add_help=False, allow_abbrev=False)
    parser.add_argument("--name", "-n", default=None)
    parser.add_argument("--load", action="store_true")
    parser.add_argument("--quiet", "-q", action="store_true")
    notebook, cli_args = parser.parse_known_args(shlex.split(line))
    return cli_args, notebook


def _build_cli_args(cli_args: list[str]) -> list[str]:
    """Prepend the default ``compile`` subcommand and supply the cell filename.

    The cell body is the source, so the magic owns the CLI's required
    ``<MODELICA_FILE>`` positional. Only append it when the user did not already
    pass one (a ``.mo`` token), so an explicit name still works and clap never
    sees two positionals.
    """
    if not cli_args or cli_args[0] not in _CLI_SUBCOMMANDS:
        cli_args = ["compile", *cli_args]
    if not any(arg.endswith(".mo") for arg in cli_args):
        cli_args = [*cli_args, _CELL_FILENAME]
    return cli_args


def _import_generated_module(result: dict[str, Any]) -> ModuleType:
    """Write a ``--target`` codegen result to disk and import its Python module.

    Imports from a real file (under a managed temp dir, with a content-derived
    unique module name) so tracebacks point at actual source lines and repeated
    generations don't collide in ``sys.modules``.
    """
    files = result.get("files")
    if not files:
        raise ModelicaMagicError(
            "--load needs a `--target` codegen result with generated files; "
            "got nothing to import"
        )
    py_files = [f for f in files if str(f.get("path", "")).endswith(".py")]
    if not py_files:
        target = result.get("target", "?")
        raise ModelicaMagicError(
            f"--load only supports Python targets (e.g. sympy, jax); target "
            f"'{target}' produced no .py files"
        )

    # Hash all file contents so regenerating identical code reuses the module and
    # any change yields a fresh one.
    digest = blake2b(digest_size=8)
    for file in files:
        digest.update(str(file.get("path", "")).encode())
        digest.update(b"\0")
        digest.update(str(file.get("content", "")).encode())
        digest.update(b"\0")
    stamp = digest.hexdigest()

    out_dir = _generated_root() / stamp
    out_dir.mkdir(parents=True, exist_ok=True)
    for file in files:
        path = out_dir / str(file["path"])
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(str(file.get("content", "")))

    primary = out_dir / str(py_files[0]["path"])
    module_name = f"rumoca_generated_{primary.stem}_{stamp}"

    spec = importlib.util.spec_from_file_location(module_name, primary)
    if spec is None or spec.loader is None:
        raise ModelicaMagicError(f"could not load generated module from {primary}")
    module = importlib.util.module_from_spec(spec)
    # Register before exec so the module can reference itself (and tracebacks
    # resolve) during import.
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


def run_modelica_cell(user_ns: dict[str, Any], line: str, cell: str) -> Any:
    """Core dispatch for ``%%modelica`` — no IPython required.

    Parses the line, runs the verbatim CLI on the cell source, and returns the
    structured result (optionally importing a ``--load`` module and binding
    ``--name``). Raises :class:`ModelicaMagicError` for user-facing failures.
    """
    cli_args, notebook = _split_notebook_flags(line)
    argv = _build_cli_args(cli_args)

    try:
        raw = _native.cli(argv, cell)
    except RuntimeError as error:
        raise ModelicaMagicError(str(error)) from None

    result: Any = json.loads(raw)

    if notebook.load:
        result = _import_generated_module(result)

    if notebook.name:
        user_ns[notebook.name] = result

    if notebook.quiet:
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
        """IPython magics for compiling Modelica via the rumoca CLI."""

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
