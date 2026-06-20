"""Smoke test for the verbatim-CLI `cli()` entry and the `%%modelica` magic.

Run directly (`python tests/magic_test.py`) against an installed/`maturin develop`
build, mirroring tests/smoke_test.py. The IPython magic portion is skipped if
IPython is not available so the core `cli()` checks still run everywhere.
"""

import json

import rumoca

BALL = "model Ball\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Ball;\n"


def test_cli_compile_returns_dae_dict() -> None:
    out = json.loads(rumoca.cli(["compile", "-m", "Ball", "Ball.mo"], BALL))
    # Same raw DAE the one-shot compile_source produces.
    direct = json.loads(rumoca.compile_source(BALL, "Ball", "Ball.mo"))
    assert out == direct
    assert "f_x" in out


def test_cli_emit_and_target() -> None:
    ir = json.loads(rumoca.cli(["compile", "-m", "Ball", "--emit", "solve-json", "Ball.mo"], BALL))
    assert isinstance(ir, dict)

    mo = json.loads(rumoca.cli(["compile", "-m", "Ball", "--emit", "dae-mo", "Ball.mo"], BALL))
    assert mo["format"] == "modelica"
    assert "Ball" in mo["source"]

    gen = json.loads(rumoca.cli(["compile", "-m", "Ball", "--target", "sympy", "Ball.mo"], BALL))
    assert gen["target"] == "sympy"
    assert any(f["path"].endswith(".py") for f in gen["files"])


def test_cli_sim_returns_payload() -> None:
    sim = json.loads(rumoca.cli(["sim", "-m", "Ball", "--t-end", "1", "Ball.mo"], BALL))
    assert sim["model"] == "Ball"
    assert "payload" in sim and "metrics" in sim


def test_cli_bad_flag_errors() -> None:
    try:
        rumoca.cli(["compile", "--nope", "Ball.mo"], BALL)
    except RuntimeError as error:
        assert "--nope" in str(error) or "unexpected" in str(error)
    else:
        raise AssertionError("unknown flag should raise")


def _ipython_shell():
    try:
        from IPython.core.interactiveshell import InteractiveShell
    except ModuleNotFoundError:
        return None
    shell = InteractiveShell.instance()
    rumoca.load_ipython_extension(shell)
    return shell


def test_magic_paths() -> None:
    ip = _ipython_shell()
    if ip is None:
        print("  (IPython not installed; skipping %%modelica magic checks)")
        return

    out = ip.run_cell_magic("modelica", "-m Ball", BALL)
    assert isinstance(out, dict) and "f_x" in out

    out = ip.run_cell_magic("modelica", "-m Ball --name dae", BALL)
    assert "dae" in ip.user_ns and isinstance(out, dict)

    sim = ip.run_cell_magic("modelica", "sim -m Ball --t-end 1", BALL)
    assert sim["model"] == "Ball" and "payload" in sim

    from IPython.core.error import UsageError

    try:
        ip.run_cell_magic("modelica", "-m Ball --nope", BALL)
    except UsageError:
        pass
    else:
        raise AssertionError("bad flag should raise UsageError")

    # --load without a --target codegen has no files to import.
    try:
        ip.run_cell_magic("modelica", "-m Ball --load", BALL)
    except UsageError:
        pass
    else:
        raise AssertionError("--load without --target should raise UsageError")


def main() -> None:
    test_cli_compile_returns_dae_dict()
    test_cli_emit_and_target()
    test_cli_sim_returns_payload()
    test_cli_bad_flag_errors()
    test_magic_paths()
    print("magic_test: OK")


if __name__ == "__main__":
    main()
