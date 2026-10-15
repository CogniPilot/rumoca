"""Read reviewed source spans; the binding suite owns Markdown extraction."""

import hashlib
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import sys


def read_span(root, entry):
    page = root / "docs/user-guide/src" / entry["page"]
    lines = page.read_text(encoding="utf-8").splitlines()
    start, count = entry["line"] - 1, entry["source_line_count"]
    assert start >= 0 and count > 0 and start + count <= len(lines), entry
    source = "\n".join(lines[start : start + count]) + "\n"
    assert hashlib.sha256(source.encode()).hexdigest() == entry["source_sha256"], entry
    return source


def commands(source):
    # This extracts argv for Clap, never executes shell installation or redirects.
    result = []
    for line in source.replace("\\\n", "").splitlines():
        lexer = shlex.shlex(line, posix=True, punctuation_chars=";&|<>")
        lexer.whitespace_split = True
        lexer.commenters = "#"
        segments, current = [], []
        for token in lexer:
            if token and all(c in ";&|<>" for c in token):
                segments.append(current)
                current = []
            else:
                current.append(token)
        segments.append(current)
        for segment in segments:
            if segment and segment[0] == "rumoca":
                result.append(segment[1:])
            elif segment[:2] == ["cargo", "run"] and "--" in segment:
                result.append(segment[segment.index("--") + 1 :])
    return result


def load_inventory(root):
    path = root / "crates/rumoca-bind-wasm/src/tests/fixtures/user_guide/non-modelica-inventory.json"
    data = json.loads(path.read_text(encoding="utf-8"))
    assert data["schema"] == 2
    return data


def bash_executable(explicit_required):
    supplied = os.environ.get("BASH")
    if explicit_required and (not supplied or not Path(supplied).is_absolute()):
        raise RuntimeError("Windows guide syntax requires absolute BASH from the owning Git Bash shell")
    executable = shutil.which(supplied or "bash")
    if executable is None:
        raise RuntimeError(f"Required guide Bash executable is unavailable: {supplied or 'bash'}")
    return executable


def check_syntax(source, entry, executable):
    command = [executable, "-n"]
    location = f"{entry['page']}:{entry['line']}"
    result = subprocess.run(command, input=source.encode("utf-8"), capture_output=True, timeout=10)
    if result.returncode:
        raise RuntimeError(
            f"{location}: Bash syntax failed: {command!r}, exit={result.returncode}\n"
            f"stdout:\n{result.stdout.decode(errors='replace')}\n"
            f"stderr:\n{result.stderr.decode(errors='replace')}"
        )


def inventory(root, mode):
    data = load_inventory(root)
    executable = bash_executable(sys.platform == "win32") if mode == "syntax" else None
    result = []
    for entry in data["entries"]:
        if entry["language"] != "bash":
            continue
        source = read_span(root, entry)
        if mode == "syntax":
            check_syntax(source, entry, executable)
        result.append({**entry, "source": source, "argv": commands(source)})
    assert len(result) == 67, len(result)
    return result


if __name__ == "__main__":
    print(json.dumps(inventory(Path(sys.argv[1]), sys.argv[2])))
