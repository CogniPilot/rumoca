"""Read reviewed source spans; the binding suite owns Markdown extraction."""

import hashlib
import json
from pathlib import Path
import shlex
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
    path = root / "docs/verification/user-guide-ci/non-modelica-inventory.json"
    data = json.loads(path.read_text(encoding="utf-8"))
    assert data["schema"] == 2
    return data


def inventory(root, mode):
    data = load_inventory(root)
    result = []
    for entry in data["entries"]:
        if entry["language"] != "bash":
            continue
        source = read_span(root, entry)
        if mode == "syntax":
            subprocess.run(["bash", "-n"], input=source, text=True, check=True, timeout=10)
        result.append({**entry, "source": source, "argv": commands(source)})
    assert len(result) == 66, len(result)
    return result


if __name__ == "__main__":
    print(json.dumps(inventory(Path(sys.argv[1]), sys.argv[2])))
