"""Bind a Nix executable to its realized output and immutable source evaluation."""

import json
from pathlib import Path
import re
import shutil


def store_path(value):
    """Nix JSON v4 makes store paths relative; legacy output uses full paths."""
    path = value if value.startswith("/nix/store/") else "/nix/store/" + value
    if not re.fullmatch(r"/nix/store/[0-9a-z]{32}-[^/]+", path):
        raise ValueError("invalid Nix store path")
    return path


def package_record(document):
    if "version" in document:
        if document["version"] != 4:
            raise ValueError("unsupported Nix derivation JSON version")
        document = document["derivations"]
    if len(document) != 1:
        raise ValueError("expected one package derivation")
    drv, record = next(iter(document.items()))
    return store_path(drv), store_path(record["outputs"]["out"]["path"]), record


def retain(binary, derivation, metadata, source, output):
    """Require both raw Nix records; an absent embedded commit is not an identity."""
    if not derivation or not metadata or not re.fullmatch(r"[0-9a-f]{40}", source or ""):
        raise ValueError("Nix identity requires derivation, metadata and exact source SHA")
    for path, name in ((derivation, "rumoca-derivation.json"),
                       (metadata, "rumoca-flake.json")):
        shutil.copyfile(path, output / name)
    drv, package, record = package_record(json.loads(Path(derivation).read_text()))
    flake = json.loads(Path(metadata).read_text())
    src = record["env"]["src"]
    if (not drv.startswith("/nix/store/") or not drv.endswith(".drv")
            or not src.startswith("/nix/store/")
            or not package.startswith("/nix/store/")
            or Path(binary).resolve() != Path(package) / "bin/rumoca"):
        raise ValueError("executable does not match the Nix derivation output")
    locked = flake["locked"]
    if (locked.get("type") != "git" or locked.get("rev") != source
            or not locked.get("narHash") or flake.get("dirtyRevision")):
        raise ValueError("Nix flake does not bind the exact clean source revision")
    return {"method": "nix-derivation-and-immutable-flake", "derivation": drv,
            "output": package, "filtered_source": src, "source_revision": source,
            "source_nar_hash": locked["narHash"], "embedded_commit": "not queried"}
