#!/usr/bin/env python3
"""Stage six real issued fixtures and strict pins; qualify them separately."""

import hashlib
import json
import os
from pathlib import Path
import re
import stat
import subprocess
import sys
import tempfile


SOURCE_MANIFEST_FORMAT = (
    "git ls-files order; sha256(raw regular-file bytes or raw symlink-target bytes), "
    "two spaces, path, newline"
)


def digest(data):
    return hashlib.sha256(data).hexdigest()


def require(condition, message):
    if not condition:
        raise ValueError(message)


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        require(key not in result, f"duplicate JSON key: {key}")
        result[key] = value
    return result


def load(data):
    def nonfinite(value):
        raise ValueError(f"nonfinite JSON number: {value}")
    # Python preserves JSON integers exactly. Fixture bytes are never reencoded.
    return json.loads(data, object_pairs_hook=unique_object, parse_constant=nonfinite)


def git(repository, *arguments):
    return subprocess.check_output(["git", *arguments], cwd=repository).decode()


def confined(directory, relative):
    require(isinstance(relative, str), "receipt path must be text")
    candidate = (directory / relative).resolve()
    require(not Path(relative).is_absolute(), "receipt path must be relative")
    require(candidate.is_relative_to(directory), f"receipt path escapes evidence: {relative}")
    return candidate


def roster(repository):
    directory = repository / "crates/rumoca-exec-wasm/tests/suite_typed_calls"
    result = {}
    for filename, model, destination, variants in [
        ("OrderedFrameFold.mo", "NativeOrderedFrameFold", "fixed_tables", ["baseline", "gain-two", "subtract"]),
        ("IntegerSeedFold.mo", "NativeIntegerSeedFold", "fixed_integer_tables", ["baseline"]),
        ("OrderedTensorFrameFold.mo", "NativeOrderedTensorFrameFold", "tensor_tables", ["baseline", "gain-two"]),
    ]:
        original = (directory / filename).read_bytes().decode()
        for variant in variants:
            source = original
            if variant != "baseline":
                before, after = ("gain = 1.0", "gain = 2.0") if variant == "gain-two" else (
                    "total + gain*values", "total - gain*values")
                require(source.count(before) == 1, f"changed source variant site: {filename}/{variant}")
                source = source.replace(before, after)
            relative = str((directory / destination / f"{variant}.json").relative_to(repository))
            result[relative] = (source, model, variant)
    return result


def verify_snapshot(repository, evidence, receipt, executable):
    require(not git(repository, "status", "--porcelain", "--untracked-files=normal").strip(), "checkout must be clean")
    require(git(repository, "rev-parse", "HEAD").strip() == receipt["compilerRevision"], "issuer source revision mismatch")
    tracked = git(repository, "ls-files", "-z").split("\0")[:-1]
    require(receipt["sourceManifestFormat"] == SOURCE_MANIFEST_FORMAT, "source manifest format mismatch")
    manifest = "".join(f"{digest(tracked_bytes(repository / name))}  {name}\n" for name in tracked).encode()
    require(manifest == (evidence / "compiler-source.sha256").read_bytes(), "tracked source manifest mismatch")
    require(digest(manifest) == receipt["compilerSourceSha256"], "compiler source digest mismatch")
    require(digest(executable.read_bytes()) == receipt["producerBinarySha256"], "actual producer executable digest mismatch")
    versions = []
    for filename, name in [("crates/rumoca-ir-dae/src/model.rs", "DAE"), ("crates/rumoca-ir-solve/src/lib.rs", "SOLVE")]:
        match = re.search(rf"pub const {name}_SCHEMA_VERSION: u16 = (\d+);", (repository / filename).read_text())
        require(match is not None, f"missing {name} schema owner")
        versions.append(int(match[1]))
    return versions


def tracked_bytes(path):
    mode = path.lstat().st_mode
    if stat.S_ISLNK(mode):
        return os.fsencode(os.readlink(path))
    if stat.S_ISREG(mode):
        return path.read_bytes()
    raise ValueError(f"unsupported tracked source entry type: {path}")


def verify_binders(value):
    source_count = 0
    if isinstance(value, dict):
        for binder in value.get("binders", []):
            if isinstance(binder, dict) and "display_name" in binder:
                provenance = binder["provenance"]  # Missing authority fails closed.
                require(provenance == "Generated" or (
                    isinstance(provenance, dict) and set(provenance) == {"Source"}
                    and type(provenance["Source"]) is int and 0 < provenance["Source"] < 2**32
                ), "invalid structured binder provenance")
                source_count += provenance != "Generated"
        source_count += sum(verify_binders(child) for child in value.values())
    elif isinstance(value, list):
        source_count += sum(verify_binders(child) for child in value)
    return source_count


def replacement_plans(repository, evidence, receipt, versions):
    expected = roster(repository)
    require(len(receipt["artifacts"]) == len(expected) == 6, "exact six-artifact roster required")
    plans = []
    for row in receipt["artifacts"]:
        require(row["fixture"] in expected, "unknown or duplicate fixture destination")
        source, model, variant = expected.pop(row["fixture"])
        source_bytes = confined(evidence, row["source"]).read_bytes()
        require(source_bytes == source.encode(), "original authored source/variant mismatch")
        require(digest(source_bytes) == row["sourceSha256"], "source receipt digest mismatch")
        raw = confined(evidence, row["artifact"]).read_bytes()
        require(digest(raw) == row["artifactSha256"], "issued artifact digest mismatch")
        issued = load(raw)
        for key in ["compilerRevision", "compilerSourceSha256", "producerBinarySha256"]:
            require(issued[key] == receipt[key], f"issued {key} mismatch")
        require(issued["source"] == source and issued["sourceSha256"] == digest(source_bytes), "artifact source identity mismatch")
        require((issued["model"], issued["variant"]) == (model, variant), "artifact model/variant mismatch")
        require([issued["daeSchemaVersion"], issued["solveSchemaVersion"]] == versions, "artifact schema owner mismatch")
        owners = issued["table"]["owners"]
        require(len(owners) == 1 and owners[0]["inputs"][0]["dimensions"] == [14400], "full original source owner missing")
        require(verify_binders(issued["table"]) > 0, "actual source iterator provenance missing")
        destination = confined(repository, row["fixture"])
        old = destination.read_bytes()
        require(load(old)["source"] == source, "historical fixture no longer matches authored source")
        plans.append((row["fixture"], destination, old, raw))
    require(not expected, "missing fixture destination")
    return plans


def pin_update(reader, receipt):
    text = reader.read_bytes().decode()
    old = {}
    for key, pattern in [
        ("compilerSourceSha256", r'(wire\["compilerSourceSha256"\],\s*")([0-9a-f]{64})(")'),
        ("producerBinarySha256", r'(wire\["producerBinarySha256"\],\s*")([0-9a-f]{64})(")'),
        ("compilerRevision", r'(issued_revision\(\s*json,\s*source,\s*")([^"]+)(")'),
    ]:
        matches = list(re.finditer(pattern, text))
        require(len(matches) == 1, f"expected exactly one strict {key} reader pin")
        old[key] = matches[0][2]
        value = receipt[key]
        require(re.fullmatch(r"[0-9a-f]{40}" if key == "compilerRevision" else r"[0-9a-f]{64}", value) is not None, "invalid observed pin format")
        text = re.sub(pattern, lambda match: match[1] + value + match[3], text)
    return text.encode(), old


def atomic_write(destination, raw):
    mode = destination.stat().st_mode & 0o777
    with tempfile.NamedTemporaryFile(dir=destination.parent, delete=False) as temporary:
        temporary.write(raw)
        temporary.flush()
        os.fsync(temporary.fileno())
        temporary_path = Path(temporary.name)
    try:
        temporary_path.chmod(mode)
        os.replace(temporary_path, destination)
    finally:
        temporary_path.unlink(missing_ok=True)


def main():
    require(len(sys.argv) in (4, 5), "REPOSITORY ISSUANCE_DIRECTORY FRESH_ARCHIVE_DIRECTORY [ISSUED_BINARY_COPY] required")
    repository, evidence, archive = [Path(value).resolve() for value in sys.argv[1:4]]
    require(not archive.is_relative_to(repository), "historical archive belongs outside the compiler checkout")
    receipt = load((evidence / "issuance.json").read_bytes())
    executable = Path(sys.argv[4]).resolve() if len(sys.argv) == 5 else Path(receipt["executable"])
    versions = verify_snapshot(repository, evidence, receipt, executable)
    plans = replacement_plans(repository, evidence, receipt, versions)
    reader = repository / "crates/rumoca-exec-wasm/tests/suite_typed_calls/source_tables.rs"
    old_reader = reader.read_bytes()
    new_reader, old_pins = pin_update(reader, receipt)
    for _, _, old, _ in plans:
        historical = load(old)
        require(all(historical[key] == value for key, value in old_pins.items()), "historical fixture and strict reader pins disagree")
    archive.mkdir()  # Validation precedes every archive/source write.
    records = []
    for relative, _, old, new in plans:
        historical = archive / relative
        historical.parent.mkdir(parents=True, exist_ok=True)
        historical.write_bytes(old)
        require(historical.read_bytes() == old, "historical archive copy mismatch")
        records.append({"fixture": relative, "historicalSha256": digest(old), "issuedSha256": digest(new)})
    (archive / "source_tables.rs.before").write_bytes(old_reader)
    (archive / "issuance.json").write_bytes((evidence / "issuance.json").read_bytes())
    (archive / "compiler-source.sha256").write_bytes((evidence / "compiler-source.sha256").read_bytes())
    for _, destination, _, new in plans:
        atomic_write(destination, new)
        require(destination.read_bytes() == new, "staged artifact byte mismatch")
    atomic_write(reader, new_reader)
    report = {"status": "STAGED_UNQUALIFIED", "issuerRevision": receipt["compilerRevision"],
              "historicalPins": old_pins, "issuedPins": {key: receipt[key] for key in old_pins},
              "historicalReaderSha256": digest(old_reader), "stagedReaderSha256": digest(new_reader),
              "fixtures": records, "requiredGate": "cargo test --locked --offline -p rumoca-exec-wasm --test typed_calls"}
    (archive / "replacement.json").write_text(json.dumps(report, indent=2) + "\n")
    print("Six fixtures and three strict pins staged; checked replay/executor qualification remains UNRUN")


if __name__ == "__main__":
    main()
