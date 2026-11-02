"""Fail closed on missing or mismatched external package identity."""

import copy
import json
from pathlib import Path
import tempfile
import unittest

import package_identity


class PackageIdentityTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.output = self.root / "output"
        self.output.mkdir()
        self.drv = self.root / "derivation.json"
        self.meta = self.root / "flake.json"
        self.sha = "a" * 40
        self.package = "/nix/store/" + "b" * 32 + "-rumoca"
        self.key = "/nix/store/" + "c" * 32 + "-rumoca.drv"
        self.record = {self.key: {"outputs": {"out": {"path": self.package}},
                                 "env": {"src": "/nix/store/" + "d" * 32 + "-rumoca-src"}}}
        self.flake = {"locked": {"type": "git", "rev": self.sha,
                                  "narHash": "sha256-retained-source"}}

    def bind(self, record=None, flake=None, binary=None, source=None):
        self.drv.write_text(json.dumps(self.record if record is None else record))
        self.meta.write_text(json.dumps(self.flake if flake is None else flake))
        return package_identity.retain(
            Path(binary or self.package + "/bin/rumoca"), self.drv, self.meta,
            self.sha if source is None else source, self.output)

    def test_exact_output_and_source_retains_both_raw_records(self):
        result = self.bind()
        self.assertEqual(result["source_revision"], self.sha)
        self.assertEqual(result["output"], self.package)
        self.assertEqual((self.output / "rumoca-derivation.json").read_bytes(), self.drv.read_bytes())
        self.assertEqual((self.output / "rumoca-flake.json").read_bytes(), self.meta.read_bytes())

    def test_other_executable_cannot_borrow_the_package_identity(self):
        for binary in (self.package + "/bin/other", "/tmp/rumoca",
                       "/nix/store/" + "e" * 32 + "-rumoca/bin/rumoca"):
            with self.subTest(binary=binary), self.assertRaises(ValueError):
                self.bind(binary=binary)

    def test_nix_v4_relative_store_paths_bind_the_same_package(self):
        record = copy.deepcopy(self.record[self.key])
        record["outputs"]["out"]["path"] = Path(self.package).name
        result = self.bind(record={"version": 4, "derivations": {Path(self.key).name: record}})
        self.assertEqual(result["derivation"], self.key)
        self.assertEqual(result["output"], self.package)
        with self.assertRaises(ValueError):
            self.bind(record={"version": 99, "derivations": self.record})

    def test_wrong_dirty_or_unpinned_source_is_refused(self):
        for locked in ({**self.flake["locked"], "rev": "f" * 40},
                       {**self.flake["locked"], "type": "path"},
                       {"type": "git", "rev": self.sha}):
            with self.subTest(locked=locked), self.assertRaises(ValueError):
                self.bind(flake={"locked": locked})
        with self.assertRaises(ValueError):
            self.bind(flake={**self.flake, "dirtyRevision": "dirty"})
        with self.assertRaises(ValueError):
            self.bind(source="short")

    def test_missing_half_of_identity_is_refused(self):
        for drv, meta in ((None, self.meta), (self.drv, None)):
            with self.subTest(drv=drv), self.assertRaises(ValueError):
                package_identity.retain(Path(self.package + "/bin/rumoca"),
                                        drv, meta, self.sha, self.output)

    def test_no_or_ambiguous_derivation_is_refused(self):
        for record in ({}, {**self.record, self.key + "other": self.record[self.key]}):
            with self.subTest(record=record), self.assertRaises(ValueError):
                self.bind(record=record)
        malformed = copy.deepcopy(self.record)
        malformed[self.key]["env"]["src"] = "/tmp/source"
        with self.assertRaises(ValueError):
            self.bind(record=malformed)


if __name__ == "__main__":
    unittest.main()
