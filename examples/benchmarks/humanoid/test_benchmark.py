"""Behavioral controls for a benchmark whose speed must never gate a PR."""

import csv
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

import fidelity
import run


class FidelityTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name) / "trace.csv"
        with (fidelity.HERE / "reference.csv").open(newline="") as stream:
            self.rows = list(csv.DictReader(stream))

    def write(self, rows=None):
        with self.path.open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=fidelity.COLUMNS)
            writer.writeheader()
            writer.writerows(self.rows if rows is None else rows)
        return self.path

    def test_reference_passes(self):
        self.assertTrue(fidelity.check(self.write())["pass"])

    def test_each_established_threshold_rejects_its_violation(self):
        for name, key, change in [
            ("no_fall", "pelvis_z", lambda row: "0.1" if float(row["time"]) < 1 else row["pelvis_z"]),
            ("upright_at_4s", "pelvis_z", lambda row: str(float(row["pelvis_z"]) + 0.051)),
            ("distance_15_percent", "pelvis_x", lambda row: str(float(row["pelvis_x"]) * 1.151)),
            ("steps_within_2", "phase", lambda row: "0"),
        ]:
            with self.subTest(name=name):
                changed = [{**row, key: change(row)} for row in self.rows]
                result = fidelity.check(self.write(changed))
                self.assertFalse(result["pass"])
                self.assertFalse(result["checks"][name])

    def test_short_trace_cannot_clamp_to_a_pass(self):
        with self.assertRaisesRegex(ValueError, "incomplete"):
            fidelity.check(self.write([row for row in self.rows if float(row["time"]) <= 2]))

    def test_missing_start_cannot_pass(self):
        with self.assertRaisesRegex(ValueError, "incomplete"):
            fidelity.check(self.write(self.rows[20:]))

    def test_nan_and_invalid_phase_fail_closed(self):
        for key, value in [("pelvis_z", "nan"), ("phase", "inf"), ("phase", "3")]:
            with self.subTest(key=key, value=value):
                rows = [dict(row) for row in self.rows]
                rows[10][key] = value
                with self.assertRaises(ValueError):
                    fidelity.check(self.write(rows))

    def test_missing_columns_and_backwards_time_are_refused(self):
        self.path.write_text("time,pelvis_x\n0,0\n4,1\n")
        with self.assertRaisesRegex(ValueError, "missing"):
            fidelity.check(self.path)
        rows = list(self.rows)
        rows[15], rows[20] = rows[20], rows[15]
        with self.assertRaisesRegex(ValueError, "backwards"):
            fidelity.check(self.write(rows))

    def test_duplicate_event_times_use_the_last_row(self):
        self.path.write_text("time,pelvis_x,pelvis_z,phase\n0,0,0.8,0\n"
                             "1,0,0.8,0\n1,0,0.8,1\n4,0.6,0.8,1\n")
        samples = fidelity.read_trace(self.path)
        self.assertEqual(len(samples), 3)
        self.assertEqual(fidelity.metrics(samples)["steps"], 1)


class RunnerTests(unittest.TestCase):
    def test_frozen_inputs_hash_exactly(self):
        run.verify_inputs()

    def test_generator_reproduces_frozen_mjcf(self):
        with tempfile.TemporaryDirectory() as directory:
            subprocess.run([sys.executable, str(run.HERE / "generate.py"),
                            str(run.HERE / "HumanoidPhysical.mo"), directory],
                           check=True, capture_output=True, timeout=10)
            expected = json.loads((run.HERE / "provenance.json").read_text())
            self.assertEqual(run.digest(Path(directory) / "twin_native_act.xml"),
                             expected["generated_mjcf_sha256"])

    def test_exit_code_is_propagated_and_raw_failure_retained(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "failed"
            with self.assertRaisesRegex(RuntimeError, "command failed"):
                run.execute([sys.executable, "-c", "print('witness');raise SystemExit(17)"], output)
            self.assertIn("witness", output.with_suffix(".stdout").read_text())
            self.assertEqual(json.loads(output.with_suffix(".command.json").read_text())["returncode"], 17)

    def test_timeout_kills_owned_descendant(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "timeout"
            pidfile = Path(directory) / "child.pid"
            script = ("import subprocess,sys,time,pathlib;"
                      "p=subprocess.Popen([sys.executable,'-c','import time;time.sleep(30)']);"
                      f"pathlib.Path({str(pidfile)!r}).write_text(str(p.pid));time.sleep(30)")
            with self.assertRaises(subprocess.TimeoutExpired):
                run.execute([sys.executable, "-c", script], output, timeout=1)
            self.assertEqual(json.loads(output.with_suffix(".command.json").read_text())["failure"],
                             "TimeoutExpired")
            pid = int(pidfile.read_text())
            try:
                status = Path(f"/proc/{pid}/stat").read_text().split()[2]
            except FileNotFoundError:
                status = "gone"
            self.assertIn(status, ("Z", "gone"))

    def test_speed_ratio_has_no_threshold_or_fidelity_override(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            data = {"speed": {"rumoca_s_per_sim_s": 1000, "mujoco_s_per_sim_s": 0.01,
                              "ratio_rumoca_over_mujoco": 100000,
                              "rumoca": {"compile_seconds": 20, "prepare_seconds": 10}},
                    "fidelity": {"rumoca": {"pass": True}, "mujoco": {"pass": False}}}
            with patch.dict(os.environ, {"GITHUB_STEP_SUMMARY": str(output / "job.md")}):
                run.report(data, output)
            self.assertEqual(json.loads((output / "result.json").read_text()), data)
            summary = (output / "job.md").read_text()
            self.assertIn("100000.000x", summary)
            self.assertIn("no speed threshold", summary)
            self.assertIn("mujoco FAIL", summary)


if __name__ == "__main__":
    unittest.main()
