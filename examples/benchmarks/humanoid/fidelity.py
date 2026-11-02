"""Four-second task gate for the frozen walking humanoid.

This is a task comparison, not equation or trajectory parity with MuJoCo.
"""

import bisect
import csv
import math
from pathlib import Path

HERE = Path(__file__).resolve().parent
COLUMNS = ("time", "pelvis_x", "pelvis_z", "phase")


def read_trace(path):
    """Keep the last event sample at each time, refusing incomplete input."""
    samples = []
    with Path(path).open(newline="") as stream:
        reader = csv.DictReader(stream)
        if not set(COLUMNS).issubset(reader.fieldnames or ()):
            raise ValueError(f"{path}: missing task columns")
        for row in reader:
            values = tuple(float(row[key]) for key in COLUMNS)
            if not all(math.isfinite(value) for value in values):
                raise ValueError(f"{path}: non-finite task sample")
            if values[3] not in (0, 1, 2):
                raise ValueError(f"{path}: invalid gait phase")
            if samples and values[0] < samples[-1][0]:
                raise ValueError(f"{path}: time moved backwards")
            if samples and values[0] == samples[-1][0]:
                samples[-1] = values
            else:
                samples.append(values)
    if len(samples) < 2 or samples[0][0] > 0.01 or samples[-1][0] < 3.998:
        raise ValueError(f"{path}: incomplete four-second trajectory")
    return samples


def at(samples, time, column):
    times = [row[0] for row in samples]
    index = bisect.bisect_left(times, time)
    # The established gate permits the final sample to be one 2 ms step short.
    if index == len(samples):
        return samples[-1][column]
    if index == 0 or times[index] == time:
        return samples[index][column]
    left, right = samples[index - 1], samples[index]
    weight = (time - left[0]) / (right[0] - left[0])
    return left[column] + weight * (right[column] - left[column])


def metrics(samples):
    return {
        "end": samples[-1][0],
        "zmin": min(row[2] for row in samples),
        "z4": at(samples, 4.0, 2),
        "x35": at(samples, 3.5, 1),
        "steps": sum(
            row[3] != previous[3] and row[3] != 0
            for previous, row in zip(samples, samples[1:])
        ),
    }


def check(path, reference=HERE / "reference.csv"):
    expected = metrics(read_trace(reference))
    measured = metrics(read_trace(path))
    if expected["x35"] <= 0 or expected["zmin"] <= 0:
        raise ValueError("invalid reference task")
    checks = {
        "reaches_4s": measured["end"] >= 3.998,
        "no_fall": measured["zmin"] >= 0.5 * expected["zmin"],
        "upright_at_4s": abs(measured["z4"] - expected["z4"]) <= 0.05,
        "distance_15_percent": abs(measured["x35"] / expected["x35"] - 1) <= 0.15,
        "steps_within_2": abs(measured["steps"] - expected["steps"]) <= 2,
    }
    return {"pass": all(checks.values()), "checks": checks,
            "measured": measured, "reference": expected}
