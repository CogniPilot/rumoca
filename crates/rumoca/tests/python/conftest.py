"""Pytest configuration for CasADi MX generated code tests.

Requires: pip install casadi numpy pytest
"""

import importlib.util
import sys
from pathlib import Path

import pytest

EXPECTED_DIR = Path(__file__).resolve().parent.parent / "expected"


def _load_module(name: str, path: Path):
    """Import a .py file as a module by path."""
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


@pytest.fixture
def oscillator():
    mod = _load_module("oscillator_mx", EXPECTED_DIR / "oscillator_mx.py")
    return mod.create_model()


@pytest.fixture
def dae_model():
    mod = _load_module("dae_model_mx", EXPECTED_DIR / "dae_model_mx.py")
    return mod.create_model()


@pytest.fixture
def withfunc():
    mod = _load_module("withfunc_mx", EXPECTED_DIR / "withfunc_mx.py")
    return mod.create_model()


@pytest.fixture
def algorithm_model():
    mod = _load_module("algorithm_mx", EXPECTED_DIR / "algorithm_mx.py")
    return mod.create_model()
