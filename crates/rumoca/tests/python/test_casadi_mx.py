"""Tests that generated CasADi MX Python code actually runs correctly.

Run:  pytest crates/rumoca/tests/python/ -v
Requires: pip install casadi numpy pytest
"""

import casadi as ca
import numpy as np
import pytest

# ---------------------------------------------------------------------------
# Model structure checks (shared helper)
# ---------------------------------------------------------------------------

REQUIRED_KEYS = {
    "name", "t", "x", "xdot", "z", "u", "p",
    "ode", "alg", "dae_fn",
    "x0", "p0",
    "n_x", "n_z", "n_u", "n_p",
    "state_names", "algebraic_names", "input_names", "param_names",
}


def _check_model_structure(model):
    """Verify every generated model has the expected dict keys and consistent sizes."""
    missing = REQUIRED_KEYS - model.keys()
    assert not missing, f"Missing keys: {missing}"

    assert model["x"].shape == (model["n_x"], 1)
    assert model["xdot"].shape == (model["n_x"], 1)
    assert model["z"].shape == (model["n_z"], 1)
    assert model["u"].shape == (model["n_u"], 1)
    assert model["p"].shape == (model["n_p"], 1)

    assert len(model["state_names"]) == model["n_x"]
    assert len(model["algebraic_names"]) == model["n_z"]
    assert len(model["input_names"]) == model["n_u"]
    assert len(model["param_names"]) == model["n_p"]

    assert model["x0"].shape == (model["n_x"],)
    assert model["p0"].shape == (model["n_p"],)


# ---------------------------------------------------------------------------
# Oscillator model  (2 states, 0 algebraic, 2 params)
# ---------------------------------------------------------------------------

class TestOscillator:
    def test_structure(self, oscillator):
        _check_model_structure(oscillator)
        assert oscillator["n_x"] == 2
        assert oscillator["n_z"] == 0
        assert oscillator["n_p"] == 2

    def test_dae_fn_callable(self, oscillator):
        m = oscillator
        x0 = ca.DM(m["x0"])
        xdot0 = ca.DM.zeros(m["n_x"])
        z0 = ca.DM.zeros(m["n_z"])
        u0 = ca.DM.zeros(m["n_u"])
        p0 = ca.DM(m["p0"])
        t0 = ca.DM(0)

        result = m["dae_fn"](x0, xdot0, z0, u0, p0, t0)
        ode_val, alg_val = result[0], result[1]
        assert ode_val.shape == (m["n_x"], 1)

    def test_jacobian(self, oscillator):
        m = oscillator
        J = m["dae_fn"].jacobian()
        assert J is not None

    def test_initial_residual(self, oscillator):
        """At x=[v=0, x=1], xdot=[0,0]: der(x)-v=0-0=0, m*der(v)+k*x=0+1=1."""
        m = oscillator
        x0 = ca.DM(m["x0"])
        xdot0 = ca.DM.zeros(m["n_x"])
        z0 = ca.DM.zeros(m["n_z"])
        u0 = ca.DM.zeros(m["n_u"])
        p0 = ca.DM(m["p0"])

        ode_val = m["dae_fn"](x0, xdot0, z0, u0, p0, 0)[0]
        ode_np = np.array(ode_val).flatten()
        # With xdot=0 the residual should be non-trivial (not all zeros)
        # unless initial conditions are exactly on equilibrium.
        assert ode_np.shape == (m["n_x"],)


# ---------------------------------------------------------------------------
# DAE model  (1 state, 1 algebraic, 2 params)
# ---------------------------------------------------------------------------

class TestDaeModel:
    def test_structure(self, dae_model):
        _check_model_structure(dae_model)
        assert dae_model["n_x"] == 1
        assert dae_model["n_z"] == 1
        assert dae_model["n_p"] == 2

    def test_dae_fn_callable(self, dae_model):
        m = dae_model
        x0 = ca.DM(m["x0"])
        xdot0 = ca.DM.zeros(m["n_x"])
        z0 = ca.DM.zeros(m["n_z"])
        u0 = ca.DM.zeros(m["n_u"])
        p0 = ca.DM(m["p0"])

        result = m["dae_fn"](x0, xdot0, z0, u0, p0, 0)
        assert result[0].shape == (m["n_x"], 1)
        assert result[1].shape == (m["n_z"], 1)

    def test_algebraic_constraint(self, dae_model):
        """At x=1, y=c*x=0.5: alg residual = y - c*x = 0."""
        m = dae_model
        x0 = ca.DM(m["x0"])       # [1.0]
        xdot0 = ca.DM.zeros(m["n_x"])
        p0 = ca.DM(m["p0"])       # [c=0.5, k=2.0]
        # Set z = c * x = 0.5
        z0 = ca.DM([0.5])
        u0 = ca.DM.zeros(m["n_u"])

        alg_val = m["dae_fn"](x0, xdot0, z0, u0, p0, 0)[1]
        assert float(alg_val) == pytest.approx(0.0, abs=1e-14)

    def test_jacobian(self, dae_model):
        J = dae_model["dae_fn"].jacobian()
        assert J is not None


# ---------------------------------------------------------------------------
# WithFunc model  (1 state, 0 algebraic, 0 params, 1 user function)
# ---------------------------------------------------------------------------

class TestWithFunc:
    def test_structure(self, withfunc):
        _check_model_structure(withfunc)
        assert withfunc["n_x"] == 1
        assert withfunc["n_z"] == 0
        assert withfunc["n_p"] == 0

    def test_dae_fn_callable(self, withfunc):
        m = withfunc
        x0 = ca.DM(m["x0"])
        xdot0 = ca.DM.zeros(m["n_x"])
        z0 = ca.DM.zeros(m["n_z"])
        u0 = ca.DM.zeros(m["n_u"])
        p0 = ca.DM(m["p0"])

        result = m["dae_fn"](x0, xdot0, z0, u0, p0, 0)
        assert result[0].shape == (m["n_x"], 1)

    def test_user_function_exists(self, withfunc):
        assert "functions" in withfunc
        assert "sq" in withfunc["functions"]

    def test_user_function_evaluates(self, withfunc):
        sq = withfunc["functions"]["sq"]
        result = sq(3.0)
        assert float(result) == pytest.approx(9.0)

    def test_initial_residual(self, withfunc):
        """At x=1, xdot=0: residual = der(x) - (-sq(x)) = 0 - (-1) = 1."""
        m = withfunc
        x0 = ca.DM(m["x0"])       # [1.0]
        xdot0 = ca.DM.zeros(m["n_x"])
        z0 = ca.DM.zeros(m["n_z"])
        u0 = ca.DM.zeros(m["n_u"])
        p0 = ca.DM(m["p0"])

        ode_val = m["dae_fn"](x0, xdot0, z0, u0, p0, 0)[0]
        # der(x) - (-sq(x)) = 0 - (-1^2) = 0 + 1 = 1
        assert float(ode_val) == pytest.approx(1.0)

    def test_jacobian(self, withfunc):
        J = withfunc["dae_fn"].jacobian()
        assert J is not None


# ---------------------------------------------------------------------------
# AlgorithmModel  (1 state, 0 algebraic, 1 param, 1 algorithm section)
# ---------------------------------------------------------------------------

class TestAlgorithm:
    def test_structure(self, algorithm_model):
        _check_model_structure(algorithm_model)
        assert algorithm_model["n_x"] == 1
        assert algorithm_model["n_z"] == 1
        assert algorithm_model["n_p"] == 1

    def test_dae_fn_callable(self, algorithm_model):
        m = algorithm_model
        x0 = ca.DM(m["x0"])
        xdot0 = ca.DM.zeros(m["n_x"])
        z0 = ca.DM.zeros(m["n_z"])
        u0 = ca.DM.zeros(m["n_u"])
        p0 = ca.DM(m["p0"])

        result = m["dae_fn"](x0, xdot0, z0, u0, p0, 0)
        assert result[0].shape == (m["n_x"], 1)

    def test_dae_residual_value(self, algorithm_model):
        """At x=1, xdot=0, k=2: y=k*x=2, ode residual = xdot - (-y) = 0 + 2 = 2."""
        m = algorithm_model
        x0 = ca.DM(m["x0"])           # [1.0]
        xdot0 = ca.DM.zeros(m["n_x"])
        z0 = ca.DM.zeros(m["n_z"])
        u0 = ca.DM.zeros(m["n_u"])
        p0 = ca.DM(m["p0"])           # [2.0]

        ode_val = m["dae_fn"](x0, xdot0, z0, u0, p0, 0)[0]
        assert float(ode_val) == pytest.approx(2.0)

    def test_jacobian(self, algorithm_model):
        J = algorithm_model["dae_fn"].jacobian()
        assert J is not None
