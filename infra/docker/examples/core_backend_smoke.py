from __future__ import annotations

import math

import casadi as ca
import onnx
import onnx.helper as oh
import onnxruntime as ort
import sympy as sp
import torch


def run_casadi_dae_smoke() -> None:
    x = ca.MX.sym("x")
    z = ca.MX.sym("z")
    p = ca.MX.sym("p")

    dae = {"x": x, "z": z, "p": p, "ode": z, "alg": z + p}
    integrator = ca.integrator("dae_step", "idas", dae, 0.0, 0.1, {})
    result = integrator(x0=1.0, p=2.0)

    xf = float(result["xf"])
    zf = float(result["zf"])

    assert math.isclose(xf, 0.8, rel_tol=0.0, abs_tol=1e-6), xf
    assert math.isclose(zf, -2.0, rel_tol=0.0, abs_tol=1e-6), zf


def run_onnx_smoke() -> None:
    input_info = oh.make_tensor_value_info("x", onnx.TensorProto.FLOAT, [1])
    output_info = oh.make_tensor_value_info("y", onnx.TensorProto.FLOAT, [1])
    node = oh.make_node("Identity", inputs=["x"], outputs=["y"])
    graph = oh.make_graph([node], "identity_graph", [input_info], [output_info])
    model = oh.make_model(
        graph,
        producer_name="rumoca-docker-smoke",
        ir_version=10,
        opset_imports=[oh.make_operatorsetid("", 13)],
    )
    onnx.checker.check_model(model)
    session = ort.InferenceSession(
        model.SerializeToString(), providers=["CPUExecutionProvider"]
    )
    output = session.run(None, {"x": [3.25]})[0]
    assert math.isclose(float(output[0]), 3.25, rel_tol=0.0, abs_tol=1e-6), output


def run_torch_smoke() -> None:
    state = torch.tensor([1.0, -2.0], dtype=torch.float64)
    step = torch.tensor([0.1, 0.0], dtype=torch.float64)
    next_state = state + step
    assert torch.allclose(next_state, torch.tensor([1.1, -2.0], dtype=torch.float64))


def run_sympy_smoke() -> None:
    x, z, p = sp.symbols("x z p")
    expr = sp.diff(x**2 + z + p, x)
    assert expr == 2 * x


def main() -> None:
    run_casadi_dae_smoke()
    run_onnx_smoke()
    run_torch_smoke()
    run_sympy_smoke()
    print("core backend smoke passed")


if __name__ == "__main__":
    main()
