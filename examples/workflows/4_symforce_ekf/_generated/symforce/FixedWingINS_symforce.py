from __future__ import annotations

import builtins
from typing import Any, Dict, Optional

import symforce

if not getattr(symforce, "_have_imported_symbolic", False):
    symforce.set_symbolic_api("symengine")
symforce.set_log_level("warning")
if not getattr(symforce, "_have_used_epsilon", False):
    symforce.set_epsilon_to_symbol()

import symforce.symbolic as sf
from symforce import codegen
from symforce.values import Values


def _sf_attr(name: str, fallback):
    return getattr(sf, name, fallback)


if not hasattr(sf, "And"):
    sf.And = lambda lhs, rhs: lhs & rhs
if not hasattr(sf, "Or"):
    sf.Or = lambda lhs, rhs: lhs | rhs
if not hasattr(sf, "Not"):
    sf.Not = lambda value: ~value


def _symbol(name: str):
    return sf.Symbol(name)


def _matrix_from_elements(dims, elements):
    if not dims:
        return elements[0]
    if len(dims) == 1:
        return sf.Matrix(int(dims[0]), 1, elements)
    if len(dims) == 2:
        return sf.Matrix(int(dims[0]), int(dims[1]), elements)
    return sf.Matrix(len(elements), 1, elements)


def _column(entries):
    return sf.Matrix(len(entries), 1, entries) if entries else sf.Matrix(0, 1, [])


def _sum(values):
    if isinstance(values, sf.Matrix):
        return builtins.sum(values)
    if isinstance(values, (list, tuple)):
        return builtins.sum(values)
    return values


def _mul_elem(lhs, rhs):
    if hasattr(lhs, "multiply_elementwise"):
        return lhs.multiply_elementwise(rhs)
    if hasattr(rhs, "multiply_elementwise"):
        return rhs.multiply_elementwise(lhs)
    return lhs * rhs


def size(value, dim=None):
    shape = getattr(value, "shape", None)
    if shape is None:
        try:
            return len(value)
        except TypeError:
            return 1
    if dim is None:
        return shape
    return shape[int(dim) - 1]


def if_else(condition, then_value, else_value):
    if condition is True:
        return then_value
    if condition is False:
        return else_value
    piecewise = getattr(sf, "Piecewise", None)
    if piecewise is not None:
        return piecewise((then_value, condition), (else_value, True))
    return then_value if bool(condition) else else_value


def pre(value):
    return _PRE_BY_SYMBOL.get(str(value), value)


def der(value):
    key = str(value)
    if key in _DER_BY_SYMBOL:
        return _DER_BY_SYMBOL[key]
    raise ValueError(f"der() called on non-state variable: {value}")


abs = _sf_attr("Abs", builtins.abs)
sign = _sf_attr("sign", lambda value: value / abs(value))
sqrt = _sf_attr("sqrt", None)
sin = _sf_attr("sin", None)
cos = _sf_attr("cos", None)
tan = _sf_attr("tan", None)
asin = _sf_attr("asin", None)
acos = _sf_attr("acos", None)
atan = _sf_attr("atan", None)
atan2 = _sf_attr("atan2", None)
sinh = _sf_attr("sinh", None)
cosh = _sf_attr("cosh", None)
tanh = _sf_attr("tanh", None)
exp = _sf_attr("exp", None)
log = _sf_attr("log", None)
log10 = lambda value: log(value, 10)
floor = _sf_attr("floor", None)
ceil = _sf_attr("ceiling", _sf_attr("ceil", None))
min = _sf_attr("Min", builtins.min)
max = _sf_attr("Max", builtins.max)
sum = _sum
zeros = lambda *dims: sf.Matrix.zeros(*(int(dim) for dim in dims))
ones = lambda *dims: sf.Matrix.ones(*(int(dim) for dim in dims))
identity = lambda dim: sf.Matrix.eye(int(dim))
transpose = lambda value: sf.Matrix(value).T
cross = lambda lhs, rhs: sf.Matrix(lhs).cross(sf.Matrix(rhs))
div = lambda lhs, rhs: floor(lhs / rhs)
mod = lambda lhs, rhs: lhs % rhs
rem = mod

_DER_BY_SYMBOL: Dict[str, Any] = {}
_PRE_BY_SYMBOL: Dict[str, Any] = {}


def create_model() -> Dict[str, Any]:
    _DER_BY_SYMBOL.clear()
    _PRE_BY_SYMBOL.clear()

    inputs = Values()
    outputs = Values()
    metadata = {
        "model_name": "FixedWingINS",
        "description": None,
        "states": ["px","py","pz","vx","vy","vz","qw","qx","qy","qz",],
        "algebraics": ["R11","R12","R13","R21","R22","R23","R31","R32","R33",],
        "inputs": ["wx","wy","wz","fx","fy","fz",],
        "outputs": ["ax","ay","az",],
        "parameters": ["g",],
        "discrete_reals": [],
        "discrete_valued": [],
    }

    time = _symbol("time")
    t = time
    inputs["time"] = time

    with inputs.scope("states"):
        px = _symbol("px")
        inputs["px"] = px
        py = _symbol("py")
        inputs["py"] = py
        pz = _symbol("pz")
        inputs["pz"] = pz
        vx = _symbol("vx")
        inputs["vx"] = vx
        vy = _symbol("vy")
        inputs["vy"] = vy
        vz = _symbol("vz")
        inputs["vz"] = vz
        qw = _symbol("qw")
        inputs["qw"] = qw
        qx = _symbol("qx")
        inputs["qx"] = qx
        qy = _symbol("qy")
        inputs["qy"] = qy
        qz = _symbol("qz")
        inputs["qz"] = qz

    with inputs.scope("derivatives"):
        der_px = _symbol("der(px)")
        _DER_BY_SYMBOL[str(px)] = der_px
        inputs["px"] = der_px
        der_py = _symbol("der(py)")
        _DER_BY_SYMBOL[str(py)] = der_py
        inputs["py"] = der_py
        der_pz = _symbol("der(pz)")
        _DER_BY_SYMBOL[str(pz)] = der_pz
        inputs["pz"] = der_pz
        der_vx = _symbol("der(vx)")
        _DER_BY_SYMBOL[str(vx)] = der_vx
        inputs["vx"] = der_vx
        der_vy = _symbol("der(vy)")
        _DER_BY_SYMBOL[str(vy)] = der_vy
        inputs["vy"] = der_vy
        der_vz = _symbol("der(vz)")
        _DER_BY_SYMBOL[str(vz)] = der_vz
        inputs["vz"] = der_vz
        der_qw = _symbol("der(qw)")
        _DER_BY_SYMBOL[str(qw)] = der_qw
        inputs["qw"] = der_qw
        der_qx = _symbol("der(qx)")
        _DER_BY_SYMBOL[str(qx)] = der_qx
        inputs["qx"] = der_qx
        der_qy = _symbol("der(qy)")
        _DER_BY_SYMBOL[str(qy)] = der_qy
        inputs["qy"] = der_qy
        der_qz = _symbol("der(qz)")
        _DER_BY_SYMBOL[str(qz)] = der_qz
        inputs["qz"] = der_qz

    with inputs.scope("algebraics"):
        R11 = _symbol("R11")
        inputs["R11"] = R11
        R12 = _symbol("R12")
        inputs["R12"] = R12
        R13 = _symbol("R13")
        inputs["R13"] = R13
        R21 = _symbol("R21")
        inputs["R21"] = R21
        R22 = _symbol("R22")
        inputs["R22"] = R22
        R23 = _symbol("R23")
        inputs["R23"] = R23
        R31 = _symbol("R31")
        inputs["R31"] = R31
        R32 = _symbol("R32")
        inputs["R32"] = R32
        R33 = _symbol("R33")
        inputs["R33"] = R33

    with inputs.scope("model_outputs"):
        ax = _symbol("ax")
        inputs["ax"] = ax
        ay = _symbol("ay")
        inputs["ay"] = ay
        az = _symbol("az")
        inputs["az"] = az

    with inputs.scope("external_inputs"):
        wx = _symbol("wx")
        inputs["wx"] = wx
        wy = _symbol("wy")
        inputs["wy"] = wy
        wz = _symbol("wz")
        inputs["wz"] = wz
        fx = _symbol("fx")
        inputs["fx"] = fx
        fy = _symbol("fy")
        inputs["fy"] = fy
        fz = _symbol("fz")
        inputs["fz"] = fz

    with inputs.scope("parameters"):
        g = _symbol("g")
        inputs["g"] = g

    with inputs.scope("constants"):
        pass

    with inputs.scope("discrete_reals"):
        pass

    with inputs.scope("discrete_valued"):
        pass

    with inputs.scope("pre"):
        pass

    with inputs.scope("conditions"):
        pass

    f_x = _column([
        (R11 - (1 - (2 * ((qy * qy) + (qz * qz))))),
        (R12 - (2 * ((qx * qy) - (qw * qz)))),
        (R13 - (2 * ((qx * qz) + (qw * qy)))),
        (R21 - (2 * ((qx * qy) + (qw * qz)))),
        (R22 - (1 - (2 * ((qx * qx) + (qz * qz))))),
        (R23 - (2 * ((qy * qz) - (qw * qx)))),
        (R31 - (2 * ((qx * qz) - (qw * qy)))),
        (R32 - (2 * ((qy * qz) + (qw * qx)))),
        (R33 - (1 - (2 * ((qx * qx) + (qy * qy))))),
        (ax - (((R11 * fx) + (R12 * fy)) + (R13 * fz))),
        (ay - (((R21 * fx) + (R22 * fy)) + (R23 * fz))),
        (az - ((((R31 * fx) + (R32 * fy)) + (R33 * fz)) - g)),
        (der(px) - vx),
        (der(py) - vy),
        (der(pz) - vz),
        (der(vx) - ax),
        (der(vy) - ay),
        (der(vz) - az),
        (der(qw) - (0.5 * (((-(qx * wx)) - (qy * wy)) - (qz * wz)))),
        (der(qx) - (0.5 * (((qw * wx) + (qy * wz)) - (qz * wy)))),
        (der(qy) - (0.5 * (((qw * wy) - (qx * wz)) + (qz * wx)))),
        (der(qz) - (0.5 * (((qw * wz) + (qx * wy)) - (qy * wx)))),
    ])
    outputs["f_x"] = f_x

    initial_residuals = _column([
    ])
    outputs["initial_residuals"] = initial_residuals

    f_z = _column([
    ])
    outputs["f_z"] = f_z

    f_m = _column([
    ])
    outputs["f_m"] = f_m

    relation_residuals = _column([
    ])
    outputs["relation_residuals"] = relation_residuals

    return {
        "inputs": inputs,
        "outputs": outputs,
        "metadata": metadata,
        "f_x": f_x,
        "initial_residuals": initial_residuals,
        "f_z": f_z,
        "f_m": f_m,
        "relation_residuals": relation_residuals,
    }


def make_residual_codegen(
    config: Optional[Any] = None,
    name: str = "FixedWingINS_dae_residual",
) -> codegen.Codegen:
    model = create_model()
    if config is None:
        config = codegen.CppConfig()
    return codegen.Codegen(
        inputs=model["inputs"],
        outputs=Values(f_x=model["f_x"]),
        config=config,
        name=name,
        return_key="f_x",
    )


def make_residual_jacobian_codegen(
    which_args=None,
    config: Optional[Any] = None,
    name: str = "FixedWingINS_dae_residual_jacobians",
) -> codegen.Codegen:
    return make_residual_codegen(config=config).with_jacobians(
        which_args=which_args,
        include_results=True,
        name=name,
    )


def generate_residual_code(
    output_dir=None,
    config: Optional[Any] = None,
    namespace: str = "sym",
    name: str = "FixedWingINS_dae_residual",
):
    residual_codegen = make_residual_codegen(config=config, name=name)
    return residual_codegen.generate_function(
        output_dir=output_dir,
        namespace=namespace,
        generated_file_name=name,
    )


if __name__ == "__main__":
    model = create_model()
    print("SymForce DAE model: FixedWingINS")
    print("states:", model["metadata"]["states"])
    print("parameters:", model["metadata"]["parameters"])
    print("residual rows:", model["f_x"].shape[0])