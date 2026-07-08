"""Type stubs for the first-class Rumoca Python API (see the package docstring).

This stub is the public contract; the runtime objects are PyO3 classes backed
directly by the Rust compiler/solver. Optional numpy/pandas/matplotlib returns
are typed as ``Any`` so the stub never forces those imports.
"""

from __future__ import annotations

from os import PathLike
from pathlib import Path
from typing import Any, Callable, Literal, Mapping, Sequence, TypedDict

from ._magic import (
    load_ipython_extension as load_ipython_extension,
    unload_ipython_extension as unload_ipython_extension,
)

Stage = Literal["ast", "flat", "dae", "solve"]
Form = Literal["dae", "solve"]
Solver = Literal["auto", "rk-like", "bdf", "esdirk34", "trbdf2"] | str
Level = Literal["error", "warning", "note", "help"]
ParamKind = Literal["tunable", "structural"]
Input = float | tuple[Any, Any] | Callable[[float], float]
Pathish = str | Path | PathLike[str]

__all__: Sequence[str]

# ── module-level stateless tools ────────────────────────────────────────────
def validate(path: Pathish, *, model: str | None = ...) -> list[Diagnostic]: ...
def validate_source(
    source: str, *, model: str | None = ..., filename: str | None = ...
) -> list[Diagnostic]: ...
def format(src: str, *, filename: str | None = ...) -> str: ...
def version() -> str: ...
def targets() -> list[Target]: ...
def solvers() -> list[SolverInfo]: ...

# ── session ─────────────────────────────────────────────────────────────────
class Session:
    roots: list[str]
    def __init__(
        self,
        roots: Sequence[str] | None = ...,
        *,
        workspace: str | None = ...,
    ) -> None: ...
    def load(self, path: Pathish, *, model: str | None = ...) -> Model: ...
    def loads(
        self, source: str, *, model: str | None = ..., filename: str | None = ...
    ) -> Model: ...
    @classmethod
    def from_scenario(cls, path: Pathish) -> tuple[Session, Model, SimConfig]: ...
    def run_scenario(
        self,
        path: Pathish,
        *,
        overrides: ScenarioOverrides | None = ...,
    ) -> ScenarioResult: ...
    def codegen_file(
        self,
        path: Pathish,
        model: str,
        target: str,
        output: Pathish,
        *,
        roots: Sequence[str] | None = ...,
    ) -> list[str]: ...
    def clear(self) -> None: ...

# ── the hub ─────────────────────────────────────────────────────────────────
class Model:
    name: str
    states: VarView
    algebraics: VarView
    inputs: VarView
    outputs: VarView
    parameters: ParamView
    def summary(self) -> str: ...
    def structure(self) -> StructuralInfo: ...
    def to_dict(self, stage: Stage = ...) -> dict[str, Any]: ...
    def to_json(self, stage: Stage = ..., *, pretty: bool = ...) -> str: ...
    def save_json(self, path: Pathish, stage: Stage = ...) -> None: ...
    def render(self, target: str) -> str: ...
    def codegen(self, target: str) -> CodegenResult: ...
    def to_casadi(
        self, form: Form = ..., *, mode: Literal["mx", "sx"] = ...
    ) -> CasadiModel | SolveExport: ...
    def to_jax(self, form: Form = ...) -> JaxModel | SolveExport: ...
    def to_sympy(self) -> SympyModel: ...
    def with_params(self, *, recompile: bool = ..., **overrides: float) -> Model: ...
    def with_start(self, **overrides: float) -> Model: ...
    def simulate(
        self,
        t: float | tuple[float, float] | Any = ...,
        *,
        dt: float | None = ...,
        config: SimConfig | None = ...,
        params: Mapping[str, float] | None = ...,
        start: Mapping[str, float] | None = ...,
        inputs: Mapping[str, Input] | None = ...,
    ) -> Result: ...
    def objective_gradient(
        self,
        objective: str,
        *,
        state: Mapping[str, float] | None = ...,
        t: float = ...,
        mode: Literal["forward", "adjoint", "reverse"] = ...,
    ) -> GradientResult: ...
    def __repr__(self) -> str: ...
    def _repr_html_(self) -> str: ...

class SimConfig:
    solver: str | None
    rtol: float | None
    atol: float | None
    dt: float | None
    max_wall_seconds: float | None
    def __init__(
        self,
        *,
        solver: Solver | None = ...,
        rtol: float | None = ...,
        atol: float | None = ...,
        dt: float | None = ...,
        max_wall_seconds: float | None = ...,
    ) -> None: ...

# ── views ───────────────────────────────────────────────────────────────────
class VarView(Sequence[VariableInfo]):
    names: list[str]
    def __getitem__(self, key: str | int) -> VariableInfo: ...  # type: ignore[override]
    def __len__(self) -> int: ...
    def __contains__(self, key: object) -> bool: ...

class ParamView(Sequence[ParameterInfo]):
    names: list[str]
    def __getitem__(self, key: str | int) -> ParameterInfo: ...  # type: ignore[override]
    def __len__(self) -> int: ...
    def __contains__(self, key: object) -> bool: ...

# ── metadata ────────────────────────────────────────────────────────────────
class VariableInfo:
    name: str
    unit: str | None
    quantity: str | None
    min: float | None
    max: float | None
    nominal: float | None
    fixed: bool
    description: str | None
    dims: list[int]

# NOTE: `ParameterInfo` mirrors `VariableInfo`'s fields and adds `value`/`kind`,
# but is a distinct runtime class (not a subclass) — declared standalone so
# `isinstance`/type checks match reality.
class ParameterInfo:
    name: str
    unit: str | None
    quantity: str | None
    min: float | None
    max: float | None
    nominal: float | None
    fixed: bool
    description: str | None
    dims: list[int]
    value: float | None
    kind: ParamKind

class StructuralInfo:
    n_states: int
    n_algebraic: int
    n_outputs: int
    n_equations: int
    n_unknowns: int
    is_balanced: bool
    is_matched: bool
    n_blocks: int
    n_algebraic_loops: int
    largest_algebraic_loop: int

# ── results & exports ───────────────────────────────────────────────────────
class Result:
    model: str
    time: Any
    names: list[str]
    termination: str | None
    metrics: dict[str, Any]
    def __getitem__(self, name: str) -> Any: ...
    def __len__(self) -> int: ...
    def __contains__(self, name: object) -> bool: ...
    def to_numpy(self, names: Sequence[str] | None = ...) -> Any: ...
    def to_dataframe(self) -> Any: ...
    def to_dict(self) -> dict[str, Any]: ...
    def to_json(self, *, pretty: bool = ...) -> str: ...
    def plot(self, *names: str, ax: Any = ...) -> Any: ...
    def __repr__(self) -> str: ...

ScenarioMode = Literal["as_fast_as_possible", "realtime", "lockstep"]
class ScenarioOverrides(TypedDict, total=False):
    t_end: float
    dt: float
    solver: str
    mode: ScenarioMode
    schedule: ScenarioMode
    output: Pathish
    output_path: Pathish
    output_dir: Pathish
    debug_log_path: Pathish
    log_path: Pathish

class ScenarioResult:
    task: Literal["simulate", "codegen"]
    status: str
    model: str | None
    schedule: str | None
    output_paths: list[str]
    termination: str | None
    diagnostics: list[Diagnostic]
    metrics: dict[str, Any]
    result: Result | None
    codegen: CodegenResult | None
    def to_dict(self) -> dict[str, Any]: ...
    def __repr__(self) -> str: ...

class GradientResult:
    model: str
    objective: str
    mode: str
    t: float
    names: list[str]
    def keys(self) -> list[str]: ...
    def __getitem__(self, name: str) -> float: ...
    def __len__(self) -> int: ...
    def __contains__(self, name: object) -> bool: ...
    def to_dict(self) -> dict[str, float]: ...
    def to_numpy(self) -> Any: ...
    def to_series(self) -> Any: ...
    def __repr__(self) -> str: ...
    def _repr_html_(self) -> str: ...

class GeneratedFile:
    path: str
    content: str

class CodegenResult:
    target: str
    files: list[GeneratedFile]
    paths: list[str]
    def __iter__(self) -> Any: ...
    def __len__(self) -> int: ...
    def save_all(self, out: Pathish) -> list[str]: ...

# ── targets / solvers ───────────────────────────────────────────────────────
class Target:
    id: str
    ir: Stage
    description: str | None
    capabilities: dict[str, Any]

class SolverInfo:
    id: str
    family: Literal["explicit", "implicit"]
    available: bool

# ── live symbolic exports ───────────────────────────────────────────────────
class CasadiModel:
    name: str
    x: Any
    xdot: Any
    z: Any
    u: Any
    p: Any
    f_x: Any
    ode: Any
    alg: Any
    dae: dict[str, Any] | None
    dae_fn: Any
    x0: Any
    p0: Any
    state_names: list[str]
    algebraic_names: list[str]
    input_names: list[str]
    parameter_names: list[str]
    functions: dict[str, Any]
    module: Any
    def integrator(self, dt: Any = ..., *, method: str = ..., **opts: Any) -> Any: ...
    def jacobian(self, of: str, wrt: str) -> Any: ...

class JaxModel:
    name: str
    x0: Any
    p0: Any
    ode_fn: Any
    state_names: list[str]
    parameter_names: list[str]
    input_names: list[str]
    module: Any
    def simulate(self, *args: Any, **kwargs: Any) -> Any: ...

class SympyModel:
    name: str
    model: Any
    module: Any
    x: Any
    y: Any
    u: Any
    w: Any
    p: Any
    f_x: Any
    def solve_explicit(self) -> Any: ...
    def summary(self) -> dict[str, Any]: ...

class SolveExport:
    name: str
    target: str
    module: Any
    rhs: Any
    state_names: list[str]
    input_names: list[str]
    parameter_names: list[str]
    n_states: int
    n_inputs: int
    n_parameters: int

# ── diagnostics & errors ────────────────────────────────────────────────────
class Diagnostic:
    rule: str | None
    level: Level
    message: str
    file: str | None
    line: int | None
    column: int | None
    suggestion: str | None

class RumocaError(Exception):
    diagnostics: list[Diagnostic]

class ParseError(RumocaError): ...
class CompileError(RumocaError): ...
class SimulationError(RumocaError): ...
class StructuralParamError(RumocaError): ...
