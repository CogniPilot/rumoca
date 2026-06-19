from __future__ import annotations

from typing import Any, Literal, Sequence

ScenarioTask = Literal["simulate", "codegen"]
Solver = Literal["auto", "rk-like", "bdf"]

__all__: Sequence[str]


class ParseResult:
    success: bool
    error: str | None

    def __bool__(self) -> bool: ...
    def __repr__(self) -> str: ...


class LintMessage:
    rule: str
    level: Literal["error", "warning", "note", "help"]
    message: str
    file: str
    line: int
    column: int
    suggestion: str | None

    def __repr__(self) -> str: ...


class SimulationOptions:
    t_start: float
    t_end: float
    dt: float | None
    solver: str | None
    rtol: float | None
    atol: float | None
    max_wall_seconds: float | None

    def __init__(
        self,
        t_end: float = 1.0,
        dt: float | None = None,
        solver: Solver | str | None = None,
        t_start: float = 0.0,
        rtol: float | None = None,
        atol: float | None = None,
        max_wall_seconds: float | None = None,
    ) -> None: ...
    def __repr__(self) -> str: ...


class CompileResult:
    model_name: str

    def __repr__(self) -> str: ...
    def __getitem__(self, key: str) -> Any: ...
    def to_dict(self) -> dict[str, Any]: ...
    def to_json(self, pretty: bool = True) -> str: ...
    def save_json(self, path: str, pretty: bool = True) -> None: ...


class SimulationResult:
    model_name: str
    metrics: dict[str, Any]
    data: dict[str, Any]

    def __repr__(self) -> str: ...
    def __getitem__(self, key: str) -> Any: ...
    def to_dict(self) -> dict[str, Any]: ...
    def to_json(self, pretty: bool = True) -> str: ...
    def save_json(self, path: str, pretty: bool = True) -> None: ...


class CodegenResult:
    model_name: str
    target: str
    paths: list[str]

    def __repr__(self) -> str: ...
    def to_dict(self) -> list[dict[str, Any]]: ...
    def to_json(self, pretty: bool = True) -> str: ...
    def save_json(self, path: str, pretty: bool = True) -> None: ...
    def save_all(self, output_dir: str) -> list[str]: ...


class Project:
    workspace_root: str
    model_name: str | None
    model_file: str | None

    @classmethod
    def open(
        cls,
        workspace_root: str = ".",
        model_name: str | None = None,
        model_file: str | None = None,
        focus_path: str | None = None,
        source_roots: list[str] | None = None,
    ) -> Project: ...

    def __repr__(self) -> str: ...
    def source_roots(self) -> list[str]: ...
    def load_source_roots(self) -> str: ...
    def compile_file(
        self,
        path: str | None = None,
        model_name: str | None = None,
    ) -> CompileResult: ...
    def compile_source(
        self,
        source: str,
        model_name: str | None = None,
        filename: str | None = None,
    ) -> CompileResult: ...
    def simulate_file(
        self,
        path: str | None = None,
        model_name: str | None = None,
        options: SimulationOptions | None = None,
    ) -> SimulationResult: ...
    def simulate_source(
        self,
        source: str,
        model_name: str | None = None,
        filename: str | None = None,
        options: SimulationOptions | None = None,
    ) -> SimulationResult: ...
    def codegen_file(
        self,
        path: str | None = None,
        target: str | None = None,
        model_name: str | None = None,
    ) -> CodegenResult: ...


class ProjectSession:
    """Reusable compiler session with source-root and compile caches."""

    @classmethod
    def from_project(
        cls,
        workspace_root: str,
        focus_path: str | None = None,
        model_name: str | None = None,
        task: ScenarioTask = "simulate",
        source_roots: list[str] | None = None,
    ) -> ProjectSession: ...

    def __init__(self, source_roots: list[str] | None = None) -> None: ...
    def __repr__(self) -> str: ...
    def clear(self) -> None: ...
    def configure_source_roots(self, source_roots: list[str]) -> None: ...
    def configure_project(
        self,
        workspace_root: str,
        focus_path: str | None = None,
        model_name: str | None = None,
        task: ScenarioTask = "simulate",
        source_roots: list[str] | None = None,
    ) -> None: ...
    def get_source_roots(self) -> list[str]: ...
    def load_source_roots(self) -> str: ...
    def source_root_statuses(self) -> str: ...
    def compile(
        self,
        source: str,
        model_name: str | None = None,
        filename: str | None = None,
    ) -> str: ...
    def compile_to_json(
        self,
        source: str,
        model_name: str | None = None,
        filename: str | None = None,
    ) -> str: ...
    def compile_file(self, path: str, model_name: str | None = None) -> str: ...
    def compile_file_to_json(self, path: str, model_name: str | None = None) -> str: ...
    def render_model(
        self,
        source: str,
        template: str,
        model_name: str | None = None,
        filename: str | None = None,
    ) -> str: ...
    def render_model_file(
        self,
        path: str,
        template: str,
        model_name: str | None = None,
    ) -> str: ...
    def render_target_model(
        self,
        source: str,
        target: str,
        model_name: str | None = None,
        filename: str | None = None,
    ) -> str: ...
    def render_target_file(
        self,
        path: str,
        target: str,
        model_name: str | None = None,
    ) -> str: ...
    def simulate(
        self,
        source: str,
        model_name: str | None = None,
        filename: str | None = None,
        options: SimulationOptions | None = None,
    ) -> str: ...
    def simulate_file(
        self,
        path: str,
        model_name: str | None = None,
        options: SimulationOptions | None = None,
    ) -> str: ...


def version() -> str: ...
def parse(source: str, filename: str | None = None) -> ParseResult: ...
def lint(source: str, filename: str | None = None) -> list[LintMessage]: ...
def check(source: str, filename: str | None = None) -> list[LintMessage]: ...
def format(source: str, filename: str | None = None) -> str: ...
def format_or_original(source: str, filename: str | None = None) -> str: ...
def get_builtin_targets() -> str: ...
def effective_source_roots(
    source_roots: list[str] | None = None,
    workspace_root: str | None = None,
    focus_path: str | None = None,
    model_name: str | None = None,
    task: ScenarioTask = "simulate",
) -> str: ...
def workspace_source_roots(workspace_root: str, focus_path: str | None = None) -> str: ...
def scenario_simulation_config(
    workspace_root: str,
    model_name: str,
    solver: Solver | str | None = None,
    t_end: float | None = None,
    dt: float | None = None,
    output_dir: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def scenario_codegen_config(workspace_root: str, model_name: str) -> str: ...
def compile(
    source: str,
    model_name: str | None = None,
    filename: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def compile_source(
    source: str,
    model_name: str | None = None,
    filename: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def compile_to_json(
    source: str,
    model_name: str | None = None,
    filename: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def compile_file(
    path: str,
    model_name: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def compile_file_to_json(
    path: str,
    model_name: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def render_model(
    source: str,
    template: str,
    model_name: str | None = None,
    filename: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def render_model_file(
    path: str,
    template: str,
    model_name: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def render_target_model(
    source: str,
    target: str,
    model_name: str | None = None,
    filename: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def render_target_file(
    path: str,
    target: str,
    model_name: str | None = None,
    source_roots: list[str] | None = None,
) -> str: ...
def simulate(
    source: str,
    model_name: str | None = None,
    filename: str | None = None,
    source_roots: list[str] | None = None,
    options: SimulationOptions | None = None,
) -> str: ...
def simulate_file(
    path: str,
    model_name: str | None = None,
    source_roots: list[str] | None = None,
    options: SimulationOptions | None = None,
) -> str: ...
