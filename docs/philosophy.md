# Rumoca Philosophy

## Scope

Rumoca is a **Modelica → symbolic system compiler** that produces **solver-agnostic DAEs** for simulation, export (e.g. FMUs), and embedding (Rust, Python, WASM).

Pipeline:
Modelica → parse → resolve → flatten → DAE → (optional) solve → execution / animation

👉 **Core scope ends at DAE generation** — everything after that is replaceable.

---

## Core Principle

Rumoca builds **portable symbolic systems**:

- Modelica = input language  
- DAE = stable system representation  
- Solver = pluggable backend  

---

## System Boundaries

- **Compiler vs Solver:** DAEs are complete, deterministic, solver-agnostic; solvers must not influence DAE structure  
- **Execution semantics:** time stepping, integrators, events, animation are solver/runtime concerns  
- **UI:** external consumer for visualization and orchestration, not part of compilation  

---

## Extensions

Rumoca includes additional components such as WASM builds (with optional solvers), editor integrations (e.g. VS Code), and example applications.

These are:

> **extensions layered on top of the core**

They must remain replaceable and consume the same DAE interface.

---

## Non-Goals

Rumoca is:

- not a solver framework  
- not a Modelica runtime  
- not UI-driven  
- not tied to a specific backend  

---

## Guiding Idea

> Rumoca extracts symbolic systems from Modelica models and makes them usable across different execution environments.