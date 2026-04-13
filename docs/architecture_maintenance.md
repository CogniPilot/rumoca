# Rumoca Architecture

## 1. Package Structure

Rumoca is organized into separate packages:

- rumoca-core — parsing, semantic analysis, flattening, DAE generation  
- rumoca-solver-ref — reference solver (depends on core)  
- rumoca-wasm-core — WASM build for core  
- rumoca-wasm-full — WASM build including solver  

Dependency direction is strictly one-way:

rumoca-core  
↑  
rumoca-solver-ref  
↑  
rumoca-wasm-full  

rumoca-wasm-core depends only on rumoca-core.

---

## 2. Core Boundary

[ compiler ] → DAE → [ solver(s) ]

- DAE is the **only contract** between compiler and execution  
- must be complete, deterministic, solver-agnostic  
- must not be shaped for specific solvers  

---

## 3. DAE Contract

- internally configurable (transformations, normalization, index reduction, etc.)  
- externally stable structure  
- solver-independent  
- aligned with FMI concepts  
- suitable for direct FMU generation  

---

## 4. Reference Solver

- implemented as a **separate package**  
- depends on rumoca-core  
- must not introduce assumptions into IR  
- must remain replaceable  

---

## 5. IR Rules

- IR is **pure data**  
- no execution logic  
- no solver coupling  
- no hidden evaluation or state  

---

## 6. Codegen Direction

DAE → export / codegen → external systems

- strictly one-way  
- no feedback into IR  

---

## 7. WASM Builds

- rumoca-wasm-core: parsing, semantics, DAE generation only  
- rumoca-wasm-full: includes solver support  

Solver-enabled WASM builds are extensions, not part of the core.

---

## 8. UI & Execution

- UI must not influence compilation  
- simulation concerns (time stepping, integrators, events, animation) belong to solver layer  
- Rumoca provides system definition and state access only  

---

## 9. Extensions

Examples:

- WASM builds with solver  
- editor integrations (e.g. VS Code)  
- example applications  

Rules:

- must consume the DAE interface  
- must remain replaceable  
- must not introduce reverse dependencies  

---

## 10. Hard Rule: Core Isolation

> The core must compile and function without any extension packages.

- no reverse dependencies (core → extensions)  
- no feature leakage  
- no optional shortcuts in core  

Violations must be rejected.

---

## 11. Design Constraints

- strict phase separation  
- stable DAE contract  
- solver-agnostic IR  
- reference solver is separate  
- UI fully decoupled  
- extensions layered on top  
- core isolation enforced  

---

## 12. Known Risk

⚠️ Reference solver drift

- tendency to optimize IR for bundled solver  
- risk of hidden coupling  

Mitigation:

- test with external solvers  
- ensure solver-free builds work  