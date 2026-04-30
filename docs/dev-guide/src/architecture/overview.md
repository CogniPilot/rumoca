# Architecture Overview

This chapter provides a high-level view of how Rumoca is structured.

## System Architecture

Rumoca is organized as a collection of Rust crates that share a common core compiler. The core compiles to both native binaries and WebAssembly, enabling multiple frontends.

```text
┌─────────────────────────────────────────────────────────────────────────────────────┐
│                              RUMOCA ARCHITECTURE                                     │
└─────────────────────────────────────────────────────────────────────────────────────┘

                              ┌──────────────────────────┐
                              │     CORE COMPILER        │
                              │    (rumoca-compile)      │
                              │  - Parsing (AST)         │
                              │  - Compilation phases    │
                              │  - DAE generation        │
                              │  - Runtime/Simulation    │
                              └───────────┬──────────────┘
                                          │
              ┌───────────────────────────┼───────────────────────────┐
              │                           │                           │
              ▼                           ▼                           ▼
┌─────────────────────────┐  ┌─────────────────────────┐  ┌─────────────────────────┐
│   LSP HANDLERS          │  │   WASM BINDINGS         │  │   CLI TOOLS             │
│  (rumoca-tool-lsp)      │  │  (rumoca-bind-wasm)     │  │  (rum, rumoca-fmt)      │
│                         │  │                         │  │                         │
│  Pure LSP logic using   │  │  wasm_bindgen exports:  │  │  Native executables     │
│  lsp_types (no I/O)     │  │  - parse()              │  │                         │
│                         │  │  - lint()               │  └─────────────────────────┘
│  handlers/              │  │  - compile_model()      │
│   ├─ completion.rs      │  │  - simulate()           │
│   ├─ hover.rs           │  │                         │
│   ├─ diagnostics.rs     │  │  Thin wrapper over      │
│   ├─ goto_definition.rs │  │  rumoca-compile         │
│   └─ ... (15+ handlers) │  │                         │
└───────────┬─────────────┘  └───────────┬─────────────┘
            │                            │
            │ #[cfg(feature="server")]   │ wasm-pack build
            ▼                            ▼
┌─────────────────────────┐  ┌─────────────────────────┐
│   TOWER-LSP SERVER      │  │   WASM MODULE           │
│  (native binary)        │  │  (rumoca_bg.wasm)       │
│                         │  │                         │
│  server.rs:             │  │  - Single-threaded      │
│  ModelicaLanguageServer │  │  - No filesystem        │
│  - Session management   │  │  - Global SESSION mutex │
│  - Source-root loading  │  │                         │
│  - Project config       │  │                         │
│  - Async tower-lsp      │  │                         │
└───────────┬─────────────┘  └───────────┬─────────────┘
            │                            │
            │ stdio/JSON-RPC             │ JS interop
            ▼                            ▼
┌─────────────────────────┐  ┌─────────────────────────┐
│   VSCODE EXTENSION      │  │   WEB PLAYGROUND        │
│  (editors/vscode)       │  │  (editors/wasm)         │
│                         │  │                         │
│  TypeScript client:     │  │  Browser UI:            │
│  - LanguageClient       │  │  - Monaco editor        │
│  - Spawns rumoca-lsp    │  │  - Web Worker           │
│  - Embedded Modelica    │  │  - Direct WASM calls    │
│  - Notebook controller  │  │  - Simulation plots     │
└─────────────────────────┘  └─────────────────────────┘
```

## Key Design Decisions

### Handler/Server Separation

The LSP handlers (`rumoca-tool-lsp/src/handlers/`) are pure functions that:
- Take AST + position → return LSP response
- Use only `lsp_types` (no I/O, no async)
- Compile to both native and WASM

The server wrapper (`rumoca-tool-lsp/src/server.rs`) adds:
- Session state management
- Source-root loading
- Async I/O via tower-lsp
- Native-only (behind `#[cfg(feature = "server")]`)

This separation allows the same LSP logic to power both VSCode and the web playground.

### Session Architecture

The `Session` type (in `rumoca-compile`) holds compilation state:

```text
┌─────────────────────────────────────────┐
│              Session                     │
├─────────────────────────────────────────┤
│  documents: HashMap<Uri, Document>      │  ◄─ Open files
│  parsed_cache: HashMap<Uri, AST>        │  ◄─ Parsed ASTs
│  resolved: Option<ResolvedTree>         │  ◄─ Name resolution
│  source_roots: HashMap<Key, SourceRoot> │  ◄─ Loaded source roots
└─────────────────────────────────────────┘
```

In the native LSP server, `Session` is wrapped in `Arc<RwLock<Session>>` for concurrent access.

In WASM, a global `Mutex<Option<Session>>` is used (single-threaded).

## Compilation Pipeline

```text
  Source Text (.mo)
         │
         ▼
  ┌─────────────────┐
  │    Parsing      │  rumoca-parser
  │  (pest + AST)   │
  └────────┬────────┘
           │
           ▼
  ┌─────────────────┐
  │  Instantiation  │  rumoca-compile/compile
  │  (class lookup, │
  │   modification) │
  └────────┬────────┘
           │
           ▼
  ┌─────────────────┐
  │  Type Checking  │  rumoca-compile/compile
  │  (expressions,  │
  │   equations)    │
  └────────┬────────┘
           │
           ▼
  ┌─────────────────┐
  │   Flattening    │  rumoca-compile/compile
  │  (expand hier-  │
  │   archy, inline)│
  └────────┬────────┘
           │
           ▼
  ┌─────────────────┐
  │  DAE Lowering   │  rumoca-compile/compile
  │  (equations to  │
  │   DAE form)     │
  └────────┬────────┘
           │
           ▼
  ┌─────────────────┐
  │   Simulation    │  rumoca-compile/runtime
  │   or Codegen    │  rumoca-eval-dae
  └─────────────────┘
```

Each phase is described in detail in the [Compiler Internals](../compiler/parsing.md) section.

## Directory Structure

```text
rumoca/
├── crates/
│   ├── rumoca-compile/      # Core compiler + runtime
│   ├── rumoca-parser/       # Pest grammar + AST
│   ├── rumoca-tool-lsp/     # LSP handlers + server
│   ├── rumoca-tool-fmt/     # Code formatter
│   ├── rumoca-tool-lint/    # Linter
│   ├── rumoca-bind-wasm/    # WASM bindings
│   └── rumoca-eval-dae/     # DAE evaluation/codegen
│
├── editors/
│   ├── vscode/              # VSCode extension (TypeScript)
│   └── wasm/                # Web playground (JS + Monaco)
│
├── docs/
│   ├── dev-guide/           # This book
│   └── user-guide/          # User documentation
│
└── tests/                   # Integration tests
```

## Platform Differences

| Aspect | Native (LSP) | WASM (Playground) |
|--------|--------------|-------------------|
| **Runtime** | tokio async | Single-threaded |
| **Communication** | stdio JSON-RPC | Direct JS calls |
| **Session** | `Arc<RwLock<Session>>` | `Mutex<Session>` |
| **File access** | Full filesystem | None (text passed in) |
| **Libraries** | Disk cache + lazy load | Pre-bundled or pasted |
| **Concurrency** | Multi-client capable | Single-threaded |

## What's Next?

- [Crate Map](./crates.md) - detailed breakdown of each crate
- [Compilation Pipeline](./pipeline.md) - how code flows through phases
- [LSP Architecture](../lsp/architecture.md) - how the language server works
