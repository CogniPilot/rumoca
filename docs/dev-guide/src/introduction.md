# Rumoca Internals

Welcome to the Rumoca developer guide. This book explains how the Rumoca Modelica compiler works internally, and how to contribute to its development.

## What is Rumoca?

Rumoca is a Modelica compiler written in Rust. It compiles Modelica models to Differential-Algebraic Equations (DAE) and can simulate them or generate code for other backends.

Key features:
- **Fast parsing** with excellent error messages
- **LSP support** for IDE integration (VSCode, web)
- **WASM target** for browser-based tooling
- **Simulation** with multiple solver backends
- **Code generation** via customizable templates

## Who is this guide for?

This guide is for developers who want to:
- Understand how the compiler works
- Fix bugs or add features
- Add new LSP capabilities
- Extend code generation backends

## Prerequisites

You should be comfortable with:
- Rust programming
- Basic compiler concepts (parsing, AST, type checking)
- Git and GitHub workflows

Familiarity with Modelica is helpful but not required - we'll explain the relevant concepts as needed.

## How to read this guide

- **Getting Started** - Build the project and run tests
- **Architecture** - Understand the high-level structure
- **Compiler Internals** - Deep dive into each compilation phase
- **Language Server** - LSP implementation details
- **Editor Integration** - VSCode and web playground

Start with [Building from Source](./getting-started/building.md) to get your development environment set up.
