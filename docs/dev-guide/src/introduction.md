# Rumoca Internals

This book explains **how the Rumoca compiler works** and **how to contribute
to it**. It is written for humans first: the chapters tell the story of a
model moving through the pipeline, then point you at the code and the specs
that own each piece.

Rumoca is a Modelica compiler written in Rust. It compiles Modelica models
to differential-algebraic equations (DAE), simulates them with pluggable
solver backends, and generates code for symbolic, compiled, and packaged
targets. The same compiler runs natively, in the `rumoca-lsp` language
server, and in WebAssembly in the browser.

## How to Use This Book

- **New to the codebase?** Read the *How the Compiler Works* part in order
  — it follows a model from source text to simulation results.
- **Fixing a bug or adding a feature?** Start with
  [Getting Started](./contributing/setup.md), then read the chapter for the
  pipeline stage you are touching, then read the spec that owns it.
- **Looking up a rule?** Skip this book and go straight to
  [`spec/`](https://github.com/CogniPilot/rumoca/tree/main/spec).

## This Book Explains; the Specs Decide

Normative architecture and contribution rules live in
[`spec/`](https://github.com/CogniPilot/rumoca/tree/main/spec), and
[`AGENTS.md`](https://github.com/CogniPilot/rumoca/blob/main/AGENTS.md) is
the routing index from task to spec. This book deliberately does not
restate spec rules — it gives you the mental model that makes the specs easy
to read, and links to them. When this book and a spec disagree, the spec
wins (and a docs fix is welcome).

## Philosophy in One Paragraph

Rumoca's core scope ends at DAE generation
([SPEC_0031](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0031_COMPILER_PHILOSOPHY.md)):
the compiler builds a stable, deterministic, solver-agnostic DAE, and
everything downstream — solvers, runtimes, viewers, bindings, code
generation targets — is a replaceable extension layered on that contract.
Most architectural decisions in the codebase trace back to defending that
boundary.

## Prerequisites

You should be comfortable with Rust, basic compiler concepts (parsing, ASTs,
type checking), and Git/GitHub workflows. Familiarity with Modelica helps
but is not required — the [user guide](https://cognipilot.github.io/rumoca/user-guide/)
and the chapters here explain the concepts where they matter.
