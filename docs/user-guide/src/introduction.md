# Rumoca User Guide

Rumoca is a Rust Modelica compiler, simulator, and symbolic code generation
toolkit.

The normal user workflow is:

1. Write or open a Modelica model.
2. Configure any external Modelica package roots, such as MSL or CMM.
3. Run either a direct command or a colocated `rum.toml` scenario.
4. Inspect results in the CLI, VS Code, browser viewer, or generated target
   output.

## What is Modelica?

Modelica is an equation-based language for modeling physical systems. Instead of writing step-by-step simulation code, you declare the equations that govern your system:

```modelica
model SpringMass
  Real x(start = 1.0) "Position";
  Real v(start = 0.0) "Velocity";
  parameter Real k = 1.0 "Spring constant";
  parameter Real m = 1.0 "Mass";
equation
  der(x) = v;
  m * der(v) = -k * x;
end SpringMass;
```

The compiler transforms these equations into a form suitable for numerical simulation.

## Project Status

Rumoca is in active development. It can compile and simulate a growing subset
of Modelica, but it is not a full OpenModelica replacement. Expect better
results with explicit examples and pinned packages than with arbitrary large
libraries.

## Getting Started

Start with [Installation](./getting-started/installation.md), then run the
[Quick Start](./getting-started/quickstart.md). The scenario pages explain
the current preferred way to keep simulation, visualization, source-root, and
code generation settings with an example model.
