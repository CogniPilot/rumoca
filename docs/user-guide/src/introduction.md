# Rumoca Handbook

Rumoca is a Modelica compiler, simulator, and code generation toolkit written
in Rust. It takes equation-based models of physical systems and turns them
into simulations you can run from the command line, VS Code, or a browser —
or into code for other ecosystems such as Python (SymPy, JAX, CasADi), C,
Rust, and FMI.

## What is Modelica?

[Modelica](https://modelica.org) is an open, equation-based language for
modeling physical systems. Instead of writing step-by-step simulation code,
you declare the equations that govern your system and let the compiler decide
how to solve them.

The example below is a damped spring-mass system. It is **live**: edit the
code, press **▶ Simulate** to integrate it right here in your browser, or
**Show DAE** to see the equation system the compiler produces. The editor has
the same syntax highlighting, completion, and error checking as the Rumoca
VS Code extension, powered by the same compiler running in WebAssembly.

```modelica,interactive
model SpringMassDamper "Mass on a spring with viscous damping"
  parameter Real m = 1.0 "Mass [kg]";
  parameter Real k = 4.0 "Spring constant [N/m]";
  parameter Real c = 0.4 "Damping coefficient [N.s/m]";
  Real x(start = 1.0) "Position [m]";
  Real v(start = 0.0) "Velocity [m/s]";
equation
  der(x) = v;
  m * der(v) = -k * x - c * v;
  annotation(experiment(StopTime = 20.0));
end SpringMassDamper;
```

Note what you did *not* have to write: no integration loop, no state vector
bookkeeping, no event logic. `der(x)` means the time derivative of `x`, and
the compiler transforms the equations into a form a numerical solver can
integrate.

## What Rumoca Gives You

| Capability | Where to read more |
|---|---|
| Compile and simulate Modelica models | [Quick Start](./getting-started/quickstart.md), [Running Simulations](./simulation/running.md) |
| Repeatable scenario files (`rum.toml`) for simulation and codegen | [Scenario Files](./simulation/scenario-tomls.md) |
| Interactive, human-in-the-loop simulation with browser 3D viewers | [Interactive Simulation](./simulation/interactive.md) |
| Code generation to SymPy, JAX, CasADi, C, Rust, FMI, and more | [Targets and Templates](./codegen/targets.md) |
| IDE support: diagnostics, completion, hover, run buttons | [VS Code Extension](./tools/vscode.md) |
| Formatter and linter for Modelica source | [Formatter and Linter](./tools/fmt-lint.md) |
| Full compiler in WebAssembly | [Web Playground](./tools/playground.md) |
| Structural analysis and debugging of models | [Inspecting and Debugging Models](./simulation/inspect.md) |

## The Normal Workflow

1. Write or open a Modelica model (`.mo` file).
2. Configure any external Modelica package roots, such as the Modelica
   Standard Library (MSL).
3. Run a direct command (`rumoca sim model.mo`) or a colocated `rum.toml`
   scenario (`rumoca sim -c rum.toml`).
4. Inspect results in the CLI, VS Code, the browser viewer, or generated
   target output.

## Project Status

Rumoca is in active development. It compiles and simulates a growing subset
of Modelica, validated continuously against the Modelica Standard Library,
but it is not yet a complete replacement for mature tools such as
OpenModelica or Dymola. See [Language Support
Status](./language/support-status.md) for an honest description of what works
today.

## How This Book Is Organized

- **Getting Started** installs Rumoca and walks you through your first model.
- **The Modelica Language** explains equation-based modeling and what Rumoca
  supports.
- **Tools** covers the CLI, VS Code extension, playground, formatter, and
  linter.
- **Simulation** covers direct runs, scenario files, solvers, interactive
  simulation, and debugging.
- **Code Generation** covers built-in and custom targets.
- **Examples** points at runnable examples, including live in-browser ones.

Developers who want to understand or modify the compiler itself should read
the companion [Rumoca Internals](https://cognipilot.github.io/rumoca/dev-guide/)
book.
