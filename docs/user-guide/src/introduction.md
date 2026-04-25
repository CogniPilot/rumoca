# Rumoca Handbook

Welcome to Rumoca, a modern Modelica compiler and simulation environment.

## What is Rumoca?

Rumoca compiles Modelica models - a declarative language for physical system modeling - into runnable simulations. It provides:

- **Command-line tools** for parsing, checking, and simulating models
- **IDE support** via VSCode with real-time diagnostics and completion
- **Web playground** for trying Modelica in the browser
- **Jupyter integration** for interactive modeling in notebooks
- **Code generation** to Python and other backends

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

## Getting Started

1. [Install Rumoca](./getting-started/installation.md)
2. [Quick Start](./getting-started/quickstart.md) - simulate your first model
3. [Your First Model](./getting-started/first-model.md) - write your own

## Getting Help

- GitHub Issues: [github.com/climamind/rumoca/issues](https://github.com/climamind/rumoca/issues)
- Source Code: [github.com/climamind/rumoca](https://github.com/climamind/rumoca)
