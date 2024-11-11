# rumoca ![CI](https://github.com/jgoppert/rumoca/actions/workflows/rust.yml/badge.svg)

A Modelica Compiler written in RUST.

## Building, Testing, and Running

```bash
cargo build
cargo run
cargo test
cargo run -- --filename src/model.mo --generator sympy
```

## Installing

```bash
cargo install --path .
```

Make sure you add your rust bin path to your .bashrc and source it, then:

```bash
$ rumoca --help
Modelica Compiler

Usage: rumoca [OPTIONS] --filename <FILENAME> --generator <GENERATOR>

Options:
  -f, --filename <FILENAME>    The filename to compile
  -v, --verbose                Verbose output
  -g, --generator <GENERATOR>  Generator to Use [possible values: sympy, json, casadi-mx, casadi-sx]
  -h, --help                   Print help
  -V, --version                Print version
```

## Running

The compiler is currently under development, but some initial results are shown below:

### Modelica input file: **src/model.mo**
```bash
model Integrator
    Real x; // test
    Real y;
equation
    der(x) = 1.0;
    der(y) = x;
end Integrator;
```

### Generated sympy output file.
```bash
$ rumoca --filename src/model.mo --generator sympy

import sympy

class Integrator:

    def __init__(self):
        self.x = sympy.symbols('x');
        self.y = sympy.symbols('y');
```

## Roadmap
1. Add more language features
2. Improve generators
3. Add support for JAX, Collimator
