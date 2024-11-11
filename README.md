# rumoca ![CI](https://github.com/jgoppert/rumoca/actions/workflows/rust.yml/badge.svg)

A Modelica compiler with focus on Casadi, Sympy, JAX, and Collimator generation.

There are many useful libraries for hybrid systems analysis, but it is difficult to
port models between different environments.

### Input: Modelica
* [Modelica](https://modelica.org/) is a concise language for representing cyber-physical systems
* Text based models, domain specific langauge makes it more human readable
* Graphical model (block diagram) support via annotations
* Exactly maps to a differential algebraic equation (DAE) as defined by the [Modelica language standard](https://specification.modelica.org/master/)
* General langauges like python/C++ etc. allow users to create models that don't map easily to a DAE
* Modelica is a language and therefore many tools have been developed for it 

### Output Computer Algebra System Targets
There are many excellent tools for analysis of cyber-physical systems, and this compilers
aims to allow you to use the best tool for the job at hand.
* [Casadi](https://github.com/casadi/casadi):
    * Algorithmic Differentiation
    * Autonomy and Controls community
    * Code generation: C
* [Sympy](https://github.com/sympy/sympy):
    * General computer algebra system
    * Code generation: user defined
* [JAX](https://github.com/jax-ml/jax): 
    * Algorithmic Differentiation
    * Machine learning communicty

## Building, Testing, and Running

This package uses the standard cargo conventions for rust.

```bash
cargo build
cargo run
cargo test
cargo run -- --filename src/model.mo --generator sympy
```

## Installing

This package uses the standard cargo installation conventions.

```bash
cargo install --path .
```

Make sure you add your rust bin path to your .bashrc and source it, then
type the follow to test that rumoca is on your path.

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


## Dependencies

* [LALRPOP](https://github.com/lalrpop/lalrpop) : Parsing, AST generation
* [LOGOS](https://github.com/maciejhirsz/logos) : Lexing
* [TERA](https://github.com/Keats/tera) : Template Engine
* [SERDE](https://serde.rs/) : AST Serialization
* [CLAP](https://github.com/clap-rs/clap) : Command Line Argument Parser

## Roadmap

### DONE
1. Flat subset of full Modelica Grammar using LALRPOP
2. Initial Lexer using LOGOS
3. Generation using TERA for Sympy/Casadi/Json
4. Command line interface using CLAP

### TODO
1. Add more language features (non-flat models, equations, statements)
2. Improve generators
3. Import multiple files
4. Flatten object oriented models 
5. Add support for JAX, Collimator
