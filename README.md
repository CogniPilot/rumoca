# rumoca ![CI](https://github.com/jgoppert/rumoca/actions/workflows/rust.yml/badge.svg)

A Modelica compiler with focus on Casadi, Sympy, JAX, and Collimator generation.

There are many useful libraries for hybrid systems analysis, but it is difficult to
port models between different environments.

### **Input**: Modelica
* [Modelica](https://modelica.org/) is a concise language for representing cyber-physical systems
* Text based models, domain specific langauge makes it more human readable
* Graphical model (block diagram) support via annotations
* Exactly maps to a differential algebraic equation (DAE) as defined by the [Modelica language standard](https://specification.modelica.org/master/)
* General langauges like python/C++ etc. allow users to create models that don't map easily to a DAE
* Modelica is a language and therefore many tools have been developed for it 

### **Output**: Computer Algebra System Targets
There are many excellent tools for analysis of cyber-physical systems, and this compilers
aims to allow you to use the best tool for the job at hand.
* [Casadi](https://github.com/casadi/casadi):
    * Written in C++: Interface Matlab/Python
    * Algorithmic Differentiation
    * Autonomy and Controls community
    * Code generation: C
* [Sympy](https://github.com/sympy/sympy):
    * Written in Python
    * General computer algebra system
    * Code generation: user defined
* [JAX](https://github.com/jax-ml/jax): 
    * Written in Python
    * Algorithmic Differentiation
    * Machine learning communicty
* [Collimator](https://py.collimator.ai/): 
    * Written in Python
    * JAX based
    * GUI for models with cloud version
    * Model database

### Existing Modelica Compilers

There are several other Modelica compilers in development, and I believe there are challenges
that make it compelling to develop a new compiler for this task. These are all my personal 
opinions and should be taken with a grain of salt.

* [Pymoca](https://github.com/pymoca/pymoca)
    * Benefits
        * Pymoca was written in Python and based on ANTLR, which is easy to use
        * It has similar goasl to rumoca, hence the same. I also am a developer for Pymoca.
        * Python is a very friendly language and easy for users to develop in
    * Drawbacks
        * Generation to listed output targets is difficult due to untyped AST
        * Since it is using ANTLR source is first converted into a Parse tree, then into an AST, process is slow
        * Python lacks strict type safety (even though type hints/ beartype exists)
        * Python is a slow language and handling large models is problematic

* [Marco](https://github.com/marco-compiler/marco)
    * Benefits
        * Marco is a new compiler being written in C++, which is a fast language
        * It is based on LLVM, which is robust
        * The focus in on high performance simulation for large scale models
    * Drawbacks
        * Generation to listed output targets is difficult due to C++ compiler
        * C++ is a non-memory safe language, unlike Rust
        * C++ libraries for templating etc are more cumbersome than rust version
        * Packaging and deployment in C++ is cumbersome
        * License limits commercialization

* [OpenModelica](https://openmodelica.org/)
    * Benefits
        * Mature open-source compiler that compiles the Modelica Standard Library
        * OMEdit provides graphical and text environment to write models
    * Drawbacks
        * Generation to listed output targets is difficult due to Modelica compiler
        * Compiler is written in Modelica itself which I find difficult to understand
        * ANTLR based parsing can be slow
        * Custom OSMC license can be prohibitive for commercialization
        * License limits commercialization

## Building, Testing, and Running

This package uses the standard cargo conventions for rust.

```bash
cargo build
cargo run
cargo test
cargo run -- -t templates/casadi_sx.tera -f models/integrator.mo 
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
Rumoca Modelica Compiler

Usage: rumoca [OPTIONS] --template-file <TEMPLATE_FILE> --filename <FILENAME>

Options:
  -t, --template-file <TEMPLATE_FILE>  The template
  -f, --filename <FILENAME>            The filename to compile
  -v, --verbose                        Verbose output
  -h, --help                           Print help
  -V, --version                        Print version
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
$ rumoca -t templates/casadi_sx.tera -f models/integrator.mo 
```
```python
import casadi as ca

class Integrator:

    def __init__(self):

        # declare states
        x = ca.SX.sym('x');
        y = ca.SX.sym('y');

        # declare state vector
        self.x = ca.vertcat(
            x,
            y);
        
        # declare state derivative equations
        der_x = 1;
        der_y = x;

        # declare state derivative vector
        self.x_dot = ca.vertcat(
            der_x,
            der_y);
        self.ode = ca.Function('ode', [self.x], [self.x_dot])
```


## Dependencies

* [LALRPOP](https://github.com/lalrpop/lalrpop) : Parsing, AST generation
* [LOGOS](https://github.com/maciejhirsz/logos) : Lexing
* [TERA](https://github.com/Keats/tera) : Template Engine
* [SERDE](https://github.com/serde-rs/serde) : AST Serialization
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
