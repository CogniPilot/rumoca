# rumoca [<img src="https://github.com/jgoppert/rumoca/actions/workflows/rust.yml/badge.svg">](https://github.com/CogniPilot/rumoca/actions) [<img src="https://img.shields.io/crates/v/rumoca">](https://crates.io/crates/rumoca)

A Modelica translator with focus on CasADi, Sympy, JAX, and PyCollimator generation.

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
There are many excellent tools for analysis of cyber-physical systems, and rumoca
aims to allow you to use the best tool for the job at hand.
* [CasADi](https://github.com/casadi/casadi):
    * Written in C++: Interface Matlab/Python
    * Automatic Differentiation
    * Autonomy and Controls community
    * Code generation: C
* [Sympy](https://github.com/sympy/sympy):
    * Written in Python
    * General computer algebra system
    * Code generation: user defined
* [JAX](https://github.com/jax-ml/jax): 
    * Written in Python
    * Automatic Differentiation
    * Machine learning communicty
* [PyCollimator](https://github.com/collimator-ai/pycollimator): 
    * Written in Python
    * JAX based
    * GUI for models with cloud version
    * Model database

### Existing Modelica Compilers/ Translators

Compiler and translator are often used interchangeably, but the goal of a compiler is typically 
to generate low level machine code. The term translator is more general and refers to 
transformation of source code to some other form. Since we are interested in generaeting
models in various languages from a Modelica model, we call Rumoca a translator.

There are several other Modelica compilers/translators in development, and I believe there are challenges
that make it compelling to develop a new translator for generation required for this project. These are all my personal 
opinions and should be taken with a grain of salt.

* [Pymoca](https://github.com/pymoca/pymoca)
    * Benefits
        * Pymoca was written in Python and based on ANTLR, which is easy to use, it is a translator
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

## Installing

1. First install [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html).
2. Next, use Cargo to install Rumoca.

```bash
cargo install rumoca
```

> [!IMPORTANT]
> Make sure you [add your cargo binary directory to your path](https://doc.rust-lang.org/book/ch14-04-installing-binaries.html).

Type the following to test that rumoca is in your path.

```bash
$ rumoca --help

Rumoca Modelica Translator

Usage: rumoca [OPTIONS] <MODELICA_FILE>

Arguments:
  <MODELICA_FILE>  The modelica *.mo file to compile

Options:
  -t, --template <TEMPLATE>          Renders a template using dae_ast [default: ]
  -a, --ast-template <AST_TEMPLATE>  Renders a template using ast [default: ]
  -v, --verbose                      Verbose output
  -h, --help                         Print help
  -V, --version                      Print version
```

## Building, Testing, and Running

This package uses the standard cargo conventions for rust.

```bash
cargo build
cargo run
cargo test
cargo run -- test/models/integrator.mo -t test/templates/casadi_sx.jinja 
```

This package uses the standard cargo installation conventions.

```bash
cargo install --path .
```

## Example

Rumoca is currently under development, but some initial results are shown below:

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

### Generated CasADi output file.
```bash
$ rumoca test/models/integrator.mo -t test/templates/casadi_sx.jinja 
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

### Creating your own template using JINJA

Output is highly customizable as the JINJA template engine is used to render the rumoca
abstract syntax tree. You can use/modify our example templates in the test directory
or create your own using the widely used [JINJA](https://docs.rs/minijinja/latest/minijinja/) template language.

## Dependencies

* [LALRPOP](https://github.com/lalrpop/lalrpop) : Parsing, AST generation
* [LOGOS](https://github.com/maciejhirsz/logos) : Lexing
* [MINIJINJA](https://github.com/mitsuhiko/minijinja) : JINJA Template Engine
* [SERDE](https://github.com/serde-rs/serde) : AST Serialization
* [CLAP](https://github.com/clap-rs/clap) : Command Line Argument Parser
* [NDARRAY](https://github.com/rust-ndarray/ndarray) : N-dimensional arrays

## Roadmap

### DONE
1. Flat subset of full Modelica Grammar using LALRPOP
2. Initial Lexer using LOGOS
3. Generation using JINJA for Sympy/CasADi/Json
4. Command line interface using CLAP

### TODO
1. Add more language features (non-flat models, equations, statements)
2. Improve generators
3. Import multiple files
4. Flatten object oriented models 
5. Add support for JAX, PyCollimator
