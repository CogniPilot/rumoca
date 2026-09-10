//! The Jacobian synthesis admission table, as the data both the
//! specification and the gates read.
//!
//! Sections 4 and 5 of the specification state, for every construct this
//! engine can meet and every shape of operands it can meet it at, whether the
//! construct is differentiated, refused by rule id, or not a Modelica program
//! at all. That statement lives here once, as [`table`], and the gates
//! generate their probes from it: an admitted pair gets a finite-difference
//! row, a refused pair gets a refusal row in both artifacts, and a pair that
//! is not a program gets a row proving the engine is never asked.
//!
//! [`unstated_pairs`] closes the set. The construct universe is read out of
//! the engine's own name and operator lists, so a rule added to the engine
//! without a row here is reported as unstated rather than left unchecked.

use std::fmt;

mod rows;

pub use rows::table;

/// The shape one operand of a probed construct is given.
///
/// Three shapes separate every rule this engine states. The vector is
/// `Real[3]` and the matrix is `Real[3, 3]`, so every product, concatenation
/// and cross product below conforms without a second vector length.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Shape {
    /// A rank-0 `Real`.
    Scalar,
    /// `Real[3]`.
    Vector,
    /// `Real[3, 3]`.
    Matrix,
}

impl Shape {
    /// Every shape, in the order the universe enumerates them.
    pub const ALL: &'static [Shape] = &[Shape::Scalar, Shape::Vector, Shape::Matrix];

    /// A moving value of this shape, written over the differentiated input.
    ///
    /// Every entry sits in `[0.3, 0.7]` for an input in `[-1, 1]`: inside the
    /// domain of every elementary rule in the table, away from the zero that
    /// `log`, `sqrt` and division would meet, and away from the `1` where
    /// `asin` and `acos` end.
    pub fn moving(self) -> &'static str {
        match self {
            Self::Scalar => "0.5 + 0.2*u[1]",
            Self::Vector => "{0.5 + 0.2*u[1], 0.6 + 0.2*u[2], 0.7 + 0.2*u[3]}",
            Self::Matrix => concat!(
                "{{0.5 + 0.2*u[1], 0.6 + 0.2*u[2], 0.7 + 0.2*u[3]}, ",
                "{0.4 + 0.2*u[2], 0.65 + 0.2*u[3], 0.35 + 0.2*u[1]}, ",
                "{0.55 + 0.2*u[3], 0.45 + 0.2*u[1], 0.6 + 0.2*u[2]}}"
            ),
        }
    }

    /// A second moving value of this shape, distinct from [`Self::moving`].
    ///
    /// The two operands must differ, or a difference between them would vanish
    /// and the gate's own magnitude assertion could not tell a correct rule
    /// from a rule that returned nothing.
    pub fn moving_again(self) -> &'static str {
        match self {
            Self::Scalar => "0.8 + 0.15*u[2]",
            Self::Vector => "{0.8 + 0.15*u[2], 0.95 + 0.15*u[3], 1.1 + 0.15*u[1]}",
            Self::Matrix => concat!(
                "{{0.8 + 0.15*u[2], 0.95 + 0.15*u[3], 1.1 + 0.15*u[1]}, ",
                "{1.2 + 0.15*u[3], 0.85 + 0.15*u[1], 1.05 + 0.15*u[2]}, ",
                "{0.9 + 0.15*u[1], 1.15 + 0.15*u[2], 0.75 + 0.15*u[3]}}"
            ),
        }
    }

    /// A value of this shape that carries no tangent.
    ///
    /// The power rules read their exponent as a translation-time constant, so
    /// the shape of a held-still value is what their rows vary.
    pub fn held_still(self) -> &'static str {
        match self {
            Self::Scalar => "3",
            Self::Vector => "{2, 3, 2}",
            Self::Matrix => "{{2, 3, 2}, {3, 2, 3}, {2, 3, 2}}",
        }
    }
}

impl fmt::Display for Shape {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Scalar => "scalar",
            Self::Vector => "vector",
            Self::Matrix => "matrix",
        })
    }
}

/// What synthesis does with one construct at one tuple of operand shapes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verdict {
    /// Differentiated, and owed a finite-difference row.
    Differentiated,
    /// Differentiated, and owed a finite-difference row taken in
    /// OpenModelica.
    ///
    /// The pair is ordinary Modelica another tool runs, but this compiler
    /// cannot run the probe that would carry its row, so there is no
    /// in-process program to compare against. Two things can put a pair here:
    /// a primal this compiler's canonical-DAE construction does not carry at
    /// all, and a primal it does carry whose *expansion* it declines.
    ///
    /// The gate proves both halves of the claim, which is what keeps this from
    /// being an excuse: this compiler really does fail on the probe, with the
    /// diagnostic printed rather than assumed, and the exported expansion
    /// really does agree with central differences taken in OpenModelica.
    DifferentiatedInOpenModelica,
    /// Refused at expansion time, in both artifacts, citing `rule` and naming
    /// `says` in the diagnostic.
    Refused {
        /// The rule id both artifacts must cite.
        rule: &'static str,
        /// Text the diagnostic must contain, so the id is not the only anchor.
        says: &'static str,
    },
    /// Not a Modelica program, so the engine is never asked. The gate proves
    /// it by compiling the probe's primal, with no `jacobian` call in it.
    Untypable,
}

/// The gate that runs a row.
///
/// Families partition the table, so each gate owns a disjoint slice and the
/// slices run side by side.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Family {
    /// The elementary functions, whose rules are the textbook ones.
    Elementary,
    /// The builtins that read or rearrange a shape.
    Shaping,
    /// The builtins whose tangent is the structural zero.
    Constant,
    /// The builtins stated non-differentiable.
    NonDifferentiable,
    /// `+ - * /` and their elementwise forms.
    Arithmetic,
    /// `^` and `.^`.
    Power,
    /// The unary operators.
    Unary,
    /// Declaration bindings and variability.
    Declaration,
    /// The expression forms built out of other expressions: array literals and
    /// `if` expressions.
    Composite,
    /// Calls to a function declared in the file, and the call shapes refused.
    Call,
    /// The statement forms a differentiated body admits, and the ones it does
    /// not.
    Statement,
}

impl Family {
    /// Every family, so a gate cannot quietly stop covering one.
    pub const ALL: &'static [Family] = &[
        Family::Elementary,
        Family::Shaping,
        Family::Constant,
        Family::NonDifferentiable,
        Family::Arithmetic,
        Family::Power,
        Family::Unary,
        Family::Declaration,
        Family::Composite,
        Family::Call,
        Family::Statement,
    ];
}

/// How a probe carries a construct's result out of the function whole.
///
/// The entries are the result's own components, one output per component, so
/// nothing between the construct and the Jacobian can absorb a rank the
/// tangent collapsed. That absorption is the failure the shape rules exist to
/// prevent, which is why no probe is allowed to reduce.
#[derive(Debug, Clone, Copy)]
pub struct Carrier {
    /// The Modelica declaration of the result variable, which is named `r`.
    pub declaration: &'static str,
    /// One output entry per component of the result, each a component of `r`.
    pub entries: &'static [&'static str],
}

/// A rank-0 result.
pub const SCALAR_OUT: Carrier = Carrier {
    declaration: "Real r",
    entries: &["r"],
};

/// A `Real[3]` result.
pub const VECTOR_OUT: Carrier = Carrier {
    declaration: "Real r[3]",
    entries: &["r[1]", "r[2]", "r[3]"],
};

/// A `Real[3, 3]` result.
pub const MATRIX_OUT: Carrier = Carrier {
    declaration: "Real r[3, 3]",
    entries: &[
        "r[1, 1]", "r[1, 2]", "r[1, 3]", "r[2, 1]", "r[2, 2]", "r[2, 3]", "r[3, 1]", "r[3, 2]",
        "r[3, 3]",
    ],
};

/// A `Real[6]` result: two vectors concatenated.
pub const VECTOR6_OUT: Carrier = Carrier {
    declaration: "Real r[6]",
    entries: &["r[1]", "r[2]", "r[3]", "r[4]", "r[5]", "r[6]"],
};

/// A `Real[6, 3]` result: two matrices concatenated along their first axis.
pub const MATRIX63_OUT: Carrier = Carrier {
    declaration: "Real r[6, 3]",
    entries: &[
        "r[1, 1]", "r[2, 2]", "r[3, 3]", "r[4, 1]", "r[5, 2]", "r[6, 3]", "r[1, 3]", "r[4, 2]",
        "r[6, 1]",
    ],
};

/// A `Real[2]` result: two rank-0 values stacked by an array literal.
pub const PAIR_OUT: Carrier = Carrier {
    declaration: "Real r[2]",
    entries: &["r[1]", "r[2]"],
};

/// A `Real[2, 2]` result: a scalar filled into a square.
pub const FILL_SCALAR_OUT: Carrier = Carrier {
    declaration: "Real r[2, 2]",
    entries: &["r[1, 1]", "r[1, 2]", "r[2, 1]", "r[2, 2]"],
};

/// A `Real[2, 3]` result: a vector filled along a new first axis.
pub const FILL_VECTOR_OUT: Carrier = Carrier {
    declaration: "Real r[2, 3]",
    entries: &[
        "r[1, 1]", "r[1, 2]", "r[1, 3]", "r[2, 1]", "r[2, 2]", "r[2, 3]",
    ],
};

/// A `Real[2, 3, 3]` result: a matrix filled along a new first axis.
pub const FILL_MATRIX_OUT: Carrier = Carrier {
    declaration: "Real r[2, 3, 3]",
    entries: &[
        "r[1, 1, 1]",
        "r[1, 2, 3]",
        "r[1, 3, 2]",
        "r[2, 1, 3]",
        "r[2, 2, 1]",
        "r[2, 3, 3]",
    ],
};

/// One admission-table row: a construct, the shapes of its operands, the probe
/// that exercises it, and what synthesis does with it.
#[derive(Debug, Clone)]
pub struct Row {
    /// The gate that runs this row.
    pub family: Family,
    /// The construct, named as the engine names it.
    pub construct: &'static str,
    /// The shape of each operand, in order. Empty for a construct whose form
    /// is named rather than shaped.
    pub operands: Vec<Shape>,
    /// The construct written over its operands: `{a}` and `{b}` are the two
    /// moving operands, `{c}` is a held-still value of the second operand's
    /// shape.
    pub form: String,
    /// How the result leaves the probe function.
    pub carrier: Carrier,
    /// A complete function body, for a construct whose form is a declaration
    /// rather than an expression. `{name}` stands for the function's name.
    pub body: Option<&'static str>,
    /// What synthesis does with the pair.
    pub verdict: Verdict,
}

impl Row {
    /// The number of Jacobian rows this row's probe function returns.
    pub fn width(&self) -> usize {
        self.carrier.entries.len()
    }

    /// The construct written out with its operands substituted.
    ///
    /// A construct written over two values whose shapes cannot differ, such as
    /// an array literal or an `if` expression, varies one operand shape and
    /// still needs a second value of it; `{b}` and `{c}` fall back to the
    /// first operand's shape so one row states both.
    pub fn expression(&self) -> String {
        let first = self.operands.first().map_or("", |shape| shape.moving());
        let paired = self.operands.get(1).or_else(|| self.operands.first());
        let second = paired.map_or("", |shape| shape.moving_again());
        let held = paired.map_or("", |shape| shape.held_still());
        self.form
            .replace("{a}", first)
            .replace("{b}", second)
            .replace("{c}", held)
    }

    /// The Modelica function this row's probe differentiates.
    ///
    /// The differentiated input is always `Real[3]`, so every row's Jacobian
    /// has three columns and one point generator serves the whole table.
    pub fn function(&self, name: &str) -> String {
        if let Some(body) = self.body {
            return body.replace("{name}", name);
        }
        let mut lines = vec![
            format!("function {name}"),
            "  input Real u[3];".to_string(),
            format!("  output Real y[{}];", self.width()),
            "protected".to_string(),
            format!("  {};", self.carrier.declaration),
            "algorithm".to_string(),
            format!("  r := {};", self.expression()),
        ];
        for (index, entry) in self.carrier.entries.iter().enumerate() {
            lines.push(format!("  y[{}] := {entry};", index + 1));
        }
        lines.push(format!("end {name};"));
        lines.join("\n")
    }
}

impl fmt::Display for Row {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "`{}`", self.construct)?;
        if self.operands.is_empty() {
            return Ok(());
        }
        let shapes: Vec<String> = self.operands.iter().map(ToString::to_string).collect();
        write!(formatter, " at ({})", shapes.join(", "))
    }
}

/// Constructs whose admission is a named form rather than a shape of operands.
pub const NAMED_FORMS: &[&str] = &[
    "^ with a moving exponent",
    ".^ with a moving exponent",
    "a bound local",
    "a bound output",
    "a constant local",
    "a parameter local",
    "a binding reading a later declaration",
    "a binding that moves under a variability that holds still",
    "a vectorized call to a function in scope",
    "an assignment statement",
    "a for statement",
    "an if statement",
    "an assert statement",
    "a while statement",
    "a call statement",
];

/// Constructs the engine reaches through an expression form rather than
/// through a name it looks up, whose admission still varies with the shape of
/// the values they are written over.
///
/// These are the surface section 4 admits without naming a builtin: an array
/// literal, an `if` expression, and a call to a function declared in the file.
/// Listing them here is what puts them in [`construct_universe`], so each owes
/// the table a row at every shape exactly as a named builtin does.
pub const SHAPED_FORMS: &[(&str, usize)] = &[
    ("an array literal", 1),
    ("an if expression", 1),
    ("a call to a function in scope", 1),
];

/// Every construct the engine can meet, with the number of operands the table
/// varies for it.
///
/// The builtins and the operators are read out of the engine's own name and
/// operator lists rather than restated, so a rule added to the engine without
/// a table row is a pair [`unstated_pairs`] reports. A unary and a binary
/// operator can share a spelling, so a spelled operator appears twice, once at
/// each arity.
///
/// The forms the engine reaches without looking up a name, which are the
/// statements, the array literal, the `if` expression and the call, cannot be
/// read out of a list the engine already keeps, because the engine reaches
/// them by matching an AST shape. They are enumerated in [`NAMED_FORMS`] and
/// [`SHAPED_FORMS`] instead, which is what keeps them inside the closure
/// rather than outside it.
pub fn construct_universe() -> Vec<(&'static str, usize)> {
    let mut universe: Vec<(&'static str, usize)> = Vec::new();
    for name in crate::builtins::UNARY_RULE {
        universe.push((name, 1));
    }
    for name in crate::builtins::BINARY_RULE {
        universe.push((name, 2));
    }
    for name in crate::builtins::MAPPED_RULE {
        // `cat(n, a, b)` varies two value operands; `fill(v, n, …)` varies one.
        universe.push((name, usize::from(*name == "cat") + 1));
    }
    for name in crate::builtins::CONSTANT {
        universe.push((name, 1));
    }
    for name in crate::builtins::REFUSED {
        universe.push((name, 1));
    }
    for (_, text) in crate::engine::BINARY_OPERATORS {
        universe.push((text, 2));
    }
    for (_, text) in crate::engine::UNARY_OPERATORS {
        universe.push((text, 1));
    }
    for name in NAMED_FORMS {
        universe.push((name, 0));
    }
    for (name, arity) in SHAPED_FORMS {
        universe.push((name, *arity));
    }
    universe
}

/// Every (construct, operand-shape) pair the universe states and the table
/// does not state exactly once, and every table row the universe does not
/// reach.
///
/// An empty result is the closure property the gates ask for: no construct
/// the engine can reach is left unstated, and no pair is stated two ways.
pub fn unstated_pairs() -> Vec<String> {
    let universe = construct_universe();
    let rows = table();
    let mut reported = Vec::new();
    for (construct, arity) in &universe {
        for operands in shape_tuples(*arity) {
            let stated = rows
                .iter()
                .filter(|row| row.construct == *construct && row.operands == operands)
                .count();
            if stated == 1 {
                continue;
            }
            let shapes: Vec<String> = operands.iter().map(ToString::to_string).collect();
            reported.push(format!(
                "`{construct}` at ({}) is stated {stated} times, not once",
                shapes.join(", ")
            ));
        }
    }
    for row in &rows {
        if !universe
            .iter()
            .any(|(construct, arity)| *construct == row.construct && *arity == row.operands.len())
        {
            reported.push(format!(
                "{row} is a row for a construct the engine cannot reach"
            ));
        }
    }
    reported
}

/// Every tuple of shapes of the given length, in a fixed order.
pub(crate) fn shape_tuples(arity: usize) -> Vec<Vec<Shape>> {
    let mut tuples = vec![Vec::new()];
    for _ in 0..arity {
        tuples = tuples
            .iter()
            .flat_map(|tuple| {
                Shape::ALL.iter().map(|shape| {
                    let mut next = tuple.clone();
                    next.push(*shape);
                    next
                })
            })
            .collect();
    }
    tuples
}
