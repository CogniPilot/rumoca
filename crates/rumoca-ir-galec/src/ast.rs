//! Array-native GALEC AST (eFMI 1.0.0 Beta 1, §3.2) per SPEC_0034 GAL-026.
//!
//! The AST covers the full fixed-sample discrete subset of GALEC plus the
//! error-signal machinery (GAL-018). Shapes that the grammar makes illegal are
//! made unrepresentable where cheap:
//!
//! - the three block-interface methods are dedicated [`Block`] fields, so a
//!   block always has exactly `Startup`/`Recalibrate`/`DoStep` and
//!   [`BlockMethod`] has no parameter list (trap T1);
//! - unary minus takes a [`Reference`], not an expression (trap T4); use
//!   [`Expression::negated_real`] / [`Expression::negated_integer`] to negate
//!   arbitrary expressions;
//! - if-expressions have a mandatory `else` branch (trap T12);
//! - block-interface methods expose only [`PredefinedSignal`]s (§3.2.5 §1.3).
//!
//! Everything else (name legality, typing, escape-set dataflow, dimensional
//! analysis) is the future validator's job and is deliberately *not* encoded
//! structurally.

/// A plain GALEC identifier (ASCII-letter-first; legality checked by the
/// validator, trap T13).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

impl Identifier {
    /// Create an identifier from any string-like value.
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    /// The identifier text.
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A GALEC name: a plain identifier or a quoted identifier such as
/// `'a.b[2].c'` or `'previous(x)'` (traceability device, trap T13).
///
/// The quoted variant stores the content *between* the quotes; the printer
/// adds the surrounding `'` characters. Well-formedness of the quoted content
/// (scalarized-reference structure, no whitespace) is a validator concern;
/// the printer only rejects content that cannot be a single lexeme at all.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    /// Plain identifier, e.g. `firstTick`.
    Ident(Identifier),
    /// Quoted identifier content, e.g. `previous(feedback.y)` for
    /// `'previous(feedback.y)'`.
    Quoted(String),
}

impl Name {
    /// Plain identifier name.
    pub fn ident(name: impl Into<String>) -> Self {
        Self::Ident(Identifier::new(name))
    }

    /// Quoted identifier name (content without the surrounding quotes).
    pub fn quoted(content: impl Into<String>) -> Self {
        Self::Quoted(content.into())
    }
}

/// GALEC primitive types (§3.1: there is no String type).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScalarType {
    Real,
    Integer,
    Boolean,
}

impl ScalarType {
    /// GALEC keyword for the type.
    #[must_use]
    pub const fn keyword(self) -> &'static str {
        match self {
            Self::Real => "Real",
            Self::Integer => "Integer",
            Self::Boolean => "Boolean",
        }
    }
}

/// Declared type of a variable: a primitive or a state-compartment (record)
/// reference (S-2.7).
#[derive(Debug, Clone, PartialEq)]
pub enum TypeRef {
    Primitive(ScalarType),
    /// Component type: the name of a `record` state compartment.
    Compartment(Name),
}

/// One declared dimension of a multi-dimensional variable.
#[derive(Debug, Clone, PartialEq)]
pub enum Dimension {
    /// Derived dimension `:` — legal only for function *input* parameters
    /// (S-2.14); enforced by the validator.
    Derived,
    /// A constant-scalar-integer-expression (literal at block level, S-2.13).
    Expr(Expression),
}

/// Modelica-style `(min = …, max = …)` declaration attributes (Beta-1 grammar
/// gap adopted per SPEC_0034 D7; semantics: saturation ranges, trap T3).
#[derive(Debug, Clone, PartialEq, Default)]
pub struct RangeAttributes {
    pub min: Option<Expression>,
    pub max: Option<Expression>,
}

impl RangeAttributes {
    /// True when neither bound is present (nothing to print).
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.min.is_none() && self.max.is_none()
    }
}

/// A variable declaration: type, name, dimensions, range attributes.
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub ty: TypeRef,
    pub name: Name,
    /// Empty means scalar (zero-dimensional, S-2.12).
    pub dimensions: Vec<Dimension>,
    pub range: RangeAttributes,
}

impl VariableDeclaration {
    /// Scalar declaration of a primitive type without range attributes.
    pub fn scalar(ty: ScalarType, name: Name) -> Self {
        Self {
            ty: TypeRef::Primitive(ty),
            name,
            dimensions: Vec::new(),
            range: RangeAttributes::default(),
        }
    }
}

/// Causality of a block-interface variable declared before `protected` (D7).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterfaceKind {
    /// `input Real u;` — control-input, read-only inside the block.
    Input,
    /// `output Real y;` — control-output.
    Output,
    /// `parameter Real p;` before `protected` — tunable parameter.
    TunableParameter,
}

/// Kind of a block-internal state entity declared after `protected`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProtectedKind {
    /// `parameter` after `protected` — dependent parameter, recomputed in
    /// `Recalibrate`.
    DependentParameter,
    /// `constant` — value never changes after `Startup`.
    Constant,
    /// Plain declaration — discrete state (e.g. `'previous(x)'`, trap T2).
    State,
}

/// A block-interface variable (before `protected`).
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceVariable {
    pub kind: InterfaceKind,
    pub decl: VariableDeclaration,
    /// Manifest-bound start value mirroring the `Startup` assignment
    /// (GAL-020). Row-major for arrays. Not part of `.alg` concrete syntax
    /// and not read by this crate's validator; the start-mirrors-Startup
    /// cross-check (GAL-017/GAL-020) is owned by the projection and
    /// manifest layers (`rumoca-galec-codegen` / `rumoca-efmi`).
    pub start: Option<Expression>,
}

/// A block-internal state entity (after `protected`).
#[derive(Debug, Clone, PartialEq)]
pub struct ProtectedEntity {
    pub kind: ProtectedKind,
    pub decl: VariableDeclaration,
    /// Manifest-bound start value; see [`InterfaceVariable::start`].
    pub start: Option<Expression>,
}

/// A `record … end …;` state compartment declaration (S-2.2). Entities
/// reuse [`ProtectedEntity`] because the grammar allows the same
/// `constant`/`parameter` prefixes on compartment entities as on block-level
/// state entities, and manifest generation needs `start` for every state
/// variable including compartment members (GAL-020).
#[derive(Debug, Clone, PartialEq)]
pub struct StateCompartment {
    pub name: Name,
    pub entities: Vec<ProtectedEntity>,
}

/// The six predefined error signals with their normative 32-bit encoding
/// positions (§3.2.5 §1.6). Block-interface methods may expose only these.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PredefinedSignal {
    InvalidArgument,
    Overflow,
    Nan,
    SolveLinearEquationsFailed,
    NoSolutionFound,
    UnspecifiedError,
}

impl PredefinedSignal {
    /// All six predefined signals in bit order.
    pub const ALL: [Self; 6] = [
        Self::InvalidArgument,
        Self::Overflow,
        Self::Nan,
        Self::SolveLinearEquationsFailed,
        Self::NoSolutionFound,
        Self::UnspecifiedError,
    ];

    /// GALEC signal name.
    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            Self::InvalidArgument => "INVALID_ARGUMENT",
            Self::Overflow => "OVERFLOW",
            Self::Nan => "NAN",
            Self::SolveLinearEquationsFailed => "SOLVE_LINEAR_EQUATIONS_FAILED",
            Self::NoSolutionFound => "NO_SOLUTION_FOUND",
            Self::UnspecifiedError => "UNSPECIFIED_ERROR",
        }
    }

    /// Bit position in the 32-bit `ErrorSignalStatus` value (bits 0–5;
    /// bits 6–15 are reserved and must stay zero).
    #[must_use]
    pub const fn bit(self) -> u8 {
        match self {
            Self::InvalidArgument => 0,
            Self::Overflow => 1,
            Self::Nan => 2,
            Self::SolveLinearEquationsFailed => 3,
            Self::NoSolutionFound => 4,
            Self::UnspecifiedError => 5,
        }
    }
}

/// The three mandatory block-interface method kinds (§3.1.3).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockMethodKind {
    Startup,
    Recalibrate,
    DoStep,
}

impl BlockMethodKind {
    /// The method's fixed GALEC name.
    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            Self::Startup => "Startup",
            Self::Recalibrate => "Recalibrate",
            Self::DoStep => "DoStep",
        }
    }
}

/// A block-interface method body. Parameter-free by construction (trap T1);
/// its name is fixed by its position in [`Block`]. Exposable signals are
/// restricted to the six predefined ones by construction (§3.2.5 §1.3).
#[derive(Debug, Clone, PartialEq, Default)]
pub struct BlockMethod {
    /// `signals …;` clause; must equal the computed escape set (validator).
    pub signals: Vec<PredefinedSignal>,
    /// Method-local `protected` variables (not listed in the manifest).
    pub locals: Vec<VariableDeclaration>,
    pub statements: Vec<Statement>,
}

/// `function` (stateless) vs `method` (stateful), S-2.3.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    /// `function` — must not write state nor call stateful functions.
    Stateless,
    /// `method` — may write state variables.
    Stateful,
}

impl FunctionKind {
    /// GALEC keyword for the function kind.
    #[must_use]
    pub const fn keyword(self) -> &'static str {
        match self {
            Self::Stateless => "function",
            Self::Stateful => "method",
        }
    }
}

/// `input` / `output` data-flow direction of a function parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Input,
    Output,
}

impl Direction {
    /// GALEC keyword for the direction.
    #[must_use]
    pub const fn keyword(self) -> &'static str {
        match self {
            Self::Input => "input",
            Self::Output => "output",
        }
    }
}

/// A function parameter declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub direction: Direction,
    pub decl: VariableDeclaration,
}

/// A user-defined function (everything except the three block methods).
/// Must be transitively called from `DoStep` (S-2.11; validator).
#[derive(Debug, Clone, PartialEq)]
pub struct UserFunction {
    pub kind: FunctionKind,
    pub name: Name,
    /// `signals …;` clause; may include user-defined signals.
    pub signals: Vec<Identifier>,
    pub parameters: Vec<Parameter>,
    pub locals: Vec<VariableDeclaration>,
    pub statements: Vec<Statement>,
}

/// A GALEC `block` — the grammar's only start symbol (R-2.1).
///
/// Section order follows G-2.1: interface variables, `protected`,
/// compartments, protected entities, error signals, protected functions,
/// `public`, `Startup`/`Recalibrate`/`DoStep` plus other public functions.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub name: Name,
    /// Interface variables before `protected` (inputs, then outputs, then
    /// tunable parameters; ordering checked by the validator).
    pub interface: Vec<InterfaceVariable>,
    /// `record` state compartments (protected section).
    pub compartments: Vec<StateCompartment>,
    /// Protected state entities: dependent parameters, constants, states.
    pub protected: Vec<ProtectedEntity>,
    /// User error-signal declarations `signal Name;` (≤ 16; validator).
    pub error_signals: Vec<Identifier>,
    /// Functions declared in the protected section.
    pub protected_functions: Vec<UserFunction>,
    /// Mandatory initialization method (builtin calls only; validator).
    pub startup: BlockMethod,
    /// Mandatory recalibration method (emitted even when empty, GAL-017).
    pub recalibrate: BlockMethod,
    /// Mandatory control-cycle method.
    pub do_step: BlockMethod,
    /// Additional public functions after the three methods.
    pub public_functions: Vec<UserFunction>,
}

impl Block {
    /// A block with the given name, empty sections, and the three mandatory
    /// (initially empty) block-interface methods.
    pub fn new(name: Name) -> Self {
        Self {
            name,
            interface: Vec::new(),
            compartments: Vec::new(),
            protected: Vec::new(),
            error_signals: Vec::new(),
            protected_functions: Vec::new(),
            startup: BlockMethod::default(),
            recalibrate: BlockMethod::default(),
            do_step: BlockMethod::default(),
            public_functions: Vec::new(),
        }
    }
}

/// One dotted part of a reference: name plus optional subscripts.
#[derive(Debug, Clone, PartialEq)]
pub struct RefPart {
    pub name: Name,
    /// `computed-dimensions`: constant-scalar-integer-expressions (S-3.1).
    pub subscripts: Vec<Expression>,
}

impl RefPart {
    /// Unsubscripted part.
    pub fn plain(name: Name) -> Self {
        Self {
            name,
            subscripts: Vec::new(),
        }
    }
}

/// A reference. Local references are a single part (grammar `local-reference`);
/// state references are `self.` followed by one or more parts — the split is
/// structural so an illegal dotted local reference cannot be represented.
#[derive(Debug, Clone, PartialEq)]
pub enum Reference {
    /// `name[subs]` — function parameter, local variable, or loop iterator.
    Local(RefPart),
    /// `self.a[subs].b[subs]…` — block state access (always via `self.`).
    State(Vec<RefPart>),
}

impl Reference {
    /// Unsubscripted local reference.
    pub fn local(name: Name) -> Self {
        Self::Local(RefPart::plain(name))
    }

    /// Unsubscripted single-part state reference `self.<name>`.
    pub fn state(name: Name) -> Self {
        Self::State(vec![RefPart::plain(name)])
    }
}

/// Binary operators with the normative precedence classes (S-3.3).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Pow,
    Mul,
    Div,
    Add,
    Sub,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}

/// Normative operator precedence classes, highest first (S-3.3). Any
/// cross-class mix must be explicitly parenthesized (trap T6).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrecedenceClass {
    Power,
    Multiplicative,
    Additive,
    Relational,
    Equality,
    LogicalAnd,
    LogicalOr,
}

/// Associativity of a precedence class.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

impl BinaryOp {
    /// GALEC operator token.
    #[must_use]
    pub const fn token(self) -> &'static str {
        match self {
            Self::Pow => "^",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Add => "+",
            Self::Sub => "-",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Ge => ">=",
            Self::Eq => "==",
            Self::Ne => "<>",
            Self::And => "and",
            Self::Or => "or",
        }
    }

    /// The operator's precedence class.
    #[must_use]
    pub const fn precedence_class(self) -> PrecedenceClass {
        match self {
            Self::Pow => PrecedenceClass::Power,
            Self::Mul | Self::Div => PrecedenceClass::Multiplicative,
            Self::Add | Self::Sub => PrecedenceClass::Additive,
            Self::Lt | Self::Gt | Self::Le | Self::Ge => PrecedenceClass::Relational,
            Self::Eq | Self::Ne => PrecedenceClass::Equality,
            Self::And => PrecedenceClass::LogicalAnd,
            Self::Or => PrecedenceClass::LogicalOr,
        }
    }
}

impl PrecedenceClass {
    /// Associativity of the class (`^` is right-to-left, all else left).
    #[must_use]
    pub const fn associativity(self) -> Associativity {
        match self {
            Self::Power => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}

/// An if-expression: self-parenthesized with mandatory `else` (trap T12).
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    /// `(condition, value)` pairs: the `if` branch followed by any `elseif`
    /// branches; must be non-empty (checked by the printer).
    pub branches: Vec<(Expression, Expression)>,
    /// Mandatory `else` value — unrepresentable without one.
    pub else_value: Box<Expression>,
}

/// A function call `name(args)`.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub function: Name,
    pub arguments: Vec<Expression>,
}

/// GALEC expressions (G-3.1).
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Boolean literal.
    Bool(bool),
    /// Integer literal.
    Integer(i64),
    /// Real literal; printed with mandatory decimal places and signed
    /// exponent (trap T7). Must be finite (printer rejects NaN/±inf).
    Real(f64),
    /// Local or state reference.
    Ref(Reference),
    /// Dimension query `size(reference, dim)`.
    Size {
        array: Reference,
        dimension: Box<Expression>,
    },
    /// Function call used as an expression (output-arity 1; validator).
    Call(FunctionCall),
    /// Explicit parenthesized expression.
    Paren(Box<Expression>),
    /// If-expression.
    If(IfExpression),
    /// Multi-dimension constructor `{…}`; nested constructors for matrices,
    /// row-major. Must be non-empty (checked by the printer).
    Array(Vec<Expression>),
    /// Unary minus — grammar-limited to references (trap T4).
    Neg(Reference),
    /// `not (…)`; the printer always parenthesizes the argument (trap T12).
    Not(Box<Expression>),
    /// Binary operation; the AST shape is the normative evaluation order
    /// (no re-association, trap T6).
    Binary {
        op: BinaryOp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}

impl Expression {
    /// Binary operation helper.
    pub fn binary(op: BinaryOp, lhs: Self, rhs: Self) -> Self {
        Self::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    /// Negate a Real-typed expression without ever producing unary minus
    /// over a non-reference (trap T4): references become [`Expression::Neg`],
    /// literals fold, anything else becomes `0.0 - (expr)` per GAL-019.
    #[must_use]
    pub fn negated_real(expr: Self) -> Self {
        match expr {
            Self::Ref(reference) => Self::Neg(reference),
            Self::Real(value) => Self::Real(-value),
            other => Self::binary(BinaryOp::Sub, Self::Real(0.0), Self::Paren(Box::new(other))),
        }
    }

    /// Integer-typed counterpart of [`Expression::negated_real`], producing
    /// `0 - (expr)` for non-reference, non-literal operands. `i64::MIN` has
    /// no literal negation and also takes the `0 - (…)` form.
    #[must_use]
    pub fn negated_integer(expr: Self) -> Self {
        match expr {
            Self::Ref(reference) => Self::Neg(reference),
            Self::Integer(value) => match value.checked_neg() {
                Some(negated) => Self::Integer(negated),
                None => Self::integer_zero_minus(Self::Integer(value)),
            },
            other => Self::integer_zero_minus(other),
        }
    }

    /// The trap-T4 rewrite `0 - (expr)` for Integer-typed negation.
    fn integer_zero_minus(expr: Self) -> Self {
        Self::binary(BinaryOp::Sub, Self::Integer(0), Self::Paren(Box::new(expr)))
    }
}

/// Branch condition of an if-statement: Boolean expression or error-signal
/// check (§3.2.5 §1.4).
#[derive(Debug, Clone, PartialEq)]
pub enum Condition {
    Expression(Expression),
    SignalCheck(SignalCheck),
}

/// The listed part of a signal check: `[not] in s1, s2, …`.
#[derive(Debug, Clone, PartialEq)]
pub struct SignalTest {
    /// `not in …` — test the in-reachable set MINUS the listed signals.
    pub negated: bool,
    /// Listed signal names; must be non-empty (checked by the printer).
    pub signals: Vec<Identifier>,
}

/// Error-signal check `signal [closure] [[not] in s1, …] [or expr]`.
/// Checking is catching: satisfied checks unset their test set (trap T10).
#[derive(Debug, Clone, PartialEq)]
pub struct SignalCheck {
    /// Optional signal-closure variable capturing the caught signals.
    pub closure: Option<Identifier>,
    /// Optional restriction; `None` = unrestricted check.
    pub test: Option<SignalTest>,
    /// Optional `or expr` fallback condition.
    pub fallback: Option<Expression>,
}

/// One `if`/`elseif` branch of an if-statement.
#[derive(Debug, Clone, PartialEq)]
pub struct IfBranch {
    pub condition: Condition,
    pub body: Vec<Statement>,
}

/// `if … then … [elseif … then …]* [else …] end if;`.
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    /// `if` branch plus `elseif` branches; must be non-empty (printer).
    pub branches: Vec<IfBranch>,
    /// Optional `else` branch (statements may be empty).
    pub else_body: Option<Vec<Statement>>,
}

/// `for i in start[:step]:stop loop … end for;` with statically-evaluated
/// integer bounds (trap T11).
#[derive(Debug, Clone, PartialEq)]
pub struct ForLoop {
    /// Optional loop-iterator declaration (optional per the grammar).
    pub iterator: Option<Name>,
    pub start: Expression,
    pub step: Option<Expression>,
    pub stop: Expression,
    pub body: Vec<Statement>,
}

/// Target of a `limit` statement.
#[derive(Debug, Clone, PartialEq)]
pub enum LimitTarget {
    /// `limit self;` — limit all ranged state variables.
    SelfState,
    /// Limit one referenced entity (variable or component).
    Reference(Reference),
}

/// GALEC statements, including the error-signal statement (a 7th statement
/// kind per SPEC_0034 D7 — Beta-1 defines it in §3.2.5 but omits it from the
/// `statement` alternatives).
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// `reference := expression;`
    Assignment {
        target: Reference,
        value: Expression,
    },
    /// `(r1, …, rn) := call;` — the only way to bind multi-output calls.
    /// The target list may be empty (`() := f();`).
    MultiAssignment {
        targets: Vec<Reference>,
        call: FunctionCall,
    },
    /// Bare call statement (procedures / discarded outputs).
    Call(FunctionCall),
    /// If statement (conditions may be signal checks).
    If(IfStatement),
    /// Bounded for loop.
    For(ForLoop),
    /// `limit t1, t2, …;` — saturate ranged entities (trap T3). Must list at
    /// least one target (checked by the printer).
    Limit(Vec<LimitTarget>),
    /// Error-signal statement `signal s1, s2, …;` — sets signals and/or
    /// re-raises closures. Must list at least one identifier (printer).
    Signal(Vec<Identifier>),
}
