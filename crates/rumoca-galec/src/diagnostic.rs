//! SPEC_0008-shaped diagnostics for the GALEC language module.
//!
//! GALEC ASTs are *generated* (never parsed from source text yet, GAL-014),
//! so there is no source span to point at. Locations are therefore
//! **structural paths** into the AST (block / method / statement index …)
//! plus an optional provenance string naming the Modelica origin. When a
//! parser lands (parol grammar + LSP, SPEC_0034 Non-Goals), an optional
//! source span slots into [`Location`] as an additional field without
//! disturbing existing variants — diagnostics keep their stable codes.
//!
//! Error codes use the stable `EG0xx` range (**G**ALEC language). Runtime
//! error *signals* (§3.2.5) are language machinery modeled in the AST, not
//! diagnostics (GAL-018) — nothing here describes runtime behavior.

use crate::ast::BlockMethodKind;

/// One segment of a structural AST path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
    /// The enclosing block, by name.
    Block(String),
    /// A state compartment, by name.
    Compartment(String),
    /// A block-level variable (interface or protected), by name.
    Variable(String),
    /// One of the three block-interface methods.
    Method(BlockMethodKind),
    /// A user-defined function, by name.
    Function(String),
    /// A function parameter, by name.
    Parameter(String),
    /// A method/function local variable, by name.
    Local(String),
    /// A statement, by zero-based index within its statement list.
    Statement(usize),
    /// An if/elseif branch, by zero-based index.
    Branch(usize),
    /// The `else` branch of an if-statement.
    Else,
    /// A branch condition.
    Condition,
}

impl std::fmt::Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block(name) => write!(f, "block {name}"),
            Self::Compartment(name) => write!(f, "record {name}"),
            Self::Variable(name) => write!(f, "variable {name}"),
            Self::Method(kind) => write!(f, "method {}", kind.name()),
            Self::Function(name) => write!(f, "function {name}"),
            Self::Parameter(name) => write!(f, "parameter {name}"),
            Self::Local(name) => write!(f, "local {name}"),
            Self::Statement(index) => write!(f, "statement {index}"),
            Self::Branch(index) => write!(f, "branch {index}"),
            Self::Else => write!(f, "else branch"),
            Self::Condition => write!(f, "condition"),
        }
    }
}

/// Structural location of a diagnostic: an AST path plus optional Modelica
/// provenance (e.g. the flattened component the construct was lowered from).
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Location {
    pub path: Vec<PathSegment>,
    /// Optional origin in the source Modelica model, supplied by the
    /// projection (`rumoca-target-galec`) when available.
    pub provenance: Option<String>,
}

impl Location {
    /// Location with the given path and no provenance.
    #[must_use]
    pub fn at(path: Vec<PathSegment>) -> Self {
        Self {
            path,
            provenance: None,
        }
    }

    /// Location for a construct handled outside any block context (e.g. the
    /// standalone Real-literal formatter).
    #[must_use]
    pub fn detached() -> Self {
        Self::default()
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.path.is_empty() {
            write!(f, "<detached>")?;
        }
        for (i, segment) in self.path.iter().enumerate() {
            let separator = if i > 0 { " / " } else { "" };
            write!(f, "{separator}{segment}")?;
        }
        if let Some(provenance) = &self.provenance {
            write!(f, " (from {provenance})")?;
        }
        Ok(())
    }
}

/// Payload of [`GalecError::TypeMismatch`]: where the mismatch happened and
/// the two disagreeing types.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeMismatchDetail {
    /// The construct being typed, e.g. `assignment to \`self.x\``.
    pub context: String,
    pub expected: String,
    pub found: String,
}

impl std::fmt::Display for TypeMismatchDetail {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: expected {}, found {}",
            self.context, self.expected, self.found
        )
    }
}

/// GALEC language errors with stable `EG0xx` codes.
///
/// The printer emits the lexeme-level subset (EG001–EG009); the validator
/// (`crate::validate`, six analyses per SPEC_0034) adds EG010–EG040,
/// reusing [`Location`].
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum GalecError {
    /// GAL-019 / trap T7: only finite Real literals have a conformant
    /// spelling.
    #[error("{location}: Real literal `{value}` is not finite and has no GALEC spelling [EG001]")]
    NonFiniteRealLiteral { location: Location, value: f64 },

    /// Identifiers must be non-empty; an empty name is a lowering bug.
    #[error("{location}: empty identifier [EG002]")]
    EmptyIdentifier { location: Location },

    /// Quoted identifiers are single lexemes: non-empty, no quotes, no
    /// whitespace or control characters inside (trap T13).
    #[error("{location}: malformed quoted identifier '{content}': {reason} [EG003]")]
    MalformedQuotedIdentifier {
        location: Location,
        content: String,
        reason: &'static str,
    },

    /// The error-signal statement must set at least one signal (§3.2.5 §1.2).
    #[error("{location}: error-signal statement with no signals [EG004]")]
    EmptySignalStatement { location: Location },

    /// A `limit` statement must name at least one target.
    #[error("{location}: limit statement with no targets [EG005]")]
    EmptyLimitStatement { location: Location },

    /// An if-statement needs at least the `if` branch.
    #[error("{location}: if-statement with no branches [EG006]")]
    IfStatementWithoutBranches { location: Location },

    /// A multi-dimension constructor `{{…}}` must have at least one element
    /// (G-3.11).
    #[error("{location}: empty multi-dimension constructor [EG007]")]
    EmptyArrayConstructor { location: Location },

    /// A signal check listing signals (`[not] in …`) must list at least one.
    #[error("{location}: signal check with an empty signal list [EG008]")]
    EmptySignalTestList { location: Location },

    /// An if-expression needs at least the `if` branch (trap T12).
    #[error("{location}: if-expression with no branches [EG009]")]
    IfExpressionWithoutBranches { location: Location },

    // -----------------------------------------------------------------
    // Validator: name analysis (trap T13)
    // -----------------------------------------------------------------
    /// Identifier violates the lexical rules (ASCII-letter-first,
    /// `[A-Za-z0-9_]*` continuation).
    #[error("{location}: illegal identifier `{name}`: {reason} [EG010]")]
    IllegalIdentifier {
        location: Location,
        name: String,
        reason: &'static str,
    },

    /// Declared name collides with a keyword, reserved keyword, `__` prefix,
    /// builtin (incl. lifted variants), Appendix C reserved name, or a
    /// predefined error signal (GAL-015 collision surface).
    #[error("{location}: `{name}` collides with a reserved GALEC name [EG011]")]
    ReservedName { location: Location, name: String },

    /// Two declarations share a name within one namespace (S-2.5).
    #[error("{location}: duplicate declaration of `{name}` among {namespace} [EG012]")]
    DuplicateName {
        location: Location,
        name: String,
        namespace: &'static str,
    },

    /// Block-interface variables must be declared inputs, then outputs, then
    /// tunable parameters (G-2.1 ordering note).
    #[error(
        "{location}: interface variable `{name}` out of order (inputs, then outputs, then tunable parameters) [EG013]"
    )]
    InterfaceVariableOrder { location: Location, name: String },

    // -----------------------------------------------------------------
    // Validator: resolution
    // -----------------------------------------------------------------
    /// A reference does not resolve to a declared entity, parameter, local,
    /// or loop iterator.
    #[error("{location}: unresolved reference `{name}` [EG014]")]
    UnresolvedReference { location: Location, name: String },

    /// A call target is neither a builtin nor a user-defined function.
    #[error("{location}: call to unknown function `{name}` [EG015]")]
    UnknownFunction { location: Location, name: String },

    // -----------------------------------------------------------------
    // Validator: type analysis (trap T5)
    // -----------------------------------------------------------------
    /// Binary operands violate S-3.4 (equal element types, `/` Real-only,
    /// logical operators Boolean-only, arithmetic shape agreement — equal
    /// ranks or scalar broadcast — and scalar-only relational/equality/
    /// logical operands). `operands` spells the offending typing, e.g.
    /// `Integer + Real`.
    #[error("{location}: mistyped operands of `{op}` ({operands}): {requirement} [EG016]")]
    BinaryOperandTypes {
        location: Location,
        op: &'static str,
        operands: String,
        requirement: &'static str,
    },

    /// General type mismatch (assignment, condition, if-expression branches,
    /// call argument, subscript, …) with no implicit promotion. The payload
    /// is boxed to keep the error enum compact for `Result` returns.
    #[error("{location}: type mismatch in {detail} [EG017]")]
    TypeMismatch {
        location: Location,
        detail: Box<TypeMismatchDetail>,
    },

    /// A call passes the wrong number of arguments.
    #[error(
        "{location}: call to `{function}` passes {found} argument(s), expected {expected} [EG018]"
    )]
    CallInputArity {
        location: Location,
        function: String,
        expected: usize,
        found: usize,
    },

    /// A call binds the wrong number of outputs for its context (calls in
    /// expressions require output-arity 1; multi-assignments must match).
    #[error(
        "{location}: call to `{function}` in {context} binds {found} output(s), expected {expected} [EG019]"
    )]
    CallOutputArity {
        location: Location,
        function: String,
        context: &'static str,
        expected: usize,
        found: usize,
    },

    /// Function parameters and local variables must not have a component
    /// (state-compartment) type (S-2.7).
    #[error(
        "{location}: `{name}` has a component type; parameters and locals must be primitively typed [EG020]"
    )]
    ComponentTypedDeclaration { location: Location, name: String },

    /// State components are valueless at runtime: component references may
    /// appear only inside `size(…)` or as `limit` targets.
    #[error(
        "{location}: state component `{name}` used as a value (components are legal only in size() and limit) [EG021]"
    )]
    ComponentValueUse { location: Location, name: String },

    // -----------------------------------------------------------------
    // Validator: dimensionality analysis (trap T11)
    // -----------------------------------------------------------------
    /// Subscripts, dimensions, and loop bounds must be constant scalar
    /// Integer expressions (loop iterators + `size()` + builtins only).
    #[error("{location}: {context} is not statically evaluable: {reason} [EG022]")]
    NonStaticExpression {
        location: Location,
        context: &'static str,
        reason: &'static str,
    },

    /// Subscript count must match the declared dimensionality (whole-array
    /// references carry no subscripts).
    #[error("{location}: `{name}` has {declared} dimension(s) but {found} subscript(s) [EG023]")]
    SubscriptArity {
        location: Location,
        name: String,
        declared: usize,
        found: usize,
    },

    /// Block-level (state entity / compartment entity) dimensions must be
    /// Integer literals ≥ 1 (S-2.13, GAL-020).
    #[error(
        "{location}: dimension of block-level `{name}` must be an Integer literal >= 1 [EG024]"
    )]
    BlockDimensionNotLiteral { location: Location, name: String },

    /// Derived dimensions `:` are legal only on function input parameters
    /// (S-2.14).
    #[error(
        "{location}: derived dimension `:` on `{name}` is only legal for function input parameters [EG025]"
    )]
    DerivedDimensionOutsideInput { location: Location, name: String },

    /// A literal subscript lies outside a literal declared dimension —
    /// the statically trivial slice of §3.2.1(a) bounds safety (indices are
    /// 1-based).
    #[error(
        "{location}: `{name}` subscript {index} is out of bounds for dimension {dimension} of size {size} [EG040]"
    )]
    SubscriptOutOfBounds {
        location: Location,
        name: String,
        /// 1-based dimension position.
        dimension: usize,
        index: i64,
        size: i64,
    },

    // -----------------------------------------------------------------
    // Validator: termination analysis (S-2.10, S-2.11, GAL-017)
    // -----------------------------------------------------------------
    /// The static function call graph must be cycle-free.
    #[error("{location}: recursive call cycle: {cycle} [EG026]")]
    RecursiveCall { location: Location, cycle: String },

    /// Every user function must be transitively called from `DoStep` (dead
    /// functions are illegal).
    #[error("{location}: function `{name}` is not transitively reachable from DoStep [EG027]")]
    UnreachableFunction { location: Location, name: String },

    /// `Startup` may call builtins only.
    #[error("{location}: Startup may call builtins only, but calls user function `{name}` [EG028]")]
    StartupCallsUserFunction { location: Location, name: String },

    // -----------------------------------------------------------------
    // Validator: side-effect analysis (S-2.3, trap T12)
    // -----------------------------------------------------------------
    /// Stateless functions must not write state variables.
    #[error("{location}: stateless function writes state `{target}` [EG029]")]
    StatelessWritesState { location: Location, target: String },

    /// Stateless functions must not (transitively) call stateful functions.
    #[error("{location}: stateless function calls stateful function `{callee}` [EG030]")]
    StatelessCallsStateful { location: Location, callee: String },

    /// A stateful call must have no sibling function-calls or
    /// state-references within the same expression.
    #[error(
        "{location}: stateful call to `{callee}` has sibling calls or state references in the same expression [EG031]"
    )]
    StatefulCallNotIsolated { location: Location, callee: String },

    /// If-expressions must not contain stateful calls (trap T12).
    #[error("{location}: stateful call to `{callee}` inside an if-expression [EG032]")]
    StatefulCallInIfExpression { location: Location, callee: String },

    /// Control-inputs, input parameters, and loop iterators are read-only.
    #[error("{location}: illegal assignment to {kind} `{name}` [EG033]")]
    WriteToReadOnly {
        location: Location,
        kind: &'static str,
        name: String,
    },

    // -----------------------------------------------------------------
    // Validator: signal analysis (§3.2.5, GAL-018, trap T10)
    // -----------------------------------------------------------------
    /// An identifier used as an error signal names neither a declared signal
    /// nor a signal-closure in scope.
    #[error("{location}: `{name}` does not name an error signal in scope [EG034]")]
    UnknownSignal { location: Location, name: String },

    /// At most 16 user-defined error signals fit the 32-bit encoding
    /// (§3.2.5 §1.6).
    #[error(
        "{location}: {count} user-defined error signals declared; at most 16 are allowed [EG035]"
    )]
    TooManyUserSignals { location: Location, count: usize },

    /// The computed escape set contains a signal missing from the declared
    /// `signals` clause (declared must EXACTLY equal computed).
    #[error(
        "{location}: signal `{signal}` can escape `{function}` but is not declared in its signals clause [EG036]"
    )]
    UndeclaredEscape {
        location: Location,
        function: String,
        signal: String,
    },

    /// The declared `signals` clause lists a signal that can never escape.
    #[error("{location}: declared signal `{signal}` can never escape `{function}` [EG037]")]
    OverdeclaredSignal {
        location: Location,
        function: String,
        signal: String,
    },

    /// A signal check tests a signal that cannot be set at that point
    /// (unsettable or already caught, trap T10).
    #[error(
        "{location}: signal `{signal}` cannot be set here (unsettable or already caught) [EG038]"
    )]
    UntestableSignal { location: Location, signal: String },

    /// The signal-test-set of a check must be non-empty (§3.2.5 §1.4).
    #[error("{location}: signal check has an empty signal-test-set [EG039]")]
    EmptySignalTestSet { location: Location },
}

impl GalecError {
    /// Stable diagnostic code (SPEC_0008).
    #[must_use]
    pub const fn code(&self) -> &'static str {
        match self {
            Self::NonFiniteRealLiteral { .. } => "EG001",
            Self::EmptyIdentifier { .. } => "EG002",
            Self::MalformedQuotedIdentifier { .. } => "EG003",
            Self::EmptySignalStatement { .. } => "EG004",
            Self::EmptyLimitStatement { .. } => "EG005",
            Self::IfStatementWithoutBranches { .. } => "EG006",
            Self::EmptyArrayConstructor { .. } => "EG007",
            Self::EmptySignalTestList { .. } => "EG008",
            Self::IfExpressionWithoutBranches { .. } => "EG009",
            Self::IllegalIdentifier { .. } => "EG010",
            Self::ReservedName { .. } => "EG011",
            Self::DuplicateName { .. } => "EG012",
            Self::InterfaceVariableOrder { .. } => "EG013",
            Self::UnresolvedReference { .. } => "EG014",
            Self::UnknownFunction { .. } => "EG015",
            Self::BinaryOperandTypes { .. } => "EG016",
            Self::TypeMismatch { .. } => "EG017",
            Self::CallInputArity { .. } => "EG018",
            Self::CallOutputArity { .. } => "EG019",
            Self::ComponentTypedDeclaration { .. } => "EG020",
            Self::ComponentValueUse { .. } => "EG021",
            Self::NonStaticExpression { .. } => "EG022",
            Self::SubscriptArity { .. } => "EG023",
            Self::BlockDimensionNotLiteral { .. } => "EG024",
            Self::DerivedDimensionOutsideInput { .. } => "EG025",
            Self::SubscriptOutOfBounds { .. } => "EG040",
            Self::RecursiveCall { .. } => "EG026",
            Self::UnreachableFunction { .. } => "EG027",
            Self::StartupCallsUserFunction { .. } => "EG028",
            Self::StatelessWritesState { .. } => "EG029",
            Self::StatelessCallsStateful { .. } => "EG030",
            Self::StatefulCallNotIsolated { .. } => "EG031",
            Self::StatefulCallInIfExpression { .. } => "EG032",
            Self::WriteToReadOnly { .. } => "EG033",
            Self::UnknownSignal { .. } => "EG034",
            Self::TooManyUserSignals { .. } => "EG035",
            Self::UndeclaredEscape { .. } => "EG036",
            Self::OverdeclaredSignal { .. } => "EG037",
            Self::UntestableSignal { .. } => "EG038",
            Self::EmptySignalTestSet { .. } => "EG039",
        }
    }

    /// The structural location the error points at.
    #[must_use]
    pub const fn location(&self) -> &Location {
        match self {
            Self::NonFiniteRealLiteral { location, .. }
            | Self::EmptyIdentifier { location }
            | Self::MalformedQuotedIdentifier { location, .. }
            | Self::EmptySignalStatement { location }
            | Self::EmptyLimitStatement { location }
            | Self::IfStatementWithoutBranches { location }
            | Self::EmptyArrayConstructor { location }
            | Self::EmptySignalTestList { location }
            | Self::IfExpressionWithoutBranches { location }
            | Self::IllegalIdentifier { location, .. }
            | Self::ReservedName { location, .. }
            | Self::DuplicateName { location, .. }
            | Self::InterfaceVariableOrder { location, .. }
            | Self::UnresolvedReference { location, .. }
            | Self::UnknownFunction { location, .. }
            | Self::BinaryOperandTypes { location, .. }
            | Self::TypeMismatch { location, .. }
            | Self::CallInputArity { location, .. }
            | Self::CallOutputArity { location, .. }
            | Self::ComponentTypedDeclaration { location, .. }
            | Self::ComponentValueUse { location, .. }
            | Self::NonStaticExpression { location, .. }
            | Self::SubscriptArity { location, .. }
            | Self::BlockDimensionNotLiteral { location, .. }
            | Self::DerivedDimensionOutsideInput { location, .. }
            | Self::SubscriptOutOfBounds { location, .. }
            | Self::RecursiveCall { location, .. }
            | Self::UnreachableFunction { location, .. }
            | Self::StartupCallsUserFunction { location, .. }
            | Self::StatelessWritesState { location, .. }
            | Self::StatelessCallsStateful { location, .. }
            | Self::StatefulCallNotIsolated { location, .. }
            | Self::StatefulCallInIfExpression { location, .. }
            | Self::WriteToReadOnly { location, .. }
            | Self::UnknownSignal { location, .. }
            | Self::TooManyUserSignals { location, .. }
            | Self::UndeclaredEscape { location, .. }
            | Self::OverdeclaredSignal { location, .. }
            | Self::UntestableSignal { location, .. }
            | Self::EmptySignalTestSet { location } => location,
        }
    }
}
