use super::*;

/// MLS §8.3.5: one complete `when`/`elsewhen` equation owner.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WhenChain {
    /// First, mandatory `when` branch.
    first: WhenBranch,
    /// Remaining source-priority ordered `elsewhen` branches.
    else_when: Vec<WhenBranch>,
    /// Source span of the complete `when`/`elsewhen` equation.
    span: Span,
}

impl WhenChain {
    /// Create one nonempty chain owned by a source `when` equation.
    pub fn new(first: WhenBranch, span: Span) -> Self {
        Self {
            first,
            else_when: Vec::new(),
            span,
        }
    }

    /// Append the next source-priority `elsewhen` branch.
    pub fn push_else_when(&mut self, branch: WhenBranch) {
        self.else_when.push(branch);
    }

    /// Return the mandatory first `when` branch.
    pub fn first(&self) -> &WhenBranch {
        &self.first
    }

    /// Iterate over every branch in source-priority order.
    pub fn branches(&self) -> impl Iterator<Item = &WhenBranch> {
        std::iter::once(&self.first).chain(self.else_when.iter())
    }

    /// Mutably iterate over every branch in source-priority order.
    pub fn branches_mut(&mut self) -> impl Iterator<Item = &mut WhenBranch> {
        std::iter::once(&mut self.first).chain(self.else_when.iter_mut())
    }

    /// Return the number of source branches.
    pub fn branch_count(&self) -> usize {
        1 + self.else_when.len()
    }

    /// Return the complete source `when`/`elsewhen` owner span.
    pub fn span(&self) -> Span {
        self.span
    }
}

/// One ordered branch inside a [`WhenChain`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhenBranch {
    /// The condition expression.
    pub condition: Expression,
    /// Equations active when the condition triggers.
    pub equations: Vec<WhenEquation>,
    /// Exact source span of this branch's condition.
    pub span: Span,
}

impl WhenBranch {
    /// Create a new branch with exact condition provenance.
    pub fn new(condition: Expression, span: Span) -> Self {
        Self {
            condition,
            equations: Vec::new(),
            span,
        }
    }

    /// Add an equation to this branch.
    pub fn add_equation(&mut self, eq: WhenEquation) {
        self.equations.push(eq);
    }
}

/// An equation inside a when-chain branch (MLS §8.3.5).
///
/// When-clauses can contain:
/// - Simple assignments: `v = expr`
/// - Reinit statements: `reinit(x, expr)`
/// - Assert statements: `assert(condition, message[, level])`
/// - Terminate statements: `terminate(message)`
/// - Conditional branches: `if cond then ... elseif ... else ... end if`
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WhenEquation {
    /// Simple assignment: `target = value`
    Assign {
        target: VarName,
        value: Expression,
        span: Span,
        origin: String,
    },
    /// Reinitialize state variable: `reinit(state, value)`
    Reinit {
        state: VarName,
        value: Expression,
        span: Span,
        origin: String,
    },
    /// Runtime assertion: `assert(condition, message, level)`
    Assert {
        condition: Expression,
        message: Expression,
        level: Option<Box<Expression>>,
        span: Span,
        origin: String,
    },
    /// Terminate simulation: `terminate(message)`
    Terminate {
        message: Expression,
        span: Span,
        origin: String,
    },
    /// Conditional if-equation inside when-clause (MLS §8.3.5).
    ///
    /// When-clauses may contain if-equations to conditionally execute
    /// different assignments based on runtime conditions.
    Conditional {
        /// Condition/equation pairs for if/elseif branches.
        branches: Vec<(Expression, Vec<WhenEquation>)>,
        /// Source-present else branch; `Some([])` is distinct from no else.
        else_branch: Option<Vec<WhenEquation>>,
        span: Span,
        origin: String,
    },
    /// Multi-output function call: `(out1, out2, ...) = func(args)`
    ///
    /// Used for functions that return multiple values, where each output
    /// is assigned to a discrete-time variable in the when-clause.
    FunctionCallOutputs {
        /// Output variables being assigned (in order)
        outputs: Vec<VarName>,
        /// The function being called
        function: Expression,
        span: Span,
        origin: String,
    },
}

impl WhenEquation {
    /// Create a new assignment when equation.
    pub fn assign(
        target: VarName,
        value: Expression,
        span: Span,
        origin: impl Into<String>,
    ) -> Self {
        Self::Assign {
            target,
            value,
            span,
            origin: origin.into(),
        }
    }

    /// Create a new reinit when equation.
    pub fn reinit(
        state: VarName,
        value: Expression,
        span: Span,
        origin: impl Into<String>,
    ) -> Self {
        Self::Reinit {
            state,
            value,
            span,
            origin: origin.into(),
        }
    }

    /// Create a new assert when equation.
    pub fn assert(
        condition: Expression,
        message: Expression,
        level: Option<Expression>,
        span: Span,
        origin: impl Into<String>,
    ) -> Self {
        Self::Assert {
            condition,
            message,
            level: level.map(Box::new),
            span,
            origin: origin.into(),
        }
    }

    /// Create a new terminate when equation.
    pub fn terminate(message: Expression, span: Span, origin: impl Into<String>) -> Self {
        Self::Terminate {
            message,
            span,
            origin: origin.into(),
        }
    }

    /// Create a new conditional when equation (if-statement inside when-clause).
    pub fn conditional(
        branches: Vec<(Expression, Vec<WhenEquation>)>,
        else_branch: Option<Vec<WhenEquation>>,
        span: Span,
        origin: impl Into<String>,
    ) -> Self {
        Self::Conditional {
            branches,
            else_branch,
            span,
            origin: origin.into(),
        }
    }

    /// Create a new multi-output function call when equation.
    pub fn function_call_outputs(
        outputs: Vec<VarName>,
        function: Expression,
        span: Span,
        origin: impl Into<String>,
    ) -> Self {
        Self::FunctionCallOutputs {
            outputs,
            function,
            span,
            origin: origin.into(),
        }
    }

    /// Get the span for this equation.
    pub fn span(&self) -> Span {
        match self {
            Self::Assign { span, .. } => *span,
            Self::Reinit { span, .. } => *span,
            Self::Assert { span, .. } => *span,
            Self::Terminate { span, .. } => *span,
            Self::Conditional { span, .. } => *span,
            Self::FunctionCallOutputs { span, .. } => *span,
        }
    }

    /// Get the origin string for this equation.
    pub fn origin(&self) -> &str {
        match self {
            Self::Assign { origin, .. } => origin,
            Self::Reinit { origin, .. } => origin,
            Self::Assert { origin, .. } => origin,
            Self::Terminate { origin, .. } => origin,
            Self::Conditional { origin, .. } => origin,
            Self::FunctionCallOutputs { origin, .. } => origin,
        }
    }
}
