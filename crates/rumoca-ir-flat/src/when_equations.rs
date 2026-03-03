use super::*;

/// MLS §8.3.5: When clause
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhenClause {
    /// The condition expression.
    pub condition: Expression,
    /// Equations active when the condition triggers.
    pub equations: Vec<WhenEquation>,
    /// Source span for error reporting.
    pub span: Span,
}

impl WhenClause {
    /// Create a new when clause with span information.
    pub fn new(condition: Expression, span: Span) -> Self {
        Self {
            condition,
            equations: Vec::new(),
            span,
        }
    }

    /// Add an equation to this when clause.
    pub fn add_equation(&mut self, eq: WhenEquation) {
        self.equations.push(eq);
    }
}

/// An equation inside a when clause (MLS §8.3.5).
///
/// When-clauses can contain:
/// - Simple assignments: `v = expr`
/// - Reinit statements: `reinit(x, expr)`
/// - Assert statements: `assert(condition, message)`
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
        message: String,
        span: Span,
        origin: String,
    },
    /// Terminate simulation: `terminate(message)`
    Terminate {
        message: String,
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
        /// Equations for the else branch (may be empty).
        else_branch: Vec<WhenEquation>,
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
        message: String,
        span: Span,
        origin: impl Into<String>,
    ) -> Self {
        Self::Assert {
            condition,
            message,
            span,
            origin: origin.into(),
        }
    }

    /// Create a new terminate when equation.
    pub fn terminate(message: String, span: Span, origin: impl Into<String>) -> Self {
        Self::Terminate {
            message,
            span,
            origin: origin.into(),
        }
    }

    /// Create a new conditional when equation (if-statement inside when-clause).
    pub fn conditional(
        branches: Vec<(Expression, Vec<WhenEquation>)>,
        else_branch: Vec<WhenEquation>,
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
