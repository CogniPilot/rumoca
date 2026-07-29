//! Exact source-model balance evidence produced alongside canonical DAE.
//!
//! Balance is a phase result, not DAE metadata. The canonical DAE therefore
//! cannot be queried later to recreate source interface corrections or omitted
//! semantic owners. ToDAE computes this record while it still owns the Flat
//! model and rejects a non-partial mismatch before returning a DAE.

/// Exact equation and unknown counts used by the ToDAE balance gate.
#[derive(Debug, Clone, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct BalanceDetail {
    pub state_unknowns: usize,
    pub algebraic_unknowns: usize,
    pub output_unknowns: usize,
    pub discrete_real_unknowns: usize,
    pub discrete_value_unknowns: usize,
    pub continuous_equations: usize,
    pub discrete_real_equations: usize,
    pub discrete_assignments: usize,
}

impl BalanceDetail {
    pub const fn unknowns(&self) -> usize {
        self.state_unknowns
            + self.algebraic_unknowns
            + self.output_unknowns
            + self.discrete_real_unknowns
            + self.discrete_value_unknowns
    }

    pub const fn equations(&self) -> usize {
        self.continuous_equations + self.discrete_real_equations + self.discrete_assignments
    }

    pub const fn equations_unknowns(&self) -> (usize, usize) {
        (self.equations(), self.unknowns())
    }

    pub fn balance(&self) -> i64 {
        self.equations() as i64 - self.unknowns() as i64
    }

    pub fn is_balanced(&self) -> bool {
        self.balance() == 0
    }
}

/// Single-line diagnostic wrapper for [`BalanceDetail`].
#[derive(Debug, Clone)]
pub struct BalanceBreakdown(pub Box<BalanceDetail>);

impl From<BalanceDetail> for BalanceBreakdown {
    fn from(detail: BalanceDetail) -> Self {
        Self(Box::new(detail))
    }
}

impl std::ops::Deref for BalanceBreakdown {
    type Target = BalanceDetail;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for BalanceBreakdown {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "unknowns[state={} algebraic={} output={} discrete_real={} discrete_value={}] \
             equations[continuous={} discrete_real={} discrete_assignment={}]",
            self.state_unknowns,
            self.algebraic_unknowns,
            self.output_unknowns,
            self.discrete_real_unknowns,
            self.discrete_value_unknowns,
            self.continuous_equations,
            self.discrete_real_equations,
            self.discrete_assignments,
        )
    }
}
