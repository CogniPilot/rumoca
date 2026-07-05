/// Balance detail after applying initialization-only deficit closure.
///
/// Modelica initialization has its own equation system: fixed starts and
/// initial equations constrain otherwise free initial unknowns without making
/// an overdetermined simulation DAE acceptable. This detail is therefore
/// deficit-only: initial equations can close missing scalar equations, but they
/// never mask surplus equations.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct InitialClosureBalanceDetail {
    pub scalar_equations: usize,
    pub scalar_unknowns: usize,
    pub deficit_before: i64,
    pub overconstrained_root_gauge_scalars: i64,
    pub overconstrained_break_edge_scalars: i64,
    pub initial_equation_scalars: i64,
    pub initial_algorithm_scalars: i64,
    pub closure_used: i64,
    pub deficit_after: i64,
}

impl InitialClosureBalanceDetail {
    pub fn scalar_equations_with_closure(&self) -> usize {
        self.scalar_equations + self.closure_used as usize
    }

    pub fn balance_with_closure(&self) -> i64 {
        self.scalar_equations_with_closure() as i64 - self.scalar_unknowns as i64
    }

    pub fn is_admissible(&self) -> bool {
        self.balance_with_closure() == 0
    }
}
