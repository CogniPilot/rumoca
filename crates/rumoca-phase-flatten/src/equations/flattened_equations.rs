use rumoca_ir_flat as flat;

/// Result of flattening equations, containing both regular equations and when-clauses.
/// This is needed because for/if-equations can contain when-equations inside them.
#[derive(Default)]
pub(crate) struct FlattenedEquations {
    /// Regular flat equations (continuous, discrete)
    pub equations: Vec<flat::Equation>,
    /// Structured source equation families (MLS §8.3.3).
    pub structured_equations: Vec<flat::StructuredEquationFamily>,
    /// Assertion equations preserved from equation sections (MLS §8.3.7).
    pub assert_equations: Vec<flat::AssertEquation>,
    /// When-clauses extracted from nested when-equations
    pub when_clauses: Vec<flat::WhenClause>,
    /// Definite roots from Connections.root() calls (MLS §9.4.1).
    /// Stores the qualified path to the overconstrained record.
    pub definite_roots: Vec<String>,
    /// Branches from Connections.branch(a, b) calls (MLS §9.4).
    /// Each entry is (from_path, to_path) forming a required edge in the VCG.
    pub branches: Vec<(String, String)>,
    /// Potential roots from Connections.potentialRoot(a, priority) calls (MLS §9.4).
    /// Each entry is (path, priority) where lower priority means more likely to be root.
    pub potential_roots: Vec<(String, i64)>,
}

impl FlattenedEquations {
    /// Merge another flattened bundle into this one.
    ///
    /// Keeping merge logic centralized avoids accidentally dropping side-channel
    /// data when adding new flattened outputs.
    pub(super) fn append(&mut self, mut other: FlattenedEquations) {
        let equation_offset = self.equations.len();
        for for_equation in &mut other.structured_equations {
            for_equation.first_equation_index += equation_offset;
        }
        self.equations.append(&mut other.equations);
        self.structured_equations
            .append(&mut other.structured_equations);
        self.assert_equations.append(&mut other.assert_equations);
        self.when_clauses.append(&mut other.when_clauses);
        self.definite_roots.append(&mut other.definite_roots);
        self.branches.append(&mut other.branches);
        self.potential_roots.append(&mut other.potential_roots);
    }

    pub(super) fn is_empty(&self) -> bool {
        self.equations.is_empty()
            && self.structured_equations.is_empty()
            && self.assert_equations.is_empty()
            && self.when_clauses.is_empty()
            && self.definite_roots.is_empty()
            && self.branches.is_empty()
            && self.potential_roots.is_empty()
    }
}
