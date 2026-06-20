//! Linear register-based ops used by compiled evaluators.

use serde::{Deserialize, Serialize};

/// Register index in a lowered op sequence.
pub type Reg = u32;

/// Scalar unary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
    Abs,
    Sign,
    Sqrt,
    Floor,
    Ceil,
    Trunc,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Exp,
    Log,
    Log10,
}

impl UnaryOp {
    #[must_use]
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Neg => "Neg",
            Self::Not => "Not",
            Self::Abs => "Abs",
            Self::Sign => "Sign",
            Self::Sqrt => "Sqrt",
            Self::Floor => "Floor",
            Self::Ceil => "Ceil",
            Self::Trunc => "Trunc",
            Self::Sin => "Sin",
            Self::Cos => "Cos",
            Self::Tan => "Tan",
            Self::Asin => "Asin",
            Self::Acos => "Acos",
            Self::Atan => "Atan",
            Self::Sinh => "Sinh",
            Self::Cosh => "Cosh",
            Self::Tanh => "Tanh",
            Self::Exp => "Exp",
            Self::Log => "Log",
            Self::Log10 => "Log10",
        }
    }
}

/// Scalar binary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    And,
    Or,
    Atan2,
    Min,
    Max,
}

impl BinaryOp {
    #[must_use]
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Add => "Add",
            Self::Sub => "Sub",
            Self::Mul => "Mul",
            Self::Div => "Div",
            Self::Pow => "Pow",
            Self::And => "And",
            Self::Or => "Or",
            Self::Atan2 => "Atan2",
            Self::Min => "Min",
            Self::Max => "Max",
        }
    }
}

/// Comparison operation that yields Modelica boolean-as-real (`0.0`/`1.0`).
///
/// Equality and inequality are exact IEEE comparisons at Solve-IR row level.
/// Relation event detection is represented separately by signed residual root
/// functions and solver root tolerances, not by tolerant `Eq`/`Ne` rows.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CompareOp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

impl CompareOp {
    #[must_use]
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Lt => "Lt",
            Self::Le => "Le",
            Self::Gt => "Gt",
            Self::Ge => "Ge",
            Self::Eq => "Eq",
            Self::Ne => "Ne",
        }
    }

    #[must_use]
    pub fn compare(self, lhs: f64, rhs: f64) -> bool {
        match self {
            Self::Lt => lhs < rhs,
            Self::Le => lhs <= rhs,
            Self::Gt => lhs > rhs,
            Self::Ge => lhs >= rhs,
            Self::Eq => lhs == rhs,
            Self::Ne => lhs != rhs,
        }
    }

    #[must_use]
    pub fn compare_as_f64(self, lhs: f64, rhs: f64) -> f64 {
        if self.compare(lhs, rhs) { 1.0 } else { 0.0 }
    }
}

/// Supported deterministic random generators in MSL's `Modelica.Math.Random`
/// package. These are solver-IR op kinds, not Modelica identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RandomGenerator {
    Xorshift64Star,
    Xorshift128Plus,
    Xorshift1024Star,
}

/// Flat linear operation stream (no strings, no dynamic dispatch).
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum LinearOp {
    Const {
        dst: Reg,
        value: f64,
    },
    LoadTime {
        dst: Reg,
    },
    LoadY {
        dst: Reg,
        index: usize,
    },
    LoadP {
        dst: Reg,
        index: usize,
    },
    /// Runtime-indexed parameter load: `p[base + clamp(round(index), 0, count-1)]`.
    ///
    /// Lowering emits this in place of an N-deep `(idx==k ? p[slot_k] : prev)`
    /// select chain when a dynamic array subscript resolves to a contiguous,
    /// row-major run of parameter slots (`base..base+count`). `index` is a
    /// register holding the 0-based flat offset; it is rounded and clamped at
    /// evaluation so an in-range model index is exact and out-of-range is
    /// saturated rather than silently zero.
    LoadIndexedP {
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
    },
    /// Load AD seed for a state/algebraic/output scalar from `v[]`.
    LoadSeed {
        dst: Reg,
        index: usize,
    },
    /// Runtime-indexed AD seed load: `seed[base + clamp(round(index), 0, count-1)]`.
    ///
    /// Forward-mode dual of [`LinearOp::LoadIndexedP`] under parameter-seed AD
    /// (`SeedMode::SolverYAndP`): the loaded parameter's tangent is the seed at
    /// the same runtime offset, shifted into the seed region.
    LoadIndexedSeed {
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
    },
    /// Copy a register value. This keeps packed register ranges explicit
    /// without introducing expression-level aliases into solver IR.
    Move {
        dst: Reg,
        src: Reg,
    },
    /// Solve one component of a dense linear system `A * x = b`.
    ///
    /// `matrix_start..matrix_start+n*n` stores row-major `A`;
    /// `rhs_start..rhs_start+n` stores `b`; `component` selects `x[component]`.
    LinearSolveComponent {
        dst: Reg,
        matrix_start: Reg,
        rhs_start: Reg,
        n: usize,
        component: usize,
    },
    /// Host-backed table bound lookup (`*_Tmin`, `*_Tmax`, `*_AbscissaUmin`, `*_AbscissaUmax`).
    TableBounds {
        dst: Reg,
        table_id: Reg,
        max: bool,
    },
    /// Host-backed table lookup (`getTimeTableValue*`, `getTable1DValue*`).
    TableLookup {
        dst: Reg,
        table_id: Reg,
        column: Reg,
        input: Reg,
    },
    /// Host-backed table lookup slope d(lookup)/d(input) for AD rows.
    TableLookupSlope {
        dst: Reg,
        table_id: Reg,
        column: Reg,
        input: Reg,
    },
    /// Host-backed table next-event lookup (`getNextTimeEvent`).
    TableNextEvent {
        dst: Reg,
        table_id: Reg,
        time: Reg,
    },
    /// Deterministic random state initialization for MSL Xorshift generators.
    RandomInitialState {
        dst: Reg,
        generator: RandomGenerator,
        local_seed: Reg,
        global_seed: Reg,
        state_len: usize,
        state_index: usize,
    },
    /// Deterministic random sample in `(0, 1]` from an input state vector.
    RandomResult {
        dst: Reg,
        generator: RandomGenerator,
        state_start: Reg,
        state_len: usize,
    },
    /// Deterministic random output state component from an input state vector.
    RandomState {
        dst: Reg,
        generator: RandomGenerator,
        state_start: Reg,
        state_len: usize,
        state_index: usize,
    },
    /// Initialize an MSL impure random stream and return its stream id.
    ImpureRandomInit {
        dst: Reg,
        seed: Reg,
    },
    /// Draw one MSL impure random sample in `(0, 1]`.
    ImpureRandom {
        dst: Reg,
        id: Reg,
        call_site: u64,
    },
    /// Draw one MSL impure random integer sample in `[imin, imax]`.
    ImpureRandomInteger {
        dst: Reg,
        id: Reg,
        imin: Reg,
        imax: Reg,
        call_site: u64,
    },
    Unary {
        dst: Reg,
        op: UnaryOp,
        arg: Reg,
    },
    Binary {
        dst: Reg,
        op: BinaryOp,
        lhs: Reg,
        rhs: Reg,
    },
    Compare {
        dst: Reg,
        op: CompareOp,
        lhs: Reg,
        rhs: Reg,
    },
    Select {
        dst: Reg,
        cond: Reg,
        if_true: Reg,
        if_false: Reg,
    },
    /// Marks final row value for residual output.
    StoreOutput {
        src: Reg,
    },
}

/// Resolve a runtime flat offset register value to an absolute slot in a
/// contiguous `[base, base+count)` run, with the round-then-clamp semantics
/// shared by [`LinearOp::LoadIndexedP`] / [`LinearOp::LoadIndexedSeed`] across
/// the interpreter, JIT, and every codegen backend. `count == 0` is degenerate
/// and saturates to `base`.
#[must_use]
pub fn resolve_indexed_slot(index_value: f64, base: usize, count: usize) -> usize {
    if count == 0 {
        return base;
    }
    let rounded = index_value.round();
    let clamped = if rounded < 0.0 {
        0
    } else if rounded as usize >= count {
        count - 1
    } else {
        rounded as usize
    };
    base + clamped
}

impl LinearOp {
    #[must_use]
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Const { .. } => "Const",
            Self::LoadTime { .. } => "LoadTime",
            Self::LoadY { .. } => "LoadY",
            Self::LoadP { .. } => "LoadP",
            Self::LoadIndexedP { .. } => "LoadIndexedP",
            Self::LoadSeed { .. } => "LoadSeed",
            Self::LoadIndexedSeed { .. } => "LoadIndexedSeed",
            Self::Move { .. } => "Move",
            Self::LinearSolveComponent { .. } => "LinearSolveComponent",
            Self::TableBounds { .. } => "TableBounds",
            Self::TableLookup { .. } => "TableLookup",
            Self::TableLookupSlope { .. } => "TableLookupSlope",
            Self::TableNextEvent { .. } => "TableNextEvent",
            Self::RandomInitialState { .. } => "RandomInitialState",
            Self::RandomResult { .. } => "RandomResult",
            Self::RandomState { .. } => "RandomState",
            Self::ImpureRandomInit { .. } => "ImpureRandomInit",
            Self::ImpureRandom { .. } => "ImpureRandom",
            Self::ImpureRandomInteger { .. } => "ImpureRandomInteger",
            Self::Unary { .. } => "Unary",
            Self::Binary { .. } => "Binary",
            Self::Compare { .. } => "Compare",
            Self::Select { .. } => "Select",
            Self::StoreOutput { .. } => "StoreOutput",
        }
    }

    pub fn dst_register(&self) -> Option<Reg> {
        match *self {
            Self::Const { dst, .. }
            | Self::LoadTime { dst }
            | Self::LoadY { dst, .. }
            | Self::LoadP { dst, .. }
            | Self::LoadIndexedP { dst, .. }
            | Self::LoadSeed { dst, .. }
            | Self::LoadIndexedSeed { dst, .. }
            | Self::Move { dst, .. }
            | Self::LinearSolveComponent { dst, .. }
            | Self::TableBounds { dst, .. }
            | Self::TableLookup { dst, .. }
            | Self::TableLookupSlope { dst, .. }
            | Self::TableNextEvent { dst, .. }
            | Self::RandomInitialState { dst, .. }
            | Self::RandomResult { dst, .. }
            | Self::RandomState { dst, .. }
            | Self::ImpureRandomInit { dst, .. }
            | Self::ImpureRandom { dst, .. }
            | Self::ImpureRandomInteger { dst, .. }
            | Self::Unary { dst, .. }
            | Self::Binary { dst, .. }
            | Self::Compare { dst, .. }
            | Self::Select { dst, .. } => Some(dst),
            Self::StoreOutput { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOp, CompareOp, LinearOp, UnaryOp};

    #[test]
    fn compare_op_equality_is_exact_not_epsilon_based() {
        let near_zero = f64::MIN_POSITIVE;

        assert!(!CompareOp::Eq.compare(0.0, near_zero));
        assert!(CompareOp::Ne.compare(0.0, near_zero));
        assert_eq!(CompareOp::Eq.compare_as_f64(0.0, near_zero), 0.0);
        assert_eq!(CompareOp::Ne.compare_as_f64(0.0, near_zero), 1.0);
    }

    #[test]
    fn linear_op_kind_name_reports_stable_variant_name() {
        let op = LinearOp::TableNextEvent {
            dst: 0,
            table_id: 1,
            time: 2,
        };

        assert_eq!(op.kind_name(), "TableNextEvent");
    }

    #[test]
    fn binary_op_kind_name_reports_stable_variant_name() {
        assert_eq!(BinaryOp::Atan2.kind_name(), "Atan2");
    }

    #[test]
    fn unary_op_kind_name_reports_stable_variant_name() {
        assert_eq!(UnaryOp::Log10.kind_name(), "Log10");
    }

    #[test]
    fn compare_op_kind_name_reports_stable_variant_name() {
        assert_eq!(CompareOp::Ne.kind_name(), "Ne");
    }
}
