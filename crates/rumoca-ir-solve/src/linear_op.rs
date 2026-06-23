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

/// Native external function families preserved in Solve IR.
///
/// These are explicit runtime dependencies. Interpreters that do not provide
/// the native runtime must fail closed instead of substituting constants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExternalFunctionKind {
    BuildingsEnergyPlusSpawnExternalObject,
    BuildingsEnergyPlusInitialize,
    BuildingsEnergyPlusGetParameters,
    BuildingsEnergyPlusExchange,
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
    /// Load AD seed for a state/algebraic/output scalar from `v[]`.
    LoadSeed {
        dst: Reg,
        index: usize,
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
    /// Native external function call. `args[..arg_count]` are initialized
    /// scalar argument registers. `external_object_index` points into
    /// `SolveProblem.external_objects` when the call requires a native
    /// ExternalObject handle.
    ExternalCall {
        dst: Reg,
        function: ExternalFunctionKind,
        #[serde(default)]
        external_object_index: Option<usize>,
        args: [Reg; 8],
        arg_count: usize,
        output_index: usize,
        #[serde(default = "default_external_call_output_count")]
        output_count: usize,
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

fn default_external_call_output_count() -> usize {
    1
}

impl LinearOp {
    pub fn dst_register(&self) -> Option<Reg> {
        match *self {
            Self::Const { dst, .. }
            | Self::LoadTime { dst }
            | Self::LoadY { dst, .. }
            | Self::LoadP { dst, .. }
            | Self::LoadSeed { dst, .. }
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
            | Self::ExternalCall { dst, .. }
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
    use super::CompareOp;

    #[test]
    fn compare_op_equality_is_exact_not_epsilon_based() {
        let near_zero = f64::MIN_POSITIVE;

        assert!(!CompareOp::Eq.compare(0.0, near_zero));
        assert!(CompareOp::Ne.compare(0.0, near_zero));
        assert_eq!(CompareOp::Eq.compare_as_f64(0.0, near_zero), 0.0);
        assert_eq!(CompareOp::Ne.compare_as_f64(0.0, near_zero), 1.0);
    }
}
