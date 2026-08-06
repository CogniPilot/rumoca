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

/// Constructor proof that every register read in one scalar program is
/// dominated by an earlier write.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScalarProgramRegisterFlow {
    register_count: usize,
}

impl ScalarProgramRegisterFlow {
    pub fn derive(program: &[LinearOp]) -> Result<Self, ScalarProgramRegisterError> {
        let mut initialized = Vec::new();
        let mut max_register = None;
        for (op_index, op) in program.iter().copied().enumerate() {
            if let Some(register) = validate_op_sources(op, op_index, &initialized)? {
                max_register = Some(max_register.map_or(register, |max: Reg| max.max(register)));
            }
            if let Some(dst) = op.dst_register() {
                mark_register_initialized(&mut initialized, dst);
                max_register = Some(max_register.map_or(dst, |max: Reg| max.max(dst)));
            }
        }
        Ok(Self {
            register_count: checked_register_count(max_register)?,
        })
    }

    pub const fn register_count(self) -> usize {
        self.register_count
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarProgramRegisterError {
    UndefinedRegister {
        op_index: usize,
        operation: &'static str,
        register: Reg,
    },
    EmptyRegisterRange {
        op_index: usize,
        operation: &'static str,
    },
    RegisterRangeOverflow {
        op_index: usize,
        operation: &'static str,
        start: Reg,
        len: usize,
    },
    InvalidProjection {
        op_index: usize,
        operation: &'static str,
        projection: usize,
        len: usize,
    },
    RegisterCountOverflow {
        register: Reg,
    },
}

impl std::fmt::Display for ScalarProgramRegisterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UndefinedRegister {
                op_index,
                operation,
                register,
            } => write!(
                f,
                "{operation} op {op_index} reads undefined register r{register}"
            ),
            Self::EmptyRegisterRange {
                op_index,
                operation,
            } => write!(f, "{operation} op {op_index} reads an empty register range"),
            Self::RegisterRangeOverflow {
                op_index,
                operation,
                start,
                len,
            } => write!(
                f,
                "{operation} op {op_index} register range r{start}..+{len} overflows"
            ),
            Self::InvalidProjection {
                op_index,
                operation,
                projection,
                len,
            } => write!(
                f,
                "{operation} op {op_index} projects element {projection} from range length {len}"
            ),
            Self::RegisterCountOverflow { register } => {
                write!(f, "register r{register} overflows the host register count")
            }
        }
    }
}

fn mark_register_initialized(initialized: &mut Vec<bool>, register: Reg) {
    let index = register as usize;
    if initialized.len() <= index {
        initialized.resize(index + 1, false);
    }
    initialized[index] = true;
}

fn validate_op_sources(
    op: LinearOp,
    op_index: usize,
    initialized: &[bool],
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    match op {
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadSeed { .. } => Ok(None),
        LinearOp::Move { src, .. }
        | LinearOp::Unary { arg: src, .. }
        | LinearOp::LoadIndexedP { index: src, .. }
        | LinearOp::LoadIndexedSeed { index: src, .. }
        | LinearOp::StoreOutput { src } => {
            require_register(op_index, op.kind_name(), src, initialized)?;
            Ok(Some(src))
        }
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
            require_register(op_index, op.kind_name(), lhs, initialized)?;
            require_register(op_index, op.kind_name(), rhs, initialized)?;
            Ok(Some(lhs.max(rhs)))
        }
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            require_register(op_index, op.kind_name(), cond, initialized)?;
            require_register(op_index, op.kind_name(), if_true, initialized)?;
            require_register(op_index, op.kind_name(), if_false, initialized)?;
            Ok(Some(cond.max(if_true).max(if_false)))
        }
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            component,
            ..
        } => linear_solve_sources(op_index, matrix_start, rhs_start, n, component, initialized)
            .map(Some),
        LinearOp::TableBounds { table_id, .. } => {
            require_register(op_index, op.kind_name(), table_id, initialized)?;
            Ok(Some(table_id))
        }
        LinearOp::TableLookup {
            table_id,
            column,
            input,
            ..
        }
        | LinearOp::TableLookupSlope {
            table_id,
            column,
            input,
            ..
        } => {
            require_register(op_index, op.kind_name(), table_id, initialized)?;
            require_register(op_index, op.kind_name(), column, initialized)?;
            require_register(op_index, op.kind_name(), input, initialized)?;
            Ok(Some(table_id.max(column).max(input)))
        }
        LinearOp::TableNextEvent { table_id, time, .. } => {
            require_register(op_index, op.kind_name(), table_id, initialized)?;
            require_register(op_index, op.kind_name(), time, initialized)?;
            Ok(Some(table_id.max(time)))
        }
        LinearOp::RandomInitialState { .. }
        | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. }
        | LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. }
        | LinearOp::ImpureRandomInteger { .. } => {
            validate_random_sources(op, op_index, initialized)
        }
    }
}

fn validate_random_sources(
    op: LinearOp,
    op_index: usize,
    initialized: &[bool],
) -> Result<Option<Reg>, ScalarProgramRegisterError> {
    match op {
        LinearOp::RandomInitialState {
            local_seed,
            global_seed,
            state_len,
            state_index,
            ..
        } => {
            validate_projection(op_index, op.kind_name(), state_index, state_len)?;
            require_register(op_index, op.kind_name(), local_seed, initialized)?;
            require_register(op_index, op.kind_name(), global_seed, initialized)?;
            Ok(Some(local_seed.max(global_seed)))
        }
        LinearOp::RandomResult {
            state_start,
            state_len,
            ..
        } => require_register_range(
            op_index,
            op.kind_name(),
            state_start,
            state_len,
            initialized,
        )
        .map(Some),
        LinearOp::RandomState {
            state_start,
            state_len,
            state_index,
            ..
        } => {
            validate_projection(op_index, op.kind_name(), state_index, state_len)?;
            require_register_range(
                op_index,
                op.kind_name(),
                state_start,
                state_len,
                initialized,
            )
            .map(Some)
        }
        LinearOp::ImpureRandomInit { seed, .. } => {
            require_register(op_index, op.kind_name(), seed, initialized)?;
            Ok(Some(seed))
        }
        LinearOp::ImpureRandom { id, .. } => {
            require_register(op_index, op.kind_name(), id, initialized)?;
            Ok(Some(id))
        }
        LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
            require_register(op_index, op.kind_name(), id, initialized)?;
            require_register(op_index, op.kind_name(), imin, initialized)?;
            require_register(op_index, op.kind_name(), imax, initialized)?;
            Ok(Some(id.max(imin).max(imax)))
        }
        _ => unreachable!("random source validation requires a random operation"),
    }
}

fn linear_solve_sources(
    op_index: usize,
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    component: usize,
    initialized: &[bool],
) -> Result<Reg, ScalarProgramRegisterError> {
    validate_projection(op_index, "LinearSolveComponent", component, n)?;
    let matrix_len = n
        .checked_mul(n)
        .ok_or(ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation: "LinearSolveComponent",
            start: matrix_start,
            len: n,
        })?;
    let matrix_end = require_register_range(
        op_index,
        "LinearSolveComponent",
        matrix_start,
        matrix_len,
        initialized,
    )?;
    let rhs_end =
        require_register_range(op_index, "LinearSolveComponent", rhs_start, n, initialized)?;
    Ok(matrix_end.max(rhs_end))
}

fn validate_projection(
    op_index: usize,
    operation: &'static str,
    projection: usize,
    len: usize,
) -> Result<(), ScalarProgramRegisterError> {
    if projection < len {
        return Ok(());
    }
    Err(ScalarProgramRegisterError::InvalidProjection {
        op_index,
        operation,
        projection,
        len,
    })
}

fn require_register(
    op_index: usize,
    operation: &'static str,
    register: Reg,
    initialized: &[bool],
) -> Result<(), ScalarProgramRegisterError> {
    if initialized.get(register as usize).copied().unwrap_or(false) {
        return Ok(());
    }
    Err(ScalarProgramRegisterError::UndefinedRegister {
        op_index,
        operation,
        register,
    })
}

fn require_register_range(
    op_index: usize,
    operation: &'static str,
    start: Reg,
    len: usize,
    initialized: &[bool],
) -> Result<Reg, ScalarProgramRegisterError> {
    let Some(last_offset) = len.checked_sub(1) else {
        return Err(ScalarProgramRegisterError::EmptyRegisterRange {
            op_index,
            operation,
        });
    };
    let last_offset = Reg::try_from(last_offset).map_err(|_| {
        ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len,
        }
    })?;
    let end = start.checked_add(last_offset).ok_or(
        ScalarProgramRegisterError::RegisterRangeOverflow {
            op_index,
            operation,
            start,
            len,
        },
    )?;
    for register in start..=end {
        if !initialized.get(register as usize).copied().unwrap_or(false) {
            return Err(ScalarProgramRegisterError::UndefinedRegister {
                op_index,
                operation,
                register,
            });
        }
    }
    Ok(end)
}

fn checked_register_count(max_register: Option<Reg>) -> Result<usize, ScalarProgramRegisterError> {
    let Some(register) = max_register else {
        return Ok(0);
    };
    usize::try_from(register)
        .ok()
        .and_then(|register| register.checked_add(1))
        .ok_or(ScalarProgramRegisterError::RegisterCountOverflow { register })
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
