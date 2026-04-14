//! Linear register-based ops used by compiled evaluators.

/// Register index in a lowered op sequence.
pub type Reg = u32;

/// Scalar unary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareOp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

/// Flat linear operation stream (no strings, no dynamic dispatch).
#[derive(Debug, Clone, Copy, PartialEq)]
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
