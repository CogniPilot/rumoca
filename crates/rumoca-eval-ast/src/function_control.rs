#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum FunctionStmtFlow {
    Continue,
    Break,
    Return,
}
