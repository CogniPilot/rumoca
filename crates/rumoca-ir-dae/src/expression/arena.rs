use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct OperandRange {
    pub(crate) start: u32,
    pub(crate) len: u32,
}

impl Serialize for OperandRange {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_u32(self.len)
    }
}

impl OperandRange {
    pub(super) fn new(
        start: usize,
        len: usize,
        at: DaeProvenance,
    ) -> Result<Self, DaeConstructionError> {
        Ok(Self {
            start: checked_u32(start, "expression operand buffer", at)?,
            len: checked_u32(len, "expression operand buffer", at)?,
        })
    }

    pub(crate) fn indices(self) -> std::ops::Range<usize> {
        let start = self.start as usize;
        start..start + self.len as usize
    }
}
#[derive(Debug, Clone, PartialEq, Default)]
pub(crate) struct ExpressionArenaStorage {
    pub(crate) nodes: Vec<ExprNode>,
    pub(crate) provenance: Vec<DaeProvenance>,
    pub(crate) value_types: Vec<u32>,
    pub(crate) variability: Vec<ExpressionVariability>,
    pub(crate) binder_domains: Vec<Option<u32>>,
    pub(crate) function_scopes: Vec<Option<u32>>,
    pub(crate) function_illegal_coordinates: Vec<Option<u32>>,
    pub(crate) function_read_sets: Vec<FunctionReadSet>,
    pub(crate) function_latest_calls: Vec<Option<FunctionCallFact>>,
    pub(crate) operands: Vec<u32>,
    pub(crate) subscripts: Vec<PackedSubscript>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FrozenExpressionArenaStorage {
    pub(crate) nodes: Box<[ExprNode]>,
    pub(crate) provenance: Box<[DaeProvenance]>,
    pub(crate) value_types: Box<[u32]>,
    pub(crate) variability: Box<[ExpressionVariability]>,
    pub(crate) binder_domains: Box<[Option<u32>]>,
    pub(crate) function_scopes: Box<[Option<u32>]>,
    pub(crate) operands: Box<[u32]>,
    pub(crate) subscripts: Box<[PackedSubscript]>,
}

impl ExpressionArenaStorage {
    pub(super) fn push_operands(
        &mut self,
        operands: impl IntoIterator<Item = u32>,
        at: DaeProvenance,
    ) -> Result<OperandRange, DaeConstructionError> {
        let start = self.operands.len();
        self.operands.extend(operands);
        OperandRange::new(start, self.operands.len() - start, at)
    }

    pub(crate) fn freeze(self) -> FrozenExpressionArenaStorage {
        FrozenExpressionArenaStorage {
            nodes: self.nodes.into_boxed_slice(),
            provenance: self.provenance.into_boxed_slice(),
            value_types: self.value_types.into_boxed_slice(),
            variability: self.variability.into_boxed_slice(),
            binder_domains: self.binder_domains.into_boxed_slice(),
            function_scopes: self.function_scopes.into_boxed_slice(),
            operands: self.operands.into_boxed_slice(),
            subscripts: self.subscripts.into_boxed_slice(),
        }
    }
}
