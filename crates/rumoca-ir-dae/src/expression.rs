mod arena;
mod array_nodes;
mod call_nodes;
mod derived_facts;
mod function_facts;
mod function_nodes;
mod leaf_nodes;
mod nodes;
mod operator_nodes;
mod record_nodes;
mod string_conversion;
mod subscript_nodes;
mod type_rules;
mod value_types;

use rumoca_core::Span;
use serde::{Deserialize, Serialize};

use crate::model::{FunctionReadSet, Storage, checked_u32, invalid_arity, unknown};
use crate::temporal::{DelayEntry, DelayKind};
use crate::{
    AlgebraicId, DaeConstructionError, DaeProvenance, DelayCoordinate, DelayId, DiscreteRealId,
    DiscreteValueId, DomainBinderId, DomainId, ExprId, FunctionDefinitionId, FunctionFoldId,
    FunctionId, FunctionParameterId, FunctionValueId, InputId, ParameterId, PeriodicClockId,
    PositiveParameter, StateId, ValueTypeId,
};
use derived_facts::{definition_type, max_variability, merge_binder_domain, merged_binder_domain};
use function_facts::{FunctionCallFact, node_function_facts};
use function_nodes::function_fold_entry;
use type_rules::{
    binary_result, builtin_result, common_value_type, range_extent, type_mismatch,
    validate_static_quotient, validate_subscript,
};

pub(crate) use arena::{ExpressionArenaStorage, FrozenExpressionArenaStorage, OperandRange};
pub(crate) use function_nodes::{
    insert_function_fold_output, insert_function_fold_parameter, insert_function_value_use,
};
pub use nodes::{BinaryOperator, CoordinateInput, PureBuiltin, UnaryOperator};
pub(crate) use nodes::{Coordinate, ExprNode, PackedSubscript, PackedSubscriptKind};
pub use string_conversion::StringConversionFormatInput;
pub use subscript_nodes::Subscript;
pub(crate) use value_types::RecordFieldType;
pub use value_types::{DaeLiteral, ExpressionVariability, ScalarType, ValueType};

/// Non-owning access to the one DAE-wide expression arena.
pub struct Expressions<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: std::marker::PhantomData<&'dae mut &'dae ()>,
}

impl<'storage, 'dae> Expressions<'storage, 'dae> {
    /// Read the constructor-derived type of an expression already owned by
    /// this aggregate.
    pub fn value_type(
        &self,
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ValueType, DaeConstructionError> {
        Ok(self.storage.expr_type(expression, provenance)?.clone())
    }

    /// Ordinal of the field `field` declares in the record layout of `base`.
    ///
    /// The record layout is the authority on which fields a value declares and
    /// in what order, so callers project a named field without re-deriving the
    /// layout from their own source metadata. `None` means the value type is
    /// not a record or declares no such field; the caller owns that diagnostic.
    pub fn record_field_ordinal(
        &self,
        base: ExprId<'dae>,
        field: &rumoca_core::VarName,
        provenance: DaeProvenance,
    ) -> Result<Option<usize>, DaeConstructionError> {
        let record = self.storage.expr_type(base, provenance)?;
        Ok((0..record.record_field_count())
            .find(|ordinal| record.record_field_name(*ordinal) == Some(field)))
    }

    /// Select the exact provenance for the single node inserted next.
    pub fn at<'scope>(&'scope mut self, provenance: DaeProvenance) -> ExpressionAt<'scope, 'dae> {
        ExpressionAt {
            source_map: self.source_map,
            storage: self.storage,
            provenance,
            marker: std::marker::PhantomData,
        }
    }
}

/// Inline node-construction scope selected by [`Expressions::at`].
pub struct ExpressionAt<'storage, 'dae> {
    source_map: &'storage rumoca_core::SourceMap,
    storage: &'storage mut Storage,
    provenance: DaeProvenance,
    marker: std::marker::PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> ExpressionAt<'_, 'dae> {
    fn insert(
        mut self,
        node: ExprNode,
        ty: ValueTypeId<'dae>,
        variability: ExpressionVariability,
        binder_domain: Option<u32>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let (id, facts) = self.prepare_insertion(&node, ty, variability, binder_domain)?;
        Ok(self.commit_insertion(id, node, facts))
    }

    fn prepare_insertion(
        &mut self,
        node: &ExprNode,
        ty: ValueTypeId<'dae>,
        variability: ExpressionVariability,
        binder_domain: Option<u32>,
    ) -> Result<(u32, ExpressionInsertionFacts), DaeConstructionError> {
        crate::model::check_provenance(self.source_map, self.provenance)?;
        let id = checked_u32(
            self.storage.expressions.nodes.len(),
            "expression arena",
            self.provenance,
        )?;
        let function_facts = node_function_facts(self.storage, node, self.provenance)?;
        Ok((
            id,
            ExpressionInsertionFacts {
                value_type: ty.index(),
                variability,
                binder_domain,
                function_scope: function_facts.scope,
                function_illegal_coordinate: function_facts.illegal_coordinate,
                function_read_set: function_facts.read_set,
                function_latest_call: function_facts.latest_call,
            },
        ))
    }

    fn commit_insertion(
        self,
        id: u32,
        node: ExprNode,
        facts: ExpressionInsertionFacts,
    ) -> ExprId<'dae> {
        ExprId::from_raw(
            self.storage
                .expressions
                .push(id, node, facts, self.provenance),
        )
    }
}

/// Per-node facts committed together with the node itself, owned by
/// [`ExpressionAt`] so no other module can fabricate arena rows.
struct ExpressionInsertionFacts {
    value_type: u32,
    variability: ExpressionVariability,
    binder_domain: Option<u32>,
    function_scope: Option<u32>,
    function_illegal_coordinate: Option<u32>,
    function_read_set: FunctionReadSet,
    function_latest_call: Option<FunctionCallFact>,
}

impl ExpressionArenaStorage {
    /// Raw arena row insertion, reachable only through
    /// [`ExpressionAt::commit_insertion`].
    fn push(
        &mut self,
        id: u32,
        node: ExprNode,
        facts: ExpressionInsertionFacts,
        provenance: DaeProvenance,
    ) -> u32 {
        debug_assert_eq!(usize::try_from(id).ok(), Some(self.nodes.len()));
        self.nodes.push(node);
        self.provenance.push(provenance);
        self.value_types.push(facts.value_type);
        self.variability.push(facts.variability);
        self.binder_domains.push(facts.binder_domain);
        self.function_scopes.push(facts.function_scope);
        self.function_illegal_coordinates
            .push(facts.function_illegal_coordinate);
        self.function_read_sets.push(facts.function_read_set);
        self.function_latest_calls.push(facts.function_latest_call);
        debug_assert_eq!(self.nodes.len(), self.provenance.len());
        debug_assert_eq!(self.nodes.len(), self.value_types.len());
        debug_assert_eq!(self.nodes.len(), self.variability.len());
        debug_assert_eq!(self.nodes.len(), self.binder_domains.len());
        debug_assert_eq!(self.nodes.len(), self.function_scopes.len());
        debug_assert_eq!(self.nodes.len(), self.function_illegal_coordinates.len());
        debug_assert_eq!(self.nodes.len(), self.function_read_sets.len());
        debug_assert_eq!(self.nodes.len(), self.function_latest_calls.len());
        id
    }
}

pub(crate) fn source_text(
    source_map: &rumoca_core::SourceMap,
    provenance: DaeProvenance,
) -> Option<&str> {
    let span: Span = provenance.span();
    let (_, source) = source_map.get_source(span.source)?;
    source.get(span.start.0..span.end.0)
}
