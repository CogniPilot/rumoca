//! One function statement as the finalized DAE exposes it.

use super::*;

#[derive(Clone)]
pub enum FunctionStatementView<'dae> {
    RequireAssigned {
        conditional: FunctionConditionalBodyView<'dae>,
        value: FunctionValueId<'dae>,
        definition: FunctionDefinitionView<'dae>,
        provenance: DaeProvenance,
    },
    /// The same state carried unchecked into `definition`, which holds the
    /// value or its absence for a field of the returned record.
    CarryAssigned {
        conditional: FunctionConditionalBodyView<'dae>,
        value: FunctionValueId<'dae>,
        definition: FunctionDefinitionView<'dae>,
        provenance: DaeProvenance,
    },
    Conditional {
        conditional: FunctionConditionalBodyView<'dae>,
    },
    Assignment {
        definition: FunctionDefinitionView<'dae>,
    },
    AssignmentGroup {
        definitions: FunctionDefinitionValues<'dae>,
    },
    Assertion {
        condition: ExprId<'dae>,
        message: ExprId<'dae>,
        level: AssertionLevel,
        provenance: DaeProvenance,
    },
    For {
        fold: FunctionFoldId<'dae>,
        statements: FunctionStatements<'dae>,
        provenance: DaeProvenance,
    },
}

impl<'dae> FunctionStatementView<'dae> {
    pub(super) fn from_wire(
        dae: &'dae Dae,
        function: FunctionId<'dae>,
        statement: &'dae FunctionStatementWire,
    ) -> Self {
        match statement {
            FunctionStatementWire::RequireAssigned {
                owner,
                target,
                definition,
                carried,
            } => {
                let definition = function_definition_view(dae, function, *definition);
                let conditional = DaeView {
                    dae,
                    marker: PhantomData,
                }
                .function_conditional(FunctionConditionalId::from_raw(function.index(), *owner))
                .expect("checked partial-state owner resolves");
                let value = FunctionValueId::from_raw(function.index(), *target);
                let provenance = definition.provenance();
                if *carried {
                    Self::CarryAssigned {
                        conditional,
                        value,
                        definition,
                        provenance,
                    }
                } else {
                    Self::RequireAssigned {
                        conditional,
                        value,
                        definition,
                        provenance,
                    }
                }
            }
            FunctionStatementWire::ScopedConditional { owner } => Self::Conditional {
                conditional: DaeView {
                    dae,
                    marker: PhantomData,
                }
                .function_conditional(FunctionConditionalId::from_raw(function.index(), *owner))
                .expect("issued conditional owner resolves"),
            },
            FunctionStatementWire::Assignment { definition } => Self::Assignment {
                definition: function_definition_view(dae, function, *definition),
            },
            FunctionStatementWire::AssignmentGroup { definitions } => Self::AssignmentGroup {
                definitions: FunctionDefinitionValues {
                    dae,
                    function,
                    raw: definitions,
                },
            },
            FunctionStatementWire::Assertion {
                condition,
                message,
                level,
                provenance,
            } => Self::Assertion {
                condition: ExprId::from_raw(*condition),
                message: ExprId::from_raw(*message),
                level: *level,
                provenance: *provenance,
            },
            FunctionStatementWire::For {
                fold,
                statements,
                provenance,
            } => Self::For {
                fold: FunctionFoldId::from_raw(function.index(), *fold),
                statements: FunctionStatements {
                    dae,
                    function,
                    statements,
                    next: 0,
                },
                provenance: *provenance,
            },
        }
    }
}
