//! Function-local aggregate values that stay as compact DAE operations.
//!
//! The DAE function graph gives every read its exact reaching
//! [`dae::FunctionDefinitionId`]. A local with one whole-value definition by a
//! compact pure aggregate operation therefore needs no GALEC storage when its
//! consumers project the definition directly. This plan records only that
//! checked identity; it never rediscovers a definition from a name or from
//! emitted statements.

use std::collections::HashMap;

use rumoca_ir_dae as dae;

#[derive(Clone, Default)]
pub(super) struct StructuralFunctionLocals<'dae> {
    definitions: HashMap<dae::FunctionValueId<'dae>, dae::FunctionDefinitionId<'dae>>,
}

impl<'dae> StructuralFunctionLocals<'dae> {
    pub(super) fn derive(view: dae::DaeView<'dae>, function: dae::FunctionView<'dae>) -> Self {
        let mut definitions_by_target = HashMap::<_, Vec<_>>::new();
        let mut index = 0;
        while let Some(id) = function.definition_id(index) {
            let definition = view
                .function_definition(id)
                .expect("checked function definition identity resolves");
            definitions_by_target
                .entry(definition.target())
                .or_default()
                .push(definition);
            index += 1;
        }

        let mut plan = Self::default();
        for value in function
            .values()
            .filter(|value| value.role() == dae::FunctionValueRole::Local)
        {
            let Some([definition]) = definitions_by_target.get(&value.id()).map(Vec::as_slice)
            else {
                continue;
            };
            if is_compact_identity_definition(view, value, *definition) {
                plan.definitions.insert(value.id(), definition.id());
            }
        }
        plan
    }

    pub(super) fn elides_definition(&self, definition: dae::FunctionDefinitionView<'dae>) -> bool {
        self.definitions.get(&definition.target()) == Some(&definition.id())
    }

    pub(super) fn elides_value(&self, value: dae::FunctionValueId<'dae>) -> bool {
        self.definitions.contains_key(&value)
    }
}

fn is_compact_identity_definition<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::FunctionValueView<'dae>,
    definition: dae::FunctionDefinitionView<'dae>,
) -> bool {
    let Some(target_type) = view.value_type(target.value_type()) else {
        return false;
    };
    let Some(rhs) = view.expression(definition.rhs()) else {
        return false;
    };
    !target_type.is_record()
        && target_type.dimensions() == rhs.value_type().dimensions()
        && matches!(
            rhs.operation(),
            dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::Identity,
                ..
            }
        )
}
