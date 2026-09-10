//! Override alias targets: the resolved class/package a replaceable alias
//! currently selects, the modifier actuals it carries, and the alias tables
//! keyed by those targets.

use super::*;

pub(crate) type ComponentOverrideMap =
    rustc_hash::FxHashMap<ComponentPath, rustc_hash::FxHashMap<String, OverrideTarget>>;
pub(super) type OverrideFunctionMap = rustc_hash::FxHashMap<String, OverrideTarget>;
pub(super) type OverrideContext = (Vec<OverrideTarget>, OverrideFunctionMap);

#[derive(Clone, Debug)]
pub(super) struct FunctionModifierArg {
    pub(super) name: String,
    pub(super) value: rumoca_ir_ast::Expression,
    pub(super) span: rumoca_core::Span,
}

pub(super) struct ResolvedClassRef<'a> {
    pub(super) name: String,
    pub(super) def_id: rumoca_core::DefId,
    pub(super) class_def: &'a rumoca_ir_ast::ClassDef,
}

/// Identity of the replaceable declaration slot a function redeclare fills.
///
/// A redeclare is honored by exact slot identity, never by name. `Exact`
/// is the typed token validation mints for the rewrite to consume;
/// `Unresolved` records a redeclare whose slot identity could not be
/// established, which the rewrite must refuse rather than silently ignore.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum FunctionSlot {
    /// The alias is not a function redeclare (component receiver, package
    /// alias, nested class default); no slot selection applies.
    Unrelated,
    /// The redeclare fills the replaceable function declaration with this
    /// exact `DefId`.
    Exact(rumoca_core::DefId),
    /// The redeclare names a function but its slot identity did not resolve;
    /// calls that may be governed by it must refuse instead of defaulting.
    Unresolved,
}

#[derive(Clone, Debug)]
pub(crate) struct OverrideTarget {
    pub(super) alias: String,
    pub(crate) name: String,
    pub(super) def_id: rumoca_core::DefId,
    pub(super) class_type: rumoca_core::ClassType,
    pub(super) active: bool,
    pub(super) modifier_args: Vec<FunctionModifierArg>,
    pub(super) function_slot: FunctionSlot,
}

impl OverrideTarget {
    pub(super) fn from_resolved(
        alias: impl Into<String>,
        target: ResolvedClassRef<'_>,
        active: bool,
    ) -> Self {
        Self::from_resolved_with_modifier_args(alias, target, active, Vec::new())
    }

    pub(super) fn from_resolved_with_modifier_args(
        alias: impl Into<String>,
        target: ResolvedClassRef<'_>,
        active: bool,
        modifier_args: Vec<FunctionModifierArg>,
    ) -> Self {
        Self {
            alias: alias.into(),
            name: target.name,
            def_id: target.def_id,
            class_type: target.class_def.class_type.clone(),
            active,
            modifier_args,
            function_slot: FunctionSlot::Unrelated,
        }
    }

    pub(super) fn with_function_slot(mut self, function_slot: FunctionSlot) -> Self {
        self.function_slot = function_slot;
        self
    }

    pub(super) fn is_package(&self) -> bool {
        self.class_type == rumoca_core::ClassType::Package
    }
}

pub(super) fn is_receiver_alias_type(class_type: &rumoca_core::ClassType) -> bool {
    matches!(
        class_type,
        rumoca_core::ClassType::Package
            | rumoca_core::ClassType::Function
            | rumoca_core::ClassType::Record
            | rumoca_core::ClassType::Model
            | rumoca_core::ClassType::Block
            | rumoca_core::ClassType::Class
    )
}

pub(super) fn function_modifier_arg_from_ast(
    expr: &rumoca_ir_ast::Expression,
) -> Option<FunctionModifierArg> {
    match expr {
        rumoca_ir_ast::Expression::NamedArgument { name, value, span } => {
            Some(FunctionModifierArg {
                name: name.text.to_string(),
                value: value.as_ref().clone(),
                span: *span,
            })
        }
        rumoca_ir_ast::Expression::Modification {
            target,
            value,
            span,
        } => Some(FunctionModifierArg {
            name: single_component_ref_name(target)?,
            value: value.as_ref().clone(),
            span: *span,
        }),
        _ => None,
    }
}

pub(super) fn resolved_class_ref_for_def_id<'a>(
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    def_id: rumoca_core::DefId,
) -> Option<ResolvedClassRef<'a>> {
    Some(ResolvedClassRef {
        name: tree.def_map.get(&def_id)?.clone(),
        def_id,
        class_def: class_index.get(def_id)?,
    })
}
