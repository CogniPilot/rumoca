//! Canonical semantic-owner bindings shared by the scoped sentinel scans.

use super::ast_recovery_success::AstBinding;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum SemanticSourceIdentity {
    External,
    AstNodes,
    AstOther,
    CoreIrPrimitives,
    CoreStatements,
    CoreOther,
}

impl SemanticSourceIdentity {
    pub(super) fn from_relative_path(relative: &str) -> Self {
        match relative {
            "crates/rumoca-ir-ast/src/nodes.rs" => Self::AstNodes,
            path if path.starts_with("crates/rumoca-ir-ast/") => Self::AstOther,
            "crates/rumoca-core/src/ir_primitives.rs" => Self::CoreIrPrimitives,
            "crates/rumoca-core/src/ir_primitives/component_refs_and_functions.rs" => {
                Self::CoreStatements
            }
            path if path.starts_with("crates/rumoca-core/") => Self::CoreOther,
            _ => Self::External,
        }
    }

    pub(super) fn is_ast_crate(self) -> bool {
        matches!(self, Self::AstNodes | Self::AstOther)
    }

    pub(super) fn is_core_crate(self) -> bool {
        matches!(
            self,
            Self::CoreIrPrimitives | Self::CoreStatements | Self::CoreOther
        )
    }
}

pub(super) fn canonical_local_binding(
    name: &str,
    source_identity: SemanticSourceIdentity,
) -> Option<AstBinding> {
    match (source_identity, name) {
        (SemanticSourceIdentity::AstNodes, "Equation") => Some(AstBinding::Equation),
        (SemanticSourceIdentity::AstNodes, "Expression") => Some(AstBinding::AstExpression),
        (SemanticSourceIdentity::AstNodes, "Statement") => Some(AstBinding::Statement),
        (SemanticSourceIdentity::AstNodes, "Subscript") => Some(AstBinding::AstSubscript),
        (SemanticSourceIdentity::AstNodes, "TerminalType") => Some(AstBinding::TerminalType),
        (SemanticSourceIdentity::CoreIrPrimitives, "Expression") => {
            Some(AstBinding::CoreExpression)
        }
        (SemanticSourceIdentity::CoreIrPrimitives, "OpUnary") => Some(AstBinding::OpUnary),
        (SemanticSourceIdentity::CoreIrPrimitives, "OpBinary") => Some(AstBinding::OpBinary),
        (SemanticSourceIdentity::CoreStatements, "Statement") => Some(AstBinding::CoreStatement),
        _ => None,
    }
}

pub(super) fn semantic_module_bindings(module: AstBinding) -> Vec<(String, AstBinding)> {
    let names: &[(&str, AstBinding)] = match module {
        AstBinding::AstModule => &[
            ("Equation", AstBinding::Equation),
            ("Expression", AstBinding::AstExpression),
            ("Statement", AstBinding::Statement),
            ("Subscript", AstBinding::AstSubscript),
            ("TerminalType", AstBinding::TerminalType),
        ],
        AstBinding::CoreModule => &[
            ("Expression", AstBinding::CoreExpression),
            ("Statement", AstBinding::CoreStatement),
            ("OpUnary", AstBinding::OpUnary),
            ("OpBinary", AstBinding::OpBinary),
        ],
        _ => return Vec::new(),
    };
    names
        .iter()
        .map(|(name, binding)| ((*name).to_string(), *binding))
        .collect()
}

pub(super) fn semantic_module_member(module: AstBinding, member: &str) -> Option<AstBinding> {
    if member == "self" {
        return matches!(module, AstBinding::AstModule | AstBinding::CoreModule).then_some(module);
    }
    semantic_module_bindings(module)
        .into_iter()
        .find_map(|(name, binding)| (name == member).then_some(binding))
}
