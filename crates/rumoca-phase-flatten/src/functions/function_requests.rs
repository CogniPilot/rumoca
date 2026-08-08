//! Resolved identity for callable collection requests.
//!
//! Requests retain declaration, specialization, and structured-reference
//! identity so function collection never has to reconstruct semantic identity
//! from rendered names.

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionRequest {
    pub(crate) name: String,
    pub(crate) target_def_id: Option<rumoca_core::DefId>,
    pub(super) target_instance_id: Option<rumoca_core::FunctionInstanceId>,
    pub(super) component_ref: Option<rumoca_core::ComponentReference>,
}

impl FunctionRequest {
    pub(crate) fn from_resolved_ast_reference(
        _resolved_name: String,
        reference: &rumoca_ir_ast::ComponentReference,
    ) -> Self {
        let parts = reference
            .parts
            .iter()
            .map(|part| rumoca_core::ComponentRefPart {
                ident: part.ident.text.to_string(),
                span: reference.span,
                subs: Vec::new(),
                def_id: part
                    .def_id
                    .expect("function collection receives only resolved references"),
            })
            .collect();
        let component_ref =
            rumoca_core::ComponentReference::construct(reference.local, reference.span, parts)
                .expect("resolved AST references satisfy the checked component-reference contract");
        Self {
            name: component_ref_name(&component_ref),
            target_def_id: Some(component_ref.target_def_id()),
            target_instance_id: None,
            component_ref: Some(component_ref),
        }
    }

    pub(super) fn from_reference(reference: &rumoca_core::Reference) -> Self {
        let component_ref = reference.component_ref().cloned();
        let name = if reference.resolved_function().is_some() {
            // Canonicalization has certified the callable-table key. The
            // structured path remains source/exposure evidence and may retain
            // its relative spelling.
            reference.as_str().to_string()
        } else {
            component_ref
                .as_ref()
                .map(component_ref_name)
                .unwrap_or_else(|| reference.as_str().to_string())
        };
        Self {
            name,
            target_def_id: reference.target_def_id(),
            target_instance_id: reference
                .resolved_function()
                .map(|resolved| resolved.instance_id),
            component_ref,
        }
    }

    pub(super) fn from_name(name: String) -> Self {
        Self {
            name,
            target_def_id: None,
            target_instance_id: None,
            component_ref: None,
        }
    }

    pub(super) fn from_type_param(param: &rumoca_core::FunctionParam) -> Self {
        Self {
            name: param.type_name.clone(),
            target_def_id: param.type_def_id,
            target_instance_id: None,
            component_ref: None,
        }
    }
}

#[derive(Default)]
pub(crate) struct FunctionRequests {
    entries: Vec<FunctionRequest>,
}

impl FunctionRequests {
    pub(crate) fn insert(&mut self, request: FunctionRequest) {
        let _ = self.insert_if_new(request);
    }

    pub(crate) fn insert_if_new(&mut self, request: FunctionRequest) -> bool {
        if self.contains(&request) {
            return false;
        }
        self.entries.push(request);
        true
    }

    pub(crate) fn contains(&self, request: &FunctionRequest) -> bool {
        self.entries
            .iter()
            .any(|existing| same_function_request(existing, request))
    }

    pub(crate) fn into_entries(self) -> Vec<FunctionRequest> {
        self.entries
    }
}

#[derive(Default)]
pub(super) struct FunctionIdentitySet {
    entries: Vec<FunctionIdentity>,
}

#[derive(Clone)]
struct FunctionIdentity {
    def_id: Option<rumoca_core::DefId>,
    instance_id: Option<rumoca_core::FunctionInstanceId>,
    name: String,
}

impl FunctionIdentitySet {
    pub(super) fn insert_request(&mut self, request: &FunctionRequest) -> bool {
        self.insert_identity(FunctionIdentity {
            def_id: request.target_def_id,
            instance_id: request.target_instance_id,
            name: request.name.clone(),
        })
    }

    pub(super) fn insert_function(&mut self, function: &rumoca_core::Function) -> bool {
        self.insert_identity(FunctionIdentity {
            def_id: function.def_id,
            instance_id: function.instance_id,
            name: function.name.as_str().to_string(),
        })
    }

    pub(super) fn contains_function(&self, function: &rumoca_core::Function) -> bool {
        self.entries.iter().any(|entry| {
            same_function_identity(
                entry,
                function.def_id,
                function.instance_id,
                function.name.as_str(),
            )
        })
    }

    pub(super) fn contains_name(&self, name: &str) -> bool {
        self.entries.iter().any(|entry| entry.name == name)
    }

    fn insert_identity(&mut self, identity: FunctionIdentity) -> bool {
        if self.entries.iter().any(|entry| {
            same_function_identity(entry, identity.def_id, identity.instance_id, &identity.name)
        }) {
            return false;
        }
        self.entries.push(identity);
        true
    }
}

fn same_function_identity(
    left: &FunctionIdentity,
    right_def_id: Option<rumoca_core::DefId>,
    right_instance_id: Option<rumoca_core::FunctionInstanceId>,
    right_name: &str,
) -> bool {
    if let (Some(left_id), Some(right_id)) = (left.instance_id, right_instance_id) {
        return left_id == right_id && left.name == right_name;
    }
    match (left.def_id, right_def_id) {
        (Some(left_id), Some(right_id)) => left_id == right_id && left.name == right_name,
        _ => left.name == right_name,
    }
}

pub(super) fn same_function_request(left: &FunctionRequest, right: &FunctionRequest) -> bool {
    if let (Some(left_id), Some(right_id)) = (left.target_instance_id, right.target_instance_id) {
        return left_id == right_id && left.name == right.name;
    }
    if let (Some(left_ref), Some(right_ref)) = (&left.component_ref, &right.component_ref)
        && left_ref == right_ref
    {
        return true;
    }
    match (left.target_def_id, right.target_def_id) {
        (Some(left_id), Some(right_id)) => left_id == right_id && left.name == right.name,
        _ => left.name == right.name,
    }
}

fn component_ref_name(comp: &rumoca_core::ComponentReference) -> String {
    comp.parts()
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>()
        .join(".")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn resolved_ast_request_keeps_the_exposed_structured_path() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("request.mo"),
            0,
            1,
        );
        let reference = rumoca_ir_ast::ComponentReference {
            local: false,
            parts: vec![
                rumoca_ir_ast::ComponentRefPart {
                    ident: rumoca_core::Token {
                        text: Arc::from("Local"),
                        ..Default::default()
                    },
                    subs: None,
                    def_id: Some(rumoca_core::DefId::new(1)),
                },
                rumoca_ir_ast::ComponentRefPart {
                    ident: rumoca_core::Token {
                        text: Arc::from("f"),
                        ..Default::default()
                    },
                    subs: None,
                    def_id: Some(rumoca_core::DefId::new(2)),
                },
            ],
            span,
            qualified_display_name: Some(rumoca_core::VarName::new("Lib.Generic.f")),
        };

        let request =
            FunctionRequest::from_resolved_ast_reference("Lib.Generic.f".to_string(), &reference);

        assert_eq!(request.name, "Local.f");
        assert_eq!(request.target_def_id, Some(rumoca_core::DefId::new(2)));
    }
}
