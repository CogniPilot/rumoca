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
    pub(super) fn from_reference(reference: &rumoca_core::Reference) -> Self {
        let component_ref = reference.component_ref().cloned();
        let name = component_ref
            .as_ref()
            .map(component_ref_name)
            .unwrap_or_else(|| reference.as_str().to_string());
        Self {
            name,
            target_def_id: reference.target_def_id(),
            target_instance_id: reference
                .resolved_function()
                .map(|resolved| resolved.instance_id),
            component_ref,
        }
    }

    pub(super) fn from_component_reference(reference: &rumoca_core::ComponentReference) -> Self {
        Self {
            name: component_ref_name(reference),
            target_def_id: reference.target_def_id,
            target_instance_id: None,
            component_ref: Some(reference.clone()),
        }
    }

    pub(crate) fn from_resolved_ast_reference(
        name: String,
        reference: &rumoca_ir_ast::ComponentReference,
    ) -> Self {
        Self {
            name,
            target_def_id: reference.target_def_id,
            target_instance_id: None,
            component_ref: Some(ast_component_ref_to_core(reference)),
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

fn ast_component_ref_to_core(
    reference: &rumoca_ir_ast::ComponentReference,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: reference.local,
        span: reference.span,
        parts: reference
            .parts
            .iter()
            .map(|part| rumoca_core::ComponentRefPart {
                ident: part.ident.text.to_string(),
                span: reference.span,
                subs: Vec::new(),
            })
            .collect(),
        def_id: reference.def_id,
        target_def_id: reference.target_def_id,
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
        return left_id == right_id;
    }
    match (left.def_id, right_def_id) {
        (Some(left_id), Some(right_id)) => left_id == right_id && left.name == right_name,
        _ => left.name == right_name,
    }
}

pub(super) fn same_function_request(left: &FunctionRequest, right: &FunctionRequest) -> bool {
    if let (Some(left_id), Some(right_id)) = (left.target_instance_id, right.target_instance_id) {
        return left_id == right_id;
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
    comp.parts
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>()
        .join(".")
}
