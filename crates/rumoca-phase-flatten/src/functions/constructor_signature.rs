use super::*;
use crate::source_spans::required_location_span;

pub(super) fn inherit_operator_constructor_defaults<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    record: &'tree ast::ClassDef,
    constructor: &mut rumoca_core::Function,
    source_map: &rumoca_core::SourceMap,
    def_map: &crate::ResolveDefMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<(), FlattenError> {
    let Some(candidate) = unique_identity_operator_constructor(class_index, record, constructor)
    else {
        return Ok(());
    };
    let candidate_name = candidate
        .def_id
        .and_then(|def_id| class_index.qualified_name(def_id))
        .ok_or_else(|| {
            FlattenError::missing_resolved_class_metadata(
                candidate.name.text.as_ref(),
                "operator-record constructor identity",
                constructor.span,
            )
        })?;
    let converted = super::convert_function(
        tree,
        class_index,
        candidate,
        candidate_name,
        source_map,
        def_map,
        member_cache,
    )?;
    if !constructor_inputs_match(&constructor.inputs, &converted.inputs) {
        return Ok(());
    }
    for (field, input) in constructor.inputs.iter_mut().zip(converted.inputs) {
        field.default = input.default;
    }
    normalize_function_local_references(constructor);
    Ok(())
}

fn unique_identity_operator_constructor<'a>(
    class_index: &ast::ClassDefIndex<'a>,
    record: &'a ast::ClassDef,
    constructor: &rumoca_core::Function,
) -> Option<&'a ast::ClassDef> {
    if !record.operator_record {
        return None;
    }
    let mut candidates = Vec::new();
    collect_identity_operator_constructors(
        class_index,
        record,
        constructor,
        &mut HashSet::new(),
        &mut candidates,
    );
    (candidates.len() == 1).then_some(*candidates.first()?)
}

fn collect_identity_operator_constructors<'a>(
    class_index: &ast::ClassDefIndex<'a>,
    record: &'a ast::ClassDef,
    constructor: &rumoca_core::Function,
    visited: &mut HashSet<usize>,
    candidates: &mut Vec<&'a ast::ClassDef>,
) {
    let record_ptr = record as *const ast::ClassDef as usize;
    if !visited.insert(record_ptr) {
        return;
    }
    if let Some(operator) = record.classes.get("'constructor'") {
        candidates.extend(
            operator
                .classes
                .values()
                .filter(|candidate| candidate.class_type == rumoca_core::ClassType::Function)
                .filter(|candidate| {
                    operator_constructor_is_field_identity(record, candidate, constructor)
                }),
        );
    }
    for ext in &record.extends {
        let base = ext
            .base_def_id
            .and_then(|def_id| class_index.get(def_id))
            .or_else(|| class_index.get_by_qualified_name(&ext.base_name.to_string()));
        if let Some(base) = base {
            collect_identity_operator_constructors(
                class_index,
                base,
                constructor,
                visited,
                candidates,
            );
        }
    }
}

fn operator_constructor_is_field_identity(
    record: &ast::ClassDef,
    candidate: &ast::ClassDef,
    constructor: &rumoca_core::Function,
) -> bool {
    let inputs = candidate
        .components
        .values()
        .filter(|component| matches!(component.causality, rumoca_core::Causality::Input(_)))
        .collect::<Vec<_>>();
    let outputs = candidate
        .components
        .values()
        .filter(|component| matches!(component.causality, rumoca_core::Causality::Output(_)))
        .collect::<Vec<_>>();
    inputs.len() == constructor.inputs.len()
        && inputs
            .iter()
            .zip(&constructor.inputs)
            .all(|(input, field)| input.name == field.name)
        && matches!(outputs.as_slice(), [output] if identity_output(record, output, &inputs))
}

fn identity_output(
    record: &ast::ClassDef,
    output: &ast::Component,
    inputs: &[&ast::Component],
) -> bool {
    if output.type_def_id.or(output.type_name.def_id) != record.def_id
        || output.modifications.len() != inputs.len()
    {
        return false;
    }
    inputs.iter().all(|input| {
        output
            .modifications
            .get(&input.name)
            .is_some_and(|value| identity_input_reference(value, input))
    })
}

fn identity_input_reference(value: &ast::Expression, input: &ast::Component) -> bool {
    let ast::Expression::ComponentReference(reference) = value else {
        return false;
    };
    let [part] = reference.parts.as_slice() else {
        return false;
    };
    part.ident.text.as_ref() == input.name
        && part.subs.as_ref().is_none_or(Vec::is_empty)
        && reference.def_id == input.def_id
}

fn constructor_inputs_match(
    fields: &[rumoca_core::FunctionParam],
    inputs: &[rumoca_core::FunctionParam],
) -> bool {
    fields.len() == inputs.len()
        && fields
            .iter()
            .zip(inputs)
            .all(|(field, input)| field.name == input.name && field.dims == input.dims)
}

fn collect_constructor_params(
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
    visited_classes: &mut HashSet<usize>,
    params: &mut Vec<rumoca_core::FunctionParam>,
    param_index: &mut HashMap<String, usize>,
    source_map: &rumoca_core::SourceMap,
    def_map: &crate::ResolveDefMap,
) -> Result<(), FlattenError> {
    let class_ptr = class_def as *const ast::ClassDef as usize;
    if !visited_classes.insert(class_ptr) {
        return Ok(());
    }

    for ext in &class_def.extends {
        let base_class = ext
            .base_def_id
            .and_then(|def_id| class_index.get(def_id))
            .or_else(|| {
                let name = ext.base_name.to_string();
                class_index.get_by_qualified_name(&name)
            });
        if let Some(base_class) = base_class {
            collect_constructor_params(
                class_index,
                base_class,
                visited_classes,
                params,
                param_index,
                source_map,
                def_map,
            )?;
        }
    }

    for (comp_name, component) in &class_def.components {
        // MLS §12.6.1 / FUNC-029: a record constructor cannot be formed for a
        // record with conditional components.
        if component.condition.is_some() {
            let span =
                required_location_span(source_map, &class_def.location, "record constructor")?;
            return Err(FlattenError::ConditionalComponentConstructor {
                record: class_def.name.text.to_string(),
                component: comp_name.clone(),
                span: rumoca_core::span_to_source_span(span),
            });
        }
        let param = convert_component_to_param(
            class_index,
            comp_name,
            component,
            source_map,
            def_map,
            &qualify::ImportMap::default(),
            &HashSet::new(),
        )?;
        if let Some(index) = param_index.get(comp_name).copied() {
            params[index] = param;
        } else {
            param_index.insert(comp_name.clone(), params.len());
            params.push(param);
        }
    }
    Ok(())
}

fn collect_constructor_local_def_ids(
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
    visited_classes: &mut HashSet<usize>,
    local_def_ids: &mut crate::ResolveDefMap,
) {
    let class_ptr = class_def as *const ast::ClassDef as usize;
    if !visited_classes.insert(class_ptr) {
        return;
    }

    for ext in &class_def.extends {
        let base_class = ext
            .base_def_id
            .and_then(|def_id| class_index.get(def_id))
            .or_else(|| {
                let name = ext.base_name.to_string();
                class_index.get_by_qualified_name(&name)
            });
        if let Some(base_class) = base_class {
            collect_constructor_local_def_ids(
                class_index,
                base_class,
                visited_classes,
                local_def_ids,
            );
        }
    }

    for (name, component) in &class_def.components {
        if let Some(def_id) = component.def_id {
            local_def_ids.insert(def_id, name.clone());
        }
    }
}

fn constructor_def_map(
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
    def_map: &crate::ResolveDefMap,
) -> crate::ResolveDefMap {
    let mut constructor_map = def_map.clone();
    let mut local_def_ids = crate::ResolveDefMap::default();
    let mut visited_classes = HashSet::new();
    collect_constructor_local_def_ids(
        class_index,
        class_def,
        &mut visited_classes,
        &mut local_def_ids,
    );
    for (def_id, name) in local_def_ids {
        constructor_map.insert(def_id, name);
    }
    constructor_map
}

/// Build a synthetic constructor signature for constructor-like class calls.
pub(super) fn convert_constructor_signature(
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    def_map: &crate::ResolveDefMap,
) -> Result<rumoca_core::Function, FlattenError> {
    let span = required_location_span(source_map, &class_def.location, "constructor signature")?;
    let mut params = Vec::new();
    let mut param_index = HashMap::new();
    let mut visited_classes = HashSet::new();
    let constructor_def_map = constructor_def_map(class_index, class_def, def_map);
    collect_constructor_params(
        class_index,
        class_def,
        &mut visited_classes,
        &mut params,
        &mut param_index,
        source_map,
        &constructor_def_map,
    )?;

    let mut func = rumoca_core::Function::new(qualified_name, span);
    func.def_id = class_def.def_id;
    func.is_constructor = true;
    for param in params {
        func.add_input(param);
    }
    normalize_function_local_references(&mut func);
    Ok(func)
}

pub(super) fn normalize_function_local_references(function: &mut rumoca_core::Function) {
    let locals = function
        .inputs
        .iter()
        .chain(function.outputs.iter())
        .chain(function.locals.iter())
        .map(|param| param.name.clone())
        .collect::<HashSet<_>>();
    if locals.is_empty() {
        return;
    }

    let mut normalizer = FunctionLocalReferenceNormalizer {
        function_name: function.name.as_str().to_string(),
        locals,
    };
    for param in function
        .inputs
        .iter_mut()
        .chain(function.outputs.iter_mut())
        .chain(function.locals.iter_mut())
    {
        if let Some(default) = &param.default {
            param.default = Some(normalizer.rewrite_expression(default));
        }
    }
    for statement in &mut function.body {
        *statement = normalizer.rewrite_statement(statement);
    }
}

struct FunctionLocalReferenceNormalizer {
    function_name: String,
    locals: HashSet<String>,
}

impl FunctionLocalReferenceNormalizer {
    fn local_reference<'a>(&self, name: &'a str) -> Option<&'a str> {
        let suffix = name.strip_prefix(&self.function_name)?;
        let local_ref = suffix.strip_prefix('.')?;
        self.locals
            .iter()
            .any(|local| {
                local_ref == local
                    || local_ref
                        .strip_prefix(local.as_str())
                        .is_some_and(|rest| rest.starts_with('.') || rest.starts_with('['))
            })
            .then_some(local_ref)
    }
}

impl ExpressionRewriter for FunctionLocalReferenceNormalizer {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        match expr {
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } => {
                if let Some(local_name) = self.local_reference(name.as_str()) {
                    return rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new(local_name.to_string()),
                        subscripts: self.rewrite_subscripts(subscripts),
                        span: *span,
                    };
                }
                self.walk_expression(expr)
            }
            _ => self.walk_expression(expr),
        }
    }
}

impl StatementRewriter for FunctionLocalReferenceNormalizer {}
