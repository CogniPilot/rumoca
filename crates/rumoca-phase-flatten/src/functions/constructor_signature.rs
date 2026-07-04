use super::*;
use crate::source_spans::required_location_span;

fn collect_constructor_params(
    tree: &ast::ClassTree,
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
                tree,
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
            tree,
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
pub(super) fn convert_constructor_signature<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    def_map: &crate::ResolveDefMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<rumoca_core::Function, FlattenError> {
    if let Some(function) = external_object_constructor_function(
        tree,
        class_index,
        class_def,
        qualified_name,
        source_map,
        def_map,
        member_cache,
    )? {
        return Ok(function);
    }
    if let Some(function) = operator_record_constructor_function(
        tree,
        class_index,
        class_def,
        qualified_name,
        source_map,
        def_map,
        member_cache,
    )? {
        return Ok(function);
    }

    let span = required_location_span(source_map, &class_def.location, "constructor signature")?;
    let mut params = Vec::new();
    let mut param_index = HashMap::new();
    let mut visited_classes = HashSet::new();
    let constructor_def_map = constructor_def_map(class_index, class_def, def_map);
    collect_constructor_params(
        tree,
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

fn external_object_constructor_function<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    def_map: &crate::ResolveDefMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<Option<rumoca_core::Function>, FlattenError> {
    let Some(constructor_def) = class_def.classes.get("constructor") else {
        return Ok(None);
    };
    if constructor_def.class_type != rumoca_core::ClassType::Function
        || constructor_def.external.is_none()
    {
        return Ok(None);
    }

    let nested_name = format!("{qualified_name}.constructor");
    let mut function = super::convert_function(
        tree,
        class_index,
        constructor_def,
        &nested_name,
        source_map,
        def_map,
        member_cache,
    )?;
    function.name = rumoca_core::VarName::new(qualified_name);
    function.def_id = class_def.def_id;
    function.is_constructor = true;
    Ok(Some(function))
}

fn operator_record_constructor_function<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    def_map: &crate::ResolveDefMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<Option<rumoca_core::Function>, FlattenError> {
    if !class_def.operator_record {
        return Ok(None);
    }
    let Some(constructor_operator) = class_def
        .classes
        .get("'constructor'")
        .or_else(|| class_def.classes.get("constructor"))
    else {
        return Ok(None);
    };
    if constructor_operator.class_type != rumoca_core::ClassType::Operator {
        return Ok(None);
    }

    for (function_name, function_def) in &constructor_operator.classes {
        if function_def.class_type != rumoca_core::ClassType::Function {
            continue;
        }
        let nested_name = format!("{qualified_name}.'constructor'.{function_name}");
        let mut function = super::convert_function(
            tree,
            class_index,
            function_def,
            &nested_name,
            source_map,
            def_map,
            member_cache,
        )?;
        if !constructor_output_matches_record(&function, qualified_name, &class_def.name.text) {
            continue;
        }
        if !operator_constructor_inputs_match_record_fields(&function, class_def) {
            continue;
        }
        remap_operator_constructor_inputs_to_record_fields(&mut function, class_def);
        function.name = rumoca_core::VarName::new(qualified_name);
        function.def_id = class_def.def_id;
        function.is_constructor = true;
        normalize_function_local_references(&mut function);
        return Ok(Some(function));
    }
    Ok(None)
}

fn operator_constructor_inputs_match_record_fields(
    function: &rumoca_core::Function,
    class_def: &ast::ClassDef,
) -> bool {
    !function.inputs.is_empty()
        && function
            .inputs
            .iter()
            .all(|input| class_def.components.contains_key(input.name.as_str()))
}

fn remap_operator_constructor_inputs_to_record_fields(
    function: &mut rumoca_core::Function,
    class_def: &ast::ClassDef,
) {
    for input in &mut function.inputs {
        if let Some(field_def_id) = class_def
            .components
            .get(input.name.as_str())
            .and_then(|field| field.def_id)
        {
            input.def_id = Some(field_def_id);
        }
    }
}

fn constructor_output_matches_record(
    function: &rumoca_core::Function,
    qualified_name: &str,
    short_name: &str,
) -> bool {
    let [output] = function.outputs.as_slice() else {
        return false;
    };
    output.type_name == qualified_name
        || output.type_name == short_name
        || qualified_name
            .strip_suffix(short_name)
            .is_some_and(|prefix| prefix.ends_with('.') && output.type_name == short_name)
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
