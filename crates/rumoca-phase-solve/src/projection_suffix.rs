#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct OutputProjectionSuffix {
    pub(crate) output_name: String,
    pub(crate) output_fields: Vec<String>,
    pub(crate) indices: Vec<usize>,
}

pub(crate) fn resolve_function_reference<'a>(
    functions: &'a indexmap::IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    name: &rumoca_core::Reference,
) -> Option<(&'a rumoca_core::VarName, &'a rumoca_core::Function)> {
    if let Some(resolved) = name.resolved_function() {
        let function =
            rumoca_core::resolve_function_instance(functions.values(), resolved.instance_id)
                .ok()?;
        return Some((&function.name, function));
    }
    resolve_flat_name_function(functions, name.as_str())
}

/// MLS §12.4.3: a multi-output call projected onto a single output can reach
/// Solve as a flat `<function>.<output>` name without a resolved function
/// instance (for example the `(a, b, nextEvent, last) = f(...)` selections
/// produced when a `when`-algorithm is lowered). Resolve those against the DAE
/// symbol table by their longest known-function prefix so the projection is
/// still a *resolved* reference, never a guessed one.
fn resolve_flat_name_function<'a>(
    functions: &'a indexmap::IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    text: &str,
) -> Option<(&'a rumoca_core::VarName, &'a rumoca_core::Function)> {
    // Runtime-special projections (the random generators' `.stateOut`/`.result`
    // tails) keep their dedicated lowering path and must not be re-routed
    // through the generic output projection.
    if rumoca_eval_dae::is_runtime_special_function_name(&rumoca_core::VarName::new(text)) {
        return None;
    }
    let mut best: Option<(&'a rumoca_core::VarName, &'a rumoca_core::Function)> = None;
    for (key, function) in functions {
        let candidate = key.as_str();
        if candidate == text {
            return Some((key, function));
        }
        if !flat_name_has_prefix(text, candidate) {
            continue;
        }
        if best.is_none_or(|(best_key, _)| best_key.as_str().len() < candidate.len()) {
            best = Some((key, function));
        }
    }
    best
}

fn flat_name_has_prefix(text: &str, prefix: &str) -> bool {
    text.len() > prefix.len()
        && text.starts_with(prefix)
        && text.as_bytes().get(prefix.len()) == Some(&b'.')
}

pub(crate) fn output_projection_suffix(
    function: &rumoca_core::Function,
    name: &rumoca_core::Reference,
) -> Option<OutputProjectionSuffix> {
    if let Some(resolved) = name.resolved_function() {
        (function.instance_id == Some(resolved.instance_id)).then_some(())?;
        let suffix = name
            .component_ref()?
            .parts
            .get(resolved.base_part_count..)?;
        return parse_output_projection_suffix(suffix);
    }
    let text = name.as_str();
    let prefix = function.name.as_str();
    flat_name_has_prefix(text, prefix).then_some(())?;
    parse_flat_output_projection_suffix(&text[prefix.len() + 1..])
}

/// Parse the rendered `<output>[.<field>...][\[i, j\]]` tail of a flat
/// projected function name. Only the final segment may carry subscripts, and
/// every subscript must already be a positive literal index.
fn parse_flat_output_projection_suffix(rest: &str) -> Option<OutputProjectionSuffix> {
    let segments: Vec<&str> = rest.split('.').collect();
    let (last, leading) = segments.split_last()?;
    if leading
        .iter()
        .any(|segment| segment.is_empty() || segment.contains('[') || segment.contains(']'))
    {
        return None;
    }
    let (last_ident, indices) = match last.split_once('[') {
        Some((ident, raw)) => (ident, parse_flat_projection_indices(raw)?),
        None => (*last, Vec::new()),
    };
    if last_ident.is_empty() {
        return None;
    }
    let mut idents: Vec<String> = leading
        .iter()
        .map(|segment| (*segment).to_string())
        .collect();
    idents.push(last_ident.to_string());
    let (output_name, output_fields) = idents.split_first()?;
    Some(OutputProjectionSuffix {
        output_name: output_name.clone(),
        output_fields: output_fields.to_vec(),
        indices,
    })
}

fn parse_flat_projection_indices(raw: &str) -> Option<Vec<usize>> {
    raw.strip_suffix(']')?
        .split(',')
        .map(|index| {
            index
                .trim()
                .parse::<usize>()
                .ok()
                .filter(|index| *index > 0)
        })
        .collect()
}

fn parse_output_projection_suffix(
    suffix: &[rumoca_core::ComponentRefPart],
) -> Option<OutputProjectionSuffix> {
    if suffix.is_empty()
        || suffix.iter().any(|part| part.ident.is_empty())
        || suffix[..suffix.len() - 1]
            .iter()
            .any(|part| !part.subs.is_empty())
    {
        return None;
    }
    let indices = suffix
        .last()?
        .subs
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => {
                usize::try_from(*value).ok().filter(|index| *index > 0)
            }
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some(OutputProjectionSuffix {
        output_name: suffix[0].ident.clone(),
        output_fields: suffix[1..].iter().map(|part| part.ident.clone()).collect(),
        indices,
    })
}

pub(crate) fn record_output_field_param<'a>(
    functions: &'a indexmap::IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    output: &'a rumoca_core::FunctionParam,
    field_path: &[String],
) -> Option<&'a rumoca_core::FunctionParam> {
    if output.type_class != Some(rumoca_core::ClassType::Record) || field_path.is_empty() {
        return None;
    }
    let mut type_def_id = output.type_def_id?;
    let mut type_name = output.type_name.as_str();
    let mut selected = None;
    for (index, field_name) in field_path.iter().enumerate() {
        let constructor =
            rumoca_core::resolve_record_constructor(functions.values(), type_name, type_def_id)
                .ok()?;
        let field = constructor
            .inputs
            .iter()
            .find(|input| input.name == *field_name)?;
        selected = Some(field);
        if index + 1 < field_path.len() {
            if field.type_class != Some(rumoca_core::ClassType::Record) {
                return None;
            }
            type_def_id = field.type_def_id?;
            type_name = field.type_name.as_str();
        }
    }
    selected
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INSTANCE_ID: rumoca_core::FunctionInstanceId = rumoca_core::FunctionInstanceId(7);

    fn projection_reference(
        suffix_parts: Vec<rumoca_core::ComponentRefPart>,
    ) -> rumoca_core::Reference {
        let span = rumoca_core::Span::DUMMY;
        let mut parts = vec![
            rumoca_core::ComponentRefPart {
                ident: "Pkg".to_string(),
                span,
                subs: Vec::new(),
            },
            rumoca_core::ComponentRefPart {
                ident: "f".to_string(),
                span,
                subs: Vec::new(),
            },
        ];
        parts.extend(suffix_parts);
        rumoca_core::Reference::from_component_reference(rumoca_core::ComponentReference {
            local: false,
            span,
            parts,
            def_id: Some(rumoca_core::DefId::new(7)),
        })
        .with_resolved_function(rumoca_core::ResolvedFunctionReference {
            instance_id: TEST_INSTANCE_ID,
            base_part_count: 2,
        })
    }

    fn function_with_instance(name: &rumoca_core::VarName) -> rumoca_core::Function {
        let mut function = rumoca_core::Function::new(name.as_str(), rumoca_core::Span::DUMMY);
        function.instance_id = Some(TEST_INSTANCE_ID);
        function
    }

    fn part(name: &str, indices: &[i64]) -> rumoca_core::ComponentRefPart {
        rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: indices
                .iter()
                .map(|index| rumoca_core::Subscript::index(*index, rumoca_core::Span::DUMMY))
                .collect(),
        }
    }

    #[test]
    fn reads_output_projection_from_structured_reference() {
        let function_name = rumoca_core::VarName::new("Pkg.f");
        assert_eq!(
            output_projection_suffix(
                &function_with_instance(&function_name),
                &projection_reference(vec![part("out", &[])])
            )
            .expect("plain output"),
            OutputProjectionSuffix {
                output_name: "out".to_string(),
                output_fields: vec![],
                indices: vec![],
            }
        );
        assert_eq!(
            output_projection_suffix(
                &function_with_instance(&function_name),
                &projection_reference(vec![part("out", &[]), part("re", &[2, 3])]),
            )
            .expect("fielded output"),
            OutputProjectionSuffix {
                output_name: "out".to_string(),
                output_fields: vec!["re".to_string()],
                indices: vec![2, 3],
            }
        );
        assert!(
            output_projection_suffix(
                &function_with_instance(&function_name),
                &projection_reference(vec![part("out", &[1]), part("re", &[])]),
            )
            .is_none()
        );
        assert!(
            output_projection_suffix(
                &function_with_instance(&function_name),
                &projection_reference(vec![part("out", &[0])]),
            )
            .is_none()
        );
    }

    #[test]
    fn duplicate_inherited_def_ids_use_flattened_instance_identity() {
        let span = rumoca_core::Span::DUMMY;
        let def_id = rumoca_core::DefId::new(17);
        let mut functions = indexmap::IndexMap::new();
        for (index, name) in ["Pkg.A.f", "Pkg.B.f"].into_iter().enumerate() {
            let mut function = rumoca_core::Function::new(name, span);
            function.def_id = Some(def_id);
            function.instance_id = Some(rumoca_core::FunctionInstanceId::new(index as u32));
            functions.insert(function.name.clone(), function);
        }
        let mut component_ref = rumoca_core::component_reference_from_flat_name(
            &rumoca_core::VarName::new("Pkg.B.f.out"),
            span,
        )
        .expect("structured projected call");
        component_ref.def_id = Some(def_id);
        let name = rumoca_core::Reference::from_component_reference(component_ref)
            .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                instance_id: rumoca_core::FunctionInstanceId::new(1),
                base_part_count: 3,
            });

        let (resolved, _) =
            resolve_function_reference(&functions, &name).expect("concrete function identity");

        assert_eq!(resolved.as_str(), "Pkg.B.f");
    }

    #[test]
    fn stale_function_instance_is_rejected_as_an_identity_contract_violation() {
        let span = rumoca_core::Span::DUMMY;
        let mut functions = indexmap::IndexMap::new();
        let mut function = rumoca_core::Function::new("Pkg.Random.random", span);
        function.def_id = Some(rumoca_core::DefId::new(23));
        function.instance_id = Some(rumoca_core::FunctionInstanceId::new(3));
        functions.insert(function.name.clone(), function);
        let mut component_ref = rumoca_core::component_reference_from_flat_name(
            &rumoca_core::VarName::new("Pkg.Random.random.result"),
            span,
        )
        .expect("structured projected call");
        component_ref.def_id = Some(rumoca_core::DefId::new(29));
        let name = rumoca_core::Reference::from_component_reference(component_ref)
            .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                instance_id: rumoca_core::FunctionInstanceId::new(4),
                base_part_count: 3,
            });

        assert!(resolve_function_reference(&functions, &name).is_none());
    }

    fn flat_multi_output_functions()
    -> indexmap::IndexMap<rumoca_core::VarName, rumoca_core::Function> {
        let span = rumoca_core::Span::DUMMY;
        let mut functions = indexmap::IndexMap::new();
        let mut function = rumoca_core::Function::new("Pkg.Table.coefficients", span);
        function
            .outputs
            .push(rumoca_core::FunctionParam::new("a", "Real", span));
        function
            .outputs
            .push(rumoca_core::FunctionParam::new("b", "Real", span));
        function.body.push(rumoca_core::Statement::Return { span });
        functions.insert(function.name.clone(), function);
        functions
    }

    #[test]
    fn flat_projected_call_name_resolves_to_its_longest_function_prefix() {
        let functions = flat_multi_output_functions();
        let name = rumoca_core::Reference::from("Pkg.Table.coefficients.b");

        let (key, function) =
            resolve_function_reference(&functions, &name).expect("flat projected call resolves");

        assert_eq!(key.as_str(), "Pkg.Table.coefficients");
        assert_eq!(
            output_projection_suffix(function, &name),
            Some(OutputProjectionSuffix {
                output_name: "b".to_string(),
                output_fields: Vec::new(),
                indices: Vec::new(),
            })
        );
    }

    #[test]
    fn flat_projected_call_name_keeps_trailing_literal_indices() {
        let functions = flat_multi_output_functions();
        let name = rumoca_core::Reference::from("Pkg.Table.coefficients.a[2, 3]");

        let (_, function) =
            resolve_function_reference(&functions, &name).expect("flat projected call resolves");

        assert_eq!(
            output_projection_suffix(function, &name),
            Some(OutputProjectionSuffix {
                output_name: "a".to_string(),
                output_fields: Vec::new(),
                indices: vec![2, 3],
            })
        );
    }

    #[test]
    fn flat_name_that_is_not_a_function_prefix_stays_unresolved() {
        let functions = flat_multi_output_functions();
        let name = rumoca_core::Reference::from("Pkg.Other.coefficients.a");

        assert!(resolve_function_reference(&functions, &name).is_none());
    }

    #[test]
    fn parse_flat_output_projection_suffix_rejects_non_literal_indices() {
        assert!(parse_flat_output_projection_suffix("a[i]").is_none());
        assert!(parse_flat_output_projection_suffix("a[0]").is_none());
        assert!(parse_flat_output_projection_suffix("a[1].b").is_none());
        assert!(parse_flat_output_projection_suffix("").is_none());
    }
}
