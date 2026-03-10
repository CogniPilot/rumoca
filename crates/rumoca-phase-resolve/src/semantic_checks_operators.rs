use super::*;

pub(super) const ER047_OPERATOR_CONTENTS: &str = "ER047";
pub(super) const ER048_OPERATOR_PLACEMENT: &str = "ER048";
pub(super) const ER049_CONNECTOR_COMPONENT_TYPES: &str = "ER049";
pub(super) const ER050_OPERATOR_RECORD_BASE: &str = "ER050";
pub(super) const ER051_REINIT_SINGLE_WHEN: &str = "ER051";
pub(super) const ER052_REINIT_BRANCH_DISTRIBUTION: &str = "ER052";
pub(super) const ER053_WHEN_SINGLE_ASSIGN: &str = "ER053";
pub(super) const ER054_CONSTANT_FIXED_FALSE: &str = "ER054";
pub(super) const ER055_OPERATOR_SINGLE_OUTPUT: &str = "ER055";
pub(super) const ER056_OPERATOR_RECORD_INPUT: &str = "ER056";
pub(super) const ER057_OPERATOR_CONSTRUCTOR_OUTPUT: &str = "ER057";
pub(super) const ER058_OPERATOR_STRING_OUTPUT: &str = "ER058";
pub(super) const ER071_OPERATOR_ENCAPSULATION: &str = "ER071";
pub(super) const ER072_ZERO_OPERATOR_SIGNATURE: &str = "ER072";

pub(super) fn check_operator_restrictions(
    class: &ClassDef,
    parent_is_operator_record: bool,
    parent_is_operator_class: bool,
    operator_record: Option<&OperatorRecordContext>,
    operator_name: Option<&str>,
    diags: &mut Vec<Diagnostic>,
) {
    if class.operator_record {
        check_operator_record_contents(class, diags);
    }

    if class.class_type == ClassType::Operator {
        check_operator_encapsulation(class, diags);
        check_operator_class_contents(class, parent_is_operator_record, diags);
    }

    if class.class_type == ClassType::Function
        && (parent_is_operator_record || parent_is_operator_class)
    {
        check_operator_function_contracts(
            class,
            parent_is_operator_record,
            parent_is_operator_class,
            operator_record,
            operator_name,
            diags,
        );
    }
}

fn check_operator_encapsulation(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.encapsulated {
        return;
    }

    diags.push(semantic_error(
        ER071_OPERATOR_ENCAPSULATION,
        format!(
            "operator '{}' must be declared encapsulated (MLS §14)",
            class.name.text
        ),
        label_from_token(
            &class.name,
            "check_operator_restrictions/operator_encapsulation",
            format!("operator '{}' must be encapsulated", class.name.text),
        ),
    ));
}

fn check_operator_record_contents(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    // MLS §14: operator records are records with additional operator/function members.
    for (name, nested) in &class.classes {
        if nested.class_type == ClassType::Function || nested.class_type == ClassType::Operator {
            continue;
        }
        diags.push(semantic_error(
            ER047_OPERATOR_CONTENTS,
            format!(
                "operator record '{}' can only contain component, function, or operator declarations (found '{}')",
                class.name.text, name
            ),
            label_from_token(
                &nested.name,
                "check_operator_restrictions/operator_record_contents",
                format!(
                    "operator record element '{}' must be a function or operator declaration",
                    name
                ),
            ),
        ));
    }
}

fn check_operator_class_contents(
    class: &ClassDef,
    parent_is_operator_record: bool,
    diags: &mut Vec<Diagnostic>,
) {
    // MLS §14: operator declarations are only legal inside operator records and contain functions.
    if !parent_is_operator_record {
        diags.push(semantic_error(
            ER048_OPERATOR_PLACEMENT,
            format!(
                "operator '{}' can only be declared inside an operator record",
                class.name.text
            ),
            label_from_token(
                &class.name,
                "check_operator_restrictions/operator_placement",
                format!(
                    "operator '{}' must be nested in an operator record",
                    class.name.text
                ),
            ),
        ));
    }

    for (_, comp) in &class.components {
        diags.push(semantic_error(
            ER047_OPERATOR_CONTENTS,
            format!(
                "operator '{}' can only contain function declarations",
                class.name.text
            ),
            label_from_token(
                &comp.name_token,
                "check_operator_restrictions/operator_component",
                format!(
                    "operator '{}' cannot contain component '{}'",
                    class.name.text, comp.name
                ),
            ),
        ));
    }

    for (name, nested) in &class.classes {
        if nested.class_type == ClassType::Function {
            continue;
        }
        diags.push(semantic_error(
            ER047_OPERATOR_CONTENTS,
            format!(
                "operator '{}' can only contain function declarations (found '{}')",
                class.name.text, name
            ),
            label_from_token(
                &nested.name,
                "check_operator_restrictions/operator_contents",
                format!("operator element '{}' must be a function", name),
            ),
        ));
    }

    check_zero_operator_contract(class, diags);
}

fn check_operator_function_contracts(
    class: &ClassDef,
    parent_is_operator_record: bool,
    parent_is_operator_class: bool,
    operator_record: Option<&OperatorRecordContext>,
    operator_name: Option<&str>,
    diags: &mut Vec<Diagnostic>,
) {
    // MLS §14 treats direct functions inside an operator record as operator
    // functions. Functions nested inside an encapsulated `operator` block are
    // covered by the operator declaration's encapsulation boundary.
    if parent_is_operator_record && !parent_is_operator_class && !class.encapsulated {
        diags.push(semantic_error(
            ER071_OPERATOR_ENCAPSULATION,
            format!(
                "operator function '{}' must be declared encapsulated (MLS §14)",
                class.name.text
            ),
            label_from_token(
                &class.name,
                "check_operator_restrictions/operator_function_encapsulation",
                format!(
                    "operator function '{}' must be encapsulated",
                    class.name.text
                ),
            ),
        ));
    }

    // MLS §14: operator functions return a single result. MLS §14.3/§14.4 refine
    // constructor and String operator signatures.
    let output_count = class
        .components
        .values()
        .filter(|comp| matches!(comp.causality, Causality::Output(_)))
        .count();

    if output_count != 1 {
        diags.push(semantic_error(
            ER055_OPERATOR_SINGLE_OUTPUT,
            format!(
                "operator function '{}' must declare exactly one output",
                class.name.text
            ),
            label_from_token(
                &class.name,
                "check_operator_restrictions/operator_single_output",
                format!(
                    "operator function '{}' declares {} outputs",
                    class.name.text, output_count
                ),
            ),
        ));
    }

    if let Some(record) = operator_record {
        check_operator_record_input_contract(class, record, operator_name, diags);
        check_operator_constructor_output_contract(class, record, operator_name, diags);
    }

    check_operator_string_output_contract(class, operator_name, diags);
}

fn check_operator_record_input_contract(
    class: &ClassDef,
    record: &OperatorRecordContext,
    operator_name: Option<&str>,
    diags: &mut Vec<Diagnostic>,
) {
    if is_constructor_operator(operator_name, class)
        || is_zero_operator(operator_name, class)
        || has_operator_record_input(class, record)
    {
        return;
    }
    diags.push(semantic_error(
        ER056_OPERATOR_RECORD_INPUT,
        format!(
            "operator function '{}' must declare at least one input of operator record type '{}'",
            class.name.text, record.name
        ),
        label_from_token(
            &class.name,
            "check_operator_restrictions/operator_record_input",
            format!(
                "operator function '{}' needs an input of type '{}'",
                class.name.text, record.name
            ),
        ),
    ));
}

fn check_operator_constructor_output_contract(
    class: &ClassDef,
    record: &OperatorRecordContext,
    operator_name: Option<&str>,
    diags: &mut Vec<Diagnostic>,
) {
    if !is_constructor_operator(operator_name, class) || has_operator_record_output(class, record) {
        return;
    }
    diags.push(semantic_error(
        ER057_OPERATOR_CONSTRUCTOR_OUTPUT,
        format!(
            "constructor function '{}' must return the operator record type '{}'",
            class.name.text, record.name
        ),
        label_from_token(
            &class.name,
            "check_operator_restrictions/operator_constructor_output",
            format!(
                "constructor function '{}' must return '{}'",
                class.name.text, record.name
            ),
        ),
    ));
}

fn check_operator_string_output_contract(
    class: &ClassDef,
    operator_name: Option<&str>,
    diags: &mut Vec<Diagnostic>,
) {
    if !is_string_operator(operator_name) || has_string_output(class) {
        return;
    }
    diags.push(semantic_error(
        ER058_OPERATOR_STRING_OUTPUT,
        format!(
            "operator 'String' function '{}' must return exactly one String output",
            class.name.text
        ),
        label_from_token(
            &class.name,
            "check_operator_restrictions/operator_string_output",
            format!(
                "operator 'String' function '{}' must return String",
                class.name.text
            ),
        ),
    ));
}

fn is_constructor_operator(operator_name: Option<&str>, class: &ClassDef) -> bool {
    operator_name_matches(operator_name, "constructor")
        || normalize_operator_name(class.name.text.as_ref()) == "constructor"
}

fn is_string_operator(operator_name: Option<&str>) -> bool {
    operator_name_matches(operator_name, "String")
}

fn is_zero_operator(operator_name: Option<&str>, class: &ClassDef) -> bool {
    operator_name_matches(operator_name, "0")
        || normalize_operator_name(class.name.text.as_ref()) == "0"
}

fn operator_name_matches(operator_name: Option<&str>, expected: &str) -> bool {
    operator_name
        .map(normalize_operator_name)
        .is_some_and(|actual| actual == expected)
}

fn normalize_operator_name(name: &str) -> &str {
    name.trim_matches('\'')
}

fn check_zero_operator_contract(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if !is_zero_operator(None, class) {
        return;
    }

    let functions: Vec<&ClassDef> = class
        .classes
        .values()
        .filter(|nested| nested.class_type == ClassType::Function)
        .collect();

    if functions.len() != 1 {
        diags.push(semantic_error(
            ER072_ZERO_OPERATOR_SIGNATURE,
            format!(
                "operator '0' in '{}' must contain exactly one function (MLS §14)",
                class.name.text
            ),
            label_from_token(
                &class.name,
                "check_operator_restrictions/zero_operator_function_count",
                "operator '0' must contain exactly one function",
            ),
        ));
        return;
    }

    let function = functions[0];
    let input_count = function
        .components
        .values()
        .filter(|comp| matches!(comp.causality, Causality::Input(_)))
        .count();
    if input_count == 0 {
        return;
    }

    diags.push(semantic_error(
        ER072_ZERO_OPERATOR_SIGNATURE,
        format!(
            "operator '0' function '{}' must declare zero inputs (MLS §14)",
            function.name.text
        ),
        label_from_token(
            &function.name,
            "check_operator_restrictions/zero_operator_input_count",
            format!(
                "operator '0' function '{}' declares {} inputs",
                function.name.text, input_count
            ),
        ),
    ));
}

fn has_operator_record_input(class: &ClassDef, record: &OperatorRecordContext) -> bool {
    class.components.values().any(|comp| {
        matches!(comp.causality, Causality::Input(_))
            && component_matches_operator_record(comp, record)
    })
}

fn has_operator_record_output(class: &ClassDef, record: &OperatorRecordContext) -> bool {
    let mut outputs = class
        .components
        .values()
        .filter(|comp| matches!(comp.causality, Causality::Output(_)));
    let Some(output) = outputs.next() else {
        return false;
    };
    outputs.next().is_none() && component_matches_operator_record(output, record)
}

fn has_string_output(class: &ClassDef) -> bool {
    let mut outputs = class
        .components
        .values()
        .filter(|comp| matches!(comp.causality, Causality::Output(_)));
    let Some(output) = outputs.next() else {
        return false;
    };
    outputs.next().is_none() && output.type_name.to_string() == "String"
}

fn component_matches_operator_record(
    component: &ast::Component,
    record: &OperatorRecordContext,
) -> bool {
    if let Some(record_def_id) = record.def_id {
        return component.type_def_id == Some(record_def_id);
    }
    component.type_name.to_string() == record.name
}

pub(super) fn check_constant_fixed_false(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for (name, comp) in &class.components {
        if !matches!(comp.variability, Variability::Constant(_)) {
            continue;
        }
        for (mod_key, modification) in &comp.modifications {
            if mod_key != "fixed" {
                continue;
            }
            if !fixed_mod_false(modification) {
                continue;
            }
            let label = label_from_expression(
                modification,
                "check_constant_fixed_false/fixed_false",
                format!("constant '{name}' has fixed = false"),
            )
            .unwrap_or_else(|| {
                label_from_token(
                    &comp.name_token,
                    "check_constant_fixed_false/fixed_false_fallback",
                    format!("constant '{name}' has fixed = false"),
                )
            });

            diags.push(semantic_error(
                ER054_CONSTANT_FIXED_FALSE,
                format!("constant '{}' cannot set 'fixed = false' (MLS §8.6)", name),
                label,
            ));
        }
    }
}

pub(super) fn check_operator_record_base_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    if !class.operator_record {
        return;
    }

    for base in &class.extends {
        let Some(base_def_id) = base.base_def_id else {
            continue;
        };
        let Some(base_class) = super::find_class_by_def_id(def, base_def_id) else {
            continue;
        };
        if !base_class.operator_record {
            let base_token = base.base_name.name.first();
            let base_label = base_token
                .map(|token| {
                    label_from_token(
                        token,
                        "check_operator_record_base_restrictions/base_not_operator",
                        format!(
                            "base class '{}' is not an operator record",
                            base_class.name.text
                        ),
                    )
                })
                .or_else(|| {
                    Some(label_from_token(
                        &class.name,
                        "check_operator_record_base_restrictions/base_name_fallback",
                        format!(
                            "base class '{}' is not an operator record",
                            base_class.name.text
                        ),
                    ))
                })
                .expect("base token must have fallback label");

            diags.push(semantic_error(
                ER050_OPERATOR_RECORD_BASE,
                format!(
                    "operator record '{}' cannot extend '{}' which is not an operator record",
                    class.name.text, base_class.name.text,
                ),
                base_label,
            ));
        }
    }
}

pub(super) fn check_when_reinit_contracts(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let mut when_global_reinit: HashMap<String, Span> = HashMap::new();
    let mut when_global_defined: HashMap<String, Span> = HashMap::new();

    for eq in &class.equations {
        let Equation::When(blocks) = eq else {
            continue;
        };

        let mut local_reinit_targets: HashMap<String, Vec<(Span, Vec<u32>)>> = HashMap::new();
        let mut local_defined_targets: HashMap<String, Vec<(Span, Vec<u32>)>> = HashMap::new();

        collect_when_definition_targets(
            blocks,
            &mut local_reinit_targets,
            &mut local_defined_targets,
            &[],
        );

        let mut local_reinit_merged: HashMap<String, Span> = HashMap::new();
        for (name, reinit_targets) in local_reinit_targets {
            merge_reinit_targets_for_name(&name, reinit_targets, &mut local_reinit_merged, diags);
        }

        for (name, local_span) in local_reinit_merged {
            if let Some(prev_span) = when_global_reinit.get(&name) {
                diags.push(semantic_error(
                    ER051_REINIT_SINGLE_WHEN,
                    format!(
                        "reinit for variable '{name}' is used in more than one when-equation (MLS §8.3.5)"
                    ),
                    label_from_span(
                        *prev_span,
                        format!("second when-equation applies reinit to '{name}'"),
                    ),
                ));
                continue;
            }

            when_global_reinit.insert(name, local_span);
        }

        let mut local_defined_merged: HashMap<String, Span> = HashMap::new();
        for (name, mut definitions) in local_defined_targets {
            if let Some((span, _)) = definitions.pop() {
                local_defined_merged.insert(name, span);
            }
        }

        for (name, local_span) in local_defined_merged {
            if let Some(prev_span) = when_global_defined.get(&name) {
                diags.push(semantic_error(
                    ER053_WHEN_SINGLE_ASSIGN,
                    format!(
                        "the same variable '{name}' is defined in more than one when-equation (MLS §8.3.5)"
                    ),
                    label_from_span(
                        *prev_span,
                        format!("'{name}' is already defined by a previous when-equation"),
                    ),
                ));
            } else {
                when_global_defined.insert(name, local_span);
            }
        }
    }
}

fn collect_when_definition_targets(
    blocks: &[ast::EquationBlock],
    reinit_targets: &mut HashMap<String, Vec<(Span, Vec<u32>)>>,
    defined_targets: &mut HashMap<String, Vec<(Span, Vec<u32>)>>,
    branch_path: &[u32],
) {
    for eq in blocks.iter().flat_map(|block| block.eqs.iter()) {
        collect_when_equation_targets(eq, reinit_targets, defined_targets, branch_path);
    }
}

fn record_reinit_branch_target(
    name: &str,
    span: Span,
    branch_path: Vec<u32>,
    seen_in_branch: &mut HashMap<Vec<u32>, Span>,
    diags: &mut Vec<Diagnostic>,
) -> bool {
    let Some(prev_span) = seen_in_branch.insert(branch_path, span) else {
        return false;
    };

    diags.push(semantic_error(
        ER052_REINIT_BRANCH_DISTRIBUTION,
        format!(
            "multiple reinit calls for '{name}' in one when-branch are not allowed (MLS §8.3.5)"
        ),
        label_from_span(
            prev_span,
            format!("multiple reinit calls for '{name}' in this branch"),
        ),
    ));
    true
}

fn merge_reinit_targets_for_name(
    name: &str,
    reinit_targets: Vec<(Span, Vec<u32>)>,
    local_reinit_merged: &mut HashMap<String, Span>,
    diags: &mut Vec<Diagnostic>,
) {
    let mut seen_in_branch: HashMap<Vec<u32>, Span> = HashMap::new();
    for (span, branch_path) in reinit_targets {
        if record_reinit_branch_target(name, span, branch_path, &mut seen_in_branch, diags) {
            continue;
        }
        local_reinit_merged.entry(name.to_string()).or_insert(span);
    }
}

fn collect_when_equation_targets(
    equation: &Equation,
    reinit_targets: &mut HashMap<String, Vec<(Span, Vec<u32>)>>,
    defined_targets: &mut HashMap<String, Vec<(Span, Vec<u32>)>>,
    branch_path: &[u32],
) {
    match equation {
        Equation::FunctionCall { comp, args } => {
            if let Some((target_name, span)) = extract_reinit_target(comp, args) {
                reinit_targets
                    .entry(target_name)
                    .or_default()
                    .push((span, branch_path.to_vec()));
            }
        }
        Equation::Simple { lhs, .. } => {
            if let Some((target_name, span)) = extract_component_reference_target(lhs) {
                defined_targets
                    .entry(target_name)
                    .or_default()
                    .push((span, branch_path.to_vec()));
            }
        }
        Equation::When(blocks) => {
            collect_when_definition_targets(blocks, reinit_targets, defined_targets, branch_path);
        }
        Equation::For { equations, .. } => {
            collect_when_block_targets_with_path(
                equations,
                reinit_targets,
                defined_targets,
                branch_path,
            );
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for (index, block) in cond_blocks.iter().enumerate() {
                let mut cond_branch = branch_path.to_vec();
                cond_branch.push(index as u32);
                collect_when_block_targets_with_path(
                    &block.eqs,
                    reinit_targets,
                    defined_targets,
                    &cond_branch,
                );
            }
            if let Some(else_block) = else_block {
                let mut else_branch = branch_path.to_vec();
                else_branch.push(cond_blocks.len() as u32);
                collect_when_block_targets_with_path(
                    else_block,
                    reinit_targets,
                    defined_targets,
                    &else_branch,
                );
            }
        }
        Equation::Assert { .. } | Equation::Connect { .. } | Equation::Empty => {}
    }
}

fn collect_when_block_targets_with_path(
    equations: &[Equation],
    reinit_targets: &mut HashMap<String, Vec<(Span, Vec<u32>)>>,
    defined_targets: &mut HashMap<String, Vec<(Span, Vec<u32>)>>,
    branch_path: &[u32],
) {
    for eq in equations {
        collect_when_equation_targets(eq, reinit_targets, defined_targets, branch_path);
    }
}

fn extract_reinit_target(comp: &ComponentReference, args: &[Expression]) -> Option<(String, Span)> {
    let first = comp.parts.first()?;
    if first.ident.text.as_ref() != "reinit" {
        return None;
    }
    let Expression::ComponentReference(target) = args.first()? else {
        return None;
    };
    let target = target.parts.first()?;
    span_from_location(&target.ident.location).map(|span| (target.ident.text.to_string(), span))
}

fn extract_component_reference_target(expr: &Expression) -> Option<(String, Span)> {
    let Expression::ComponentReference(comp) = expr else {
        return None;
    };
    let target = comp.parts.first()?;
    span_from_location(&target.ident.location).map(|span| (target.ident.text.to_string(), span))
}

fn label_from_span(span: Span, message: String) -> PrimaryLabel {
    PrimaryLabel::new(span).with_message(message)
}

fn fixed_mod_false(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Terminal {
            terminal_type: TerminalType::Bool,
            token,
        } if token.text.as_ref() == "false"
    )
}
