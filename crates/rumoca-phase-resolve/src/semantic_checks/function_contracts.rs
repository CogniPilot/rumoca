fn check_function_array_dimension_sources(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let known_names: HashSet<String> = class.components.keys().cloned().collect();
    let allowed_names: HashSet<String> = class
        .components
        .iter()
        .filter_map(|(name, comp)| {
            if matches!(comp.causality, Causality::Input(_))
                || matches!(
                    comp.variability,
                    Variability::Parameter(_) | Variability::Constant(_)
                )
            {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect();

    for (name, comp) in &class.components {
        for sub in &comp.shape_expr {
            let Subscript::Expression(dim_expr) = sub else {
                continue;
            };
            let mut refs = HashSet::new();
            collect_component_refs(dim_expr, &known_names, &mut refs, false);
            if refs.iter().all(|r| allowed_names.contains(r)) {
                continue;
            }
            diags.push(
                Diagnostic::error(format!(
                    "array dimension for '{}' in function '{}' must depend only on inputs/constants/parameters (MLS §12.2)",
                    name, class.name.text
                ))
                .with_code(ER044_FUNCTION_ARRAY_DIMENSION_SOURCE),
            );
        }
    }
}

fn check_input_default_independence(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let known_names: HashSet<String> = class.components.keys().cloned().collect();
    let input_names: HashSet<String> = class
        .components
        .iter()
        .filter_map(|(name, comp)| {
            if matches!(comp.causality, Causality::Input(_)) {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect();

    for (name, comp) in &class.components {
        if !matches!(comp.causality, Causality::Input(_)) {
            continue;
        }
        let Some(default_expr) = &comp.binding else {
            continue;
        };
        let mut refs = HashSet::new();
        collect_component_refs(default_expr, &known_names, &mut refs, false);
        if refs.iter().all(|r| input_names.contains(r)) {
            continue;
        }
        diags.push(
            Diagnostic::error(format!(
                "default value for input '{}' in function '{}' cannot depend on non-input variables (MLS §12.4.1)",
                name, class.name.text
            ))
            .with_code(ER054_INPUT_DEFAULT_DEPENDS_ON_NON_INPUT),
        );
    }
}

fn check_external_function_purity_deprecation(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.external.is_none() || !class.pure {
        return;
    }
    diags.push(
        Diagnostic::error(format!(
            "external function '{}' must be explicitly declared impure in this compiler profile (MLS §12.9 deprecation)",
            class.name.text
        ))
        .with_code(ER052_EXTERNAL_PURITY_DEPRECATED),
    );
}

fn check_functional_parameter_type_contract(
    class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    diags: &mut Vec<Diagnostic>,
) {
    for (name, comp) in &class.components {
        if !matches!(comp.causality, Causality::Input(_)) {
            continue;
        }
        let Some(type_def_id) = comp.type_def_id else {
            continue;
        };
        let Some(function_type) = class_index.get(&type_def_id) else {
            continue;
        };
        if function_type.class_type != ClassType::Function {
            continue;
        }

        let uses_record_or_enum = function_type
            .components
            .values()
            .filter_map(|func_param| func_param.type_def_id)
            .filter_map(|param_type_id| class_index.get(&param_type_id))
            .any(|param_type| {
                param_type.class_type == ClassType::Record
                    || (param_type.class_type == ClassType::Type
                        && !param_type.enum_literals.is_empty())
            });
        if !uses_record_or_enum {
            continue;
        }
        diags.push(
            Diagnostic::error(format!(
                "function type parameter '{}' in function '{}' cannot use record/enumeration type specifiers (MLS §12.4.2)",
                name, class.name.text
            ))
            .with_code(ER053_FUNCTIONAL_PARAM_RECORD_ENUM),
        );
    }
}

#[derive(Clone, Default)]
struct DerivativeAnnotationCheck {
    order: u32,
    zero_derivative: Vec<String>,
    no_derivative: Vec<String>,
}

fn check_derivative_annotation_contracts(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let derivatives = extract_derivative_annotations_for_checks(&class.annotation);
    if derivatives.is_empty() {
        return;
    }

    let has_output = class
        .components
        .values()
        .any(|comp| matches!(comp.causality, Causality::Output(_)));
    if !has_output {
        diags.push(
            Diagnostic::error(format!(
                "function '{}' has derivative annotation but no output variables (MLS §12.7.1)",
                class.name.text
            ))
            .with_code(ER050_DERIVATIVE_OUTPUTS_EMPTY),
        );
    }

    let input_names: HashSet<String> = class
        .components
        .iter()
        .filter_map(|(name, comp)| {
            if matches!(comp.causality, Causality::Input(_)) {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect();
    for deriv in &derivatives {
        for name in &deriv.zero_derivative {
            if input_names.contains(name) {
                continue;
            }
            diags.push(
                Diagnostic::error(format!(
                    "zeroDerivative target '{}' in function '{}' must reference an input variable (MLS §12.7.1)",
                    name, class.name.text
                ))
                .with_code(ER051_ZERODERIVATIVE_INVALID_TARGET),
            );
        }
    }

    let mut prev_score: Option<(usize, usize, u32)> = None;
    for deriv in derivatives {
        let score = (
            deriv.no_derivative.len(),
            deriv.zero_derivative.len(),
            deriv.order,
        );
        if let Some(prev) = prev_score
            && score > prev
        {
            diags.push(
                Diagnostic::error(format!(
                    "derivative annotations for function '{}' must be ordered most-restrictive first (MLS §12.7.1)",
                    class.name.text
                ))
                .with_code(ER055_DERIVATIVE_ORDERING),
            );
            break;
        }
        prev_score = Some(score);
    }
}

fn extract_derivative_annotations_for_checks(
    annotations: &[Expression],
) -> Vec<DerivativeAnnotationCheck> {
    annotations
        .iter()
        .filter_map(extract_single_derivative_for_check)
        .collect()
}

fn extract_single_derivative_for_check(expr: &Expression) -> Option<DerivativeAnnotationCheck> {
    // Parser-normalized pattern from source: derivative(...) = F_der
    if let Expression::Binary { op, lhs, rhs } = expr
        && matches!(op, OpBinary::Assign(_))
        && extract_function_name_from_expr(rhs).is_some()
    {
        return extract_derivative_target_for_check(lhs);
    }

    if let Expression::NamedArgument { name, value } = expr
        && name.text.as_ref() == "derivative"
        && extract_function_name_from_expr(value).is_some()
    {
        return Some(DerivativeAnnotationCheck {
            order: 1,
            zero_derivative: Vec::new(),
            no_derivative: Vec::new(),
        });
    }

    if let Expression::Modification { value, .. } = expr
        && extract_function_name_from_expr(value).is_some()
    {
        return extract_derivative_target_for_check(expr);
    }

    if let Expression::ClassModification { modifications, .. } = expr {
        let has_function_name = modifications
            .iter()
            .any(|mod_expr| extract_function_name_from_expr(mod_expr).is_some());
        if has_function_name {
            return extract_derivative_target_for_check(expr);
        }
    }

    None
}

fn extract_derivative_target_for_check(expr: &Expression) -> Option<DerivativeAnnotationCheck> {
    if let Expression::NamedArgument { name, .. } = expr
        && name.text.as_ref() == "derivative"
    {
        return Some(DerivativeAnnotationCheck {
            order: 1,
            zero_derivative: Vec::new(),
            no_derivative: Vec::new(),
        });
    }

    if let Expression::Modification { target, .. } = expr
        && target.parts.len() == 1
        && target.parts[0].ident.text.as_ref() == "derivative"
    {
        let mut deriv = DerivativeAnnotationCheck {
            order: 1,
            zero_derivative: Vec::new(),
            no_derivative: Vec::new(),
        };
        extract_derivative_modifiers_from_subscripts_for_check(&target.parts[0].subs, &mut deriv);
        return Some(deriv);
    }

    let Expression::ClassModification {
        target,
        modifications,
    } = expr
    else {
        return None;
    };
    if target.parts.len() != 1 || target.parts[0].ident.text.as_ref() != "derivative" {
        return None;
    }
    let mut deriv = DerivativeAnnotationCheck {
        order: 1,
        zero_derivative: Vec::new(),
        no_derivative: Vec::new(),
    };
    for mod_expr in modifications {
        extract_derivative_modifier_for_check(mod_expr, &mut deriv);
    }
    Some(deriv)
}

fn extract_derivative_modifiers_from_subscripts_for_check(
    subs: &Option<Vec<Subscript>>,
    deriv: &mut DerivativeAnnotationCheck,
) {
    let Some(subscripts) = subs else {
        return;
    };
    for sub in subscripts {
        if let Subscript::Expression(expr) = sub {
            extract_derivative_modifier_for_check(expr, deriv);
        }
    }
}

fn extract_derivative_modifier_for_check(expr: &Expression, deriv: &mut DerivativeAnnotationCheck) {
    if let Expression::NamedArgument { name, value } = expr {
        apply_derivative_modifier_for_check(name.text.as_ref(), value, deriv);
    }
    if let Expression::Modification { target, value } = expr
        && target.parts.len() == 1
    {
        apply_derivative_modifier_for_check(target.parts[0].ident.text.as_ref(), value, deriv);
    }
}

fn apply_derivative_modifier_for_check(
    name: &str,
    value: &Expression,
    deriv: &mut DerivativeAnnotationCheck,
) {
    match name {
        "order" => {
            if let Some(order) = extract_integer_literal_from_expr(value) {
                deriv.order = order as u32;
            }
        }
        "zeroDerivative" => {
            if let Some(var_name) = extract_variable_name_from_expr(value) {
                deriv.zero_derivative.push(var_name);
            }
        }
        "noDerivative" => {
            if let Some(var_name) = extract_variable_name_from_expr(value) {
                deriv.no_derivative.push(var_name);
            }
        }
        _ => {}
    }
}

fn extract_function_name_from_expr(expr: &Expression) -> Option<String> {
    let Expression::ComponentReference(cref) = expr else {
        return None;
    };
    Some(
        cref.parts
            .iter()
            .map(|part| part.ident.text.to_string())
            .collect::<Vec<_>>()
            .join("."),
    )
}

fn extract_variable_name_from_expr(expr: &Expression) -> Option<String> {
    let Expression::ComponentReference(cref) = expr else {
        return None;
    };
    cref.parts.last().map(|part| part.ident.text.to_string())
}

fn extract_integer_literal_from_expr(expr: &Expression) -> Option<i64> {
    let Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token,
    } = expr
    else {
        return None;
    };
    token.text.parse::<i64>().ok()
}

fn check_function_call_argument_contracts_in_class(
    class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    top_level_class_ids: &HashSet<DefId>,
    diags: &mut Vec<Diagnostic>,
) {
    for comp in class.components.values() {
        if let Some(binding) = &comp.binding {
            check_function_call_argument_contracts_in_expr(
                binding,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        if !matches!(comp.start, Expression::Empty) {
            check_function_call_argument_contracts_in_expr(
                &comp.start,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        if let Some(cond) = &comp.condition {
            check_function_call_argument_contracts_in_expr(
                cond,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
    }

    for eq in &class.equations {
        check_function_call_argument_contracts_in_equation(
            eq,
            class,
            class_index,
            top_level_class_ids,
            diags,
        );
    }
    for eq in &class.initial_equations {
        check_function_call_argument_contracts_in_equation(
            eq,
            class,
            class_index,
            top_level_class_ids,
            diags,
        );
    }
    for alg in &class.algorithms {
        for stmt in alg {
            check_function_call_argument_contracts_in_statement(
                stmt,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
    }
    for alg in &class.initial_algorithms {
        for stmt in alg {
            check_function_call_argument_contracts_in_statement(
                stmt,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
    }
}

#[allow(clippy::too_many_lines)]
fn check_function_call_argument_contracts_in_equation(
    eq: &Equation,
    class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    top_level_class_ids: &HashSet<DefId>,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Empty | Equation::Connect { .. } => {}
        Equation::Simple { lhs, rhs } => {
            check_function_call_argument_contracts_in_expr(
                lhs,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            check_function_call_argument_contracts_in_expr(
                rhs,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Equation::For { indices, equations } => {
            for index in indices {
                check_function_call_argument_contracts_in_expr(
                    &index.range,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
            for inner in equations {
                check_function_call_argument_contracts_in_equation(
                    inner,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_function_call_argument_contracts_in_expr(
                    &block.cond,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
                for inner in &block.eqs {
                    check_function_call_argument_contracts_in_equation(
                        inner,
                        class,
                        class_index,
                        top_level_class_ids,
                        diags,
                    );
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_function_call_argument_contracts_in_expr(
                    &block.cond,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
                for inner in &block.eqs {
                    check_function_call_argument_contracts_in_equation(
                        inner,
                        class,
                        class_index,
                        top_level_class_ids,
                        diags,
                    );
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_function_call_argument_contracts_in_equation(
                        inner,
                        class,
                        class_index,
                        top_level_class_ids,
                        diags,
                    );
                }
            }
        }
        Equation::FunctionCall { comp, args } => {
            check_function_call_argument_contracts_on_call(
                comp,
                args,
                &[],
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            for arg in args {
                check_function_call_argument_contracts_in_expr(
                    arg,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
        Equation::Assert {
            condition,
            message,
            level,
        } => {
            check_function_call_argument_contracts_in_expr(
                condition,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            check_function_call_argument_contracts_in_expr(
                message,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            if let Some(level_expr) = level {
                check_function_call_argument_contracts_in_expr(
                    level_expr,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
    }
}

#[allow(clippy::too_many_lines)]
fn check_function_call_argument_contracts_in_statement(
    stmt: &Statement,
    class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    top_level_class_ids: &HashSet<DefId>,
    diags: &mut Vec<Diagnostic>,
) {
    match stmt {
        Statement::Empty | Statement::Return { .. } | Statement::Break { .. } => {}
        Statement::Assignment { value, .. } => {
            check_function_call_argument_contracts_in_expr(
                value,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            check_function_call_argument_contracts_on_call(
                comp,
                args,
                outputs,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            for arg in args {
                check_function_call_argument_contracts_in_expr(
                    arg,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
            for output in outputs {
                check_function_call_argument_contracts_in_expr(
                    output,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
        Statement::For { indices, equations } => {
            for index in indices {
                check_function_call_argument_contracts_in_expr(
                    &index.range,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
            for inner in equations {
                check_function_call_argument_contracts_in_statement(
                    inner,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_function_call_argument_contracts_in_expr(
                    &block.cond,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
                for inner in &block.stmts {
                    check_function_call_argument_contracts_in_statement(
                        inner,
                        class,
                        class_index,
                        top_level_class_ids,
                        diags,
                    );
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    check_function_call_argument_contracts_in_statement(
                        inner,
                        class,
                        class_index,
                        top_level_class_ids,
                        diags,
                    );
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_function_call_argument_contracts_in_expr(
                    &block.cond,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
                for inner in &block.stmts {
                    check_function_call_argument_contracts_in_statement(
                        inner,
                        class,
                        class_index,
                        top_level_class_ids,
                        diags,
                    );
                }
            }
        }
        Statement::While(block) => {
            check_function_call_argument_contracts_in_expr(
                &block.cond,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            for inner in &block.stmts {
                check_function_call_argument_contracts_in_statement(
                    inner,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
        Statement::Reinit { value, .. } => {
            check_function_call_argument_contracts_in_expr(
                value,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => {
            check_function_call_argument_contracts_in_expr(
                condition,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            check_function_call_argument_contracts_in_expr(
                message,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            if let Some(level_expr) = level {
                check_function_call_argument_contracts_in_expr(
                    level_expr,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
    }
}

#[allow(clippy::too_many_lines)]
fn check_function_call_argument_contracts_in_expr(
    expr: &Expression,
    class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    top_level_class_ids: &HashSet<DefId>,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::Empty | Expression::Terminal { .. } => {}
        Expression::ComponentReference(cref) => {
            for part in &cref.parts {
                if let Some(subs) = &part.subs {
                    check_function_call_argument_contracts_in_subscripts(
                        subs,
                        class,
                        class_index,
                        top_level_class_ids,
                        diags,
                    );
                }
            }
        }
        Expression::Modification { value, .. } => {
            check_function_call_argument_contracts_in_expr(
                value,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Expression::Range { start, step, end } => {
            check_function_call_argument_contracts_in_expr(
                start,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            if let Some(step_expr) = step {
                check_function_call_argument_contracts_in_expr(
                    step_expr,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
            check_function_call_argument_contracts_in_expr(
                end,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Expression::Unary { rhs, .. } => {
            check_function_call_argument_contracts_in_expr(
                rhs,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_function_call_argument_contracts_in_expr(
                lhs,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            check_function_call_argument_contracts_in_expr(
                rhs,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Expression::FunctionCall { comp, args } => {
            check_function_call_argument_contracts_on_call(
                comp,
                args,
                &[],
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            for arg in args {
                check_function_call_argument_contracts_in_expr(
                    arg,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
        Expression::ClassModification {
            target: _,
            modifications,
        } => {
            for mod_expr in modifications {
                check_function_call_argument_contracts_in_expr(
                    mod_expr,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
        Expression::NamedArgument { value, .. } => {
            check_function_call_argument_contracts_in_expr(
                value,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for elem in elements {
                check_function_call_argument_contracts_in_expr(
                    elem,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_function_call_argument_contracts_in_expr(
                    cond,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
                check_function_call_argument_contracts_in_expr(
                    then_expr,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
            check_function_call_argument_contracts_in_expr(
                else_branch,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Expression::Parenthesized { inner } => {
            check_function_call_argument_contracts_in_expr(
                inner,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            check_function_call_argument_contracts_in_expr(
                expr,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            for index in indices {
                check_function_call_argument_contracts_in_expr(
                    &index.range,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
            if let Some(filter_expr) = filter {
                check_function_call_argument_contracts_in_expr(
                    filter_expr,
                    class,
                    class_index,
                    top_level_class_ids,
                    diags,
                );
            }
        }
        Expression::ArrayIndex { base, subscripts } => {
            check_function_call_argument_contracts_in_expr(
                base,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
            check_function_call_argument_contracts_in_subscripts(
                subscripts,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
        Expression::FieldAccess { base, .. } => {
            check_function_call_argument_contracts_in_expr(
                base,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
    }
}

fn check_function_call_argument_contracts_in_subscripts(
    subscripts: &[Subscript],
    class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    top_level_class_ids: &HashSet<DefId>,
    diags: &mut Vec<Diagnostic>,
) {
    for sub in subscripts {
        if let Subscript::Expression(sub_expr) = sub {
            check_function_call_argument_contracts_in_expr(
                sub_expr,
                class,
                class_index,
                top_level_class_ids,
                diags,
            );
        }
    }
}

#[allow(clippy::too_many_lines)]
fn check_function_call_argument_contracts_on_call(
    comp: &ComponentReference,
    args: &[Expression],
    outputs: &[Expression],
    caller_class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    top_level_class_ids: &HashSet<DefId>,
    diags: &mut Vec<Diagnostic>,
) {
    let local_record_constructor = if !comp.local {
        match comp.parts.as_slice() {
            [local] => caller_class
                .classes
                .get(local.ident.text.as_ref())
                .is_some_and(|nested| nested.class_type == ClassType::Record),
            [owner, local] if owner.ident.text.as_ref() == caller_class.name.text.as_ref() => {
                caller_class
                    .classes
                    .get(local.ident.text.as_ref())
                    .is_some_and(|nested| nested.class_type == ClassType::Record)
            }
            _ => false,
        }
    } else {
        false
    };
    if local_record_constructor {
        diags.push(
            Diagnostic::error(format!(
                "record constructor '{}' must reference a globally scoped record (MLS §12.6)",
                comp
            ))
            .with_code(ER048_RECORD_CONSTRUCTOR_NON_GLOBAL),
        );
        return;
    }

    let Some(def_id) = comp.def_id else {
        return;
    };
    let Some(callee_class) = class_index.get(&def_id) else {
        return;
    };

    let is_record_constructor_stub = callee_class.class_type == ClassType::Function
        && !callee_class
            .components
            .values()
            .any(|c| matches!(c.causality, Causality::Output(_)))
        && callee_class.algorithms.is_empty()
        && callee_class.initial_algorithms.is_empty()
        && callee_class.external.is_none();
    if callee_class.class_type == ClassType::Record || is_record_constructor_stub {
        let is_unqualified_constructor = comp.parts.len() == 1 && !comp.local;
        if !top_level_class_ids.contains(&def_id) && is_unqualified_constructor {
            diags.push(
                Diagnostic::error(format!(
                    "record constructor '{}' must reference a globally scoped record (MLS §12.6)",
                    comp
                ))
                .with_code(ER048_RECORD_CONSTRUCTOR_NON_GLOBAL),
            );
        }
        if callee_class.class_type == ClassType::Record
            && callee_class
            .components
            .values()
            .any(|component| component.condition.is_some())
        {
            diags.push(
                Diagnostic::error(format!(
                    "record constructor '{}' targets a record with conditional components (MLS §12.6.1)",
                    comp
                ))
                .with_code(ER049_RECORD_CONSTRUCTOR_CONDITIONAL_COMPONENT),
            );
        }
        return;
    }

    if callee_class.class_type != ClassType::Function {
        return;
    }
    if callee_class.partial {
        diags.push(
            Diagnostic::error(format!(
                "partial function '{}' cannot be called for simulation (MLS §12)",
                comp
            ))
            .with_code(ER037_PARTIAL_FUNCTION_CALL_FORBIDDEN),
        );
        return;
    }

    let input_slots: Vec<_> = callee_class
        .components
        .iter()
        .filter(|(_, c)| matches!(c.causality, Causality::Input(_)))
        .collect();
    let mut slot_indices: HashMap<&str, usize> = HashMap::new();
    for (idx, (name, _)) in input_slots.iter().enumerate() {
        slot_indices.insert(name.as_str(), idx);
    }

    let mut filled = vec![false; input_slots.len()];
    let mut arg_dims_by_slot: Vec<Option<Vec<usize>>> = vec![None; input_slots.len()];

    // Pass 1: positional non-named arguments fill input slots in order.
    let mut positional_idx = 0usize;
    for arg in args {
        if matches!(arg, Expression::NamedArgument { .. }) {
            continue;
        }
        if positional_idx < input_slots.len() {
            filled[positional_idx] = true;
            arg_dims_by_slot[positional_idx] = infer_expr_dims_for_call_arg(arg, caller_class);
        }
        positional_idx += 1;
    }

    // Pass 2: named arguments must not target already-filled slots.
    for arg in args {
        let Expression::NamedArgument { name, value } = arg else {
            continue;
        };
        let Some(slot_idx) = slot_indices.get(name.text.as_ref()).copied() else {
            continue;
        };
        if filled[slot_idx] {
            diags.push(
                Diagnostic::error(format!(
                    "named argument '{}' targets an input slot already filled \
                     in call to '{}' (MLS §12.4.1)",
                    name.text, comp
                ))
                .with_code(ER033_NAMED_ARGUMENT_SLOT_FILLED),
            );
        } else {
            filled[slot_idx] = true;
            arg_dims_by_slot[slot_idx] = infer_expr_dims_for_call_arg(value, caller_class);
        }
    }

    // FUNC-026/FUNC-027: automatic vectorization constraints.
    let mut vectorized_arg_dims = Vec::new();
    for (idx, (_, input_comp)) in input_slots.iter().enumerate() {
        if !component_decl_is_scalar(input_comp) {
            continue;
        }
        let Some(arg_dims) = arg_dims_by_slot[idx].as_ref() else {
            continue;
        };
        if arg_dims.is_empty() {
            continue;
        }
        vectorized_arg_dims.push(arg_dims.clone());
    }
    if !vectorized_arg_dims.is_empty() && callee_class.is_replaceable {
        diags.push(
            Diagnostic::error(format!(
                "automatic vectorization of replaceable function '{}' is not allowed (MLS §12.4.6)",
                comp
            ))
            .with_code(ER045_FUNCTION_VECTORIZATION_REPLACEABLE),
        );
    }
    if let Some(first_dims) = vectorized_arg_dims.first()
        && vectorized_arg_dims
            .iter()
            .skip(1)
            .any(|dims| dims != first_dims)
    {
        diags.push(
            Diagnostic::error(format!(
                "automatic vectorization arguments for '{}' must have matching array sizes (MLS §12.4.6)",
                comp
            ))
            .with_code(ER046_FUNCTION_VECTORIZATION_SIZE_MISMATCH),
        );
    }

    // Pass 3: remaining unfilled slots require defaults.
    for (idx, (input_name, input_comp)) in input_slots.iter().enumerate() {
        if filled[idx] {
            continue;
        }
        let has_default = input_comp.binding.is_some() || input_comp.has_explicit_binding;
        if has_default {
            continue;
        }
        diags.push(
            Diagnostic::error(format!(
                "function call '{}' is missing required input argument '{}' (MLS §12.4.1)",
                comp, input_name
            ))
            .with_code(ER034_FUNCTION_CALL_MISSING_INPUT),
        );
    }

    if !outputs.is_empty() {
        check_function_call_output_targets_match_signature(
            comp,
            outputs,
            caller_class,
            callee_class,
            diags,
        );
    }
}

fn component_decl_is_scalar(component: &ast::Component) -> bool {
    component.shape.is_empty() && component.shape_expr.is_empty()
}

fn component_decl_dims(component: &ast::Component) -> Option<Vec<usize>> {
    if !component.shape.is_empty() {
        return Some(component.shape.clone());
    }
    if component.shape_expr.is_empty() {
        return Some(Vec::new());
    }

    let mut dims = Vec::new();
    for sub in &component.shape_expr {
        let Subscript::Expression(expr) = sub else {
            return None;
        };
        let dim = extract_integer_literal_from_expr(expr)?;
        if dim < 0 {
            return None;
        }
        dims.push(dim as usize);
    }
    Some(dims)
}

fn infer_expr_dims_for_call_arg(expr: &Expression, class: &ClassDef) -> Option<Vec<usize>> {
    match expr {
        Expression::NamedArgument { value, .. } | Expression::Parenthesized { inner: value } => {
            infer_expr_dims_for_call_arg(value, class)
        }
        Expression::Array { elements, .. } => {
            if elements.is_empty() {
                return Some(vec![0]);
            }
            let mut dims = vec![elements.len()];
            if let Some(inner) = infer_expr_dims_for_call_arg(&elements[0], class)
                && !inner.is_empty()
            {
                dims.extend(inner);
            }
            Some(dims)
        }
        Expression::ComponentReference(cref) => {
            let name = component_reference_head_name(cref)?;
            let component = class.components.get(name)?;
            component_decl_dims(component)
        }
        Expression::ArrayIndex { .. } | Expression::FieldAccess { .. } => Some(Vec::new()),
        _ => None,
    }
}

fn check_function_call_output_targets_match_signature(
    comp: &ComponentReference,
    outputs: &[Expression],
    caller_class: &ClassDef,
    callee_class: &ClassDef,
    diags: &mut Vec<Diagnostic>,
) {
    let callee_outputs: Vec<_> = callee_class
        .components
        .iter()
        .filter(|(_, c)| matches!(c.causality, Causality::Output(_)))
        .collect();
    if outputs.len() > callee_outputs.len() {
        diags.push(
            Diagnostic::error(format!(
                "function call '{}' assigns {} output target(s) but function declares {} output(s) (MLS §12.4.3)",
                comp,
                outputs.len(),
                callee_outputs.len()
            ))
            .with_code(ER047_FUNCTION_OUTPUT_TARGET_MISMATCH),
        );
        return;
    }

    for (target_expr, (_, declared_output)) in outputs.iter().zip(callee_outputs.iter()) {
        let Some(target_component) =
            resolve_output_target_component(target_expr, &caller_class.components)
        else {
            continue;
        };
        if target_component.type_name.to_string() != declared_output.type_name.to_string() {
            diags.push(
                Diagnostic::error(format!(
                    "output target type '{}' does not match declared output type '{}' in call to '{}' (MLS §12.4.3)",
                    target_component.type_name,
                    declared_output.type_name,
                    comp
                ))
                .with_code(ER047_FUNCTION_OUTPUT_TARGET_MISMATCH),
            );
            continue;
        }
        let target_dims = component_decl_dims(target_component);
        let output_dims = component_decl_dims(declared_output);
        if target_dims.is_some() && output_dims.is_some() && target_dims != output_dims {
            diags.push(
                Diagnostic::error(format!(
                    "output target dimensions do not match function output signature in call to '{}' (MLS §12.4.3)",
                    comp
                ))
                .with_code(ER047_FUNCTION_OUTPUT_TARGET_MISMATCH),
            );
        }
    }
}

fn resolve_output_target_component<'a>(
    expr: &'a Expression,
    components: &'a indexmap::IndexMap<String, ast::Component>,
) -> Option<&'a ast::Component> {
    match expr {
        Expression::ComponentReference(cref) => {
            let name = component_reference_head_name(cref)?;
            components.get(name)
        }
        Expression::Parenthesized { inner } => resolve_output_target_component(inner, components),
        Expression::ArrayIndex { base, .. } | Expression::FieldAccess { base, .. } => {
            resolve_output_target_component(base, components)
        }
        _ => None,
    }
}

#[derive(Clone, Copy)]
struct ImpureCallScopeCtx {
    in_impure_function: bool,
    in_when: bool,
    in_initial: bool,
    in_pure_wrapper: bool,
}

impl ImpureCallScopeCtx {
    fn new(in_impure_function: bool) -> Self {
        Self {
            in_impure_function,
            in_when: false,
            in_initial: false,
            in_pure_wrapper: false,
        }
    }

    fn with_when(self) -> Self {
        Self {
            in_when: true,
            ..self
        }
    }

    fn with_initial(self) -> Self {
        Self {
            in_initial: true,
            ..self
        }
    }

    fn with_pure_wrapper(self) -> Self {
        Self {
            in_pure_wrapper: true,
            ..self
        }
    }

    fn allows_impure_call(self) -> bool {
        self.in_impure_function || self.in_when || self.in_initial || self.in_pure_wrapper
    }
}

fn check_impure_call_scope_in_class(
    class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    diags: &mut Vec<Diagnostic>,
) {
    let base_ctx = ImpureCallScopeCtx::new(class.class_type == ClassType::Function && !class.pure);

    for comp in class.components.values() {
        if let Some(binding) = &comp.binding {
            check_impure_call_scope_in_expr(binding, class_index, base_ctx, diags);
        }
        if !matches!(comp.start, Expression::Empty) {
            check_impure_call_scope_in_expr(&comp.start, class_index, base_ctx, diags);
        }
        if let Some(cond) = &comp.condition {
            check_impure_call_scope_in_expr(cond, class_index, base_ctx, diags);
        }
    }

    for eq in &class.equations {
        check_impure_call_scope_in_equation(eq, class_index, base_ctx, diags);
    }
    for eq in &class.initial_equations {
        check_impure_call_scope_in_equation(eq, class_index, base_ctx.with_initial(), diags);
    }
    for alg in &class.algorithms {
        for stmt in alg {
            check_impure_call_scope_in_statement(stmt, class_index, base_ctx, diags);
        }
    }
    for alg in &class.initial_algorithms {
        for stmt in alg {
            check_impure_call_scope_in_statement(stmt, class_index, base_ctx.with_initial(), diags);
        }
    }
}

fn check_impure_call_scope_in_equation(
    eq: &Equation,
    class_index: &HashMap<DefId, &ClassDef>,
    ctx: ImpureCallScopeCtx,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Empty | Equation::Connect { .. } => {}
        Equation::Simple { lhs, rhs } => {
            check_impure_call_scope_in_expr(lhs, class_index, ctx, diags);
            check_impure_call_scope_in_expr(rhs, class_index, ctx, diags);
        }
        Equation::For { indices, equations } => {
            for index in indices {
                check_impure_call_scope_in_expr(&index.range, class_index, ctx, diags);
            }
            for inner in equations {
                check_impure_call_scope_in_equation(inner, class_index, ctx, diags);
            }
        }
        Equation::When(blocks) => {
            let when_ctx = ctx.with_when();
            for block in blocks {
                check_impure_call_scope_in_expr(&block.cond, class_index, when_ctx, diags);
                for inner in &block.eqs {
                    check_impure_call_scope_in_equation(inner, class_index, when_ctx, diags);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_impure_call_scope_in_expr(&block.cond, class_index, ctx, diags);
                for inner in &block.eqs {
                    check_impure_call_scope_in_equation(inner, class_index, ctx, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_impure_call_scope_in_equation(inner, class_index, ctx, diags);
                }
            }
        }
        Equation::FunctionCall { comp, args } => {
            check_impure_function_call_scope(comp, class_index, ctx, diags);
            for arg in args {
                check_impure_call_scope_in_expr(arg, class_index, ctx, diags);
            }
        }
        Equation::Assert {
            condition,
            message,
            level,
        } => {
            check_impure_call_scope_in_expr(condition, class_index, ctx, diags);
            check_impure_call_scope_in_expr(message, class_index, ctx, diags);
            if let Some(level_expr) = level {
                check_impure_call_scope_in_expr(level_expr, class_index, ctx, diags);
            }
        }
    }
}

fn check_impure_call_scope_in_statement(
    stmt: &Statement,
    class_index: &HashMap<DefId, &ClassDef>,
    ctx: ImpureCallScopeCtx,
    diags: &mut Vec<Diagnostic>,
) {
    match stmt {
        Statement::Empty | Statement::Return { .. } | Statement::Break { .. } => {}
        Statement::Assignment { value, .. } => {
            check_impure_call_scope_in_expr(value, class_index, ctx, diags);
        }
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            check_impure_function_call_scope(comp, class_index, ctx, diags);
            for arg in args {
                check_impure_call_scope_in_expr(arg, class_index, ctx, diags);
            }
            for output in outputs {
                check_impure_call_scope_in_expr(output, class_index, ctx, diags);
            }
        }
        Statement::For { indices, equations } => {
            for index in indices {
                check_impure_call_scope_in_expr(&index.range, class_index, ctx, diags);
            }
            for inner in equations {
                check_impure_call_scope_in_statement(inner, class_index, ctx, diags);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_impure_call_scope_in_expr(&block.cond, class_index, ctx, diags);
                for inner in &block.stmts {
                    check_impure_call_scope_in_statement(inner, class_index, ctx, diags);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    check_impure_call_scope_in_statement(inner, class_index, ctx, diags);
                }
            }
        }
        Statement::When(blocks) => {
            let when_ctx = ctx.with_when();
            for block in blocks {
                check_impure_call_scope_in_expr(&block.cond, class_index, when_ctx, diags);
                for inner in &block.stmts {
                    check_impure_call_scope_in_statement(inner, class_index, when_ctx, diags);
                }
            }
        }
        Statement::While(block) => {
            check_impure_call_scope_in_expr(&block.cond, class_index, ctx, diags);
            for inner in &block.stmts {
                check_impure_call_scope_in_statement(inner, class_index, ctx, diags);
            }
        }
        Statement::Reinit { value, .. } => {
            check_impure_call_scope_in_expr(value, class_index, ctx, diags);
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => {
            check_impure_call_scope_in_expr(condition, class_index, ctx, diags);
            check_impure_call_scope_in_expr(message, class_index, ctx, diags);
            if let Some(level_expr) = level {
                check_impure_call_scope_in_expr(level_expr, class_index, ctx, diags);
            }
        }
    }
}

fn check_impure_call_scope_in_expr(
    expr: &Expression,
    class_index: &HashMap<DefId, &ClassDef>,
    ctx: ImpureCallScopeCtx,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::Empty | Expression::Terminal { .. } => {}
        Expression::ComponentReference(cref) => {
            for part in &cref.parts {
                let Some(subs) = part.subs.as_deref() else {
                    continue;
                };
                check_impure_call_scope_in_subscripts(subs, class_index, ctx, diags);
            }
        }
        Expression::Modification { value, .. } | Expression::NamedArgument { value, .. } => {
            check_impure_call_scope_in_expr(value, class_index, ctx, diags);
        }
        Expression::Range { start, step, end } => {
            check_impure_call_scope_in_expr(start, class_index, ctx, diags);
            if let Some(step_expr) = step {
                check_impure_call_scope_in_expr(step_expr, class_index, ctx, diags);
            }
            check_impure_call_scope_in_expr(end, class_index, ctx, diags);
        }
        Expression::Unary { rhs, .. } => {
            check_impure_call_scope_in_expr(rhs, class_index, ctx, diags)
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_impure_call_scope_in_expr(lhs, class_index, ctx, diags);
            check_impure_call_scope_in_expr(rhs, class_index, ctx, diags);
        }
        Expression::FunctionCall { comp, args } => {
            if is_pure_wrapper_call(comp) {
                let pure_ctx = ctx.with_pure_wrapper();
                for arg in args {
                    check_impure_call_scope_in_expr(arg, class_index, pure_ctx, diags);
                }
                return;
            }
            check_impure_function_call_scope(comp, class_index, ctx, diags);
            for arg in args {
                check_impure_call_scope_in_expr(arg, class_index, ctx, diags);
            }
        }
        Expression::ClassModification {
            target: _,
            modifications,
        } => {
            for mod_expr in modifications {
                check_impure_call_scope_in_expr(mod_expr, class_index, ctx, diags);
            }
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for elem in elements {
                check_impure_call_scope_in_expr(elem, class_index, ctx, diags);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_impure_call_scope_in_expr(cond, class_index, ctx, diags);
                check_impure_call_scope_in_expr(then_expr, class_index, ctx, diags);
            }
            check_impure_call_scope_in_expr(else_branch, class_index, ctx, diags);
        }
        Expression::Parenthesized { inner } => {
            check_impure_call_scope_in_expr(inner, class_index, ctx, diags);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            check_impure_call_scope_in_expr(expr, class_index, ctx, diags);
            for index in indices {
                check_impure_call_scope_in_expr(&index.range, class_index, ctx, diags);
            }
            if let Some(filter_expr) = filter {
                check_impure_call_scope_in_expr(filter_expr, class_index, ctx, diags);
            }
        }
        Expression::ArrayIndex { base, subscripts } => {
            check_impure_call_scope_in_expr(base, class_index, ctx, diags);
            check_impure_call_scope_in_subscripts(subscripts, class_index, ctx, diags);
        }
        Expression::FieldAccess { base, .. } => {
            check_impure_call_scope_in_expr(base, class_index, ctx, diags);
        }
    }
}

fn check_impure_call_scope_in_subscripts(
    subscripts: &[Subscript],
    class_index: &HashMap<DefId, &ClassDef>,
    ctx: ImpureCallScopeCtx,
    diags: &mut Vec<Diagnostic>,
) {
    for sub in subscripts {
        if let Subscript::Expression(sub_expr) = sub {
            check_impure_call_scope_in_expr(sub_expr, class_index, ctx, diags);
        }
    }
}

fn check_impure_function_call_scope(
    comp: &ComponentReference,
    class_index: &HashMap<DefId, &ClassDef>,
    ctx: ImpureCallScopeCtx,
    diags: &mut Vec<Diagnostic>,
) {
    if !is_impure_user_function_call(comp, class_index) || ctx.allows_impure_call() {
        return;
    }
    diags.push(
        Diagnostic::error(format!(
            "impure function call '{}' is only allowed from impure functions, when clauses, pure(), or initial sections (MLS §12.3)",
            comp
        ))
        .with_code(ER041_IMPURE_CALL_SCOPE_VIOLATION),
    );
}

fn is_impure_user_function_call(
    comp: &ComponentReference,
    class_index: &HashMap<DefId, &ClassDef>,
) -> bool {
    let Some(def_id) = comp.def_id else {
        return false;
    };
    let Some(callee) = class_index.get(&def_id) else {
        return false;
    };
    callee.class_type == ClassType::Function && !callee.pure
}

fn is_pure_wrapper_call(comp: &ComponentReference) -> bool {
    comp.parts
        .first()
        .is_some_and(|first| first.ident.text.as_ref() == "pure")
}
