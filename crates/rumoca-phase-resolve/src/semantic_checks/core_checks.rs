fn check_input_assignment(
    stmts: &[Statement],
    input_names: &std::collections::HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    for stmt in stmts {
        match stmt {
            Statement::Assignment { comp, .. } => {
                if let Some(first) = comp.parts.first()
                    && input_names.contains(&*first.ident.text)
                {
                    diags.push(
                        Diagnostic::error(format!(
                            "cannot assign to input parameter '{}' (MLS §12.2)",
                            first.ident.text
                        ))
                        .with_code(ER014_FUNCTION_INPUT_ASSIGNED),
                    );
                }
            }
            Statement::For { equations, .. } => {
                check_input_assignment(equations, input_names, diags);
            }
            Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    check_input_assignment(&block.stmts, input_names, diags);
                }
                if let Some(else_stmts) = else_block {
                    check_input_assignment(else_stmts, input_names, diags);
                }
            }
            Statement::When(blocks) => {
                for block in blocks {
                    check_input_assignment(&block.stmts, input_names, diags);
                }
            }
            Statement::While(block) => {
                check_input_assignment(&block.stmts, input_names, diags);
            }
            _ => {}
        }
    }
}

// ============================================================================
// Cross-class checks (need access to full StoredDefinition)
// ============================================================================

/// Look up a class by type name in the StoredDefinition.
fn find_class_by_name<'a>(def: &'a StoredDefinition, type_name: &str) -> Option<&'a ClassDef> {
    // Try direct lookup
    if let Some(cls) = def.classes.get(type_name) {
        return Some(cls);
    }
    // Try nested lookup (one level)
    for parent in def.classes.values() {
        if let Some(cls) = parent.classes.get(type_name) {
            return Some(cls);
        }
    }
    None
}

/// Cross-class checks that need to look up type classes.
fn check_cross_class_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for (name, comp) in &class.components {
        let type_name = comp.type_name.to_string();
        let type_class = find_class_by_name(def, &type_name);

        // DECL-005: Record components restricted to record/type
        if class.class_type == ClassType::Record
            && let Some(tc) = type_class
            && !matches!(tc.class_type, ClassType::Record | ClassType::Type)
        {
            diags.push(
                Diagnostic::error(format!(
                    "record component '{}' has type '{}' which is a {}, \
                             but only record or type components are allowed (MLS §4.7)",
                    name,
                    type_name,
                    tc.class_type.as_str()
                ))
                .with_code(ER023_RECORD_INVALID_COMPONENT_TYPE),
            );
        }

        // DECL-014: Partial class instantiation in simulation models
        if matches!(class.class_type, ClassType::Model | ClassType::Block)
            && let Some(tc) = type_class
            && tc.partial
        {
            diags.push(
                Diagnostic::error(format!(
                    "component '{}' instantiates partial {} '{}' (MLS §4.7)",
                    name,
                    tc.class_type.as_str(),
                    type_name
                ))
                .with_code(ER005_PARTIAL_CLASS_INSTANTIATION),
            );
        }

        // CONN-007: Connector component cannot be parameter/constant
        if let Some(tc) = type_class
            && tc.class_type == ClassType::Connector
            && matches!(
                comp.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            )
        {
            let var_str = match &comp.variability {
                Variability::Parameter(_) => "parameter",
                Variability::Constant(_) => "constant",
                _ => unreachable!(),
            };
            diags.push(
                Diagnostic::error(format!(
                    "connector component '{}' cannot have '{}' prefix (MLS §9.1)",
                    name, var_str
                ))
                .with_code(ER027_CONNECTOR_PARAMETER_OR_CONSTANT),
            );
        }

        // CONN-017: Unbalanced connector (flow count != potential count)
        // Check when a connector class is defined, not when used
    }

    // CONN-017: Check connector balance when defining the connector itself
    if class.class_type == ClassType::Connector && !class.partial && !class.expandable {
        check_connector_balance(class, diags);
    }

    // DECL-002: Block connector components need input/output prefix
    if class.class_type == ClassType::Block {
        for (name, comp) in &class.components {
            let type_name = comp.type_name.to_string();
            if let Some(tc) = find_class_by_name(def, &type_name)
                && tc.class_type == ClassType::Connector
                    && !comp.is_protected
                    && matches!(comp.causality, Causality::Empty)
                    // Also check if the type alias itself provides causality
                    && matches!(tc.causality, Causality::Empty)
            {
                diags.push(
                    Diagnostic::error(format!(
                        "public connector component '{}' in block '{}' must \
                             have input or output prefix (MLS §4.7)",
                        name, class.name.text
                    ))
                    .with_code(ER020_BLOCK_CONNECTOR_MISSING_IO_PREFIX),
                );
            }
        }
    }
}

/// CONN-017: Check flow/potential balance in connector.
fn check_connector_balance(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let mut flow_count = 0usize;
    let mut potential_count = 0usize;

    for (_, comp) in &class.components {
        // Only count Real-typed components for balance.
        // Non-Real types (Integer, Boolean, String) are not physical variables.
        // Non-primitive types (records, models) expand to multiple scalars
        // which we can't accurately count without type info.
        let type_name = comp.type_name.to_string();
        if type_name != "Real" {
            if !matches!(type_name.as_str(), "Integer" | "Boolean" | "String") {
                return; // Skip balance check for connectors with non-primitive members
            }
            continue; // Skip non-Real primitives for counting
        }
        match &comp.connection {
            Connection::Flow(_) => flow_count += 1,
            Connection::Stream(_) => {} // stream doesn't count
            Connection::Empty => potential_count += 1,
        }
    }

    if flow_count > 0 && flow_count != potential_count {
        diags.push(
            Diagnostic::error(format!(
                "connector '{}' is unbalanced: {} flow variable(s) vs {} \
                 potential variable(s) (MLS §9.3.1)",
                class.name.text, flow_count, potential_count
            ))
            .with_code(ER028_UNBALANCED_CONNECTOR),
        );
    }
}

/// EXPR-012: Check that parameter/constant bindings don't reference continuous variables.
fn check_parameter_variability(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let in_function = class.class_type == ClassType::Function;

    // Collect continuous variables: only Real-typed with no variability prefix and no
    // input/output causality. Input/output variables are determined externally and their
    // structural properties (like array size) are valid in parameter bindings.
    let continuous_vars: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| {
            matches!(c.variability, Variability::Empty)
                && c.type_name.to_string() == "Real"
                && (matches!(c.causality, Causality::Empty)
                    || (in_function && matches!(c.causality, Causality::Input(_))))
        })
        .map(|(n, _)| n.clone())
        .collect();

    if continuous_vars.is_empty() {
        return;
    }

    for (name, comp) in &class.components {
        if !matches!(
            comp.variability,
            Variability::Parameter(_) | Variability::Constant(_)
        ) {
            continue;
        }
        if let Some(binding) = &comp.binding {
            let mut refs = HashSet::new();
            collect_component_refs(binding, &continuous_vars, &mut refs, false);
            if !refs.is_empty() {
                let var_str = match &comp.variability {
                    Variability::Parameter(_) => "parameter",
                    Variability::Constant(_) => "constant",
                    _ => unreachable!(),
                };
                let dep = refs.into_iter().next().unwrap();
                diags.push(
                    Diagnostic::error(format!(
                        "{} '{}' cannot depend on continuous variable '{}' (MLS §3.8.4)",
                        var_str, name, dep
                    ))
                    .with_code(ER006_PARAMETER_VARIABILITY),
                );
            }
        }
    }
}

/// INST-008: Detect cyclic parameter bindings.
fn check_cyclic_parameter_bindings(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    use std::collections::HashMap;

    // Build dependency graph: parameter name -> set of parameter names referenced in binding
    let param_names: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| {
            matches!(
                c.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            )
        })
        .map(|(n, _)| n.clone())
        .collect();

    if param_names.is_empty() {
        return;
    }

    let mut deps: HashMap<String, HashSet<String>> = HashMap::new();
    for (name, comp) in &class.components {
        if !param_names.contains(name) {
            continue;
        }
        let mut refs = HashSet::new();
        if let Some(binding) = &comp.binding {
            // Skip if-branches to avoid false cycles from conditional mutual deps
            collect_component_refs(binding, &param_names, &mut refs, true);
        }
        deps.insert(name.clone(), refs);
    }

    // DFS cycle detection
    let mut visited = HashSet::new();
    let mut on_stack = HashSet::new();

    for name in param_names {
        if !visited.contains(&name) && has_cycle(&name, &deps, &mut visited, &mut on_stack) {
            diags.push(
                Diagnostic::error(format!(
                    "cyclic dependency in parameter binding for '{}' (MLS §7.2.3)",
                    name
                ))
                .with_code(ER007_CYCLIC_PARAMETER_BINDING),
            );
        }
    }
}

/// FUNC-023: Detect cyclic function default/binding dependencies.
fn check_function_binding_cycles(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    use std::collections::HashMap;

    if class.class_type != ClassType::Function {
        return;
    }

    let bound_names: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| c.binding.is_some())
        .map(|(n, _)| n.clone())
        .collect();
    if bound_names.is_empty() {
        return;
    }

    let mut deps: HashMap<String, HashSet<String>> = HashMap::new();
    for (name, comp) in &class.components {
        if !bound_names.contains(name) {
            continue;
        }
        let mut refs = HashSet::new();
        if let Some(binding) = &comp.binding {
            collect_component_refs(binding, &bound_names, &mut refs, false);
        }
        deps.insert(name.clone(), refs);
    }

    let mut visited = HashSet::new();
    let mut on_stack = HashSet::new();
    for name in bound_names {
        if !visited.contains(&name) && has_cycle(&name, &deps, &mut visited, &mut on_stack) {
            diags.push(
                Diagnostic::error(format!(
                    "cyclic dependency in function binding for '{}' (MLS §12.4.4)",
                    name
                ))
                .with_code(ER042_FUNCTION_BINDING_CYCLE),
            );
        }
    }
}

fn has_cycle(
    node: &str,
    deps: &std::collections::HashMap<String, HashSet<String>>,
    visited: &mut HashSet<String>,
    on_stack: &mut HashSet<String>,
) -> bool {
    visited.insert(node.to_string());
    on_stack.insert(node.to_string());

    let found_cycle = deps.get(node).is_some_and(|neighbors| {
        neighbors.iter().any(|neighbor| {
            if !visited.contains(neighbor) {
                has_cycle(neighbor, deps, visited, on_stack)
            } else {
                on_stack.contains(neighbor)
            }
        })
    });

    if !found_cycle {
        on_stack.remove(node);
    }
    found_cycle
}

/// Collect component references from an expression that refer to known parameter names.
/// Only matches single-part references (e.g., `x`) not multi-part (e.g., `system.x`),
/// since multi-part references access sub-components rather than the parameter itself.
/// When `skip_if_branches` is true, only collects from if-conditions (not branches),
/// avoiding false positive cycles from conditional mutual dependencies.
fn collect_component_refs(
    expr: &Expression,
    known_params: &HashSet<String>,
    refs: &mut HashSet<String>,
    skip_if_branches: bool,
) {
    match expr {
        Expression::ComponentReference(cref) => {
            // Only match single-part references for direct dependencies.
            // Multi-part refs like `system.x` access sub-components, not the param itself.
            if let [part] = cref.parts.as_slice() {
                let name = part.ident.text.to_string();
                if known_params.contains(&name) {
                    refs.insert(name);
                }
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_component_refs(lhs, known_params, refs, skip_if_branches);
            collect_component_refs(rhs, known_params, refs, skip_if_branches);
        }
        Expression::Unary { rhs, .. } => {
            collect_component_refs(rhs, known_params, refs, skip_if_branches);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_component_refs(arg, known_params, refs, skip_if_branches);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                collect_component_refs(cond, known_params, refs, skip_if_branches);
                if !skip_if_branches {
                    collect_component_refs(then_expr, known_params, refs, skip_if_branches);
                }
            }
            if !skip_if_branches {
                collect_component_refs(else_branch, known_params, refs, skip_if_branches);
            }
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                collect_component_refs(elem, known_params, refs, skip_if_branches);
            }
        }
        Expression::Parenthesized { inner, .. } => {
            collect_component_refs(inner, known_params, refs, skip_if_branches);
        }
        _ => {}
    }
}

// ============================================================================
// Batch 2: Context-sensitive checks
// ============================================================================

/// Check equations for context-sensitive issues.
fn check_equation(eq: &Equation, ctx: &mut CheckContext, diags: &mut Vec<Diagnostic>) {
    match eq {
        Equation::When(blocks) => {
            // EQN-005: Nested when-equations
            if ctx.in_when_equation {
                diags.push(
                    Diagnostic::error("when-equations cannot be nested (MLS §8.3.5)".to_string())
                        .with_code(ER017_NESTED_WHEN_EQUATION),
                );
            }

            let was = ctx.in_when_equation;
            ctx.in_when_equation = true;
            for block in blocks {
                for inner_eq in &block.eqs {
                    check_equation(inner_eq, ctx, diags);
                }
            }
            ctx.in_when_equation = was;
        }
        Equation::For {
            indices, equations, ..
        } => {
            // EQN-010: Track for-loop variables
            let new_vars: Vec<String> = indices.iter().map(|i| i.ident.text.to_string()).collect();
            ctx.for_loop_vars.extend(new_vars.clone());

            for inner_eq in equations {
                check_for_variable_assignment_eq(inner_eq, &ctx.for_loop_vars, diags);
                check_equation(inner_eq, ctx, diags);
            }

            for var in &new_vars {
                ctx.for_loop_vars.retain(|v| v != var);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_equation(inner_eq, ctx, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_equation(inner_eq, ctx, diags);
                }
            }
        }
        Equation::FunctionCall { comp, .. } => {
            // EQN-015: reinit outside when-equation
            if let Some(first) = comp.parts.first()
                && &*first.ident.text == "reinit"
                && !ctx.in_when_equation
            {
                diags.push(
                    Diagnostic::error(
                        "reinit() can only be used inside when-equations (MLS §8.3.6)".to_string(),
                    )
                    .with_code(ER008_REINIT_OUTSIDE_WHEN),
                );
            }
        }
        _ => {}
    }
}

/// Check initial equations for when-clause presence (EQN-006, EQN-037).
fn check_initial_equation(eq: &Equation, diags: &mut Vec<Diagnostic>) {
    match eq {
        Equation::When(_) => {
            diags.push(
                Diagnostic::error(
                    "when-equations are not allowed in initial equation sections (MLS §8.6)"
                        .to_string(),
                )
                .with_code(ER018_WHEN_IN_INITIAL_SECTION),
            );
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_initial_equation(inner, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_initial_equation(inner, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_initial_equation(inner, diags);
                }
            }
        }
        _ => {}
    }
}

/// Check initial algorithm statements for when-clause presence (EQN-037).
fn check_initial_statement(stmt: &Statement, diags: &mut Vec<Diagnostic>) {
    match stmt {
        Statement::When(_) => {
            diags.push(
                Diagnostic::error(
                    "when-statements are not allowed in initial algorithm sections (MLS §8.6)"
                        .to_string(),
                )
                .with_code(ER018_WHEN_IN_INITIAL_SECTION),
            );
        }
        Statement::For { equations, .. } => {
            for inner in equations {
                check_initial_statement(inner, diags);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.stmts {
                    check_initial_statement(inner, diags);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    check_initial_statement(inner, diags);
                }
            }
        }
        _ => {}
    }
}

/// Check statements for context-sensitive issues.
fn check_statement(stmt: &Statement, ctx: &mut CheckContext, diags: &mut Vec<Diagnostic>) {
    match stmt {
        Statement::When(blocks) => {
            // ALG-007/FUNC-007: When-statement in function
            if ctx.in_function {
                diags.push(
                    Diagnostic::error(
                        "when-statements are not allowed in functions (MLS §12.2)".to_string(),
                    )
                    .with_code(ER015_WHEN_IN_FUNCTION),
                );
            }

            // ALG-009: Nested when-statements
            if ctx.in_when_statement {
                diags.push(
                    Diagnostic::error("when-statements cannot be nested (MLS §11.2.7)".to_string())
                        .with_code(ER016_NESTED_WHEN_STATEMENT),
                );
            }

            let was = ctx.in_when_statement;
            ctx.in_when_statement = true;
            for block in blocks {
                for inner_stmt in &block.stmts {
                    check_statement(inner_stmt, ctx, diags);
                }
            }
            ctx.in_when_statement = was;
        }
        Statement::For { equations, .. } => {
            for inner_stmt in equations {
                check_statement(inner_stmt, ctx, diags);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner_stmt in &block.stmts {
                    check_statement(inner_stmt, ctx, diags);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner_stmt in else_stmts {
                    check_statement(inner_stmt, ctx, diags);
                }
            }
        }
        Statement::While(block) => {
            for inner_stmt in &block.stmts {
                check_statement(inner_stmt, ctx, diags);
            }
        }
        Statement::Reinit { .. } if !ctx.in_when_statement => {
            diags.push(
                Diagnostic::error(
                    "reinit() can only be used inside when-statements (MLS §8.3.6)".to_string(),
                )
                .with_code(ER008_REINIT_OUTSIDE_WHEN),
            );
        }
        _ => {}
    }
}

/// Check for-loop variable assignment in equations (EQN-010).
fn check_for_variable_assignment_eq(
    eq: &Equation,
    for_vars: &[String],
    diags: &mut Vec<Diagnostic>,
) {
    if let Equation::Simple { lhs, .. } = eq
        && let Expression::ComponentReference(comp) = lhs
        && let Some(first) = comp.parts.first()
        && for_vars.iter().any(|v| v.as_str() == &*first.ident.text)
    {
        diags.push(
            Diagnostic::error(format!(
                "cannot assign to for-loop variable '{}' (MLS §8.3.3)",
                first.ident.text
            ))
            .with_code(ER019_FOR_LOOP_VARIABLE_ASSIGNED),
        );
    }
}

// ============================================================================
// Batch 3: Expression checks
// ============================================================================

/// EXPR-014: Check for chained relational operators (e.g., 1 < 2 < 3).
pub fn check_chained_relationals(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for class in def.classes.values() {
        check_chained_in_class(class, &mut diags);
    }
    diags
}

fn check_chained_in_class(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for eq in &class.equations {
        check_chained_in_eq(eq, diags);
    }
    for eq in &class.initial_equations {
        check_chained_in_eq(eq, diags);
    }
    for alg in &class.algorithms {
        for stmt in alg {
            check_chained_in_stmt(stmt, diags);
        }
    }
    for nested in class.classes.values() {
        check_chained_in_class(nested, diags);
    }
}

fn check_chained_in_eq(eq: &Equation, diags: &mut Vec<Diagnostic>) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_chained_in_expr(lhs, diags);
            check_chained_in_expr(rhs, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_chained_in_eq(inner, diags);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_chained_in_expr(&block.cond, diags);
                for inner in &block.eqs {
                    check_chained_in_eq(inner, diags);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                check_chained_in_expr(&block.cond, diags);
                for inner in &block.eqs {
                    check_chained_in_eq(inner, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_chained_in_eq(inner, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_chained_in_stmt(stmt: &Statement, diags: &mut Vec<Diagnostic>) {
    match stmt {
        Statement::Assignment { value, .. } => check_chained_in_expr(value, diags),
        Statement::For { equations, .. } => {
            for inner in equations {
                check_chained_in_stmt(inner, diags);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                check_chained_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_chained_in_stmt(inner, diags);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    check_chained_in_stmt(inner, diags);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_chained_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_chained_in_stmt(inner, diags);
                }
            }
        }
        _ => {}
    }
}

fn is_relational(op: &OpBinary) -> bool {
    matches!(
        op,
        OpBinary::Lt(_)
            | OpBinary::Le(_)
            | OpBinary::Gt(_)
            | OpBinary::Ge(_)
            | OpBinary::Eq(_)
            | OpBinary::Neq(_)
    )
}

fn expr_is_relational(expr: &Expression) -> bool {
    matches!(expr, Expression::Binary { op, .. } if is_relational(op))
}

fn check_chained_in_expr(expr: &Expression, diags: &mut Vec<Diagnostic>) {
    match expr {
        Expression::Binary { op, lhs, rhs, .. } => {
            if is_relational(op) && (expr_is_relational(lhs) || expr_is_relational(rhs)) {
                diags.push(Diagnostic::error(
                    "chained relational operators are not allowed (MLS §3.2)".to_string(),
                ));
            }
            check_chained_in_expr(lhs, diags);
            check_chained_in_expr(rhs, diags);
        }
        Expression::Unary { rhs, .. } => check_chained_in_expr(rhs, diags),
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_chained_in_expr(arg, diags);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_chained_in_expr(cond, diags);
                check_chained_in_expr(then_expr, diags);
            }
            check_chained_in_expr(else_branch, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_chained_in_expr(elem, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => check_chained_in_expr(inner, diags),
        _ => {}
    }
}

/// EXPR-004: Check for der() in function algorithm sections.
pub fn check_der_in_functions(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for class in def.classes.values() {
        check_der_in_func_class(class, &mut diags);
    }
    diags
}

fn check_der_in_func_class(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.class_type == ClassType::Function {
        for alg in &class.algorithms {
            for stmt in alg {
                check_der_in_stmt(stmt, diags);
            }
        }
    }
    for nested in class.classes.values() {
        check_der_in_func_class(nested, diags);
    }
}

fn check_der_in_stmt(stmt: &Statement, diags: &mut Vec<Diagnostic>) {
    match stmt {
        Statement::Assignment { value, .. } => check_der_in_expr(value, diags),
        Statement::For { equations, .. } => {
            for inner in equations {
                check_der_in_stmt(inner, diags);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                check_der_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_der_in_stmt(inner, diags);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    check_der_in_stmt(inner, diags);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_der_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_der_in_stmt(inner, diags);
                }
            }
        }
        Statement::FunctionCall { args, .. } => {
            for arg in args {
                check_der_in_expr(arg, diags);
            }
        }
        _ => {}
    }
}

fn check_der_in_expr(expr: &Expression, diags: &mut Vec<Diagnostic>) {
    match expr {
        Expression::FunctionCall { comp, args, .. } => {
            if let Some(first) = comp.parts.first()
                && &*first.ident.text == "der"
            {
                diags.push(
                    Diagnostic::error("der() is not allowed in functions (MLS §12.2)".to_string())
                        .with_code(ER030_DER_IN_FUNCTION),
                );
            }
            for arg in args {
                check_der_in_expr(arg, diags);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_der_in_expr(lhs, diags);
            check_der_in_expr(rhs, diags);
        }
        Expression::Unary { rhs, .. } => check_der_in_expr(rhs, diags),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_der_in_expr(cond, diags);
                check_der_in_expr(then_expr, diags);
            }
            check_der_in_expr(else_branch, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_der_in_expr(elem, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => check_der_in_expr(inner, diags),
        _ => {}
    }
}

// ============================================================================
// DECL-020: der() on discrete variables
// ============================================================================

/// Check equations for der() applied to discrete variables.
fn check_der_on_discrete_eq(
    eq: &Equation,
    discrete_vars: &HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_der_on_discrete_expr(lhs, discrete_vars, diags);
            check_der_on_discrete_expr(rhs, discrete_vars, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_der_on_discrete_eq(inner, discrete_vars, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_der_on_discrete_eq(inner, discrete_vars, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_der_on_discrete_eq(inner, discrete_vars, diags);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner in &block.eqs {
                    check_der_on_discrete_eq(inner, discrete_vars, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_der_on_discrete_expr(
    expr: &Expression,
    discrete_vars: &HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::FunctionCall { comp, args, .. } => {
            if let Some(first) = comp.parts.first()
                && &*first.ident.text == "der"
            {
                // Check if the argument is a discrete variable
                if let Some(arg) = args.first()
                    && let Expression::ComponentReference(cref) = arg
                    && let Some(part) = cref.parts.first()
                    && discrete_vars.contains(&*part.ident.text)
                {
                    diags.push(
                        Diagnostic::error(format!(
                            "der() cannot be applied to discrete variable '{}' (MLS §3.8.5)",
                            part.ident.text
                        ))
                        .with_code(ER026_DER_ON_DISCRETE),
                    );
                }
            }
            for arg in args {
                check_der_on_discrete_expr(arg, discrete_vars, diags);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_der_on_discrete_expr(lhs, discrete_vars, diags);
            check_der_on_discrete_expr(rhs, discrete_vars, diags);
        }
        Expression::Unary { rhs, .. } => check_der_on_discrete_expr(rhs, discrete_vars, diags),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_der_on_discrete_expr(cond, discrete_vars, diags);
                check_der_on_discrete_expr(then_expr, discrete_vars, diags);
            }
            check_der_on_discrete_expr(else_branch, discrete_vars, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_der_on_discrete_expr(elem, discrete_vars, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => {
            check_der_on_discrete_expr(inner, discrete_vars, diags);
        }
        _ => {}
    }
}

// ============================================================================
// DECL-009: Protected dot access
// ============================================================================

/// Check equations for protected component access (e.g., `a.x` where x is protected).
fn check_protected_access_eq(
    eq: &Equation,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_protected_access_expr(lhs, class, def, diags);
            check_protected_access_expr(rhs, class, def, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_protected_access_eq(inner, class, def, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_protected_access_eq(inner, class, def, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_protected_access_eq(inner, class, def, diags);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner in &block.eqs {
                    check_protected_access_eq(inner, class, def, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_protected_access_expr(
    expr: &Expression,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::ComponentReference(cref) => {
            check_cref_protected_access(cref, class, def, diags);
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_protected_access_expr(lhs, class, def, diags);
            check_protected_access_expr(rhs, class, def, diags);
        }
        Expression::Unary { rhs, .. } => {
            check_protected_access_expr(rhs, class, def, diags);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_protected_access_expr(arg, class, def, diags);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_protected_access_expr(cond, class, def, diags);
                check_protected_access_expr(then_expr, class, def, diags);
            }
            check_protected_access_expr(else_branch, class, def, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_protected_access_expr(elem, class, def, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => {
            check_protected_access_expr(inner, class, def, diags);
        }
        _ => {}
    }
}

// ============================================================================
// CONN-029: Connect requires connectors
// ============================================================================

/// Check that connect() arguments refer to connector types.
/// Check if a component reference accesses a protected member.
fn check_cref_protected_access(
    cref: &ComponentReference,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    if cref.parts.len() < 2 {
        return;
    }
    let first_name = &*cref.parts[0].ident.text;
    let second_name = &*cref.parts[1].ident.text;

    let Some(comp) = class.components.get(first_name) else {
        return;
    };
    let type_name = comp.type_name.to_string();
    let Some(type_class) = find_class_by_name(def, &type_name) else {
        return;
    };
    if let Some(target) = type_class.components.get(second_name)
        && target.is_protected
    {
        diags.push(
            Diagnostic::error(format!(
                "cannot access protected component '{}.{}' (MLS §5.3)",
                first_name, second_name
            ))
            .with_code(ER025_PROTECTED_DOT_ACCESS),
        );
    }
}

fn check_connect_requires_connectors_eq(
    eq: &Equation,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Connect { lhs, rhs, .. } => {
            check_connect_arg_is_connector(lhs, class, def, diags);
            check_connect_arg_is_connector(rhs, class, def, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_connect_requires_connectors_eq(inner, class, def, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_connect_requires_connectors_eq(inner, class, def, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_connect_requires_connectors_eq(inner, class, def, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_connect_arg_is_connector(
    cref: &ComponentReference,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    // Only check single-part references (e.g., connect(x, y)).
    // Multi-part references like connect(r1.p, r2.n) access sub-components
    // which may be connectors even if the parent is a model.
    if cref.parts.len() != 1 {
        return;
    }
    if let Some(first) = cref.parts.first() {
        let comp_name = &*first.ident.text;
        if let Some(comp) = class.components.get(comp_name) {
            let type_name = comp.type_name.to_string();
            // Built-in types (Real, Integer, Boolean, String) are not connectors
            if matches!(
                type_name.as_str(),
                "Real" | "Integer" | "Boolean" | "String"
            ) {
                diags.push(
                    Diagnostic::error(format!(
                        "connect argument '{}' must be a connector, but has type '{}' (MLS §9.1)",
                        comp_name, type_name
                    ))
                    .with_code(ER009_CONNECT_ARG_NOT_CONNECTOR),
                );
                return;
            }
            // Look up the type class
            if let Some(type_class) = find_class_by_name(def, &type_name)
                && type_class.class_type != ClassType::Connector
            {
                diags.push(
                    Diagnostic::error(format!(
                        "connect argument '{}' must be a connector, but '{}' is a {} (MLS §9.1)",
                        comp_name,
                        type_name,
                        type_class.class_type.as_str()
                    ))
                    .with_code(ER009_CONNECT_ARG_NOT_CONNECTOR),
                );
            }
        }
    }
}

// ============================================================================
// EXPR-013: 'end' outside subscript context
// ============================================================================

/// Check equations for 'end' used outside of array subscripts.
fn check_end_outside_subscript_eq(eq: &Equation, diags: &mut Vec<Diagnostic>) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_end_outside_subscript_expr(lhs, false, diags);
            check_end_outside_subscript_expr(rhs, false, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_end_outside_subscript_eq(inner, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_end_outside_subscript_eq(inner, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_end_outside_subscript_eq(inner, diags);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner in &block.eqs {
                    check_end_outside_subscript_eq(inner, diags);
                }
            }
        }
        _ => {}
    }
}

/// Check an expression for 'end' used outside subscript context.
/// `in_subscript` tracks whether we're inside a subscript.
fn check_end_outside_subscript_expr(
    expr: &Expression,
    in_subscript: bool,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::End,
            ..
        } if !in_subscript => {
            diags.push(
                Diagnostic::error(
                    "'end' can only be used within array subscripts (MLS §10.5.1)".to_string(),
                )
                .with_code(ER031_END_OUTSIDE_SUBSCRIPT),
            );
        }
        Expression::ComponentReference(cref) => {
            // Subscripts inside component references count as subscript context
            let subs = cref.parts.iter().filter_map(|p| p.subs.as_ref()).flatten();
            for sub in subs {
                check_end_outside_subscript_subscript(sub, diags);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_end_outside_subscript_expr(lhs, in_subscript, diags);
            check_end_outside_subscript_expr(rhs, in_subscript, diags);
        }
        Expression::Unary { rhs, .. } => {
            check_end_outside_subscript_expr(rhs, in_subscript, diags);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_end_outside_subscript_expr(arg, in_subscript, diags);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                check_end_outside_subscript_expr(cond, in_subscript, diags);
                check_end_outside_subscript_expr(then_expr, in_subscript, diags);
            }
            check_end_outside_subscript_expr(else_branch, in_subscript, diags);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_end_outside_subscript_expr(elem, in_subscript, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => {
            check_end_outside_subscript_expr(inner, in_subscript, diags);
        }
        _ => {}
    }
}

fn check_end_outside_subscript_subscript(sub: &Subscript, diags: &mut Vec<Diagnostic>) {
    match sub {
        Subscript::Expression(expr) => {
            // Inside a subscript, 'end' is valid
            check_end_outside_subscript_expr(expr, true, diags);
        }
        Subscript::Empty | Subscript::Range { .. } => {}
    }
}

// ============================================================================
// EXPR-002: Real equality, EXPR-016: non-Boolean if, TYPE-005: class as value
// ============================================================================

/// Check equations for expression-level type issues that can be detected without
/// full type inference.
fn check_expr_type_issues_eq(
    eq: &Equation,
    class: &ClassDef,
    def: &StoredDefinition,
    real_vars: &HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_expr_type_issues(lhs, class, def, real_vars, diags);
            check_expr_type_issues(rhs, class, def, real_vars, diags);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                check_expr_type_issues_eq(inner, class, def, real_vars, diags);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    check_expr_type_issues_eq(inner, class, def, real_vars, diags);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    check_expr_type_issues_eq(inner, class, def, real_vars, diags);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner in &block.eqs {
                    check_expr_type_issues_eq(inner, class, def, real_vars, diags);
                }
            }
        }
        _ => {}
    }
}

fn check_expr_type_issues(
    expr: &Expression,
    class: &ClassDef,
    def: &StoredDefinition,
    real_vars: &HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        // EXPR-002: Real equality/inequality comparison
        Expression::Binary { op, lhs, rhs, .. } => {
            if matches!(op, OpBinary::Eq(_) | OpBinary::Neq(_))
                && (expr_is_real(lhs, real_vars) || expr_is_real(rhs, real_vars))
            {
                diags.push(
                    Diagnostic::error(
                        "equality comparison on Real values is not allowed \
                             outside functions (MLS §3.5)"
                            .to_string(),
                    )
                    .with_code(ER029_REAL_EQUALITY_COMPARISON),
                );
            }
            check_expr_type_issues(lhs, class, def, real_vars, diags);
            check_expr_type_issues(rhs, class, def, real_vars, diags);
        }
        // EXPR-016: non-Boolean if-expression condition
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                if expr_is_numeric_literal(cond) {
                    diags.push(
                        Diagnostic::error(
                            "if-expression condition must be Boolean, \
                             not a numeric value (MLS §3.6.5)"
                                .to_string(),
                        )
                        .with_code(ER010_IF_CONDITION_NOT_BOOLEAN),
                    );
                }
                check_expr_type_issues(cond, class, def, real_vars, diags);
                check_expr_type_issues(then_expr, class, def, real_vars, diags);
            }
            check_expr_type_issues(else_branch, class, def, real_vars, diags);
        }
        // TYPE-005: Class name used as value in equation
        Expression::ComponentReference(cref) if cref.parts.len() == 1 => {
            let name = &*cref.parts[0].ident.text;
            // Check if this refers to a class rather than a component
            if !class.components.contains_key(name) && find_class_by_name(def, name).is_some() {
                diags.push(
                    Diagnostic::error(format!(
                        "'{}' is a class, not a variable; cannot be used as a value (MLS §4.4)",
                        name
                    ))
                    .with_code(ER011_CLASS_USED_AS_VALUE),
                );
            }
        }
        Expression::Unary { rhs, .. } => {
            check_expr_type_issues(rhs, class, def, real_vars, diags);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_expr_type_issues(arg, class, def, real_vars, diags);
            }
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_expr_type_issues(elem, class, def, real_vars, diags);
            }
        }
        Expression::Parenthesized { inner, .. } => {
            check_expr_type_issues(inner, class, def, real_vars, diags);
        }
        _ => {}
    }
}

/// Check if an expression is clearly a Real-typed component reference.
fn expr_is_real(expr: &Expression, real_vars: &HashSet<String>) -> bool {
    if let Expression::ComponentReference(cref) = expr
        && let Some(first) = cref.parts.first()
    {
        return real_vars.contains(&*first.ident.text);
    }
    // Real literals
    if let Expression::Terminal {
        terminal_type: TerminalType::UnsignedReal,
        ..
    } = expr
    {
        return true;
    }
    false
}

/// Check if an expression is a numeric literal (not Boolean).
fn expr_is_numeric_literal(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger | TerminalType::UnsignedReal,
            ..
        }
    )
}
