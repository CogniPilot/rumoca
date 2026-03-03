use super::*;

pub(super) fn bind_function_inputs(
    function_def: &ast::ClassDef,
    args: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    caller_locals: Option<&FxHashMap<String, i64>>,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<()> {
    let inputs: Vec<_> = function_def
        .components
        .iter()
        .filter(|(_, comp)| matches!(comp.causality, ast::Causality::Input(_)))
        .collect();

    let mut positional_idx = 0usize;
    for arg in args {
        if matches!(arg, ast::Expression::NamedArgument { .. }) {
            continue;
        }
        if let Some((name, _)) = inputs.get(positional_idx) {
            let value = try_eval_integer_expr_with_depth_and_locals(
                arg,
                env.mod_env,
                env.effective_components,
                env.tree,
                depth,
                caller_locals,
            )?;
            local_values.insert((*name).clone(), value);
        }
        positional_idx += 1;
    }

    for arg in args {
        if let ast::Expression::NamedArgument { name, value } = arg
            && let Some((param_name, _)) = inputs
                .iter()
                .find(|(input_name, _)| input_name.as_str() == name.text.as_ref())
        {
            let input_value = try_eval_integer_expr_with_depth_and_locals(
                value,
                env.mod_env,
                env.effective_components,
                env.tree,
                depth,
                caller_locals,
            )?;
            local_values.insert((*param_name).clone(), input_value);
        }
    }

    for (input_name, input_component) in inputs {
        if local_values.contains_key(input_name.as_str()) {
            continue;
        }
        if let Some(binding) = &input_component.binding
            && let Some(value) = try_eval_integer_expr_with_depth_and_locals(
                binding,
                env.mod_env,
                env.effective_components,
                env.tree,
                depth,
                Some(local_values),
            )
        {
            local_values.insert(input_name.clone(), value);
            continue;
        }
        if let Some(value) = try_eval_integer_expr_with_depth_and_locals(
            &input_component.start,
            env.mod_env,
            env.effective_components,
            env.tree,
            depth,
            Some(local_values),
        ) {
            local_values.insert(input_name.clone(), value);
        }
    }

    Some(())
}

pub(super) fn initialize_function_locals(
    function_def: &ast::ClassDef,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) {
    for (name, component) in &function_def.components {
        if local_values.contains_key(name.as_str()) {
            continue;
        }
        if let Some(binding) = &component.binding
            && let Some(value) = try_eval_integer_expr_with_depth_and_locals(
                binding,
                env.mod_env,
                env.effective_components,
                env.tree,
                depth,
                Some(local_values),
            )
        {
            local_values.insert(name.clone(), value);
            continue;
        }
        if let Some(value) = try_eval_integer_expr_with_depth_and_locals(
            &component.start,
            env.mod_env,
            env.effective_components,
            env.tree,
            depth,
            Some(local_values),
        ) {
            local_values.insert(name.clone(), value);
        }
    }
}

pub(super) fn find_function_output_name(function_def: &ast::ClassDef) -> Option<String> {
    function_def
        .components
        .iter()
        .find(|(_, comp)| matches!(comp.causality, ast::Causality::Output(_)))
        .map(|(name, _)| name.clone())
}

pub(super) fn interpret_function_statements(
    statements: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<bool> {
    for statement in statements {
        if interpret_function_statement(statement, env, depth, local_values)? {
            return Some(true);
        }
    }
    Some(false)
}

fn interpret_function_statement(
    statement: &ast::Statement,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<bool> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    match statement {
        ast::Statement::Assignment { comp, value } => {
            let evaluated = try_eval_integer_expr_with_depth_and_locals(
                value,
                env.mod_env,
                env.effective_components,
                env.tree,
                depth + 1,
                Some(local_values),
            )?;
            local_values.insert(comp.to_string(), evaluated);
            Some(false)
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => interpret_function_if(cond_blocks, else_block.as_deref(), env, depth, local_values),
        ast::Statement::For { indices, equations } => {
            interpret_function_for(indices, equations, env, depth, local_values)
        }
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            handle_function_call_statement(comp, args, outputs, env, depth, local_values);
            Some(false)
        }
        ast::Statement::Return { .. } => Some(true),
        ast::Statement::Empty
        | ast::Statement::Break { .. }
        | ast::Statement::While(_)
        | ast::Statement::When(_)
        | ast::Statement::Reinit { .. }
        | ast::Statement::Assert { .. } => Some(false),
    }
}

fn interpret_function_if(
    cond_blocks: &[rumoca_ir_ast::StatementBlock],
    else_block: Option<&[ast::Statement]>,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<bool> {
    for block in cond_blocks {
        if try_eval_bool_expr_with_depth_and_locals(
            &block.cond,
            env,
            depth + 1,
            Some(local_values),
        )? {
            return interpret_function_statements(&block.stmts, env, depth + 1, local_values);
        }
    }
    if let Some(else_stmts) = else_block {
        return interpret_function_statements(else_stmts, env, depth + 1, local_values);
    }
    Some(false)
}

fn interpret_function_for(
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<bool> {
    if indices.len() != 1 {
        return Some(false);
    }
    let index = &indices[0];
    let loop_name = index.ident.text.to_string();
    let values = evaluate_for_index_values(&index.range, env, depth + 1, Some(local_values))?;
    for value in values {
        local_values.insert(loop_name.clone(), value);
        if interpret_function_statements(equations, env, depth + 1, local_values)? {
            local_values.remove(&loop_name);
            return Some(true);
        }
    }
    local_values.remove(&loop_name);
    Some(false)
}

fn handle_function_call_statement(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    outputs: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) {
    let result = eval_integer_function_call(comp, args, env, depth + 1, Some(local_values));
    if let Some(value) = result
        && outputs.len() == 1
        && let ast::Expression::ComponentReference(output_ref) = &outputs[0]
    {
        local_values.insert(output_ref.to_string(), value);
    }
}

fn evaluate_for_index_values(
    range: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<Vec<i64>> {
    match range {
        ast::Expression::Range { start, step, end } => {
            let start_value = try_eval_integer_expr_with_depth_and_locals(
                start,
                env.mod_env,
                env.effective_components,
                env.tree,
                depth + 1,
                local_ints,
            )?;
            let end_value = try_eval_integer_expr_with_depth_and_locals(
                end,
                env.mod_env,
                env.effective_components,
                env.tree,
                depth + 1,
                local_ints,
            )?;
            let step_value = if let Some(step_expr) = step {
                try_eval_integer_expr_with_depth_and_locals(
                    step_expr,
                    env.mod_env,
                    env.effective_components,
                    env.tree,
                    depth + 1,
                    local_ints,
                )?
            } else {
                1
            };
            if step_value == 0 {
                return None;
            }

            let mut values = Vec::new();
            let mut current = start_value;
            if step_value > 0 {
                while current <= end_value {
                    values.push(current);
                    current += step_value;
                }
            } else {
                while current >= end_value {
                    values.push(current);
                    current += step_value;
                }
            }
            Some(values)
        }
        _ => {
            let end_value = try_eval_integer_expr_with_depth_and_locals(
                range,
                env.mod_env,
                env.effective_components,
                env.tree,
                depth + 1,
                local_ints,
            )?;
            (end_value >= 1).then(|| (1..=end_value).collect())
        }
    }
}

fn try_eval_bool_expr_with_depth_and_locals(
    expr: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<bool> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    let recurse = |e| try_eval_bool_expr_with_depth_and_locals(e, env, depth + 1, local_ints);
    let int_eval = |e| {
        try_eval_integer_expr_with_depth_and_locals(
            e,
            env.mod_env,
            env.effective_components,
            env.tree,
            depth + 1,
            local_ints,
        )
    };

    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        ast::Expression::Unary {
            op: rumoca_ir_ast::OpUnary::Not(_),
            rhs,
        } => Some(!recurse(rhs)?),
        ast::Expression::Parenthesized { inner } => recurse(inner),
        ast::Expression::Binary { op, lhs, rhs } => match op {
            rumoca_ir_ast::OpBinary::And(_) => Some(recurse(lhs)? && recurse(rhs)?),
            rumoca_ir_ast::OpBinary::Or(_) => Some(recurse(lhs)? || recurse(rhs)?),
            rumoca_ir_ast::OpBinary::Eq(_) => Some(int_eval(lhs)? == int_eval(rhs)?),
            rumoca_ir_ast::OpBinary::Neq(_) => Some(int_eval(lhs)? != int_eval(rhs)?),
            rumoca_ir_ast::OpBinary::Lt(_) => Some(int_eval(lhs)? < int_eval(rhs)?),
            rumoca_ir_ast::OpBinary::Le(_) => Some(int_eval(lhs)? <= int_eval(rhs)?),
            rumoca_ir_ast::OpBinary::Gt(_) => Some(int_eval(lhs)? > int_eval(rhs)?),
            rumoca_ir_ast::OpBinary::Ge(_) => Some(int_eval(lhs)? >= int_eval(rhs)?),
            _ => None,
        },
        _ => None,
    }
}

/// Evaluate array dimensions from shape_expr subscripts.
/// Returns None if any dimension cannot be evaluated.
///
/// MLS §10.1: Array dimensions can depend on parameters that are overridden
/// by modifications. We try evaluating shape_expr with the current mod_env first,
/// which handles cases like `Plug starpoints(m=mSystems)` where `mSystems=1`
/// overrides the default `m=3`. Falls back to pre-computed shape if shape_expr
/// evaluation fails.
pub(crate) fn evaluate_array_dimensions(
    shape: &[usize],
    shape_expr: &[rumoca_ir_ast::Subscript],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<Vec<i64>> {
    // Prefer shape_expr because it reflects active modifications.
    // Fall back to precomputed shape only if expression evaluation fails.
    if !shape_expr.is_empty()
        && let Some(dims) = eval_shape_expr(shape_expr, mod_env, effective_components, tree)
    {
        return Some(dims);
    }

    if !shape.is_empty() {
        return Some(shape.iter().map(|&d| d as i64).collect());
    }

    Some(vec![]) // Scalar
}

/// Try to evaluate shape_expr subscripts to concrete dimensions.
fn eval_shape_expr(
    shape_expr: &[rumoca_ir_ast::Subscript],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    for sub in shape_expr {
        match sub {
            rumoca_ir_ast::Subscript::Expression(expr) => {
                // Shape expressions may include structural `if` branches that are
                // valid at translation time for parameter-based dimensions (MLS §10.1).
                // Keep this behavior scoped to shape evaluation so general integer
                // expression evaluation remains unchanged.
                let dim = try_eval_integer_shape_expr(expr, mod_env, effective_components, tree)?;
                if dim < 0 {
                    return None;
                }
                dims.push(dim);
            }
            rumoca_ir_ast::Subscript::Range { .. } | rumoca_ir_ast::Subscript::Empty => {
                return None;
            }
        }
    }
    Some(dims)
}

pub(crate) fn try_eval_integer_shape_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<i64> {
    try_eval_integer_shape_expr_with_depth(expr, mod_env, effective_components, tree, 0)
}

fn try_eval_integer_shape_expr_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    let recurse = |e| {
        try_eval_integer_shape_expr_with_depth(e, mod_env, effective_components, tree, depth + 1)
    };

    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.parse::<i64>().ok(),
        ast::Expression::ComponentReference(comp_ref) => {
            eval_integer_shape_component_ref(comp_ref, mod_env, effective_components, tree, depth)
        }
        ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, branch_expr) in branches {
                if !shape_condition_uses_static_components(cond, mod_env, effective_components) {
                    return None;
                }
                let condition_value = evaluate_component_condition_with_depth(
                    cond,
                    mod_env,
                    effective_components,
                    tree,
                    depth + 1,
                )?;
                if condition_value {
                    return recurse(branch_expr);
                }
            }
            recurse(else_branch)
        }
        ast::Expression::Binary { op, lhs, rhs } => {
            let l = recurse(lhs)?;
            let r = recurse(rhs)?;
            eval_integer_binary(op, l, r)
        }
        ast::Expression::Unary { op, rhs } => {
            let value = recurse(rhs)?;
            match op {
                rumoca_ir_ast::OpUnary::Minus(_) => Some(-value),
                rumoca_ir_ast::OpUnary::Plus(_) => Some(value),
                _ => None,
            }
        }
        ast::Expression::Parenthesized { inner } => recurse(inner),
        ast::Expression::FunctionCall { .. } => {
            try_eval_integer_expr_with_depth(expr, mod_env, effective_components, tree, depth + 1)
        }
        _ => None,
    }
}

fn eval_integer_shape_component_ref(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    let recurse = |expr| {
        try_eval_integer_shape_expr_with_depth(expr, mod_env, effective_components, tree, depth + 1)
    };

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    let dotted = comp_ref
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    if let Some(mod_value) = mod_env.get(&param_path) {
        return recurse(&mod_value.value);
    }

    if comp_ref.parts.len() == 1 {
        let name = comp_ref.parts[0].ident.text.as_ref();
        if let Some(component) = effective_components.get(name) {
            return recurse(component_expr_for_structural_eval(component)?);
        }
    }

    if let Some(component) = effective_components.get(dotted.as_str()) {
        return recurse(component_expr_for_structural_eval(component)?);
    }

    for candidate in enclosing_scope_candidates(dotted.as_str()) {
        let qn = ast::QualifiedName::from_dotted(&candidate);
        if let Some(mod_value) = mod_env.get(&qn) {
            return recurse(&mod_value.value);
        }
        if let Some(component) = effective_components.get(candidate.as_str()) {
            return recurse(component_expr_for_structural_eval(component)?);
        }
    }

    let dotted_suffix = format!(".{dotted}");
    let mut mod_suffix_match = None;
    for (qn, mv) in &mod_env.active {
        let qn_dotted = qn
            .parts
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        if qn_dotted.ends_with(&dotted_suffix) && mod_suffix_match.replace(mv).is_some() {
            mod_suffix_match = None;
            break;
        }
    }
    if let Some(mv) = mod_suffix_match {
        return recurse(&mv.value);
    }

    let mut comp_suffix_match: Option<&ast::Component> = None;
    for (name, comp) in effective_components {
        if name.ends_with(&dotted_suffix) && comp_suffix_match.replace(comp).is_some() {
            comp_suffix_match = None;
            break;
        }
    }
    if let Some(comp) = comp_suffix_match {
        return recurse(component_expr_for_structural_eval(comp)?);
    }

    let env = IntegerEvalEnv {
        mod_env,
        effective_components,
        tree,
    };
    if let Some(value) = eval_integer_class_redeclare_field_ref(comp_ref, env, depth, None) {
        return Some(value);
    }

    // Fall back to record-field resolution for paths like `data.n` where the
    // value lives in a record default/modification environment.
    eval_integer_record_field_ref(comp_ref, env, depth)
}

fn shape_condition_uses_static_components(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
) -> bool {
    match expr {
        ast::Expression::Terminal { .. } => true,
        ast::Expression::ComponentReference(comp_ref) => {
            shape_component_ref_is_static(comp_ref, mod_env, effective_components)
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            shape_condition_uses_static_components(lhs, mod_env, effective_components)
                && shape_condition_uses_static_components(rhs, mod_env, effective_components)
        }
        ast::Expression::Unary { rhs, .. } => {
            shape_condition_uses_static_components(rhs, mod_env, effective_components)
        }
        ast::Expression::Parenthesized { inner } => {
            shape_condition_uses_static_components(inner, mod_env, effective_components)
        }
        _ => false,
    }
}

fn shape_component_ref_is_static(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
) -> bool {
    if comp_ref.parts.is_empty() {
        return false;
    }

    let dotted = comp_ref
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    let is_static_component = |component: &ast::Component| {
        component.is_structural
            || matches!(
                component.variability,
                rumoca_ir_ast::Variability::Parameter(_) | rumoca_ir_ast::Variability::Constant(_)
            )
    };

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    if mod_env.get(&param_path).is_some() {
        return true;
    }

    if let Some(component) = effective_components.get(dotted.as_str()) {
        return is_static_component(component);
    }

    if comp_ref.parts.len() == 1
        && let Some(component) = effective_components.get(comp_ref.parts[0].ident.text.as_ref())
    {
        return is_static_component(component);
    }

    let dotted_suffix = format!(".{dotted}");
    let mut mod_suffix_found = false;
    for qn in mod_env.active.keys() {
        let qn_dotted = qn
            .parts
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        if qn_dotted.ends_with(&dotted_suffix) {
            if mod_suffix_found {
                mod_suffix_found = false;
                break;
            }
            mod_suffix_found = true;
        }
    }
    if mod_suffix_found {
        return true;
    }

    let mut comp_suffix_match: Option<&ast::Component> = None;
    for (name, comp) in effective_components {
        if name.ends_with(&dotted_suffix) && comp_suffix_match.replace(comp).is_some() {
            comp_suffix_match = None;
            break;
        }
    }
    if let Some(component) = comp_suffix_match {
        return is_static_component(component);
    }

    // Unresolved refs in conditions are typically enum literals and are checked
    // by evaluate_component_condition_with_depth.
    true
}

/// Generate all array indices for multi-dimensional arrays.
/// For dims = [2, 3], generates: [[1,1], [1,2], [1,3], [2,1], [2,2], [2,3]]
/// Uses 1-based indexing per Modelica semantics (MLS §10.1).
pub(crate) fn generate_array_indices(dims: &[i64]) -> Vec<Vec<i64>> {
    if dims.is_empty() {
        return vec![]; // Scalar, no indices needed
    }

    let total: usize = dims.iter().map(|&d| d as usize).product();
    let mut result = Vec::with_capacity(total);

    // Generate all combinations using iterative approach
    let mut indices = vec![1i64; dims.len()];
    loop {
        result.push(indices.clone());

        // Increment indices from right to left (like counting)
        let mut i = dims.len();
        while i > 0 {
            i -= 1;
            indices[i] += 1;
            if indices[i] <= dims[i] {
                break;
            }
            // Carry over
            if i == 0 {
                return result; // All combinations generated
            }
            indices[i] = 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::{
        enum_values_equal, eval_integer_binary, evaluate_array_dimensions,
        evaluate_component_condition, try_eval_integer_expr,
    };
    use indexmap::IndexMap;
    use rumoca_ir_ast as ast;

    #[test]
    fn enum_values_equal_accepts_different_qualification_prefixes() {
        let a = "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve";
        let b = "Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve";
        assert!(enum_values_equal(a, b));
    }

    #[test]
    fn enum_values_equal_rejects_different_enum_types() {
        let a = "Modelica.Blocks.Types.SimpleController.PI";
        let b = "Modelica.Blocks.Types.Init.PI";
        assert!(!enum_values_equal(a, b));
    }

    fn token(text: &str) -> ast::Token {
        ast::Token {
            text: Arc::from(text),
            ..ast::Token::default()
        }
    }

    fn cref(path: &str) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: path
                .split('.')
                .map(|part| ast::ComponentRefPart {
                    ident: token(part),
                    subs: None,
                })
                .collect(),
            def_id: None,
        }
    }

    fn eq_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op: ast::OpBinary::Eq(token("==")),
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
        }
    }

    fn add_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op: ast::OpBinary::Add(token("+")),
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
        }
    }

    fn if_expr(
        branches: Vec<(ast::Expression, ast::Expression)>,
        else_branch: ast::Expression,
    ) -> ast::Expression {
        ast::Expression::If {
            branches,
            else_branch: Arc::new(else_branch),
        }
    }

    fn int_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: token(&value.to_string()),
        }
    }

    fn bool_expr(value: bool) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token: token(if value { "true" } else { "false" }),
        }
    }

    #[test]
    fn integer_div_operator_requires_exact_quotient() {
        assert_eq!(
            eval_integer_binary(&ast::OpBinary::Div(token("/")), 8, 2),
            Some(4)
        );
        assert_eq!(
            eval_integer_binary(&ast::OpBinary::Div(token("/")), 7, 2),
            None
        );
    }

    #[test]
    fn integer_div_builtin_remains_truncating() {
        let expr = ast::Expression::FunctionCall {
            comp: cref("div"),
            args: vec![int_expr(7), int_expr(2)],
        };

        assert_eq!(
            try_eval_integer_expr(
                &expr,
                &ast::ModificationEnvironment::new(),
                &IndexMap::new(),
                &ast::ClassTree::new(),
            ),
            Some(3)
        );
    }

    #[test]
    fn evaluate_component_condition_with_resolved_enum_ref() {
        let mut components = IndexMap::new();
        let mut model_structure = ast::Component {
            name: "modelStructure".to_string(),
            ..ast::Component::default()
        };
        model_structure.start =
            ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb"));
        components.insert("modelStructure".to_string(), model_structure);

        let condition = eq_expr(
            ast::Expression::ComponentReference(cref("modelStructure")),
            ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
        );
        let value = evaluate_component_condition(
            &condition,
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
        );

        assert_eq!(value, Some(true));
    }

    #[test]
    fn evaluate_component_condition_with_unresolved_enum_ref_is_unknown() {
        let mut components = IndexMap::new();
        let model_structure = ast::Component {
            name: "modelStructure".to_string(),
            ..ast::Component::default()
        };
        components.insert("modelStructure".to_string(), model_structure);

        let condition = eq_expr(
            ast::Expression::ComponentReference(cref("modelStructure")),
            ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
        );
        let value = evaluate_component_condition(
            &condition,
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
        );

        assert_eq!(value, None);
    }

    #[test]
    fn evaluate_array_dimensions_supports_structural_if_shape_refs() {
        let mut components = IndexMap::new();
        components.insert(
            "useLumpedPressure".to_string(),
            ast::Component {
                name: "useLumpedPressure".to_string(),
                variability: rumoca_ir_ast::Variability::Parameter(token("parameter")),
                start: bool_expr(false),
                ..ast::Component::default()
            },
        );
        components.insert(
            "nFMLumped".to_string(),
            ast::Component {
                name: "nFMLumped".to_string(),
                variability: rumoca_ir_ast::Variability::Parameter(token("parameter")),
                start: int_expr(2),
                ..ast::Component::default()
            },
        );
        components.insert(
            "nFMDistributed".to_string(),
            ast::Component {
                name: "nFMDistributed".to_string(),
                variability: rumoca_ir_ast::Variability::Parameter(token("parameter")),
                start: int_expr(1),
                ..ast::Component::default()
            },
        );
        components.insert(
            "nFM".to_string(),
            ast::Component {
                name: "nFM".to_string(),
                start: if_expr(
                    vec![(
                        ast::Expression::ComponentReference(cref("useLumpedPressure")),
                        ast::Expression::ComponentReference(cref("nFMLumped")),
                    )],
                    ast::Expression::ComponentReference(cref("nFMDistributed")),
                ),
                ..ast::Component::default()
            },
        );

        let dims = evaluate_array_dimensions(
            &[1],
            &[ast::Subscript::Expression(add_expr(
                ast::Expression::ComponentReference(cref("nFM")),
                int_expr(1),
            ))],
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
        );

        assert_eq!(dims, Some(vec![2]));
    }

    #[test]
    fn evaluate_array_dimensions_rejects_runtime_if_shape_condition() {
        let mut components = IndexMap::new();
        components.insert(
            "runtimeSwitch".to_string(),
            ast::Component {
                name: "runtimeSwitch".to_string(),
                start: bool_expr(false),
                ..ast::Component::default()
            },
        );
        components.insert(
            "nA".to_string(),
            ast::Component {
                name: "nA".to_string(),
                variability: rumoca_ir_ast::Variability::Parameter(token("parameter")),
                start: int_expr(2),
                ..ast::Component::default()
            },
        );
        components.insert(
            "nB".to_string(),
            ast::Component {
                name: "nB".to_string(),
                variability: rumoca_ir_ast::Variability::Parameter(token("parameter")),
                start: int_expr(1),
                ..ast::Component::default()
            },
        );
        components.insert(
            "n".to_string(),
            ast::Component {
                name: "n".to_string(),
                start: if_expr(
                    vec![(
                        ast::Expression::ComponentReference(cref("runtimeSwitch")),
                        ast::Expression::ComponentReference(cref("nA")),
                    )],
                    ast::Expression::ComponentReference(cref("nB")),
                ),
                ..ast::Component::default()
            },
        );

        let dims = evaluate_array_dimensions(
            &[1],
            &[ast::Subscript::Expression(add_expr(
                ast::Expression::ComponentReference(cref("n")),
                int_expr(1),
            ))],
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
        );

        // Non-compile-time condition should keep dimension-expression evaluation
        // disabled and preserve the precomputed fallback shape.
        assert_eq!(dims, Some(vec![1]));
    }

    #[test]
    fn try_eval_integer_expr_prefers_binding_over_start_for_component_refs() {
        let mut components = IndexMap::new();
        components.insert(
            "n".to_string(),
            ast::Component {
                name: "n".to_string(),
                variability: rumoca_ir_ast::Variability::Parameter(token("parameter")),
                // Unresolvable start should not override explicit binding.
                start: ast::Expression::ComponentReference(cref("missing.scope.value")),
                binding: Some(int_expr(1)),
                has_explicit_binding: true,
                ..ast::Component::default()
            },
        );

        let value = try_eval_integer_expr(
            &add_expr(ast::Expression::ComponentReference(cref("n")), int_expr(1)),
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
        );

        assert_eq!(value, Some(2));
    }

    #[test]
    fn evaluate_array_dimensions_prefers_binding_over_start_for_shape_refs() {
        let mut components = IndexMap::new();
        components.insert(
            "m".to_string(),
            ast::Component {
                name: "m".to_string(),
                variability: rumoca_ir_ast::Variability::Parameter(token("parameter")),
                // Keep start unresolved and provide the structural value via binding.
                start: ast::Expression::ComponentReference(cref("missing.scope.value")),
                binding: Some(int_expr(1)),
                has_explicit_binding: true,
                ..ast::Component::default()
            },
        );

        let dims = evaluate_array_dimensions(
            &[0],
            &[ast::Subscript::Expression(add_expr(
                ast::Expression::ComponentReference(cref("m")),
                int_expr(1),
            ))],
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
        );

        assert_eq!(dims, Some(vec![2]));
    }

    #[test]
    fn evaluate_array_dimensions_reads_record_field_from_class_modification() {
        let mut components = IndexMap::new();
        components.insert(
            "stackData".to_string(),
            ast::Component {
                name: "stackData".to_string(),
                variability: rumoca_ir_ast::Variability::Parameter(token("parameter")),
                ..ast::Component::default()
            },
        );

        let mut mod_env = ast::ModificationEnvironment::new();
        let stack_data_mod = ast::Expression::ClassModification {
            target: cref("StackData"),
            modifications: vec![
                ast::Expression::NamedArgument {
                    name: token("Ns"),
                    value: Arc::new(int_expr(3)),
                },
                ast::Expression::NamedArgument {
                    name: token("Np"),
                    value: Arc::new(int_expr(2)),
                },
            ],
        };
        mod_env.add(
            ast::QualifiedName::from_ident("stackData"),
            ast::ModificationValue::simple(stack_data_mod),
        );

        let dims = evaluate_array_dimensions(
            &[1, 1],
            &[
                ast::Subscript::Expression(ast::Expression::ComponentReference(cref(
                    "stackData.Ns",
                ))),
                ast::Subscript::Expression(ast::Expression::ComponentReference(cref(
                    "stackData.Np",
                ))),
            ],
            &mod_env,
            &components,
            &ast::ClassTree::new(),
        );

        assert_eq!(dims, Some(vec![3, 2]));
    }

    #[test]
    fn try_eval_integer_expr_resolves_enclosing_scope_component_ref() {
        let mut components = IndexMap::new();
        components.insert(
            "pipe2.nFM".to_string(),
            ast::Component {
                name: "pipe2.nFM".to_string(),
                variability: rumoca_ir_ast::Variability::Parameter(token("parameter")),
                start: int_expr(1),
                binding: Some(int_expr(1)),
                has_explicit_binding: true,
                ..ast::Component::default()
            },
        );

        let value = try_eval_integer_expr(
            &add_expr(
                ast::Expression::ComponentReference(cref("pipe2.flowModel.nFM")),
                int_expr(1),
            ),
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
        );

        assert_eq!(value, Some(2));
    }

    #[test]
    fn try_eval_integer_expr_keeps_if_expressions_unresolved() {
        let expr = if_expr(vec![(bool_expr(true), int_expr(2))], int_expr(1));
        let value = try_eval_integer_expr(
            &expr,
            &ast::ModificationEnvironment::new(),
            &IndexMap::new(),
            &ast::ClassTree::new(),
        );

        assert_eq!(value, None);
    }
}
