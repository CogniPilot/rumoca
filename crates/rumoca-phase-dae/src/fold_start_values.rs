//! Constant-fold parameter/state `start` expressions to typed literals.
//!
//! Many Modelica parameters have `start` values that reference other parameters
//! (e.g., `G_T = G_T_ref` where `G_T_ref = 300.15`). Template backends (Julia MTK,
//! SymPy, etc.) need concrete values. This pass iteratively evaluates start
//! expressions using a fixed-point approach, replacing evaluable expressions
//! with literal values.

use crate::errors::ToDaeError;
use rumoca_core::ExpressionVisitor;
use rumoca_core::{Expression, Span, VarName};
use rumoca_eval_dae::constant::{ConstValue, eval_const_expr_with_shape};
use rumoca_ir_dae::{Dae, DaeVariableMutVisitor, DaeVariablePartition, DaeVisitor, Variable};
use std::collections::HashMap;

/// Evaluate all parameter/state/constant start expressions to typed literals
/// where possible. Modifies the DAE in place.
pub(crate) fn fold_start_values_to_literals(dae: &mut Dae) -> Result<(), ToDaeError> {
    // Phase 1: build a name→value map from constants, enum ordinals, and
    // parameter start expressions (fixed-point iteration).
    let mut values: HashMap<String, ConstValue> = HashMap::new();
    let dims = rumoca_eval_dae::collect_var_dims(dae);

    // Seed with enum literal ordinals
    for (name, ordinal) in &dae.symbols.enum_literal_ordinals {
        values.insert(name.clone(), ConstValue::Real(*ordinal as f64));
    }

    // Collect all named start bindings (constants, parameters, inputs, states,
    // discrete reals, discrete valued, algebraics, outputs)
    let mut bindings = Vec::new();
    StartBindingCollector {
        bindings: &mut bindings,
    }
    .visit_dae(dae);

    // Fixed-point iteration: resolve chains like A = B, B = 3.14
    let max_passes = bindings.len().max(1) * 2;
    for _ in 0..max_passes {
        let mut changed = false;
        for (name, expr) in &bindings {
            if values.contains_key(name.as_str()) {
                continue;
            }
            if let Some(value) = eval_start_const_expr(expr, &values, &dims)
                && value.is_finite()
            {
                values.insert(name.to_string(), value);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    // Set of parameter names: a parameter start expression that references
    // another parameter stays symbolic (see below), matching the bare-alias
    // preservation that already exists for top-level VarRef starts.
    let param_names: std::collections::HashSet<String> = dae
        .variables
        .parameters
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();

    // Parameters whose translation-time values were consumed by a fold.
    // MLS 3.7 §4.5.2: using a parameter's value during translation turns it
    // into an evaluated parameter, and "it must be ensured that the parameter
    // cannot be assigned a different value after translation". Folding a
    // start expression bakes the referenced parameters' values into the
    // literal, so those parameters are pinned non-tunable below.
    let mut consumed_params: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Phase 2: rewrite start expressions to literals where we found values.
    // Also clear self-referencing defaults (start = VarRef(self_name)).
    let mut rewrite_error = Ok(());
    let rewrite = |var: &mut Variable, is_parameter: bool| {
        if rewrite_error.is_err() {
            return;
        }
        if let Some(ref start) = var.start {
            // Check for self-reference: start = VarRef(own_name)
            if let Expression::VarRef {
                name, subscripts, ..
            } = start
                && subscripts.is_empty()
                && is_self_start_reference(name, var)
            {
                var.start = None;
                return;
            }
            // Preserve top-level VarRef starts symbolically.
            // - Enum literal refs (MLS §4.8.5) carry identity: codegen for
            //   FMI 3.0, embedded C, and Julia MTK needs the literal name,
            //   not its ordinal.
            // - Parameter refs preserve the dependency so downstream
            //   overrides (FMI parameter tweaks, structural parameters)
            //   flow through to dependents instead of being locked at
            //   compile time. Topo-sorting (sort_parameters_by_start_deps)
            //   already orders the chain for forward-eval templates.
            if let Expression::VarRef { subscripts, .. } = start
                && subscripts.is_empty()
            {
                return;
            }
            // MLS 3.7 §4.5 makes the translation-vs-initialization split for
            // evaluable parameters tool dependent; keeping the expression
            // (`massRatio = exp(dv/(Isp*g0))`) defers evaluation to
            // initialization so a runtime override of a base parameter
            // (`Isp`) still propagates to its dependents. The parameter
            // chain is already topo-ordered for forward evaluation.
            // String-dependent expressions (MSL TransformerData selects by
            // winding-connection letter) still fold: string evaluation is
            // translation-time only in the solve runtime, and the consumed
            // parameters are pinned non-tunable per §4.5.2.
            if is_parameter
                && start_references_parameter(start, &param_names)
                && !expr_depends_on_string_value(start, &values)
                && !expr_is_shape_only(start)
            {
                return;
            }
            let Some(val) = values.get(var.name.as_str()).cloned() else {
                return;
            };
            if !expr_is_shape_only(start) {
                let mut refs: Vec<VarName> = Vec::new();
                start.collect_var_refs(&mut refs);
                consumed_params.extend(
                    refs.iter()
                        .map(|name| name.as_str().to_string())
                        .filter(|name| param_names.contains(name)),
                );
            }
            let span = match folded_start_span(var, start) {
                Ok(span) => span,
                Err(err) => {
                    rewrite_error = Err(err);
                    return;
                }
            };
            var.start = Some(Expression::Literal {
                value: val.into_literal(),
                span,
            });
        }
    };

    StartRewriter { rewrite }.visit_variables_mut(&mut dae.variables);
    rewrite_error?;

    // Evaluated-parameter lock (MLS 3.7 §4.5.2): parameters whose values
    // were folded into literals above must reject post-translation overrides.
    for (name, var) in dae.variables.parameters.iter_mut() {
        if consumed_params.contains(name.as_str()) {
            var.is_tunable = false;
        }
    }
    Ok(())
}

fn folded_start_span(var: &Variable, start: &Expression) -> Result<Span, ToDaeError> {
    var.start_attribute_span()
        .or_else(|| start.span())
        .filter(|span| !span.is_dummy())
        .ok_or_else(|| {
            ToDaeError::runtime_metadata_violation(format!(
                "folded start literal for `{}` is missing source provenance",
                var.name.as_str()
            ))
        })
}

/// True when the expression contains a String literal or references a name
/// whose folded constant value is a String. Such expressions are evaluable
/// only at translation time (the solve runtime has no string operations), so
/// they must keep folding to literals.
fn expr_depends_on_string_value(expr: &Expression, values: &HashMap<String, ConstValue>) -> bool {
    let mut refs: Vec<VarName> = Vec::new();
    expr.collect_var_refs(&mut refs);
    if refs
        .iter()
        .any(|name| matches!(values.get(name.as_str()), Some(ConstValue::String(_))))
    {
        return true;
    }
    struct StringLiteralFinder {
        found: bool,
    }
    impl ExpressionVisitor for StringLiteralFinder {
        fn visit_expression(&mut self, expr: &Expression) {
            if let Expression::Literal {
                value: rumoca_core::Literal::String(_),
                ..
            } = expr
            {
                self.found = true;
            }
            self.walk_expression(expr);
        }
    }
    let mut finder = StringLiteralFinder { found: false };
    finder.visit_expression(expr);
    finder.found
}

/// True when the start expression references at least one parameter by name.
fn start_references_parameter(
    expr: &Expression,
    param_names: &std::collections::HashSet<String>,
) -> bool {
    let mut refs: Vec<VarName> = Vec::new();
    expr.collect_var_refs(&mut refs);
    refs.iter().any(|name| param_names.contains(name.as_str()))
}

fn expr_is_shape_only(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            ..
        } => args.len() >= 2,
        Expression::FunctionCall { name, args, .. } if name.last_segment() == "size" => {
            args.len() >= 2
        }
        Expression::Unary { rhs, .. } => expr_is_shape_only(rhs),
        Expression::Binary { lhs, rhs, .. } => expr_is_shape_only(lhs) && expr_is_shape_only(rhs),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches
                .iter()
                .all(|(cond, branch)| expr_is_shape_only(cond) && expr_is_shape_only(branch))
                && expr_is_shape_only(else_branch)
        }
        _ => false,
    }
}

fn is_self_start_reference(name: &rumoca_core::Reference, var: &Variable) -> bool {
    match (name.component_ref(), var.component_ref.as_ref()) {
        (Some(name_ref), Some(var_ref)) => component_refs_same_target(name_ref, var_ref),
        (Some(_), None) => false,
        _ => name.as_str() == var.name.as_str(),
    }
}

fn component_refs_same_target(
    lhs: &rumoca_core::ComponentReference,
    rhs: &rumoca_core::ComponentReference,
) -> bool {
    if let (Some(lhs_def), Some(rhs_def)) = (lhs.def_id, rhs.def_id) {
        return lhs_def == rhs_def;
    }
    lhs.parts.len() == rhs.parts.len()
        && lhs
            .parts
            .iter()
            .zip(&rhs.parts)
            .all(|(lhs, rhs)| lhs.ident == rhs.ident && lhs.subs == rhs.subs)
}

struct StartBindingCollector<'a> {
    bindings: &'a mut Vec<(VarName, Expression)>,
}

impl DaeVisitor for StartBindingCollector<'_> {
    fn visit_variable(
        &mut self,
        _partition: DaeVariablePartition,
        name: &VarName,
        variable: &Variable,
    ) {
        // MLS 3.7 §4.5: with `fixed = false` the variable is determined by
        // the initialization problem and `start` is only a guess value, so
        // it must not be treated as the variable's value when evaluating
        // other start expressions.
        if variable.fixed == Some(false) {
            return;
        }
        if let Some(expr) = &variable.start {
            self.bindings.push((name.clone(), expr.clone()));
        }
    }
}

struct StartRewriter<F> {
    rewrite: F,
}

impl<F> DaeVariableMutVisitor for StartRewriter<F>
where
    F: FnMut(&mut Variable, bool),
{
    fn visit_variable_mut(
        &mut self,
        partition: DaeVariablePartition,
        _name: &VarName,
        variable: &mut Variable,
    ) {
        (self.rewrite)(variable, partition == DaeVariablePartition::Parameter);
    }
}

fn eval_start_const_expr(
    expr: &Expression,
    env: &HashMap<String, ConstValue>,
    dims: &indexmap::IndexMap<String, Vec<i64>>,
) -> Option<ConstValue> {
    eval_const_expr_with_shape(
        expr,
        &|name, subscripts| {
            if !subscripts.is_empty() {
                return None;
            }
            for key in reference_lookup_names(name) {
                if let Some(value) = env.get(key.as_str()).cloned().or_else(|| {
                    rumoca_ir_dae::component_base_name(key.as_str())
                        .and_then(|base| env.get(&base).cloned())
                }) {
                    return Some(value);
                }
            }
            None
        },
        &|name| dims.get(name).cloned(),
    )
}

fn reference_lookup_names(name: &rumoca_core::Reference) -> Vec<String> {
    let mut names = Vec::new();
    if let Some(component_ref) = name.component_ref() {
        names.push(component_ref_flat_name(component_ref));
    }
    names.push(name.as_str().to_string());
    names.sort();
    names.dedup();
    names
}

fn component_ref_flat_name(component_ref: &rumoca_core::ComponentReference) -> String {
    component_ref
        .parts
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>()
        .join(".")
}

// ---------------------------------------------------------------------------
// Topological sort of parameters by start-expression dependencies
// ---------------------------------------------------------------------------

/// Collect all parameter/constant names referenced in an expression.
fn collect_param_refs(
    expr: &Expression,
    param_names: &std::collections::HashSet<String>,
) -> Vec<String> {
    let mut collector = ParamRefCollector {
        param_names,
        refs: Vec::new(),
    };
    collector.visit_expression(expr);
    collector.refs
}

struct ParamRefCollector<'a> {
    param_names: &'a std::collections::HashSet<String>,
    refs: Vec<String>,
}

impl ExpressionVisitor for ParamRefCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        self.insert_ref(name.as_str());
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

impl ParamRefCollector<'_> {
    fn insert_ref(&mut self, name: &str) {
        if self.param_names.contains(name) && !self.refs.iter().any(|item| item == name) {
            self.refs.push(name.to_string());
            return;
        }

        // Also match base name for subscripted refs: "e[1]" -> "e".
        let base = var_base_name(name);
        if base != name
            && self.param_names.contains(base)
            && !self.refs.iter().any(|item| item == base)
        {
            self.refs.push(base.to_string());
        }
    }
}

/// Sort algebraic and output variable maps by equation dependency order.
///
/// For each variable in `dae.variables.algebraics` or `dae.variables.outputs`, finds its defining
/// equation in `dae.continuous.equations` and extracts which other algebraic/output variables
/// the equation references. Then topologically sorts so that variables are
/// evaluated after their dependencies.
pub(crate) fn sort_algebraics_by_equation_deps(dae: &mut Dae) -> Result<(), ToDaeError> {
    use std::collections::HashSet;

    // Collect all algebraic + output variable names
    let alg_names: HashSet<String> = dae
        .variables
        .algebraics
        .keys()
        .chain(dae.variables.outputs.keys())
        .map(|k| k.as_str().to_string())
        .collect();

    if alg_names.len() <= 1 {
        return Ok(());
    }

    // For each algebraic/output variable, find which other alg/output vars
    // its equation references
    let mut eq_deps: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();
    for alg_name in &alg_names {
        eq_deps.insert(alg_name.clone(), Vec::new());
    }

    for eq in &dae.continuous.equations {
        let refs = collect_param_refs(&eq.rhs, &alg_names);
        // This equation may define one of our algebraic vars.
        // Identify candidate definitions once per equation instead of scanning
        // every algebraic/output name against the same expression tree.
        for alg_name in collect_defined_var_refs(&eq.rhs, &alg_names) {
            let deps: Vec<String> = refs
                .iter()
                .filter(|r| r.as_str() != alg_name.as_str())
                .cloned()
                .collect();
            eq_deps.insert(alg_name, deps);
        }
    }

    // Sort dae.variables.algebraics
    dae.variables.algebraics = topo_sort_by_eq_deps(&dae.variables.algebraics, &eq_deps)?;
    // Sort dae.variables.outputs
    dae.variables.outputs = topo_sort_by_eq_deps(&dae.variables.outputs, &eq_deps)?;
    Ok(())
}

#[cfg(test)]
fn is_var_ref_named(expr: &Expression, name: &str) -> bool {
    matches!(expr, Expression::VarRef { name: n, .. } if n.as_str() == name || var_base_name(n.as_str()) == name)
}

fn collect_defined_var_refs(
    rhs: &Expression,
    alg_names: &std::collections::HashSet<String>,
) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    collect_additive_var_refs(rhs)
        .into_iter()
        .filter(|name| alg_names.contains(name) && seen.insert(name.clone()))
        .collect()
}

/// Extract the base name from a possibly-subscripted variable name.
/// E.g., `"e[1]"` → `"e"`, `"q_err_w"` → `"q_err_w"`.
fn var_base_name(name: &str) -> &str {
    rumoca_core::strip_scalar_name_subscripts(name).unwrap_or(name)
}

/// Collect all VarRef names from an additive expression tree.
fn collect_additive_var_refs(expr: &Expression) -> Vec<String> {
    match expr {
        Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        }
        | Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            let mut v = collect_additive_var_refs(lhs);
            v.extend(collect_additive_var_refs(rhs));
            v
        }
        Expression::Unary { rhs, .. } => collect_additive_var_refs(rhs),
        Expression::VarRef { name, .. } => vec![var_base_name(name.as_str()).to_string()],
        _ => vec![],
    }
}

fn topo_sort_by_eq_deps(
    map: &indexmap::IndexMap<VarName, Variable>,
    eq_deps: &std::collections::HashMap<String, Vec<String>>,
) -> Result<indexmap::IndexMap<VarName, Variable>, ToDaeError> {
    use std::collections::{HashSet, VecDeque};

    if map.len() <= 1 {
        return Ok(map.clone());
    }

    let entries: Vec<_> = map.iter().collect();
    let name_list: Vec<String> = entries
        .iter()
        .map(|(name, _)| name.as_str().to_string())
        .collect();
    let name_set: HashSet<&str> = name_list.iter().map(|s| s.as_str()).collect();

    // Build adjacency
    let mut deps_idx: Vec<HashSet<usize>> = Vec::with_capacity(map.len());
    for (idx, name) in name_list.iter().enumerate() {
        let dep_names = eq_deps.get(name).ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!("start-value dependency set missing for `{name}`"),
                entries[idx].1.source_span,
            )
        })?;
        let dep_indices: HashSet<usize> = dep_names
            .iter()
            .filter(|d| name_set.contains(d.as_str()))
            .filter_map(|d| name_list.iter().position(|n| n == d))
            .collect();
        deps_idx.push(dep_indices);
    }

    // Kahn's algorithm
    let n = map.len();
    let mut in_degree = vec![0usize; n];
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (i, dep_set) in deps_idx.iter().enumerate() {
        in_degree[i] = dep_set.len();
        for &d in dep_set {
            dependents[d].push(i);
        }
    }

    let mut queue: VecDeque<usize> = VecDeque::new();
    for (i, &deg) in in_degree.iter().enumerate() {
        if deg == 0 {
            queue.push_back(i);
        }
    }

    let mut order: Vec<usize> = Vec::with_capacity(n);
    while let Some(idx) = queue.pop_front() {
        order.push(idx);
        for &dep in &dependents[idx] {
            in_degree[dep] -= 1;
            if in_degree[dep] == 0 {
                queue.push_back(dep);
            }
        }
    }

    // Append cyclic entries in original order
    if order.len() < n {
        for i in 0..n {
            if !order.contains(&i) {
                order.push(i);
            }
        }
    }

    reorder_by_index_order(map, &order)
}

fn reorder_by_index_order(
    map: &indexmap::IndexMap<VarName, Variable>,
    order: &[usize],
) -> Result<indexmap::IndexMap<VarName, Variable>, ToDaeError> {
    let mut sorted = indexmap::IndexMap::with_capacity(map.len());
    for &idx in order {
        let Some((k, v)) = map.get_index(idx) else {
            return Err(ToDaeError::internal(format!(
                "topological order index {idx} out of range for {} variables",
                map.len()
            )));
        };
        sorted.insert(k.clone(), v.clone());
    }
    Ok(sorted)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{BuiltinFunction, Literal, OpBinary};

    fn test_span(start: usize, end: usize) -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fold_start_values_fixture.mo"),
            start,
            end,
        )
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: VarName::new(name).into(),
            subscripts: vec![],
            span: test_span(1, 2),
        }
    }

    fn structured_var_ref(rendered: &str, target: &str, def_id: u32) -> Expression {
        Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                rendered,
                rumoca_core::ComponentReference::from_flat_segments(
                    target,
                    test_span(1, 2),
                    Some(rumoca_core::DefId::new(def_id)),
                ),
            ),
            subscripts: vec![],
            span: test_span(1, 2),
        }
    }

    fn real(value: f64) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span: test_span(3, 4),
        }
    }

    fn integer(value: i64) -> Expression {
        Expression::Literal {
            value: Literal::Integer(value),
            span: test_span(5, 6),
        }
    }

    fn string(value: &str) -> Expression {
        Expression::Literal {
            value: Literal::String(value.to_string()),
            span: test_span(7, 8),
        }
    }

    fn parameter(name: &str, start: Expression) -> Variable {
        let mut var = Variable::new(
            VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        var.start = Some(start);
        var.is_tunable = false;
        var
    }

    fn source_constant(name: &str, def_id: u32, start: Expression) -> Variable {
        let mut var = parameter(name, start);
        var.component_ref = Some(rumoca_core::ComponentReference::from_flat_segments(
            name,
            test_span(1, 2),
            Some(rumoca_core::DefId::new(def_id)),
        ));
        var
    }

    #[test]
    fn var_base_name_strips_only_trailing_subscripts() {
        assert_eq!(var_base_name("e[1]"), "e");
        assert_eq!(var_base_name("e[1,2]"), "e");
        assert_eq!(var_base_name("q_err_w"), "q_err_w");
        assert_eq!(var_base_name("record[1].field"), "record[1].field");
        assert_eq!(var_base_name("record[1].field[2]"), "record[1].field");
        assert_eq!(var_base_name("record[index.re]"), "record[index.re]");
    }

    #[test]
    fn fold_start_values_keeps_structured_alias_with_rewritten_display_name() {
        let mut dae = Dae::new();
        dae.variables.constants.insert(
            VarName::new("sineVoltage.pi"),
            source_constant(
                "sineVoltage.pi",
                39973,
                structured_var_ref("sineVoltage.pi", "Modelica.Constants.pi", 86),
            ),
        );
        dae.variables.constants.insert(
            VarName::new("Modelica.Constants.pi"),
            source_constant("Modelica.Constants.pi", 86, real(std::f64::consts::PI)),
        );

        fold_start_values_to_literals(&mut dae).expect("constant alias starts should be valid");

        let Some(Expression::VarRef { name, .. }) =
            &dae.variables.constants[&VarName::new("sineVoltage.pi")].start
        else {
            panic!("structured alias start must not be treated as a self-reference");
        };
        assert_eq!(name.as_str(), "sineVoltage.pi");
        assert_eq!(
            name.component_ref().map(component_ref_flat_name).as_deref(),
            Some("Modelica.Constants.pi")
        );
    }

    #[test]
    fn var_ref_dependency_match_uses_trailing_scalar_subscript_base() {
        assert!(is_var_ref_named(&var("e[1]"), "e"));
        assert!(is_var_ref_named(
            &var("record[1].field[2]"),
            "record[1].field"
        ));
        assert!(!is_var_ref_named(&var("record[1].field"), "record"));
        assert!(!is_var_ref_named(&var("record[index.re]"), "record"));
    }

    #[test]
    fn reorder_by_index_order_reports_invalid_topological_index() {
        let mut variables = indexmap::IndexMap::new();
        variables.insert(
            VarName::new("x"),
            Variable::new(
                VarName::new("x"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        let err = reorder_by_index_order(&variables, &[1]).expect_err("invalid index");

        assert!(
            matches!(err, ToDaeError::Internal(message) if message.contains("topological order index 1 out of range for 1 variables"))
        );
    }

    #[test]
    fn folds_string_substring_condition_before_runtime_parameter_tail() {
        let mut dae = Dae::new();
        dae.variables.parameters.insert(
            VarName::new("transformer1.VectorGroup"),
            parameter("transformer1.VectorGroup", string("Dy01")),
        );
        dae.variables.parameters.insert(
            VarName::new("transformerData1.C1"),
            parameter(
                "transformerData1.C1",
                Expression::FunctionCall {
                    name: VarName::new("Modelica.Utilities.Strings.substring").into(),
                    args: vec![var("transformer1.VectorGroup"), integer(1), integer(1)],
                    is_constructor: false,
                    span: test_span(9, 10),
                },
            ),
        );
        dae.variables.parameters.insert(
            VarName::new("transformerData1.V1"),
            parameter("transformerData1.V1", real(100.0)),
        );
        dae.variables.parameters.insert(
            VarName::new("transformerData1.V1ph"),
            parameter(
                "transformerData1.V1ph",
                Expression::Binary {
                    op: OpBinary::Div,
                    lhs: Box::new(var("transformerData1.V1")),
                    rhs: Box::new(Expression::If {
                        branches: vec![(
                            Expression::Binary {
                                op: OpBinary::Eq,
                                lhs: Box::new(var("transformerData1.C1")),
                                rhs: Box::new(string("D")),
                                span: test_span(11, 12),
                            },
                            integer(1),
                        )],
                        else_branch: Box::new(Expression::BuiltinCall {
                            function: BuiltinFunction::Sqrt,
                            args: vec![integer(3)],
                            span: test_span(13, 14),
                        }),
                        span: test_span(15, 16),
                    }),
                    span: test_span(17, 18),
                },
            ),
        );

        fold_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("start folding should succeed: {err}"));

        let c1 = &dae.variables.parameters[&VarName::new("transformerData1.C1")];
        assert!(matches!(
            c1.start,
            Some(Expression::Literal {
                value: Literal::String(ref value),
                ..
            }) if value == "D"
        ));

        let v1ph = &dae.variables.parameters[&VarName::new("transformerData1.V1ph")];
        match v1ph.start {
            Some(Expression::Literal {
                value: Literal::Real(value),
                ..
            }) => assert!((value - 100.0).abs() <= 1.0e-12),
            ref other => panic!("expected folded numeric V1ph start, got {other:?}"),
        }
    }

    #[test]
    fn preserves_numeric_parameter_start_depending_on_parameters() {
        let mut dae = Dae::new();
        dae.variables
            .parameters
            .insert(VarName::new("Isp"), parameter("Isp", real(300.0)));
        dae.variables
            .parameters
            .insert(VarName::new("g0"), parameter("g0", real(9.81)));
        dae.variables.parameters.insert(
            VarName::new("ve"),
            parameter(
                "ve",
                Expression::Binary {
                    op: OpBinary::Mul,
                    lhs: Box::new(var("Isp")),
                    rhs: Box::new(var("g0")),
                    span: test_span(19, 20),
                },
            ),
        );

        fold_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("start folding should succeed: {err}"));

        // A runtime override of `Isp` must propagate to `ve`, so the computed
        // start keeps its expression instead of folding to 2943.0.
        let ve = &dae.variables.parameters[&VarName::new("ve")];
        assert!(
            matches!(ve.start, Some(Expression::Binary { .. })),
            "expected preserved expression start for ve, got {:?}",
            ve.start
        );
    }

    #[test]
    fn folded_start_literal_preserves_start_attribute_span() {
        let mut dae = Dae::new();
        let span = test_span(10, 20);
        let mut parameter = parameter(
            "ratio",
            Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(real(2.0)),
                rhs: Box::new(real(3.0)),
                span: test_span(1, 8),
            },
        );
        parameter.start_span = Some(span);
        dae.variables
            .parameters
            .insert(VarName::new("ratio"), parameter);

        fold_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("start folding should succeed: {err}"));

        assert_eq!(
            dae.variables.parameters[&VarName::new("ratio")]
                .start
                .as_ref()
                .and_then(Expression::span),
            Some(span)
        );
    }

    #[test]
    fn folds_parameter_start_size_from_dae_variable_dims() {
        let mut dae = Dae::new();
        let mut table = parameter("table", real(0.0));
        table.dims = vec![3, 2];
        dae.variables
            .parameters
            .insert(VarName::new("table"), table);
        dae.variables.parameters.insert(
            VarName::new("nout"),
            parameter(
                "nout",
                Expression::BuiltinCall {
                    function: BuiltinFunction::Size,
                    args: vec![var("table"), integer(1)],
                    span: test_span(30, 45),
                },
            ),
        );

        fold_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("start folding should succeed: {err}"));

        assert!(matches!(
            dae.variables.parameters[&VarName::new("nout")].start,
            Some(Expression::Literal {
                value: Literal::Real(value),
                ..
            }) if (value - 3.0).abs() <= 1.0e-12
        ));
    }

    #[test]
    fn folded_start_literal_preserves_expression_span_without_attribute_span() {
        let mut dae = Dae::new();
        let span = test_span(30, 40);
        dae.variables.parameters.insert(
            VarName::new("ratio"),
            parameter(
                "ratio",
                Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(real(2.0)),
                    rhs: Box::new(real(3.0)),
                    span,
                },
            ),
        );

        fold_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("start folding should succeed: {err}"));

        assert_eq!(
            dae.variables.parameters[&VarName::new("ratio")]
                .start
                .as_ref()
                .and_then(Expression::span),
            Some(span)
        );
    }

    #[test]
    fn folded_start_literal_rejects_missing_provenance() {
        let mut dae = Dae::new();
        dae.variables.parameters.insert(
            VarName::new("ratio"),
            parameter(
                "ratio",
                Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(Expression::Literal {
                        value: Literal::Real(2.0),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rhs: Box::new(Expression::Literal {
                        value: Literal::Real(3.0),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
        );

        let err = fold_start_values_to_literals(&mut dae)
            .expect_err("unspanned folded start should fail");

        assert!(matches!(err, ToDaeError::RuntimeMetadataViolation { .. }));
    }
}
