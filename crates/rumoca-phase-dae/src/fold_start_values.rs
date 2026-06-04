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
use rumoca_eval_dae::constant::{ConstValue, eval_const_expr_with};
use rumoca_ir_dae::{Dae, DaeVariableMutVisitor, DaeVariablePartition, DaeVisitor, Variable};
use std::collections::HashMap;

/// Evaluate all parameter/state/constant start expressions to typed literals
/// where possible. Modifies the DAE in place.
///
/// When `preserve_overridable_param_starts` is set, a *parameter* whose `start`
/// expression references one or more other parameters is left symbolic instead
/// of being folded to a literal. This keeps the dependency live so that a
/// runtime override of a base parameter (e.g. `Isp`) propagates to its computed
/// dependents (e.g. `massRatio = exp(dv_hop/(Isp*g0))`) when the interpreter
/// re-evaluates start expressions, without recompiling from source. The
/// parameter chain is already ordered for forward evaluation earlier in the
/// pipeline. Defaults to the historical behaviour (`false`) so codegen backends
/// keep getting literals.
pub(crate) fn fold_start_values_to_literals(
    dae: &mut Dae,
    preserve_overridable_param_starts: bool,
) {
    // Phase 1: build a name→value map from constants, enum ordinals, and
    // parameter start expressions (fixed-point iteration).
    let mut values: HashMap<String, ConstValue> = HashMap::new();

    // Set of all parameter names — used by the override-preservation carve-out
    // to detect parameter→parameter dependencies in a start expression. Only
    // needed when preservation is on, so the default (codegen) path pays
    // nothing.
    let param_names: std::collections::HashSet<String> = if preserve_overridable_param_starts {
        dae.variables
            .parameters
            .keys()
            .map(|k| k.as_str().to_string())
            .collect()
    } else {
        std::collections::HashSet::new()
    };

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
            if let Some(value) = eval_start_const_expr(expr, &values)
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

    // Phase 2: rewrite start expressions to literals where we found values.
    // Also clear self-referencing defaults (start = VarRef(self_name)).
    let rewrite = |var: &mut Variable, is_param: bool| {
        if let Some(ref start) = var.start {
            // Check for self-reference: start = VarRef(own_name)
            if let Expression::VarRef {
                name, subscripts, ..
            } = start
                && subscripts.is_empty()
                && name.as_str() == var.name.as_str()
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
            // Override-preservation carve-out: keep a *computed* parameter start
            // (not just a bare alias) symbolic when it references another
            // parameter, so runtime base-parameter overrides propagate to it.
            if is_param
                && preserve_overridable_param_starts
                && !collect_param_refs(start, &param_names).is_empty()
            {
                return;
            }
            if let Some(val) = values.get(var.name.as_str()).cloned() {
                var.start = Some(Expression::Literal {
                    value: val.into_literal(),
                    span: rumoca_core::Span::DUMMY,
                });
            }
        }
    };

    StartRewriter { rewrite }.visit_variables_mut(&mut dae.variables);
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
) -> Option<ConstValue> {
    eval_const_expr_with(expr, &|name, subscripts| {
        if !subscripts.is_empty() {
            return None;
        }
        env.get(name.as_str()).cloned().or_else(|| {
            rumoca_ir_dae::component_base_name(name.as_str())
                .and_then(|base| env.get(&base).cloned())
        })
    })
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
        // Try to identify which variable this equation defines
        // by checking if it matches the pattern `0 = var - expr` or additive form.
        for alg_name in &alg_names {
            if equation_defines_var(&eq.rhs, alg_name) {
                let deps: Vec<String> = refs
                    .iter()
                    .filter(|r| r.as_str() != alg_name.as_str())
                    .cloned()
                    .collect();
                eq_deps.insert(alg_name.clone(), deps);
            }
        }
    }

    // Sort dae.variables.algebraics
    dae.variables.algebraics = topo_sort_by_eq_deps(&dae.variables.algebraics, &eq_deps)?;
    // Sort dae.variables.outputs
    dae.variables.outputs = topo_sort_by_eq_deps(&dae.variables.outputs, &eq_deps)?;
    Ok(())
}

/// Check if an equation's RHS defines a given variable (appears as LHS of subtraction
/// or as a term in an additive equation).
fn equation_defines_var(rhs: &Expression, var_name: &str) -> bool {
    match rhs {
        Expression::Binary {
            op,
            lhs,
            rhs: rhs_inner,
            ..
        } => {
            if matches!(op, rumoca_core::OpBinary::Sub) {
                // 0 = var - expr or 0 = expr - var
                if is_var_ref_named(lhs, var_name) || is_var_ref_named(rhs_inner, var_name) {
                    return true;
                }
            }
            if matches!(op, rumoca_core::OpBinary::Add) {
                // Check additive terms
                let terms = collect_additive_var_refs(rhs);
                if terms.iter().any(|t| t == var_name) {
                    return true;
                }
            }
            false
        }
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: inner,
            ..
        } => equation_defines_var(inner, var_name),
        Expression::VarRef { name, .. } => name.as_str() == var_name,
        _ => false,
    }
}

/// Extract the base name from a possibly-subscripted variable name.
/// E.g., `"e[1]"` → `"e"`, `"q_err_w"` → `"q_err_w"`.
fn var_base_name(name: &str) -> &str {
    rumoca_core::strip_scalar_name_subscripts(name).unwrap_or(name)
}

fn is_var_ref_named(expr: &Expression, name: &str) -> bool {
    matches!(expr, Expression::VarRef { name: n, .. } if n.as_str() == name || var_base_name(n.as_str()) == name)
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

    let name_list: Vec<String> = map.keys().map(|k| k.as_str().to_string()).collect();
    let name_set: HashSet<&str> = name_list.iter().map(|s| s.as_str()).collect();

    // Build adjacency
    let mut deps_idx: Vec<HashSet<usize>> = Vec::with_capacity(map.len());
    for name in &name_list {
        let dep_names = eq_deps.get(name).ok_or_else(|| {
            let span = map
                .get(&VarName::new(name))
                .map(|var| var.source_span)
                .unwrap_or(Span::DUMMY);
            ToDaeError::RuntimeContractViolation {
                detail: format!("start-value dependency set missing for `{name}`"),
                span: rumoca_core::span_to_source_span(span),
            }
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

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: VarName::new(name).into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn real(value: f64) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn integer(value: i64) -> Expression {
        Expression::Literal {
            value: Literal::Integer(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn string(value: &str) -> Expression {
        Expression::Literal {
            value: Literal::String(value.to_string()),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn parameter(name: &str, start: Expression) -> Variable {
        let mut var = Variable::new(VarName::new(name));
        var.start = Some(start);
        var.is_tunable = false;
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
    fn preserve_overridable_keeps_computed_param_start_symbolic() {
        // `base = 2.0` (literal); `derived.start = base * 3` is a *computed*
        // start that references another parameter.
        let make_dae = || {
            let mut dae = Dae::new();
            dae.variables
                .parameters
                .insert(VarName::new("base"), parameter("base", real(2.0)));
            dae.variables.parameters.insert(
                VarName::new("derived"),
                parameter(
                    "derived",
                    Expression::Binary {
                        op: OpBinary::Mul,
                        lhs: Box::new(var("base")),
                        rhs: Box::new(real(3.0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
            );
            dae
        };

        // preserve = true: the computed, parameter-referencing start stays
        // symbolic so a runtime override of `base` propagates to `derived`.
        let mut dae = make_dae();
        fold_start_values_to_literals(&mut dae, true);
        let derived = &dae.variables.parameters[&VarName::new("derived")];
        assert!(
            matches!(derived.start, Some(Expression::Binary { .. })),
            "expected symbolic start under preservation, got {:?}",
            derived.start
        );

        // preserve = false (default): folds to the literal 6.0.
        let mut dae = make_dae();
        fold_start_values_to_literals(&mut dae, false);
        let derived = &dae.variables.parameters[&VarName::new("derived")];
        assert!(
            matches!(
                derived.start,
                Some(Expression::Literal { value: Literal::Real(v), .. }) if (v - 6.0).abs() <= 1.0e-12
            ),
            "expected folded literal 6.0, got {:?}",
            derived.start
        );
    }

    #[test]
    fn reorder_by_index_order_reports_invalid_topological_index() {
        let mut variables = indexmap::IndexMap::new();
        variables.insert(VarName::new("x"), Variable::new(VarName::new("x")));

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
                    span: rumoca_core::Span::DUMMY,
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
                                span: rumoca_core::Span::DUMMY,
                            },
                            integer(1),
                        )],
                        else_branch: Box::new(Expression::BuiltinCall {
                            function: BuiltinFunction::Sqrt,
                            args: vec![integer(3)],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
        );

        fold_start_values_to_literals(&mut dae, false);

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
}
