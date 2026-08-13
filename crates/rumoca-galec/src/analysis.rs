// analyze.rs, produces facts and accepts/rejects models based on GALEC standard

// temp until the upstream irs are fixed to include a guard field
use super::errors::AdmissibilityError;
use rumoca_core::{BuiltinFunction, Expression, ExpressionVisitor, Reference, Subscript};
use rumoca_ir_dae::{Dae, Equation, VariableOrigin};
use std::collections::{HashMap, HashSet, VecDeque};

pub struct AnalysisResult {
    pub groups: Vec<StatementGroup>,
}

pub fn analyze(dae: &Dae) -> Result<AnalysisResult, Vec<AdmissibilityError>> {
    check_admissibility(dae)?;

    let source_vars: std::collections::HashSet<String> = dae
        .variables
        .discrete_reals
        .values()
        .chain(dae.variables.discrete_valued.values())
        .chain(dae.variables.outputs.values())
        .filter(|v| v.origin == VariableOrigin::Source)
        .map(|v| v.name.to_string())
        .collect();

    let equations: Vec<&Equation> = dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
        .filter(|eq| {
            eq.lhs
                .as_ref()
                .map(|lhs| source_vars.contains(&lhs.to_string()))
                .unwrap_or(false)
        })
        .collect();

    let order = topological_sort_discrete(&equations).map_err(|e| vec![e])?;

    let groups = build_groups(&order, &equations);

    Ok(AnalysisResult { groups })
}

pub struct StatementGroup {
    pub condition: Option<Expression>,
    pub statements: Vec<StatementId>,
}

pub struct StatementId(pub usize);

macro_rules! push_error_if {
    ($errors:expr, $ok:expr, $variant:expr) => {
        if !$ok {
            $errors.push($variant);
        }
    };
}

pub fn check_admissibility(dae: &Dae) -> Result<(), Vec<AdmissibilityError>> {
    let mut errors: Vec<AdmissibilityError> = Vec::new();

    push_error_if!(
        errors,
        dae.variables.states.is_empty(),
        AdmissibilityError::HasContinuousStates
    );
    push_error_if!(
        errors,
        dae.continuous.equations.is_empty(),
        AdmissibilityError::HasContinuousEquations
    );
    push_error_if!(
        errors,
        dae.variables.algebraics.is_empty(),
        AdmissibilityError::HasAlgebraicVariables
    );
    push_error_if!(
        errors,
        dae.events.event_actions.is_empty(),
        AdmissibilityError::HasRuntimeEvents
    );
    push_error_if!(
        errors,
        dae.clocks.triggered_conditions.is_empty(),
        AdmissibilityError::HasDynamicClocks
    );

    match dae.clocks.schedules.len() {
        0 => errors.push(AdmissibilityError::NoClock),
        1 => {}
        n => errors.push(AdmissibilityError::MultiRate { count: n }),
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(())
}

struct ReadCollector {
    reads: HashSet<String>,
}

impl ExpressionVisitor for ReadCollector {
    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function != BuiltinFunction::Pre {
            self.walk_builtin_call(function, args);
        }
    }

    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        self.reads.insert(name.to_string());
        self.walk_var_ref(name, subscripts);
    }
}

fn collect_reads(expr: &Expression) -> HashSet<String> {
    let mut collector = ReadCollector {
        reads: HashSet::new(),
    };
    collector.visit_expression(expr);
    collector.reads
}

pub fn topological_sort_discrete(
    equations: &[&Equation],
) -> Result<Vec<StatementId>, AdmissibilityError> {
    let n = equations.len();

    let mut writer: HashMap<String, usize> = HashMap::new();
    for (i, eq) in equations.iter().enumerate() {
        if let Some(lhs) = &eq.lhs {
            writer.insert(lhs.to_string(), i);
        }
    }

    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
    let mut in_degree: Vec<usize> = vec![0; n];
    let mut seen_edges: HashSet<(usize, usize)> = HashSet::new();

    for (i, eq) in equations.iter().enumerate() {
        for var in collect_reads(&eq.rhs) {
            if let Some(&j) = writer.get(&var)
                && j != i
                && seen_edges.insert((j, i))
            {
                adj[j].push(i);
                in_degree[i] += 1;
            }
        }
    }

    let mut queue: VecDeque<usize> = (0..n).filter(|&i| in_degree[i] == 0).collect();

    let mut order: Vec<StatementId> = Vec::with_capacity(n);

    while let Some(i) = queue.pop_front() {
        order.push(StatementId(i));
        for &j in &adj[i] {
            in_degree[j] -= 1;
            if in_degree[j] == 0 {
                queue.push_back(j);
            }
        }
    }

    if order.len() < n {
        return Err(AdmissibilityError::AlgebraicLoop);
    }

    Ok(order)
}

fn extract_condition(rhs: &Expression) -> Option<Expression> {
    if let Expression::If {
        branches,
        else_branch,
        ..
    } = rhs
        && let Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            ..
        } = else_branch.as_ref()
        && let Some((cond, _)) = branches.first()
    {
        return Some(cond.clone());
    }
    None
}

fn build_groups(order: &[StatementId], equations: &[&Equation]) -> Vec<StatementGroup> {
    let mut groups: Vec<StatementGroup> = Vec::new();

    for id in order {
        let condition = extract_condition(&equations[id.0].rhs);

        let same = groups
            .last()
            .map(|g| g.condition == condition)
            .unwrap_or(false);

        if same {
            groups
                .last_mut()
                .unwrap()
                .statements
                .push(StatementId(id.0));
        } else {
            groups.push(StatementGroup {
                condition,
                statements: vec![StatementId(id.0)],
            })
        }
    }

    groups
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::VarName;
    use rumoca_core::{BuiltinFunction, Expression, Literal, Reference, Span};
    use rumoca_ir_dae::{ClockSchedule, Dae, Equation, Variable};

    fn dummy_span() -> Span {
        Span::DUMMY
    }

    fn make_admissible_dae() -> Dae {
        let mut dae = Dae::new();
        dae.clocks.schedules.push(ClockSchedule {
            period_seconds: 0.001,
            phase_seconds: 0.0,
            source_span: dummy_span(),
        });
        dae
    }

    fn make_var(name: &str) -> Variable {
        let mut v = Variable::empty_with_span(dummy_span());
        v.name = VarName::new(name);
        v
    }

    fn make_literal_eq(lhs: &str, val: f64) -> Equation {
        Equation {
            lhs: Some(Reference::new(lhs)),
            rhs: Expression::Literal {
                value: Literal::Real(val),
                span: dummy_span(),
            },
            span: dummy_span(),
            origin: "test".into(),
            scalar_count: 1,
        }
    }

    fn make_ref_eq(lhs: &str, rhs: &str) -> Equation {
        Equation {
            lhs: Some(Reference::new(lhs)),
            rhs: Expression::VarRef {
                name: Reference::new(rhs),
                subscripts: vec![],
                span: dummy_span(),
            },
            span: dummy_span(),
            origin: "test".into(),
            scalar_count: 1,
        }
    }

    fn make_guarded_eq(lhs: &str, cond_name: &str, true_val: f64) -> Equation {
        let cond = Expression::VarRef {
            name: Reference::new(cond_name),
            subscripts: vec![],
            span: dummy_span(),
        };
        let true_branch = Expression::Literal {
            value: Literal::Real(true_val),
            span: dummy_span(),
        };
        let pre_rhs = Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![Expression::VarRef {
                name: Reference::new(lhs),
                subscripts: vec![],
                span: dummy_span(),
            }],
            span: dummy_span(),
        };
        Equation {
            lhs: Some(Reference::new(lhs)),
            rhs: Expression::If {
                branches: vec![(cond, true_branch)],
                else_branch: Box::new(pre_rhs),
                span: dummy_span(),
            },
            span: dummy_span(),
            origin: "test".into(),
            scalar_count: 1,
        }
    }

    #[test]
    fn admissibility_passes_clean_discrete() {
        let dae = make_admissible_dae();
        assert!(check_admissibility(&dae).is_ok());
    }

    #[test]
    fn admissibility_rejects_continuous_states() {
        let mut dae = make_admissible_dae();
        dae.variables
            .states
            .insert(VarName::new("x"), make_var("x"));
        let errs = check_admissibility(&dae).unwrap_err();
        assert!(
            errs.iter()
                .any(|e| matches!(e, AdmissibilityError::HasContinuousStates))
        );
    }

    #[test]
    fn admissibility_rejects_no_clock() {
        let mut dae = make_admissible_dae();
        dae.clocks.schedules.clear();
        let errs = check_admissibility(&dae).unwrap_err();
        assert!(
            errs.iter()
                .any(|e| matches!(e, AdmissibilityError::NoClock))
        );
    }

    #[test]
    fn admissibility_rejects_multirate() {
        let mut dae = make_admissible_dae();
        dae.clocks.schedules.push(ClockSchedule {
            period_seconds: 0.01,
            phase_seconds: 0.0,
            source_span: dummy_span(),
        });
        let errs = check_admissibility(&dae).unwrap_err();
        assert!(
            errs.iter()
                .any(|e| matches!(e, AdmissibilityError::MultiRate { .. }))
        );
    }

    #[test]
    fn topological_sort_independent_equations() {
        let eq_a = make_literal_eq("a", 1.0);
        let eq_b = make_literal_eq("b", 2.0);
        let eqs: Vec<&Equation> = vec![&eq_a, &eq_b];
        let order = topological_sort_discrete(&eqs).unwrap();
        assert_eq!(order.len(), 2);
    }

    #[test]
    fn topological_sort_dependency_order() {
        // b := a, a := 1.0 → a must come before b
        let eq_a = make_literal_eq("a", 1.0);
        let eq_b = make_ref_eq("b", "a");
        // intentionally give b before a
        let eqs: Vec<&Equation> = vec![&eq_b, &eq_a];
        let order = topological_sort_discrete(&eqs).unwrap();
        // eq_a is index 1, eq_b is index 0; a must appear before b in sorted order
        let pos_a = order.iter().position(|id| id.0 == 1).unwrap();
        let pos_b = order.iter().position(|id| id.0 == 0).unwrap();
        assert!(
            pos_a < pos_b,
            "a (index 1) must be sorted before b (index 0)"
        );
    }

    #[test]
    fn topological_sort_cycle_returns_algebraic_loop() {
        let eq_a = make_ref_eq("a", "b");
        let eq_b = make_ref_eq("b", "a");
        let eqs: Vec<&Equation> = vec![&eq_a, &eq_b];
        assert!(matches!(
            topological_sort_discrete(&eqs),
            Err(AdmissibilityError::AlgebraicLoop)
        ));
    }

    #[test]
    fn build_groups_same_condition_merged() {
        let eq_a = make_guarded_eq("a", "c", 1.0);
        let eq_b = make_guarded_eq("b", "c", 2.0);
        let eqs: Vec<&Equation> = vec![&eq_a, &eq_b];
        let order = vec![StatementId(0), StatementId(1)];
        let groups = build_groups(&order, &eqs);
        assert_eq!(groups.len(), 1, "same condition should produce one group");
        assert_eq!(groups[0].statements.len(), 2);
    }

    #[test]
    fn build_groups_different_conditions_not_merged() {
        let eq_a = make_guarded_eq("a", "c1", 1.0);
        let eq_b = make_guarded_eq("b", "c2", 2.0);
        let eqs: Vec<&Equation> = vec![&eq_a, &eq_b];
        let order = vec![StatementId(0), StatementId(1)];
        let groups = build_groups(&order, &eqs);
        assert_eq!(
            groups.len(),
            2,
            "different conditions should produce two groups"
        );
    }

    #[test]
    fn build_groups_ungrouped_has_no_condition() {
        let eq_a = make_literal_eq("a", 1.0);
        let eqs: Vec<&Equation> = vec![&eq_a];
        let order = vec![StatementId(0)];
        let groups = build_groups(&order, &eqs);
        assert_eq!(groups.len(), 1);
        assert!(groups[0].condition.is_none());
    }
}
