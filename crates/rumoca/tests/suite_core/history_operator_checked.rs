use rumoca::Compiler;
use rumoca_ir_dae::{
    BinaryOperator, CoordinateView, DaeGeneration, DaeProvenanceOrigin, ExpressionOperation,
    UnaryOperator,
};
use rumoca_sim::{SimOptions, simulate_dae};

const HISTORY_MODEL: &str = r#"
model HistoryPulse
  Boolean b(start = false, fixed = true);
  Integer i(start = 0, fixed = true);
  Boolean edgeValue;
  Boolean boolChange;
  Boolean intChange;
  discrete Integer edgeHits(start = 0, fixed = true);
  discrete Integer intHits(start = 0, fixed = true);
equation
  b = time >= 0.25;
  i = if time < 0.5 then 0 else 1;
  edgeValue = edge(b);
  boolChange = change(b);
  intChange = change(i);
  when edge(b) then
    edgeHits = pre(edgeHits) + 1;
  end when;
  when change(i) then
    intHits = pre(intHits) + 1;
  end when;
end HistoryPulse;
"#;

fn assert_source_current<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    expression: rumoca_ir_dae::ExprId<'dae>,
) {
    let current = view.expression(expression).expect("current read resolves");
    assert_eq!(current.provenance().origin(), DaeProvenanceOrigin::Source);
    assert!(matches!(
        current.operation(),
        ExpressionOperation::Coordinate(CoordinateView::DiscreteValue(_))
    ));
}

fn assert_pre_coordinate<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    expression: rumoca_ir_dae::ExprId<'dae>,
) {
    let previous = view
        .expression(expression)
        .expect("pre-value read resolves");
    assert_eq!(
        previous.provenance().origin(),
        DaeProvenanceOrigin::Generated(DaeGeneration::PreValueLowering)
    );
    assert!(matches!(
        previous.operation(),
        ExpressionOperation::Coordinate(CoordinateView::PreDiscreteValue(_))
    ));
}

fn assert_history_provenance(dae: &rumoca_compile::compile::Dae) {
    dae.inspect(|view| {
        let mut counts = [0_usize; 4];
        for index in 0..view.expression_count() {
            let expression = view
                .expression(
                    view.expression_id(index)
                        .expect("expression identity resolves"),
                )
                .expect("expression view resolves");
            if expression.provenance().origin()
                != DaeProvenanceOrigin::Generated(DaeGeneration::PreValueLowering)
            {
                continue;
            }
            match expression.operation() {
                ExpressionOperation::Coordinate(CoordinateView::PreDiscreteValue(_)) => {
                    counts[0] += 1;
                }
                ExpressionOperation::Unary {
                    operator: UnaryOperator::Not,
                    operand,
                } => {
                    counts[1] += 1;
                    assert_pre_coordinate(view, operand);
                }
                ExpressionOperation::Binary {
                    operator: BinaryOperator::And,
                    lhs,
                    rhs,
                } => {
                    counts[2] += 1;
                    assert_source_current(view, lhs);
                    let not = view.expression(rhs).expect("edge negation resolves");
                    assert!(matches!(
                        not.operation(),
                        ExpressionOperation::Unary {
                            operator: UnaryOperator::Not,
                            ..
                        }
                    ));
                }
                ExpressionOperation::Binary {
                    operator: BinaryOperator::NotEqual,
                    lhs,
                    rhs,
                } => {
                    counts[3] += 1;
                    assert_source_current(view, lhs);
                    assert_pre_coordinate(view, rhs);
                }
                _ => panic!("PreValueLowering is reserved for the checked DAE-C06 expansion"),
            }
        }
        assert_eq!(counts, [5, 2, 2, 3]);
        assert!(
            (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .filter_map(|id| view.expression(id))
                .all(|expression| expression.provenance().origin()
                    != DaeProvenanceOrigin::Generated(DaeGeneration::ConditionLowering)
                    || !matches!(
                        view.source_text(expression.provenance()),
                        Some(text) if text.starts_with("edge(") || text.starts_with("change(")
                    ))
        );
    });
}

fn final_value(simulation: &rumoca_sim::SimResult, name: &str) -> f64 {
    let column = simulation
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is visible"));
    simulation.data[column]
        .last()
        .copied()
        .expect("simulation trace is non-empty")
}

#[test]
fn edge_and_change_expand_once_round_trip_and_fire_at_exact_transitions() {
    let compiled = Compiler::new()
        .model("HistoryPulse")
        .compile_str(HISTORY_MODEL, "history_pulse.mo")
        .expect("certified edge/change occurrences construct checked DAE");
    assert_history_provenance(&compiled.dae);
    let wire = serde_json::to_string(&compiled.dae).expect("history DAE serializes");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("history DAE reconstructs through checked wire replay");
    assert_history_provenance(&decoded);
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("history operators lower to executable Solve IR");
    assert_eq!(final_value(&simulation, "edgeHits"), 1.0);
    assert_eq!(final_value(&simulation, "intHits"), 1.0);
}

#[test]
fn whole_array_history_expansion_preserves_the_compact_shape() {
    let compiled = Compiler::new()
        .model("ArrayHistory")
        .compile_str(
            r#"
model ArrayHistory
  Boolean source[2](each start = false, each fixed = true);
  Boolean rising[2];
  Boolean changed[2];
equation
  source = {time >= 0.25, time >= 0.5};
  rising = edge(source);
  changed = change(source);
end ArrayHistory;
"#,
            "array_history.mo",
        )
        .expect("whole-array edge/change keep one checked array expression");
    compiled.dae.inspect(|view| {
        let history = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                expression.provenance().origin()
                    == DaeProvenanceOrigin::Generated(DaeGeneration::PreValueLowering)
            })
            .collect::<Vec<_>>();
        assert_eq!(history.len(), 5);
        assert!(
            history
                .iter()
                .all(|expression| expression.value_type().dimensions() == [2])
        );
        assert_eq!(
            history
                .iter()
                .filter(|expression| matches!(
                    expression.operation(),
                    ExpressionOperation::Binary { .. }
                ))
                .count(),
            2
        );
    });
    let wire = serde_json::to_string(&compiled.dae).expect("array history DAE serializes");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("array history DAE reconstructs with compact shapes");
    decoded.inspect(|view| assert_eq!(view.discrete_value_owner_count(), 3));
}

#[test]
fn invalid_history_operands_fail_closed_without_reaching_builtin_lowering() {
    let cases = [
        (
            "EdgeInteger",
            "model EdgeInteger Integer i; Boolean y; equation i = 0; y = edge(i); end EdgeInteger;",
        ),
        (
            "EdgeExpression",
            "model EdgeExpression Boolean a; Boolean b; Boolean y; equation a=false; b=false; y=edge(a and b); end EdgeExpression;",
        ),
        (
            "ChangeLiteral",
            "model ChangeLiteral Boolean y; equation y=change(1); end ChangeLiteral;",
        ),
        (
            "ChangeArity",
            "model ChangeArity Boolean a; Boolean y; equation a=false; y=change(a,a); end ChangeArity;",
        ),
        (
            "FunctionHistory.M",
            "package FunctionHistory function f input Boolean x; output Boolean y; algorithm y := edge(x); end f; model M Boolean y; equation y=f(false); end M; end FunctionHistory;",
        ),
    ];
    for (model, source) in cases {
        let error = Compiler::new()
            .model(model)
            .compile_str(source, "invalid_history.mo")
            .expect_err("uncertified history operands must remain typed errors");
        let message = error.to_string();
        assert!(
            message.contains("edge") || message.contains("change"),
            "diagnostic stays scoped to the rejected history operator: {message}"
        );
    }
}
