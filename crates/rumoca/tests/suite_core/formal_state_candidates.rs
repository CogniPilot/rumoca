//! SPEC_0007 / STRUCT-T07: source-bound state proposals precede numerical admission.
use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::{StructuralError, construct_formal_derivatives};

fn compile(source: &str, model: &str) -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model(model)
        .compile_str(source, "state_candidates.mo")
        .unwrap()
        .dae
}

fn named<'d>(view: dae::DaeView<'d>, name: &str) -> dae::VariableId<'d> {
    view.variables()
        .find(|(_, v)| v.name().as_str() == name)
        .unwrap()
        .0
}

#[test]
fn constrained_rotation_lowering_selects_only_independent_states() {
    let source = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let lowered =
        rumoca_phase_solve::lower_solve_model(&source, &std::collections::HashMap::new(), |_| {})
            .unwrap();
    assert_eq!(lowered.model().state_scalar_count(), 2);
    source.inspect(|view| {
        for (_, variable) in view.variables() {
            for scalar in 0..variable.scalar_count() {
                assert!(
                    lowered
                        .model()
                        .visible_names
                        .contains(&variable.scalar_name(scalar).unwrap())
                );
            }
        }
    });
}

#[test]
fn candidate_start_projects_authored_tensor_guesses_without_fixing_them() {
    for (attribute, expected) in [
        ("start={{1,2},{3,p}}", [7.0, 1.0, 3.0, 2.0]),
        ("each start=p", [7.0; 4]),
    ] {
        let source = compile(
            &format!(
                "model Matrix parameter Real p=7; Real x[2,2]({attribute},each fixed=false); equation der(x)=-x; end Matrix;"
            ),
            "Matrix",
        );
        let formal = construct_formal_derivatives(&source).unwrap();
        let candidate = formal
            .construct_state_candidate(|view| {
                let x = named(view.source, "x");
                [3, 0, 2, 1]
                    .into_iter()
                    .map(|scalar| view.state_coordinate(x, 0, scalar))
                    .collect()
            })
            .unwrap();
        candidate.inspect(|system| {
            let state = system.view.variable(system.state().unwrap()).unwrap();
            assert_eq!(state.fixed(), Some(false));
            let mut evaluator = rumoca_eval_dae::NumericEvaluator::new(system.view);
            assert_eq!(evaluator.initial_value(state.id()).unwrap(), expected);
            assert_eq!(
                system.view.initialization_owner_count(),
                system.formal.view.initialization_owner_count()
            );
        });
    }
}

#[test]
fn required_algebraic_state_has_a_formal_derivative_successor() {
    let source = compile(
        "model Requested Real x(stateSelect=StateSelect.always); Real q(stateSelect=StateSelect.avoid); equation x=q; der(q)=-q; end Requested;",
        "Requested",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal
        .construct_state_candidate(|view| {
            assert_eq!(view.formal_dimension(), 1);
            Ok(vec![view.state_coordinate(
                named(view.source, "x"),
                0,
                0,
            )?])
        })
        .unwrap();
}

#[test]
fn preparing_a_candidate_does_not_repeat_required_state_promotion() {
    let source = compile(
        "model Required Real x(start=1,fixed=true,stateSelect=StateSelect.always); equation der(x)=-x; end Required;",
        "Required",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    let candidate = formal
        .construct_state_candidate(|view| {
            Ok(vec![view.state_coordinate(
                named(view.source, "x"),
                0,
                0,
            )?])
        })
        .unwrap();
    let candidate_variables = candidate.inspect(|system| system.view.variables().count());
    let prepared = candidate.into_prepared().unwrap();
    prepared.inspect(|system| {
        assert_eq!(system.view.variables().count(), candidate_variables);
        assert_eq!(
            system
                .view
                .variables()
                .filter(|(_, variable)| variable.role() == dae::VariableRole::State)
                .map(|(_, variable)| variable.scalar_count())
                .sum::<usize>(),
            1,
        );
        assert_eq!(
            system
                .view
                .variable(named(system.view, "x"))
                .unwrap()
                .role(),
            dae::VariableRole::Algebraic,
        );
        assert!(system.structural.is_some());
        assert!(!system.pins.is_empty());
    });
}

#[test]
fn rotation_candidate_preserves_source_owners_and_binds_formal_successors() {
    let source = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let before = serde_json::to_vec(source.as_ref()).unwrap();
    let formal = construct_formal_derivatives(&source).unwrap();
    for speed_order in [false, true] {
        let candidate = formal
            .construct_state_candidate(|view| {
                let q = named(view.source, "q");
                let (speed, order) = if speed_order {
                    (q, 1)
                } else {
                    (named(view.source, "w"), 0)
                };
                Ok(vec![
                    view.state_coordinate(q, 0, 1)?,
                    view.state_coordinate(speed, order, 1)?,
                ])
            })
            .unwrap();
        candidate.inspect(|system| {
            let state = system.view.variable(system.state().unwrap()).unwrap();
            assert_eq!(state.role(), dae::VariableRole::State);
            assert_eq!(state.value_type().dimensions(), [2]);
            assert_eq!(state.fixed(), Some(false));
            let mut evaluator = rumoca_eval_dae::NumericEvaluator::new(system.view);
            assert_eq!(evaluator.initial_value(state.id()).unwrap(), [0.0; 2]);
            assert_eq!(
                system
                    .view
                    .expression(state.start().unwrap())
                    .unwrap()
                    .value_type(),
                state.value_type()
            );
            assert_eq!(
                system.view.function_count(),
                system.formal.view.function_count()
            );
            assert_eq!(
                system.view.initialization_owner_count(),
                system.formal.view.initialization_owner_count()
            );
            assert_eq!(
                system.view.continuous_owner_count(),
                system.formal.view.continuous_owner_count() + 2
            );
            assert_eq!(
                rumoca_phase_structural::sort(system.view)
                    .unwrap()
                    .matching
                    .len(),
                133
            );
            let q = named(system.formal.source, "q");
            let (value, successor, scalar) = system.projection(0).unwrap();
            assert_eq!(value, system.coordinate(q, 0).unwrap());
            assert_eq!(successor, system.coordinate(q, 1).unwrap());
            assert_eq!(scalar, 1);
            assert!(system.projection(2).is_none());
            for (id, original) in system.formal.source.variables() {
                let preserved = system
                    .view
                    .variable(system.coordinate(id, 0).unwrap())
                    .unwrap();
                assert_eq!(original.value_type(), preserved.value_type());
                assert_eq!(original.fixed(), preserved.fixed());
                assert_eq!(original.state_select(), preserved.state_select());
            }
        });
    }
    assert_eq!(serde_json::to_vec(source.as_ref()).unwrap(), before);
}

#[test]
fn matrix_coordinates_keep_the_tensor_owner_and_row_major_projection() {
    let source = compile(
        "model Matrix Real x[2,2]; equation der(x)=-x; end Matrix;",
        "Matrix",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    let candidate = formal
        .construct_state_candidate(|view| {
            let x = named(view.source, "x");
            (0..4)
                .map(|scalar| view.state_coordinate(x, 0, scalar))
                .collect()
        })
        .unwrap();
    candidate.inspect(|system| {
        let x = named(system.formal.source, "x");
        let value = system.coordinate(x, 0).unwrap();
        let derivative = system.coordinate(x, 1).unwrap();
        for coordinate in [value, derivative] {
            assert_eq!(
                system
                    .view
                    .variable(coordinate)
                    .unwrap()
                    .value_type()
                    .dimensions(),
                [2, 2]
            );
        }
        for slot in 0..4 {
            assert_eq!(
                system.projection(slot),
                Some((value, derivative, slot as u32))
            );
        }
        for (offset, coordinate) in [value, derivative].into_iter().enumerate() {
            let owner = system
                .view
                .continuous_owner(system.formal.view.continuous_owner_count() + offset)
                .unwrap();
            check_matrix_projection(
                system.view,
                owner,
                coordinate,
                system.state().unwrap(),
                offset == 1,
            );
        }
        assert_eq!(
            rumoca_phase_structural::sort(system.view)
                .unwrap()
                .matching
                .len(),
            12
        );
    });
}

fn check_matrix_projection<'d>(
    view: dae::DaeView<'d>,
    owner: dae::ContinuousOwnerView<'d>,
    source: dae::VariableId<'d>,
    state: dae::VariableId<'d>,
    derivative: bool,
) {
    let dae::ContinuousOwnerView::Structured { family, .. } = owner else {
        panic!("aggregate map has a tensor owner")
    };
    let body = family.bodies().get(0).unwrap();
    let dae::ExpressionOperation::Binary { lhs, rhs, .. } =
        view.expression(body).unwrap().operation()
    else {
        panic!("projection equality")
    };
    match view.expression(lhs).unwrap().operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(id)) if !derivative => {
            assert_eq!(dae::VariableId::from(id), state)
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(id)) if derivative => {
            assert_eq!(dae::VariableId::from(id), state)
        }
        _ => panic!("projection must define the selected state or its derivative"),
    }
    let dae::ExpressionOperation::Array(values) = view.expression(rhs).unwrap().operation() else {
        panic!("aggregate coordinate projection")
    };
    for (slot, expression) in values.iter().enumerate() {
        let dae::ExpressionOperation::Index { base, subscripts } =
            view.expression(expression).unwrap().operation()
        else {
            panic!("source tensor projection")
        };
        assert!(
            matches!(view.expression(base).unwrap().operation(), dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) if dae::VariableId::from(id) == source)
        );
        let expected = [slot / 2 + 1, slot % 2 + 1];
        for (subscript, index) in subscripts.iter().zip(expected) {
            let dae::SubscriptView::Index { expression, .. } = subscript else {
                panic!("literal component")
            };
            assert!(
                matches!(view.expression(expression).unwrap().operation(), dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) if *value == index as i64)
            );
        }
    }
}

#[test]
fn candidate_selection_rejects_wrong_dimension_duplicates_and_invalid_successors() {
    let source = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    for (case, expected) in [
        (0, "count"),
        (1, "repeats"),
        (2, "in-bounds"),
        (3, "successor"),
        (4, "overflows"),
    ] {
        let error = formal
            .construct_state_candidate(|view| {
                let q = named(view.source, "q");
                let first = view.state_coordinate(q, 0, 1)?;
                Ok(match case {
                    0 => vec![first],
                    1 => vec![first, first],
                    2 => vec![first, view.state_coordinate(q, 0, 3)?],
                    3 => vec![first, view.state_coordinate(q, 2, 1)?],
                    _ => vec![first, view.state_coordinate(q, usize::MAX, 1)?],
                })
            })
            .err()
            .expect("invalid coordinate map");
        assert!(error.to_string().contains(expected), "{error}");
    }
}

#[test]
fn candidate_retains_required_initial_values_preferences_and_assertions() {
    let source = compile(
        "model Required Real x(start=3,fixed=true,stateSelect=StateSelect.always); equation der(x)=-x; assert(x > -5,\"bound\"); end Required;",
        "Required",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    let candidate = formal
        .construct_state_candidate(|view| {
            Ok(vec![view.state_coordinate(
                named(view.source, "x"),
                0,
                0,
            )?])
        })
        .unwrap();
    candidate.inspect(|system| {
        let x = system
            .view
            .variable(
                system
                    .coordinate(named(system.formal.source, "x"), 0)
                    .unwrap(),
            )
            .unwrap();
        assert_eq!(x.fixed(), Some(true));
        assert!(x.start().is_some());
        assert_eq!(x.state_select(), rumoca_core::StateSelect::Always);
        assert_eq!(
            system.view.event_action_count(),
            system.formal.source.event_action_count()
        );
        assert_eq!(
            system
                .view
                .variable(system.state().unwrap())
                .unwrap()
                .fixed(),
            Some(false)
        );
    });
    let source = compile(
        "model Never Real x(stateSelect=StateSelect.never); equation der(x)=-x; end Never;",
        "Never",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    let error = formal
        .construct_state_candidate(|view| {
            Ok(vec![view.state_coordinate(
                named(view.source, "x"),
                0,
                0,
            )?])
        })
        .err()
        .expect("never cannot become a candidate state");
    assert!(error.to_string().contains("StateSelect.never"));
}

#[test]
fn algebraic_system_needs_no_added_state_or_initial_condition() {
    let source = compile(
        "model Pinned Real x,v,a; equation der(x)=v; der(v)=a; x=0; end Pinned;",
        "Pinned",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    let candidate = formal.construct_state_candidate(|_| Ok(vec![])).unwrap();
    candidate.inspect(|system| {
        assert!(system.state().is_none());
        assert_eq!(
            system.view.variables().count(),
            system.formal.view.variables().count()
        );
        assert_eq!(
            system.view.continuous_owner_count(),
            system.formal.view.continuous_owner_count()
        );
        assert_eq!(
            rumoca_phase_structural::sort(system.view)
                .unwrap()
                .matching
                .len(),
            6
        );
    });
}

#[test]
fn structural_matching_rejects_a_coordinate_set_with_no_velocity_freedom() {
    let source = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    let error = formal
        .construct_state_candidate(|view| {
            let q = named(view.source, "q");
            Ok(vec![
                view.state_coordinate(q, 0, 0)?,
                view.state_coordinate(q, 0, 1)?,
            ])
        })
        .err()
        .expect("angles alone cannot supply both integration freedoms");
    assert!(matches!(error, StructuralError::Singular { .. }), "{error}");
}

#[test]
fn candidate_cannot_omit_required_continuous_states() {
    let source = compile(
        "model RequiredChoice Real x(stateSelect=StateSelect.always),y,a; equation der(x)=a; der(y)=1; x=y*y; end RequiredChoice;",
        "RequiredChoice",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    let error = formal
        .construct_state_candidate(|view| {
            Ok(vec![view.state_coordinate(
                named(view.source, "y"),
                0,
                0,
            )?])
        })
        .err()
        .expect("required x must be retained");
    assert!(error.to_string().contains("StateSelect.always"), "{error}");
}

#[test]
fn static_preferences_do_not_create_integration_coordinates() {
    let source = compile(
        "model Static parameter Real p(stateSelect=StateSelect.always)=2; Real x; equation der(x)=-p*x; end Static;",
        "Static",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal
        .construct_state_candidate(|view| {
            Ok(vec![view.state_coordinate(
                named(view.source, "x"),
                0,
                0,
            )?])
        })
        .unwrap();
    let source = compile("model Empty end Empty;", "Empty");
    assert!(matches!(
        construct_formal_derivatives(&source).err(),
        Some(StructuralError::EmptySystem)
    ));
}

#[test]
fn independent_state_basis_respects_fixed_initial_geometry() {
    // MLS §8.6: q's fixed values constrain the initial geometry even though
    // p and the redundant radius s have different, unfixed start guesses.
    let source = compile(
        r#"model InitialRadius
  Real p[2](start={2,0});
  Real q[2](start={0,1},each fixed=true,each stateSelect=StateSelect.avoid);
  Real v[2](each fixed=true);
  Real s(start=3);
  Real speed;
  Real n[2](start={1,0});
  Real u[2];
  Real lambda;
initial equation
  n[2]=0;
  u[2]=1;
equation
  der(n)=u;
  der(u)=-n+lambda*n;
  n*n=1;
  q=p;
  der(p)=v;
  der(v)=-q;
  s=sqrt(p*p);
  speed=der(s);
end InitialRadius;
"#,
        "InitialRadius",
    );
    let result = rumoca_sim::simulate_dae(
        &source,
        &rumoca_sim::SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .expect("coordinate selection must be regular at the prescribed initial geometry");
    assert!(result.times.len() > 1);
    assert_eq!(result.times.last().copied(), Some(0.1));
    for (name, cosine, sine) in [
        ("p[1]", 0.0, 0.0),
        ("p[2]", 1.0, 0.0),
        ("s", 1.0, 0.0),
        ("speed", 0.0, -1.0),
        ("n[1]", 1.0, 0.0),
        ("n[2]", 0.0, 1.0),
    ] {
        let index = result.names.iter().position(|n| n == name).unwrap();
        for (&actual, &time) in result.data[index].iter().zip(&result.times) {
            let expected = cosine * time.cos() + sine * time.sin();
            assert!(
                (actual - expected).abs() < 5e-6,
                "{name} at {time}: {actual}, expected {expected}"
            );
        }
    }
}
