//! STRUCT-T07: staged views partition coupled equations without scalar owners.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_dae as dae;
use rumoca_phase_structural::{FormalDerivativeView, construct_formal_derivatives};

fn compile(source: &str, name: &str) -> std::sync::Arc<dae::Dae> {
    rumoca::Compiler::new()
        .model(name)
        .compile_str(source, "formal_stages.mo")
        .unwrap()
        .dae
}

#[test]
fn rotation_stages_separate_angle_rate_and_highest_derivative_freedoms() {
    let source = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        let counts = system
            .stages()
            .map(|stage| {
                (
                    stage.level(),
                    stage.scalar_equation_count(),
                    stage.scalar_coordinate_count(),
                    stage.formal_dimension(),
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(counts, [(-2, 38, 39, 1), (-1, 44, 45, 1), (0, 47, 47, 0)]);
        let source_states = system
            .stages()
            .flat_map(|stage| {
                stage.coordinates().filter_map(move |coordinate| {
                    let source = system.source.variable(coordinate.source()).unwrap();
                    (coordinate.order() == 0 && source.role() == dae::VariableRole::State)
                        .then(|| (stage.level(), source.name().to_string()))
                })
            })
            .collect::<Vec<_>>();
        assert_eq!(source_states, [(-2, "q".into()), (-1, "w".into())]);
        check_partition_and_dependencies(system);
    });
}

#[test]
fn stages_preserve_loop_domains_and_zero_dimensional_constraints() {
    let source = compile(
        "model Domain Real x[2],v[2],a[2]; equation for i in 1:2 loop der(x[i])=i*v[i]; der(v[i])=a[i]; x[i]=0; end for; end Domain;",
        "Domain",
    );
    let before = serde_json::to_vec(source.as_ref()).unwrap();
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 0);
        assert!(system.stages().all(|stage| stage.formal_dimension() == 0));
        assert_eq!(
            system
                .stages()
                .map(|stage| stage.scalar_equation_count())
                .sum::<usize>(),
            12
        );
        check_partition_and_dependencies(system);
    });
    assert_eq!(serde_json::to_vec(source.as_ref()).unwrap(), before);
}

#[test]
fn unconstrained_tensor_states_have_an_equation_free_value_stage() {
    let source = compile(
        "model Matrix parameter Real p=2; input Real u=1; Real x[2,2]; Real empty[0]; equation der(x)=-p*x+fill(u,2,2); empty=zeros(0); end Matrix;",
        "Matrix",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        let first = system.stages().next().unwrap();
        assert_eq!(first.level(), -1);
        assert_eq!(first.scalar_coordinate_count(), 4);
        assert_eq!(first.scalar_equation_count(), 0);
        assert_eq!(first.coordinates().count(), 1);
        assert_eq!(system.formal_dimension(), 4);
        check_partition_and_dependencies(system);
    });
}

#[test]
fn function_and_time_dependencies_do_not_invent_later_stage_reads() {
    let source = compile(
        "function square input Real u; output Real v; algorithm v:=u*u; end square; model Function parameter Real p=2; Real x,y,a; equation der(x)=a; der(y)=p; x=square(y)+time; end Function;",
        "Function",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 1);
        assert_eq!(system.stages().count(), 2);
        check_partition_and_dependencies(system);
    });
}

#[test]
fn supplied_no_derivative_rate_stays_within_its_stage() {
    // `rotate_der` reads `R.w`, which the body of `rotate` never reads. The
    // twice-prolonged closure keeps that supplied derivative, so construction
    // must certify the rate aliases `R1.w`/`R2.w` as stage -1 coordinates
    // instead of leaving them to the highest stage.
    let source = compile(
        include_str!("../fixtures/index_reduction/NoDerivativeRateLoop.mo"),
        "NoDerivativeRateLoop",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 2);
        check_partition_and_dependencies(system);
    });
}

#[test]
fn algebraic_system_has_one_square_stage() {
    let source = compile(
        "model Algebraic Real x; equation x=1; end Algebraic;",
        "Algebraic",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        let stages = system.stages().collect::<Vec<_>>();
        assert_eq!(stages.len(), 1);
        assert_eq!(stages[0].level(), 0);
        assert_eq!(stages[0].scalar_coordinate_count(), 1);
        assert_eq!(stages[0].scalar_equation_count(), 1);
        check_partition_and_dependencies(system);
    });
}

fn owner_key(owner: dae::ContinuousOwnerView<'_>) -> (bool, u32) {
    match owner {
        dae::ContinuousOwnerView::Residual { id, .. } => (false, id.index()),
        dae::ContinuousOwnerView::Structured { id, .. } => (true, id.index()),
    }
}

fn check_partition_and_dependencies(system: FormalDerivativeView<'_, '_, '_>) {
    let mut levels = BTreeMap::new();
    let mut owners = BTreeSet::new();
    let mut freedoms = 0;
    for stage in system.stages() {
        freedoms += stage.formal_dimension();
        for coordinate in stage.coordinates() {
            assert!(
                levels
                    .insert(coordinate.value().index(), stage.level())
                    .is_none()
            );
            assert_eq!(
                system.coordinate(coordinate.source(), coordinate.order()),
                Some(coordinate.value())
            );
            let original = system.source.variable(coordinate.source()).unwrap();
            let value = system.view.variable(coordinate.value()).unwrap();
            assert_eq!(original.value_type(), value.value_type());
        }
        for equation in stage.equations() {
            assert!(owners.insert(owner_key(equation.value())));
            check_owner_projection(system, equation);
        }
    }
    assert_eq!(freedoms, system.formal_dimension());
    let expected_variables = system
        .view
        .variables()
        .filter_map(|(id, variable)| {
            (variable.role() == dae::VariableRole::Algebraic).then_some(id.index())
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(
        levels.keys().copied().collect::<BTreeSet<_>>(),
        expected_variables
    );
    assert_eq!(
        owners,
        system.view.continuous_owners().map(owner_key).collect()
    );
    for stage in system.stages() {
        for equation in stage.equations() {
            check_owner_dependencies(system.view, equation.value(), stage.level(), &levels);
        }
    }
}

fn check_owner_projection<'source, 'formal>(
    system: FormalDerivativeView<'_, 'source, 'formal>,
    equation: rumoca_phase_structural::FormalStageEquation<'source, 'formal>,
) {
    match (equation.source(), equation.value()) {
        (
            dae::ContinuousOwnerView::Residual {
                equation: source, ..
            },
            dae::ContinuousOwnerView::Residual {
                equation: value, ..
            },
        ) => {
            assert_eq!(source.provenance().span(), value.provenance().span());
            assert_eq!(
                system
                    .source
                    .expression(source.residual())
                    .unwrap()
                    .value_type(),
                system
                    .view
                    .expression(value.residual())
                    .unwrap()
                    .value_type()
            );
        }
        (
            dae::ContinuousOwnerView::Structured { family: source, .. },
            dae::ContinuousOwnerView::Structured { family: value, .. },
        ) => {
            assert_eq!(source.provenance().span(), value.provenance().span());
            assert_eq!(source.scalar_view(), value.scalar_view());
            assert_eq!(source.scalar_rows(), value.scalar_rows());
            assert_eq!(source.bodies().len(), value.bodies().len());
            assert_eq!(
                system.source.domain(source.domain()).unwrap().structured(),
                system.view.domain(value.domain()).unwrap().structured()
            );
        }
        _ => panic!("formal derivatives retain their source equation owner kind"),
    }
}

fn check_owner_dependencies<'d>(
    view: dae::DaeView<'d>,
    owner: dae::ContinuousOwnerView<'d>,
    level: i64,
    levels: &BTreeMap<u32, i64>,
) {
    let family = match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => {
            check_reads(view, equation.residual(), 0, None, level, levels);
            return;
        }
        dae::ContinuousOwnerView::Structured { family, .. } => family,
    };
    let domain = view.domain(family.domain()).unwrap();
    for point in 0..domain.scalar_count() as usize {
        let tuple = domain.structured().index_tuple_at(point).unwrap().unwrap();
        let scalar = family
            .scalar_view()
            .body_scalar(point, domain.extents())
            .unwrap();
        for body in family.bodies().iter() {
            check_reads(
                view,
                body,
                scalar,
                Some((family.domain(), &tuple)),
                level,
                levels,
            );
        }
    }
}

fn check_reads<'d>(
    view: dae::DaeView<'d>,
    expression: dae::ExprId<'d>,
    scalar: usize,
    point: Option<(dae::DomainId<'d>, &[i64])>,
    level: i64,
    levels: &BTreeMap<u32, i64>,
) {
    rumoca_eval_dae::for_each_scalar_coordinate(
        view,
        expression,
        scalar,
        point,
        |coordinate, _| {
            if let dae::CoordinateView::Algebraic(id) = coordinate {
                assert!(levels[&id.index()] <= level, "later-stage dependency");
            }
        },
    )
    .unwrap();
}
