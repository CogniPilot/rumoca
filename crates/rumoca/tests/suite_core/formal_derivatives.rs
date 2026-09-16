//! Construction evidence for coupled derivatives before state selection.
use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::{StructuralError, construct_formal_derivatives};

fn compile(source: &str, name: &str) -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model(name)
        .compile_str(source, "formal_derivatives.mo")
        .unwrap()
        .dae
}

#[test]
fn formal_rotation_equations_preserve_tensors_and_initialization() {
    let source = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let before = serde_json::to_vec(source.as_ref()).unwrap();
    let formal = rumoca_phase_structural::construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 2);
        assert_eq!(system.source.function_count(), system.view.function_count());
        let variables: usize = system.view.variables().map(|(_, v)| v.scalar_count()).sum();
        assert_eq!(variables, 131);
        let mut equations = 0;
        for owner in system.view.continuous_owners() {
            match owner {
                dae::ContinuousOwnerView::Residual { .. } => equations += 1,
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    equations += system.view.domain(family.domain()).unwrap().scalar_count()
                        as usize
                        * family.bodies().len();
                }
            }
        }
        assert_eq!(equations, 129);
        assert_eq!(
            system.source.initialization_owners().count(),
            system.view.initialization_owners().count()
        );
        for (id, variable) in system.source.variables() {
            let rebuilt = system
                .view
                .variable(system.coordinate(id, 0).unwrap())
                .unwrap();
            assert_eq!(variable.name(), rebuilt.name());
            assert_eq!(variable.value_type(), rebuilt.value_type());
            assert_eq!(variable.fixed(), rebuilt.fixed());
            for derivative in (1..=2).filter_map(|order| system.coordinate(id, order)) {
                let derivative = system.view.variable(derivative).unwrap();
                assert_eq!(derivative.value_type(), variable.value_type());
                assert_eq!(derivative.fixed(), Some(false));
                assert_eq!(derivative.role(), dae::VariableRole::Algebraic);
                let mut evaluator = rumoca_eval_dae::NumericEvaluator::new(system.view);
                assert_eq!(
                    evaluator.initial_value(derivative.id()).unwrap(),
                    vec![0.0; derivative.scalar_count()]
                );
                assert_eq!(
                    system
                        .view
                        .expression(derivative.start().unwrap())
                        .unwrap()
                        .value_type(),
                    derivative.value_type()
                );
            }
        }
    });
    assert_eq!(serde_json::to_vec(source.as_ref()).unwrap(), before);
}

#[test]
fn pinned_source_values_keep_their_formal_derivative_constraints() {
    let source = compile(
        "model Pinned Real x, v, a; equation der(x)=v; der(v)=a; x=0; end Pinned;",
        "Pinned",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 0);
        let sorted = rumoca_phase_structural::sort(system.view).unwrap();
        assert_eq!(sorted.matching.len(), 6);
        let x = system
            .source
            .variables()
            .find(|(_, v)| v.name().as_str() == "x")
            .unwrap()
            .0;
        let first = system.coordinate(x, 1).unwrap();
        let second = system.coordinate(x, 2).unwrap();
        let constraints = system
            .view
            .continuous_owners()
            .filter_map(|owner| {
                let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                    return None;
                };
                Some(algebraic_reads(system.view, equation.residual()))
            })
            .collect::<Vec<_>>();
        for derivative in [first, second] {
            assert!(
                constraints
                    .iter()
                    .any(|reads| reads.len() == 1 && reads.contains(&(derivative.index(), 0))),
                "the derivative of x=0 must remain an equation on its own formal coordinate"
            );
        }
    });
}

#[test]
fn incompatible_uniform_orders_and_reinit_mapping_are_typed_refusals() {
    for (name, text, expected) in [
        (
            "Mixed",
            "model Mixed Real x[2]; equation {der(x[1]),x[2]}={x[2],0}; end Mixed;",
            "compatible whole-tensor",
        ),
        (
            "Reset",
            "model Reset Real x; equation der(x)=1; when time>1 then reinit(x,0); end when; end Reset;",
            "event reinitialization",
        ),
        (
            "Nonsmooth",
            "model Nonsmooth Real x,y,a; equation der(x)=a; der(y)=1; x=abs(y); end Nonsmooth;",
            "formal derivative operation",
        ),
    ] {
        let source = compile(text, name);
        let before = serde_json::to_vec(source.as_ref()).unwrap();
        let error = construct_formal_derivatives(&source)
            .err()
            .expect("unsupported construction must fail");
        assert!(matches!(
            error,
            StructuralError::ContractViolation { .. }
                | StructuralError::UnspannedContractViolation { .. }
        ));
        assert!(error.to_string().contains(expected), "{name}: {error}");
        assert_eq!(serde_json::to_vec(source.as_ref()).unwrap(), before);
    }
}

#[test]
fn formal_differentiation_retains_compact_equation_domains() {
    let source = compile(
        "model Domain Real x[2],v[2],a[2]; equation for i in 1:2 loop der(x[i])=i*v[i]; der(v[i])=a[i]; x[i]=0; end for; end Domain;",
        "Domain",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 0);
        assert_eq!(
            rumoca_phase_structural::sort(system.view)
                .unwrap()
                .matching
                .len(),
            12
        );
        assert!(system.view.continuous_family_count() > system.source.continuous_family_count());
        assert!(
            system
                .view
                .variables()
                .all(|(_, v)| v.value_type().dimensions() == [2])
        );
    });
}

#[test]
fn formal_construction_preserves_fixed_values_preferences_and_assertions() {
    let source = compile(
        "model Attributes parameter Real a=2; Real x(start=3,fixed=true,stateSelect=StateSelect.always); equation der(x)=-a*x; assert(x > -5,\"bound\"); end Attributes;",
        "Attributes",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 1);
        assert!(system.source.event_action_count() > 0);
        assert_eq!(
            system.source.event_action_count(),
            system.view.event_action_count()
        );
        for (id, original) in system.source.variables() {
            let variable = system
                .view
                .variable(system.coordinate(id, 0).unwrap())
                .unwrap();
            assert_eq!(variable.fixed(), original.fixed());
            assert_eq!(variable.state_select(), original.state_select());
            assert_eq!(variable.start().is_some(), original.start().is_some());
            assert_eq!(variable.causality(), original.causality());
        }
    });
}

#[test]
fn formal_differentiation_uses_checked_function_argument_substitution() {
    let source = compile(
        "function square input Real x; output Real y; algorithm y:=x*x; end square; model Function Real x,y,a; equation der(x)=a; der(y)=1; x=square(y); end Function;",
        "Function",
    );
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 1);
        let error = rumoca_phase_structural::sort(system.view).unwrap_err();
        assert!(matches!(
            error,
            StructuralError::Singular {
                n_equations: 4,
                n_unknowns: 5,
                n_matched: 4,
                ..
            }
        ));
    });
}

fn algebraic_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> std::collections::BTreeSet<(u32, usize)> {
    let mut reads = std::collections::BTreeSet::new();
    let mut traversal = dae::ExpressionTraversal::new();
    traversal.visit_pruned(view, [expression], |_, node| {
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) =
            node.operation()
        {
            reads.insert((id.index(), 0));
        }
        true
    });
    reads
}
