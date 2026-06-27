use super::*;

mod algorithm_lowering;
mod conditions;
mod initialization;
mod root;

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span: Span::DUMMY,
    }
}

fn der_ref(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_ref(name)],
        span: Span::DUMMY,
    }
}

#[test]
fn overconstrained_derivative_alias_rewrite_targets_root_state() {
    let mut dae = dae::Dae::new();
    dae.continuous.equations.push(dae::Equation::residual(
        Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(der_ref("branch.port.reference.gamma")),
            rhs: Box::new(var_ref("branch.omega")),
            span: Span::DUMMY,
        },
        Span::DUMMY,
        "branch omega",
    ));
    let alias_roots = FxHashMap::from_iter([(
        VarName::new("branch.port.reference.gamma"),
        VarName::new("root.port.reference.gamma"),
    )]);

    let mut flat = flat::Model::new();
    flat.oc_break_edge_scalar_count = 1;
    for name in ["branch.port.reference.gamma", "root.port.reference.gamma"] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..flat::Variable::empty_with_span(Span::DUMMY)
            },
        );
    }
    rewrite_overconstrained_derivative_alias_refs(&mut dae, &flat, &alias_roots)
        .expect("alias rewrite should succeed");

    assert_eq!(dae.continuous.equations.len(), 2);
    let Expression::Binary { lhs, .. } = &dae.continuous.equations[0].rhs else {
        panic!("expected residual binary expression");
    };
    let Expression::BuiltinCall { args, .. } = lhs.as_ref() else {
        panic!("expected der() call on lhs");
    };
    let Expression::VarRef { name, .. } = &args[0] else {
        panic!("expected der() argument var ref");
    };
    assert_eq!(name.var_name(), &VarName::new("root.port.reference.gamma"));
    assert_eq!(
        dae.continuous.equations[1].origin,
        "overconstrained derivative alias: branch.port.reference.gamma = root.port.reference.gamma"
    );
}
