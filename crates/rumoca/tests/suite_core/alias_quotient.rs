//! STRUCT-T02 alias quotient: copies and negations collapse onto one
//! representative while every eliminated declaration stays observable.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::quotient_aliases;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

fn compile(source: &str, name: &str) -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model(name)
        .compile_str(source, "alias_quotient.mo")
        .unwrap()
        .dae
}

/// Declaration names read by each continuous owner, in owner order.
fn owner_reads(model: &dae::Dae) -> Vec<Vec<String>> {
    model.inspect(|view| {
        view.continuous_owners()
            .map(|owner| owner_names(view, owner))
            .collect()
    })
}

fn owner_names<'dae>(
    view: dae::DaeView<'dae>,
    owner: dae::ContinuousOwnerView<'dae>,
) -> Vec<String> {
    let roots = match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => vec![equation.residual()],
        dae::ContinuousOwnerView::Structured { family, .. } => family.bodies().iter().collect(),
    };
    let mut names = Vec::new();
    let mut read = |_, node: dae::ExpressionView<'dae>| {
        if let Some(variable) = node.variable_coordinate() {
            names.push(view.variable(variable).unwrap().name().to_string());
        }
    };
    for root in roots {
        dae::for_each_expression(view, root, &mut read);
    }
    names.sort();
    names.dedup();
    names
}

const CHAIN: &str = "
model AliasChain
  Real x(start = 1, fixed = true);
  Real a;
  Real b;
  Real c;
  Real v[2];
  Real w[2];
equation
  der(x) = -a;
  a = -b;
  b + c = 0;
  c = x;
  v = {x, 2*x};
  w = -v;
end AliasChain;";

#[test]
fn a_sign_chain_quotients_onto_the_state_and_keeps_every_name() {
    let source = compile(CHAIN, "AliasChain");
    let quotient = quotient_aliases(&source)
        .unwrap()
        .expect("the chain and the tensor negation are eligible");
    let names = |model: &dae::Dae| {
        model.inspect(|view| {
            view.variables()
                .map(|(_, variable)| (variable.name().to_string(), variable.role()))
                .collect::<Vec<_>>()
        })
    };
    assert_eq!(names(&source), names(&quotient));
    let reads = owner_reads(&quotient);
    assert!(
        reads.contains(&vec!["a".to_string(), "x".to_string()])
            && !reads
                .iter()
                .any(|owner| owner.contains(&"a".to_string()) && owner.len() > 2),
        "the derivative equation reads the representative, and `a` keeps only its definition: {reads:?}"
    );
    let result = simulate_dae_with_diagnostics(
        &source,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .unwrap();
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        &result.data[index]
    };
    for (row, &time) in result.times.iter().enumerate() {
        let x = (-time).exp();
        for (name, expected) in [
            ("x", x),
            ("a", x),
            ("b", -x),
            ("c", x),
            ("v[2]", 2.0 * x),
            ("w[1]", -x),
            ("w[2]", -2.0 * x),
        ] {
            let actual = column(name)[row];
            assert!(
                (actual - expected).abs() < 1e-4,
                "{name} at {time}: {actual} != {expected}"
            );
        }
    }
}

#[test]
fn guarded_pre_read_and_doubly_seeded_aliases_stay_unquotiented() {
    for (name, text) in [
        (
            "Guarded",
            "model Guarded Real x(start = 1, fixed = true); Real a; Real b;
             equation der(x) = -x; b = x * x;
             if x > 0.5 then a = -b; else a = b; end if; end Guarded;",
        ),
        (
            "PreRead",
            "model PreRead Real x(start = 1, fixed = true); Real a; discrete Real m;
             equation der(x) = -x; a = x; when x < 0.5 then m = pre(a); end when;
             end PreRead;",
        ),
        (
            "Seeded",
            "model Seeded Real x(start = 1, fixed = true); Real a(start = 1); Real b(start = 2);
             equation der(x) = -a; a = b; b = x * x; end Seeded;",
        ),
    ] {
        let source = compile(text, name);
        let reads_before = owner_reads(&source);
        match quotient_aliases(&source).unwrap() {
            None => {}
            Some(quotient) => panic!(
                "{name}: no class may be quotiented, but reads changed from {reads_before:?} to {:?}",
                owner_reads(&quotient)
            ),
        }
    }
}
