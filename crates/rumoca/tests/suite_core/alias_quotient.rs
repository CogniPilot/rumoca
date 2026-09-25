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

const REPORTED: &str = "
model AliasReport
  Real x(start = 1, fixed = true);
  Real a;
  Real b;
  Real c(start = 2);
  Real d(start = 3);
equation
  der(x) = -a;
  a = -b;
  b = -x;
  c = d;
  d = x * x;
end AliasReport;";

#[test]
fn structure_inspection_reports_the_quotient_it_analyzes() {
    use rumoca_phase_structural::{AliasClassReport, AliasMemberReport, AliasRefusal};
    let source = compile(REPORTED, "AliasReport");
    let report = rumoca_sim::structural_report_for_dae(&source, &SimOptions::default())
        .expect("the quotiented model is structurally regular");
    let member = |name: &str, negated| AliasMemberReport {
        name: name.to_string(),
        negated,
    };
    assert_eq!(
        report.aliases.classes,
        [
            AliasClassReport::Quotiented {
                representative: "x".to_string(),
                eliminated: vec![member("a", false), member("b", true)],
                retained_states: Vec::new(),
            },
            AliasClassReport::Unchanged {
                members: vec!["c".to_string(), "d".to_string()],
                reason: AliasRefusal::SeveralAnchors,
            },
        ]
    );
    let rendered = report.to_string();
    for expected in [
        "alias quotient (STRUCT-T02): 2 class(es), 1 quotiented, 2 member(s) eliminated",
        "x <- a, -b",
        "unchanged (several members request a state or carry a seed): c, d",
    ] {
        assert!(
            rendered.contains(expected),
            "missing `{expected}`: {rendered}"
        );
    }
    assert_eq!(report.n_equations, 5, "the quotient keeps every equation");
    assert!(
        report
            .matching
            .iter()
            .any(|(equation, unknown)| unknown == "a" && equation.contains("a = -b")),
        "the eliminated member is determined by its own alias edge: {rendered}"
    );
}

const INDEXED: &str = "
model AliasIndexed
  Real x(start = 1, fixed = true);
  Real v[3];
  Real w[3];
equation
  der(x) = -x;
  for i in 1:3 loop
    v[i] = i * x;
  end for;
  for i in 1:3 loop
    w[i] = -v[i];
  end for;
end AliasIndexed;";

#[test]
fn an_indexed_negation_family_quotients_through_its_binders() {
    let source = compile(INDEXED, "AliasIndexed");
    let Some(quotient) = quotient_aliases(&source).unwrap() else {
        panic!("the indexed negation family is eligible");
    };
    let reads = owner_reads(&quotient);
    assert!(
        reads.iter().any(|owner| owner == &["v", "w"]),
        "`w` keeps only its indexed definition against `v`: {reads:?}"
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
        for (name, expected) in [("v[3]", 3.0 * x), ("w[1]", -x), ("w[3]", -3.0 * x)] {
            let actual = column(name)[row];
            assert!(
                (actual - expected).abs() < 1e-4,
                "{name} at {time}: {actual} != {expected}"
            );
        }
    }
}

/// `RateCancellation` executes a reduced state selection. Its source relation
/// `der(q) = rate` prolongs to `$formal_derivative.1.q = rate`, a copy edge with
/// a formal-derivative endpoint, so the second quotient application reads the
/// formal coordinate through `rate` everywhere except its own defining edge.
#[test]
fn a_prolonged_derivative_relation_is_quotiented_after_state_selection() {
    let source = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let lowered =
        rumoca_phase_solve::lower_solve_model(&source, &std::collections::HashMap::new(), |_| {})
            .unwrap();
    assert_eq!(lowered.model().state_scalar_count(), 2);
    let prepared = lowered.prepared_dae();
    let formal = "$formal_derivative.1.q".to_string();
    let readers = owner_reads(prepared)
        .into_iter()
        .filter(|names| names.contains(&formal))
        .collect::<Vec<_>>();
    assert_eq!(
        readers,
        [vec![formal.clone(), "rate".to_string()]],
        "only the defining edge reads the eliminated formal coordinate"
    );
    let roles = |model: &dae::Dae| {
        model.inspect(|view| {
            view.variables()
                .filter(|(_, variable)| variable.origin() == dae::VariableOrigin::Source)
                .map(|(_, variable)| variable.name().to_string())
                .collect::<Vec<_>>()
        })
    };
    assert_eq!(
        roles(&source),
        roles(prepared),
        "every source declaration survives both applications"
    );
}

/// Two seeded copies of the prolonged rate `der(q)` form one formal-scope class
/// with two anchors, which the second application leaves unchanged and names.
#[test]
fn a_formal_scope_refusal_is_reported_by_lowering_and_structure_inspection() {
    let text = include_str!("../fixtures/index_reduction/RateCancellation.mo")
        .replace(
            "Real ax, az;",
            "Real ax, az;\n      Real u[3](each start=0.1), s[3](each start=0.2);",
        )
        .replace(
            "der(q)=rate;",
            "der(q)=rate;\n      u=der(q);\n      s=der(q);",
        );
    let source = compile(&text, "RateCancellation");
    let lowered =
        rumoca_phase_solve::lower_solve_model(&source, &std::collections::HashMap::new(), |_| {})
            .unwrap();
    let refused = lowered
        .formal_alias_report()
        .classes
        .iter()
        .find_map(|class| match class {
            rumoca_phase_structural::AliasClassReport::Unchanged { members, reason } => {
                Some((members.clone(), *reason))
            }
            rumoca_phase_structural::AliasClassReport::Quotiented { .. } => None,
        });
    let Some((members, reason)) = refused else {
        panic!(
            "the formal application reports an unchanged class: {:?}",
            lowered.formal_alias_report()
        );
    };
    assert_eq!(
        reason,
        rumoca_phase_structural::AliasRefusal::SeveralAnchors
    );
    for name in ["u", "s", "$formal_derivative.1.q"] {
        assert!(
            members.iter().any(|member| member == name),
            "{name}: {members:?}"
        );
    }
}

/// Structure inspection of a reduced system names the formal-scope classes the
/// Solve lowering of the same model reports.
#[test]
fn structure_inspection_reports_the_formal_application() {
    let source = compile(
        include_str!("../fixtures/index_reduction/RateCancellation.mo"),
        "RateCancellation",
    );
    let report = rumoca_sim::structural_report_for_dae(&source, &SimOptions::default()).unwrap();
    let lowered =
        rumoca_phase_solve::lower_solve_model(&source, &std::collections::HashMap::new(), |_| {})
            .unwrap();
    assert!(!report.formal_aliases.classes.is_empty());
    assert_eq!(&report.formal_aliases, lowered.formal_alias_report());
    let text = report.to_string();
    assert!(
        text.contains("formal-derivative application of the alias quotient"),
        "{text}"
    );
}
