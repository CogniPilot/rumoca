use super::*;

fn explicit(lhs: &str, rhs: Expression, origin: &str) -> rumoca_ir_dae::Equation {
    let lhs = VarName::new(lhs);
    rumoca_ir_dae::Equation::explicit(
        flat_to_dae_var_name(&lhs),
        flat_to_dae_expression(&rhs),
        Span::DUMMY,
        origin.to_string(),
    )
}

fn reaches_source_alias_chain(
    start: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> bool {
    let mut cur = start.to_string();
    let mut visited = std::collections::HashSet::<String>::new();
    for _ in 0..16 {
        if cur == "src" || !visited.insert(cur.clone()) {
            return cur == "src";
        }
        let next = match aliases.get(&cur) {
            Some(next) => next,
            None => return false,
        };
        cur = next.clone();
    }
    false
}

#[test]
fn canonicalize_discrete_assignments_reroutes_connection_aliases_for_defined_target() {
    let mut dae = Dae::new();
    for name in ["y", "u", "v"] {
        dae.discrete_valued.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }

    dae.f_m.push(explicit(
        "y",
        Expression::Literal(Literal::Integer(1)),
        "explicit equation from source",
    ));
    dae.f_m.push(explicit(
        "y",
        Expression::VarRef {
            name: VarName::new("u"),
            subscripts: vec![],
        },
        "connection equation: y = u",
    ));
    dae.f_m.push(explicit(
        "y",
        Expression::VarRef {
            name: VarName::new("v"),
            subscripts: vec![],
        },
        "connection equation: y = v",
    ));

    canonicalize_discrete_assignment_equations(&mut dae);

    let mut has_source = false;
    let mut has_u_alias = false;
    let mut has_v_alias = false;
    for eq in &dae.f_m {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        match lhs.as_str() {
            "y" => {
                has_source |= matches!(eq.rhs, dae::Expression::Literal(dae::Literal::Integer(1)));
                assert!(
                    !is_connection_equation_origin(&eq.origin),
                    "y should keep only non-connection defining equation"
                );
            }
            "u" => {
                has_u_alias |= matches!(
                    eq.rhs,
                    dae::Expression::VarRef { ref name, ref subscripts }
                        if name.as_str() == "y" && subscripts.is_empty()
                );
            }
            "v" => {
                has_v_alias |= matches!(
                    eq.rhs,
                    dae::Expression::VarRef { ref name, ref subscripts }
                        if name.as_str() == "y" && subscripts.is_empty()
                );
            }
            _ => {}
        }
    }

    assert!(has_source, "expected source definition for y");
    assert!(has_u_alias, "expected rerouted alias u = y");
    assert!(has_v_alias, "expected rerouted alias v = y");
}

#[test]
fn canonicalize_discrete_assignments_resolves_reroute_target_collisions() {
    let mut dae = Dae::new();
    for name in ["y", "u", "v"] {
        dae.discrete_valued.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }

    dae.f_m.push(explicit(
        "y",
        Expression::Literal(Literal::Integer(1)),
        "explicit equation from source",
    ));
    dae.f_m.push(explicit(
        "y",
        Expression::VarRef {
            name: VarName::new("u"),
            subscripts: vec![],
        },
        "connection equation: y = u",
    ));
    dae.f_m.push(explicit(
        "u",
        Expression::VarRef {
            name: VarName::new("v"),
            subscripts: vec![],
        },
        "connection equation: u = v",
    ));

    canonicalize_discrete_assignment_equations(&mut dae);

    let mut lhs_counts: IndexMap<String, usize> = IndexMap::new();
    let mut has_u_from_y = false;
    let mut has_v_from_u = false;
    let mut has_y_source = false;

    for eq in &dae.f_m {
        if let Some(lhs) = eq.lhs.as_ref() {
            *lhs_counts.entry(lhs.as_str().to_string()).or_default() += 1;
        }
        match eq.lhs.as_ref().map(|name| name.as_str()) {
            Some("y") => {
                has_y_source |=
                    matches!(eq.rhs, dae::Expression::Literal(dae::Literal::Integer(1)));
            }
            Some("u") => {
                has_u_from_y |= matches!(
                    eq.rhs,
                    dae::Expression::VarRef { ref name, ref subscripts }
                        if name.as_str() == "y" && subscripts.is_empty()
                );
            }
            Some("v") => {
                has_v_from_u |= matches!(
                    eq.rhs,
                    dae::Expression::VarRef { ref name, ref subscripts }
                        if name.as_str() == "u" && subscripts.is_empty()
                );
            }
            _ => {}
        }
    }

    for (lhs, count) in lhs_counts {
        assert!(
            count <= 1,
            "duplicate discrete assignment target after canonicalize: {lhs} ({count})"
        );
    }
    assert!(has_y_source, "expected source definition for y");
    assert!(has_u_from_y, "expected rerouted alias u = y");
    assert!(has_v_from_u, "expected collision resolution alias v = u");
}

#[test]
fn canonicalize_discrete_assignments_preserves_chain_connectivity_to_source() {
    let mut dae = Dae::new();
    for name in ["src", "a", "b", "c"] {
        dae.discrete_valued.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }

    dae.f_m.push(explicit(
        "src",
        Expression::Literal(Literal::Integer(1)),
        "explicit equation from source",
    ));
    dae.f_m.push(explicit(
        "src",
        Expression::VarRef {
            name: VarName::new("a"),
            subscripts: vec![],
        },
        "connection equation: src = a",
    ));
    dae.f_m.push(explicit(
        "a",
        Expression::VarRef {
            name: VarName::new("b"),
            subscripts: vec![],
        },
        "connection equation: a = b",
    ));
    dae.f_m.push(explicit(
        "b",
        Expression::VarRef {
            name: VarName::new("c"),
            subscripts: vec![],
        },
        "connection equation: b = c",
    ));

    canonicalize_discrete_assignment_equations(&mut dae);

    let mut lhs_counts: IndexMap<String, usize> = IndexMap::new();
    let mut aliases = std::collections::HashMap::<String, String>::new();
    let mut has_source = false;
    for eq in &dae.f_m {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        *lhs_counts.entry(lhs.as_str().to_string()).or_default() += 1;
        if lhs.as_str() == "src" {
            has_source |= matches!(eq.rhs, dae::Expression::Literal(dae::Literal::Integer(1)));
        }
        if let dae::Expression::VarRef { name, subscripts } = &eq.rhs
            && subscripts.is_empty()
        {
            aliases.insert(lhs.as_str().to_string(), name.as_str().to_string());
        }
    }

    for (lhs, count) in lhs_counts {
        assert!(
            count <= 1,
            "duplicate discrete assignment target after canonicalize: {lhs} ({count})"
        );
    }
    assert!(has_source, "expected source definition for src");

    assert!(
        reaches_source_alias_chain("a", &aliases),
        "expected a to remain connected to src, aliases={aliases:?}"
    );
    assert!(
        reaches_source_alias_chain("b", &aliases),
        "expected b to remain connected to src, aliases={aliases:?}"
    );
    assert!(
        reaches_source_alias_chain("c", &aliases),
        "expected c to remain connected to src, aliases={aliases:?}"
    );
}
