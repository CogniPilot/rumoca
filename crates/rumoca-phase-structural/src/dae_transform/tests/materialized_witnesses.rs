use super::*;

const CHAIN: usize = 48;

#[test]
fn shared_witnesses_prove_each_definition_once_and_match_one_shot_proofs() {
    let (model, roots) = definition_chain();
    model.inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        let expected = roots
            .iter()
            .map(|&root| facts.materialized_state_anchors(view, root))
            .collect::<Vec<_>>();
        assert!(
            expected
                .iter()
                .all(|anchors| anchors.as_deref() == Some(&[0][..]))
        );
        let before = constraints::witness_proofs();
        let mut anchors = constraints::MaterializedAnchors::new(&facts);
        let shared = roots
            .iter()
            .rev()
            .map(|&root| anchors.state_anchors(view, root))
            .collect::<Vec<_>>();
        let proofs = constraints::witness_proofs() - before;
        assert_eq!(
            shared.into_iter().rev().collect::<Vec<_>>(),
            expected,
            "a shared witness proves exactly what a one-shot walk proves"
        );
        assert_eq!(
            proofs,
            CHAIN - 1,
            "every chained definition is proved once for all roots, not once per root"
        );
    });
}

#[test]
fn shared_witnesses_do_not_depend_on_the_root_order() {
    let (chain, _) = definition_chain();
    for model in [
        chain,
        super::alternative_definitions::alternative_definitions(vec![], true),
    ] {
        model.inspect(assert_root_order_independent);
    }
}

fn assert_root_order_independent(view: dae::DaeView<'_>) {
    let facts = constraints::DifferentiationFacts::collect(view);
    let roots = (0..view.expression_count() as u32).collect::<Vec<_>>();
    let expected = roots
        .iter()
        .map(|&root| facts.materialized_state_anchors(view, root))
        .collect::<Vec<_>>();
    for order in [roots.clone(), roots.iter().rev().copied().collect()] {
        let mut anchors = constraints::MaterializedAnchors::new(&facts);
        let proved = order
            .iter()
            .map(|&root| (root, anchors.state_anchors(view, root)))
            .collect::<Vec<_>>();
        for (root, anchors) in proved {
            assert_eq!(
                anchors, expected[root as usize],
                "a witness does not depend on the root that first reached it"
            );
        }
    }
}

/// `a[0] = x + x` and
/// `a[k] = a[k-1] + a[k-1]`; returns the right-hand side of every definition.
fn definition_chain() -> (dae::Dae, Vec<u32>) {
    let text = "Real x; Real a[n]; equation a[1] = x + x; a[k] = a[k-1] + a[k-1];";
    let mut sources = SourceMap::new();
    let source = sources.add("materialized_witnesses.mo", text);
    let at = source_provenance(source, text, text);
    let mut roots = Vec::new();
    let model = dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, []), at))?;
        let x = model.variables(|v| v.state(VarName::new("x"), real, at, Default::default()))?;
        let links = (0..CHAIN)
            .map(|k| {
                model.variables(|v| {
                    v.algebraic(VarName::new(format!("a{k}")), real, at, Default::default())
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let rows = model.expressions(|e| {
            let mut rows = Vec::new();
            for k in 0..CHAIN {
                let operand = match k {
                    0 => dae::CoordinateInput::State(x),
                    _ => dae::CoordinateInput::Algebraic(links[k - 1]),
                };
                let lhs = e.at(at).coordinate(operand)?;
                let rhs = e.at(at).coordinate(operand)?;
                let value = e.at(at).binary(dae::BinaryOperator::Add, lhs, rhs)?;
                roots.push(value.index());
                let link = e
                    .at(at)
                    .coordinate(dae::CoordinateInput::Algebraic(links[k]))?;
                rows.push(
                    e.at(at)
                        .binary(dae::BinaryOperator::Subtract, link, value)?,
                );
            }
            Ok(rows)
        })?;
        model.continuous(|c| {
            for row in rows {
                c.value_equation(at, row)?;
            }
            Ok(())
        })
    })
    .unwrap();
    (model, roots)
}
