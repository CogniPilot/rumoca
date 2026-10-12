//! MLS §3.6.5 / §10.4: array construction stays inside its selected arm.
use super::*;

fn array_value<'dae>(
    dae: &mut DaeConstruction<'dae>,
    at: DaeProvenance,
    extent: i64,
    range: bool,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    if range {
        return dae.expressions(|e| {
            let start = e.at(at).literal(DaeLiteral::Integer(1))?;
            let stop = e.at(at).literal(DaeLiteral::Integer(extent))?;
            e.at(at).range(start, None, stop)
        });
    }
    let domain = dae.domains(|domains| {
        domains.structured(
            StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    provenance: rumoca_core::IndexBinderProvenance::Generated,
                    id: 0,
                    display_name: "i".into(),
                    lower: 1,
                    upper: extent,
                    step: 1,
                }],
            },
            at,
        )
    })?;
    let binder = dae.domains(|domains| domains.binder(domain, 0, at))?;
    dae.expressions(|e| {
        let body = e.at(at).binder(binder)?;
        e.at(at).comprehension(domain, body)
    })
}

fn conditional_array(extent: i64, range: bool) -> Dae {
    let source = TestSource::new("if c then {i for i in 1:n} else {i for i in 1:n}");
    let at = source.source("if c then {i for i in 1:n} else {i for i in 1:n}", 0);
    Dae::construct(source.map, |dae| {
        let guard = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        let owner = dae.begin_model_conditional(guard, at)?;
        let first =
            dae.model_conditional_arm(&owner, true, |dae| array_value(dae, at, extent, range))?;
        assert!(matches!(
            dae.expressions(|e| e.at(at).unary(UnaryOperator::Plus, first)),
            Err(DaeConstructionError::InvalidModelActivationScope { .. })
        ));
        let second = dae.model_conditional_arm(&owner, false, |dae| {
            assert!(matches!(
                dae.expressions(|e| e.at(at).unary(UnaryOperator::Plus, first)),
                Err(DaeConstructionError::InvalidModelActivationScope { .. })
            ));
            array_value(dae, at, extent, range)
        })?;
        let value = dae.finish_model_conditional(owner, [first, second])?;
        dae.expressions(|e| e.at(at).unary(UnaryOperator::Plus, value))?;
        Ok(())
    })
    .expect("an array constructor discharges its binder, retaining its lexical arm")
}

fn assert_compact_replay(range: bool) {
    let mut counts = Vec::new();
    for extent in [2, 1_000_000] {
        let original = conditional_array(extent, range);
        counts.push(original.inspect(|view| view.expression_count()));
        let json = serde_json::to_vec(&original).unwrap();
        let decoded: Dae = serde_json::from_slice(&json).unwrap();
        assert_eq!(serde_json::to_vec(&decoded).unwrap(), json);
        assert_eq!(decoded.inspect(|view| view.model_conditional_count()), 1);
    }
    assert_eq!(counts[0], counts[1], "extent never expands the array owner");
}

#[test]
fn conditional_comprehension_keeps_its_issued_arm_and_compact_domain() {
    assert_compact_replay(false);
}

#[test]
fn conditional_range_keeps_its_issued_arm_and_compact_domain() {
    assert_compact_replay(true);
}
