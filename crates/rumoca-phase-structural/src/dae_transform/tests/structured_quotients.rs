//! Actual whole-DAE structural replay of compact quotient event ownership.
use super::*;

#[test]
fn structured_quotient_reconstruction_preserves_issued_domain_and_boundary() {
    let text = "input Real n; for i in 1:2 loop mod(n+i,7); end for;";
    let mut map = SourceMap::new();
    let source = map.add("StructuredQuotient.mo", text);
    let at = source_provenance(source, text, "mod(n+i,7)");
    let original = dae::Dae::construct(map, |model| {
        let ty = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let n = model.variables(|variables| {
            variables.input(
                VarName::new("n"),
                ty,
                dae::InputVariability::Continuous,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let domain = model.domains(|domains| {
            domains.structured(
                rumoca_core::StructuredIndexDomain {
                    binders: vec![rumoca_core::StructuredIndexBinder {
                        provenance: rumoca_core::IndexBinderProvenance::Generated,
                        id: 0,
                        display_name: "i".into(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                at,
            )
        })?;
        let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
        let operands = model.expressions(|expressions| {
            let n = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(n))?;
            let i = expressions.at(at).binder(binder)?;
            let lhs = expressions.at(at).binary(dae::BinaryOperator::Add, n, i)?;
            let rhs = expressions.at(at).literal(dae::DaeLiteral::Integer(7))?;
            Ok([lhs, rhs])
        })?;
        model.runtime_quotient(dae::PureBuiltin::Mod, operands, at)?;
        Ok(())
    })
    .unwrap();
    let snapshot = |view: dae::DaeView<'_>| {
        assert_eq!(view.runtime_quotient_owner_count(), 1);
        assert_eq!(view.root_count(), 0);
        assert_eq!(view.structured_root_count(), 1);
        let owner = view.runtime_quotient_owner_at(0).unwrap();
        let dae::RuntimeQuotientOwnerKind::StructuredModelEvent {
            domain,
            generated,
            root,
        } = owner.kind()
        else {
            panic!("structured owner survives")
        };
        let boundary = view.structured_root(root).unwrap();
        assert_eq!(boundary.domain(), domain);
        assert_eq!(boundary.expression(), generated[5]);
        assert_eq!(
            view.expression(owner.quotient()).unwrap().binder_domain(),
            Some(domain)
        );
        (
            domain.index(),
            root.index(),
            generated.map(dae::ExprId::index),
            view.expression_count(),
        )
    };
    let expected = original.inspect(snapshot);
    let plan = original.inspect(|view| vec![None; view.expression_count()]);
    let first = reconstruction::rebuild_literal_expressions(&original, &plan).unwrap();
    let second = reconstruction::rebuild_literal_expressions(&first, &plan).unwrap();
    assert_eq!(first.inspect(snapshot), expected);
    assert_eq!(second.inspect(snapshot), expected);
    let wire = serde_json::to_vec(&second).unwrap();
    let decoded: dae::Dae = serde_json::from_slice(&wire).unwrap();
    assert_eq!(decoded.inspect(snapshot), expected);
    assert_eq!(serde_json::to_vec(&decoded).unwrap(), wire);
}
