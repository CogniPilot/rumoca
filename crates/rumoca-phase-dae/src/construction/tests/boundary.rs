use rumoca_core::Reference;

use super::super::*;
use super::support::*;

#[test]
fn production_lowering_enters_only_through_construct() {
    let source = TestSource::new("model M Real x; equation 0 = x - 1.0; end M;");
    let model = scalar_real_model(&source);
    let dae = construct(&model, source.map).unwrap();

    dae.inspect(|view| {
        assert_eq!(view.variable_count(), 1);
        assert_eq!(view.continuous_equation_count(), 1);
        let variable = view.variable(view.variable_id(0).unwrap()).unwrap();
        assert_eq!(variable.role(), dae::VariableRole::Algebraic);
        let equation = view.continuous_equation(0).unwrap();
        assert_eq!(equation.provenance().span(), model.equations[0].span);
        assert_eq!(
            view.expression(equation.residual())
                .unwrap()
                .provenance()
                .origin(),
            dae::DaeProvenanceOrigin::Source
        );
    });
}

#[test]
fn missing_predefined_type_identity_fails_before_dae_construction() {
    let source = TestSource::new("model M end M;");
    let model = flat::Model::new();
    let error = construct(&model, source.map)
        .expect_err("Flat without exact predefined type identities is not constructible");
    assert!(matches!(
        error,
        ToDaeError::MissingSemanticIdentity { identity }
            if identity == "predefined scalar canonical TypeIds"
    ));
}

#[test]
fn undefined_references_fail_before_construction() {
    let source = TestSource::new("model M Real x; equation 0 = x - 1.0; // missing\nend M;");
    let mut model = scalar_real_model(&source);
    let span = source.span("missing", 0);
    model.equations[0].residual = Expression::VarRef {
        name: Reference::new("missing"),
        subscripts: Vec::new(),
        span,
    };

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::UnresolvedReference { name, span: found })
            if name == "missing" && found == span
    ));
}

#[test]
fn missing_expression_provenance_is_not_defaulted() {
    let source = TestSource::new("model M Real x; equation 0 = x - 1.0; end M;");
    let mut model = scalar_real_model(&source);
    model.equations[0].residual = Expression::VarRef {
        name: Reference::new("x"),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::MissingProvenance { .. })
    ));
}

#[test]
fn binding_lowering_does_not_fallback_to_declaration_provenance() {
    let source = TestSource::new("model M Real x = 1.0; end M;");
    let mut model = test_model();
    add_primitive_variable(&mut model, &source, "x", "Real x", 7, Vec::new(), false);
    model.variables.get_mut(&VarName::new("x")).unwrap().binding = Some(Expression::Literal {
        value: Literal::Real(1.0),
        span: source.span("1.0", 0),
    });
    model.is_partial = true;

    let analysis = analyze(&model).expect("valid binding must be accepted during analysis");
    let variable_plan =
        plan_variable_construction(&model, &analysis).expect("valid attributes must be planned");
    let Some(Expression::Literal { span, .. }) = model
        .variables
        .get_mut(&VarName::new("x"))
        .and_then(|variable| variable.binding.as_mut())
    else {
        panic!("fixture must retain its scalar binding");
    };
    *span = Span::DUMMY;

    let error = dae::Dae::construct(source.map, |construction| {
        build_checked(&model, &analysis, &variable_plan, construction)
    })
    .expect_err("lowering must recheck exact binding provenance");
    assert!(matches!(
        error,
        dae::DaeConstructionError::MissingProvenance {
            origin: dae::DaeProvenanceOrigin::Source,
            attempted_span: None,
        }
    ));
}
