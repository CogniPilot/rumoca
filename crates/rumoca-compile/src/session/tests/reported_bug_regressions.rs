//! Original compact-domain source reproductions, before backend preparation.

use super::*;

fn compile(source: &str, model: &str) -> CompilationResult {
    let mut session = Session::default();
    session
        .add_document("Repro.mo", source)
        .expect("source parses");
    session
        .compile_model(model)
        .unwrap_or_else(|error| panic!("source compiles: {error:#}"))
}

#[test]
fn issue_426_inactive_neighbor_preserves_the_structured_family() {
    // MLS §3.6.5: only the selected if-expression arm is evaluated.
    let compiled = compile(include_str!("fixtures/reported_bugs/issue_426.mo"), "G1");
    assert_eq!(compiled.balance_detail.equations_unknowns(), (4, 4));
}

#[test]
fn issue_423_iterator_mod_parameter_stays_in_its_domain() {
    // MLS §3.7.2: a time-invariant quotient needs no model event root.
    let compiled = compile(include_str!("fixtures/reported_bugs/issue_423.mo"), "C13");
    // Parameter-only assignments settle m before runtime balance is counted.
    assert_eq!(
        compiled.flat.variables[&rumoca_core::VarName::new("m")].dims,
        vec![3]
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
    compiled
        .dae
        .inspect(|view| assert_eq!(view.root_count(), 0));
}

#[test]
fn issue_424_original_constructor_rejects_incompatible_declared_shape() {
    // MLS §10.4.1.2 makes the last textual iterator outermost: [2, 3].
    // The original issue declares [3, 2]; pinned OMC rejects that source too.
    let mut session = Session::default();
    session
        .add_document(
            "Repro.mo",
            include_str!("fixtures/reported_bugs/issue_424.mo"),
        )
        .expect("original source parses");
    match session.compile_model_phases("C3").expect("phase result") {
        PhaseResult::Failed {
            phase,
            error_code,
            error,
            ..
        } => {
            assert_eq!(phase, FailedPhase::ToDae);
            assert_eq!(error_code.as_deref(), Some("ED020"));
            assert!(error.contains("expression shape mismatch"), "{error}");
        }
        other => panic!("original mismatched constructor must be typed-rejected: {other:?}"),
    }
}

#[test]
fn issue_424_conforming_constructor_has_last_iterator_outermost() {
    let compiled = compile(
        r#"
model C3Conforming
  parameter Real m[2,3] = {i+10*j for i in 1:3, j in 1:2};
  Real y(start=1, fixed=true);
equation
  der(y) = -m[2,3]*y;
end C3Conforming;
"#,
        "C3Conforming",
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
}

#[test]
fn issue_424_reported_reverse_order_constructor_has_conforming_dimensions_and_values() {
    let compiled = compile(
        r#"
model C3ReverseOrder
  parameter Real m[3,2] = {i+10*j for j in 1:2, i in 1:3};
  Real y(start=1, fixed=true);
equation
  der(y) = -m[3,2]*y;
end C3ReverseOrder;
"#,
        "C3ReverseOrder",
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
    let parameter = &compiled.flat.variables[&rumoca_core::VarName::new("m")];
    assert_eq!(parameter.dims, vec![3, 2]);
    let values = compiled.dae.inspect(|view| {
        let (_, declaration) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "m")
            .expect("checked matrix parameter");
        let binding = declaration.binding().expect("checked constructor binding");
        rumoca_eval_dae::NumericEvaluator::new(view)
            .expression(binding)
            .expect("checked matrix constructor evaluates")
    });
    assert_eq!(values, vec![11.0, 21.0, 12.0, 22.0, 13.0, 23.0]);
}

fn assert_checked_constructor_values(
    compiled: &CompilationResult,
    name: &str,
    dimensions: &[i64],
    expected: &[f64],
) {
    let variable = &compiled.flat.variables[&rumoca_core::VarName::new(name)];
    assert_eq!(variable.dims, dimensions);
    assert!(
        matches!(
            variable.binding.as_ref(),
            Some(rumoca_core::Expression::ArrayComprehension { .. })
        ),
        "parameter-evaluable constructor retains its compact owner"
    );
    let values = compiled.dae.inspect(|view| {
        let (_, declaration) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == name)
            .expect("checked constructor declaration");
        let binding = declaration.binding().expect("checked constructor binding");
        rumoca_eval_dae::NumericEvaluator::new(view)
            .expression(binding)
            .expect("checked constructor evaluates")
    });
    assert_eq!(values, expected);
}

#[test]
fn issue_424_parameter_extents_preserve_both_constructor_axis_orders() {
    for (declaration, dimensions, expected) in [
        (
            "parameter Real m[nr,nx]={i+10*j for i in 1:nx,j in 1:nr};",
            [2, 3],
            [11.0, 12.0, 13.0, 21.0, 22.0, 23.0],
        ),
        (
            "parameter Real m[nx,nr]={i+10*j for j in 1:nr,i in 1:nx};",
            [3, 2],
            [11.0, 21.0, 12.0, 22.0, 13.0, 23.0],
        ),
    ] {
        let source = format!(
            "model C3ParameterExtents parameter Integer nx=3; parameter Integer nr=2; \
             {declaration} Real y(start=1,fixed=true); \
             equation der(y)=-m[1,1]*y; end C3ParameterExtents;"
        );
        let compiled = compile(&source, "C3ParameterExtents");
        assert_checked_constructor_values(&compiled, "m", &dimensions, &expected);
    }
}

#[test]
fn issue_424_boolean_parameter_masks_preserve_axis_order_and_exact_values() {
    for (declaration, dimensions, expected) in [
        (
            "parameter Boolean solid[nr,nx]={xc[i]<0.3 and j>2 for i in 1:nx,j in 1:nr};",
            [4, 3],
            [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0],
        ),
        (
            "parameter Boolean solid[nx,nr]={xc[i]<0.3 and j>2 for j in 1:nr,i in 1:nx};",
            [3, 4],
            [0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0],
        ),
    ] {
        let source = format!(
            "model C3BooleanParameters parameter Integer nx=3; parameter Integer nr=4; \
             parameter Real xc[nx]={{0.1,0.4,0.2}}; {declaration} \
             Real y(start=1,fixed=true); \
             equation der(y)=if solid[1,1] then -y else -2*y; end C3BooleanParameters;"
        );
        let compiled = compile(&source, "C3BooleanParameters");
        assert_checked_constructor_values(&compiled, "solid", &dimensions, &expected);
    }
}

#[test]
fn issue_424_reported_boolean_binding_accepts_conforming_parameter_dimensions() {
    let compiled = compile(
        "model C3BooleanBinding parameter Integer nx=3; parameter Integer nr=4; \
         parameter Real xc[nx]={0.1,0.4,0.2}; \
         Boolean solid[nx,nr]={xc[i]<0.3 and j>2 for j in 1:nr,i in 1:nx}; \
         Real y(start=1,fixed=true); \
         equation der(y)=if solid[1,1] then -y else -2*y; end C3BooleanBinding;",
        "C3BooleanBinding",
    );
    let variable = &compiled.flat.variables[&rumoca_core::VarName::new("solid")];
    assert_eq!(variable.dims, vec![3, 4]);
    assert!(matches!(
        variable.binding.as_ref(),
        Some(rumoca_core::Expression::ArrayComprehension { .. })
    ));
}

#[test]
fn issue_424_partially_unknown_declaration_keeps_mismatched_known_axis() {
    let mut session = Session::default();
    session.add_document("Mixed.mo", "model Mixed parameter Real m[3,:]={i+10*j for i in 1:3,j in 1:2}; Real y(start=1,fixed=true); equation der(y)=-m[1,1]*y; end Mixed;").expect("source parses");
    match session.compile_model_phases("Mixed").expect("phase result") {
        PhaseResult::Failed { phase, error, .. } => {
            assert!(matches!(phase, FailedPhase::Typecheck | FailedPhase::ToDae));
            assert!(
                error.contains("shape mismatch") || error.contains("dimension"),
                "{error}"
            );
        }
        other => panic!("known first axis must not be reauthored: {other:?}"),
    }
}

#[test]
fn issue_536_constraining_clause_modifiers_apply_to_the_default_declaration() {
    // MLS §7.3.2: constraining-clause modifiers apply in the declaration and in
    // any redeclaration, below the declaration's own modifiers (§7.2.3 merge).
    let source = include_str!("fixtures/reported_bugs/issue_536.mo");
    for (model, expected) in [
        ("ConstrainedByMod.Holder", &[("spring.k", 3.0)][..]),
        ("ConstrainedByMod.HolderRedeclared", &[("spring.k", 3.0)]),
        ("ConstrainedByMod.ClassForm", &[("spring.k", 3.0)]),
        ("ConstrainedByMod.DeclarationFirst", &[("spring.k", 5.0)]),
        (
            "ConstrainedByMod.NestedMerge",
            &[("h.p.a", 5.0), ("h.p.b", 3.0)],
        ),
        (
            "ConstrainedByMod.NestedClassForm",
            &[("h.p.a", 5.0), ("h.p.b", 3.0)],
        ),
        (
            "ConstrainedByMod.NestedFinalKept",
            &[("h.p.a", 1.0), ("h.p.b", 4.0)],
        ),
        (
            "ConstrainedByMod.NestedFinalClassKept",
            &[("h.p.a", 1.0), ("h.p.b", 4.0)],
        ),
        // A declared whole record supersedes a non-final constrained member,
        // as on main before #536 and in OpenModelica.
        (
            "ConstrainedByMod.RecordKept",
            &[("h.p.a", 5.0), ("h.p.b", 4.0)],
        ),
        (
            "ConstrainedByMod.RecordClassKept",
            &[("h.p.a", 5.0), ("h.p.b", 4.0)],
        ),
    ] {
        for &(name, value) in expected {
            assert_eq!(binding(source, model, name), vec![value], "{model}: {name}");
        }
    }
    // Not #536, recorded so a change shows: rumoca reads the written-out
    // `p(b = 3) = R(5, 4)` with the member modifier winning, where
    // OpenModelica gives b = 4 (i-c888c50d).
    assert_eq!(
        binding(source, "ConstrainedByMod.RecordDirect", "h.p.b"),
        vec![3.0],
        "RecordDirect: h.p.b"
    );
    // Attributes are not part of the value, so they still merge below a
    // declared binding, a final one included.
    for (model, minimum) in [
        ("ConstrainedByMod.AttributeKept", Some(vec![0.0])),
        ("ConstrainedByMod.AttributeClassKept", Some(vec![0.0])),
        ("ConstrainedByMod.AttributeFinalKept", None),
    ] {
        assert_eq!(binding(source, model, "h.k"), vec![5.0], "{model}");
        assert_eq!(
            attributes(source, model, "h.k"),
            (minimum, Some("N/m".to_string())),
            "{model}"
        );
    }
}

#[test]
fn issue_536_a_declared_binding_classifies_constrained_members_by_type() {
    // MLS 7.2.3: a whole binding replaces the value, so it supersedes lower
    // modifications of a record's elements, whatever they are named, and
    // refuses a final one (7.2.6). The element's type decides, not the name:
    // `start` here is a record field, not the Real attribute.
    let source = include_str!("fixtures/reported_bugs/issue_536_collisions.mo");
    for (model, expected) in [
        (
            "AttributeNameCollision.RecordKept",
            &[("h.p.start", 5.0), ("h.p.b", 4.0)][..],
        ),
        // A binding that also states nested modifications is still a binding.
        (
            "UpperNestedAttributes.RecordKept",
            &[("h.p.a", 5.0), ("h.p.b", 4.0)],
        ),
    ] {
        for &(name, value) in expected {
            assert_eq!(binding(source, model, name), vec![value], "{model}: {name}");
        }
    }
    for (model, element) in [
        ("AttributeNameCollision.RecordFinal", "h.p.start"),
        ("UpperNestedAttributes.RecordFinal", "h.p.b"),
    ] {
        let message = refusal(source, model);
        assert!(
            message.contains(&format!("`{element}`")) && message.contains("final"),
            "{model}: {message}"
        );
    }
}

#[test]
fn issue_536_a_class_form_merge_waits_for_the_class_to_be_used() {
    // A class definition resolves no types, so a merge that needs them is
    // made, or refused, where the class is used: a file whose replaceable
    // class is never used still compiles everything else, as on main.
    let source = include_str!("fixtures/reported_bugs/issue_536_scope.mo");
    compile(source, "ScopeWitness.Healthy");
    // Used, the class merges `k(start = 0)` below its `k = 5`, as
    // OpenModelica gives; main dropped the start.
    assert_eq!(binding(source, "ScopeWitness.Unused", "h.k"), vec![5.0]);
    assert_eq!(start(source, "ScopeWitness.Unused", "h.k"), Some(vec![0.0]));
    let collisions = include_str!("fixtures/reported_bugs/issue_536_collisions.mo");
    assert_eq!(
        binding(
            collisions,
            "AttributeNameCollision.RecordClassKept",
            "h.p.start"
        ),
        vec![5.0]
    );
    assert_eq!(
        binding(
            collisions,
            "AttributeNameCollision.RecordClassKept",
            "h.p.b"
        ),
        vec![4.0]
    );
    let message = refusal(collisions, "AttributeNameCollision.RecordClassFinal");
    assert!(
        message.contains("`H.p.start`") && message.contains("final"),
        "{message}"
    );
}

/// The error compiling `model` reports.
fn refusal(source: &str, model: &str) -> String {
    let mut session = Session::default();
    if let Err(error) = session.add_document("Repro.mo", source) {
        return format!("{error:#}");
    }
    let Err(error) = session.compile_model(model) else {
        panic!("{model}: compiles, but must be refused");
    };
    format!("{error:#}")
}

#[test]
fn issue_536_extends_modifier_with_attributes_keeps_its_binding() {
    // MLS §7.2: `k(min = 0, unit = "N/m") = 5` modifies k's attributes and
    // binds it; the extends form applied neither (k = 0.0), unlike a
    // component modifier. OpenModelica gives k(min = 0.0, unit = "N/m") = 5.0.
    let source = include_str!("fixtures/reported_bugs/issue_536.mo");
    for (model, name) in [
        ("ConstrainedByMod.ExtendsAttributes", "k"),
        ("ConstrainedByMod.ClassAttributes", "g.k"),
    ] {
        assert_eq!(binding(source, model, name), vec![5.0], "{model}: {name}");
        assert_eq!(
            attributes(source, model, name),
            (Some(vec![0.0]), Some("N/m".to_string())),
            "{model}: {name}"
        );
    }
}

/// The evaluated `start` of one variable of `model`.
fn start(source: &str, model: &str, name: &str) -> Option<Vec<f64>> {
    compile(source, model).dae.inspect(|view| {
        let (_, declaration) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == name)
            .unwrap_or_else(|| panic!("{model}: {name} is declared"));
        declaration.start().map(|start| {
            rumoca_eval_dae::NumericEvaluator::new(view)
                .expression(start)
                .expect("start evaluates")
        })
    })
}

/// The evaluated `min` and the `unit` of one variable of `model`.
fn attributes(source: &str, model: &str, name: &str) -> (Option<Vec<f64>>, Option<String>) {
    compile(source, model).dae.inspect(|view| {
        let (_, declaration) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == name)
            .unwrap_or_else(|| panic!("{model}: {name} is declared"));
        let minimum = declaration.minimum().map(|minimum| {
            rumoca_eval_dae::NumericEvaluator::new(view)
                .expression(minimum)
                .expect("min evaluates")
        });
        (minimum, declaration.unit().map(ToString::to_string))
    })
}

/// The evaluated binding of one variable of `model`.
fn binding(source: &str, model: &str, name: &str) -> Vec<f64> {
    compile(source, model).dae.inspect(|view| {
        let (_, declaration) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == name)
            .unwrap_or_else(|| panic!("{model}: {name} is declared"));
        let binding = declaration
            .binding()
            .unwrap_or_else(|| panic!("{model}: {name} has a binding"));
        rumoca_eval_dae::NumericEvaluator::new(view)
            .expression(binding)
            .expect("binding evaluates")
    })
}

#[test]
fn issue_536_constraining_clause_final_modifiers_refuse_a_declared_override() {
    // MLS §7.2.6: what the constraining clause makes final, the declaration
    // (component form) or the class it constrains (class form) may not modify.
    let source = include_str!("fixtures/reported_bugs/issue_536_final.mo");
    for (model, element) in [
        ("ConstrainedByFinal.TopFinal", "spring.k"),
        ("ConstrainedByFinal.NestedFinal", "h.p.a"),
        ("ConstrainedByFinal.RecordFinal", "h.p.a"),
    ] {
        let mut session = Session::default();
        session
            .add_document("Repro.mo", source)
            .expect("source parses");
        let Err(error) = session.compile_model(model) else {
            panic!("{model}: the final override is refused");
        };
        let message = format!("{error:#}");
        assert!(
            message.contains(&format!("`{element}`")) && message.contains("final"),
            "{model}: {message}"
        );
    }
    for (declared, constraining, element) in [
        ("p(a = 5)", "final p(b = 3)", "p"),
        ("p(a = 5)", "p(final a = 1, b = 3)", "p.a"),
        // A declared record over a final member, whatever its name.
        ("p = R(5, 4)", "p(final a = 1, b = 3)", "p.a"),
    ] {
        let class_form = format!(
            "package P\n  record R\n    Real a;\n    Real b;\n  end R;\n  \
             partial model Partial\n    parameter R p;\n  end Partial;\n  model Full\n    extends Partial;\n  \
             end Full;\n  model C\n    replaceable model H = Full({declared}) constrainedby Partial({constraining});\n    \
             H h;\n  end C;\nend P;\n"
        );
        let message = refusal(&class_form, "P.C");
        assert!(
            message.contains(&format!("`H.{element}`")) && message.contains("final"),
            "class form {declared} against {constraining}: {message}"
        );
    }
}
