use super::*;

fn seed_overlay_catalogs(overlay: &mut InstanceOverlay) {
    let sentinel_type = TypeId::new(u32::MAX - 1);
    let sentinel_declaration = DefId::new(u32::MAX - 1);
    overlay.type_roots.insert(sentinel_type, sentinel_type);
    overlay.enumeration_type_roots.insert(sentinel_type);
    overlay
        .type_ids_by_def_id
        .insert(sentinel_declaration, sentinel_type);
}

#[test]
fn missing_used_function_alias_base_fails_in_resolve() {
    let source = r#"
        type Broken = MissingLibrary.Value;

        function consumeBroken
            input Broken value;
            output Real result;
        algorithm
            result := 1.0;
        end consumeBroken;

        model Test
            Real result;
        equation
            result = consumeBroken(1.0);
        end Test;
    "#;
    let diagnostics = resolve(parse(source))
        .expect_err("a missing alias base must fail before an instanced overlay can exist");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER003")
                && diagnostic.message.contains("MissingLibrary.Value")),
        "Resolve owns the exact missing alias-base refusal: {diagnostics:?}",
    );
}

#[test]
fn missing_exact_used_function_signature_refuses_atomic_catalog_install() {
    let source = r#"
        package P
            function select
                input Real value;
                output Real result;
            algorithm
                result := value;
            end select;
        end P;
        package Q
            function select
                input Real value;
                output Real result;
            algorithm
                result := value;
            end select;
        end Q;
        model Test
            Real result;
        equation
            result = P.select(1.0);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("qualified calls resolve exact callable targets");
    let mut instanced = {
        let tree = resolved.into_inner();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let selected = instanced
        .tree
        .get_def_id_by_name("P.select")
        .expect("selected function declaration exists");
    let collision = instanced
        .tree
        .get_def_id_by_name("Q.select")
        .expect("same-spelling collision exists");
    let mut checker = TypeChecker::new();
    let type_table = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the nominal type graph is complete");
    checker.function_signatures.remove(&collision);
    checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect("an unused same-spelling function is not the selected callable");

    checker.function_signatures.remove(&selected);
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("the selected callable cannot lose its checked signature");
    assert!(matches!(
        error.as_ref(),
        TypeCheckError::PhaseDiagnostic { code, message, .. }
            if code == "ET012" && message.contains(&format!("{selected:?}"))
    ));
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn absent_required_record_declaration_refuses_atomic_catalog_install() {
    let source = r#"
        record Payload
            Real value;
        end Payload;
        function consume
            input Payload value;
            output Real result;
        algorithm
            result := value.value;
        end consume;
        model Test
            Real result;
        equation
            result = consume(Payload(1.0));
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("record call resolves");
    let mut instanced = {
        let tree = resolved.into_inner();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let record = instanced
        .tree
        .get_def_id_by_name("Payload")
        .expect("record declaration exists");
    let mut checker = TypeChecker::new();
    let type_table = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the intact record graph produces a nominal table");
    instanced.tree.definitions.classes.shift_remove("Payload");
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a required record class cannot disappear from the checked graph");
    assert!(
        matches!(
            error.as_ref(),
            TypeCheckError::PhaseDiagnostic {
                code,
                message,
                label,
                ..
            } if code == "ET000"
                && message.contains("alias edge")
                && message.contains("no exact target")
                && label.contains("incomplete type identity")
        ),
        "unexpected refusal after removing {record:?}: {error:?}",
    );
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn missing_resolved_callable_target_refuses_atomic_catalog_install() {
    let source = r#"
        function selected
            input Real value;
            output Real result;
        algorithm
            result := value;
        end selected;
        model Test
            Real result;
        equation
            result = selected(1.0);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("call resolves before the identity mutation");
    let mut instanced = {
        let tree = resolved.into_inner();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let call = instanced
        .overlay
        .classes
        .values_mut()
        .flat_map(|class| &mut class.equations)
        .find_map(|equation| match &mut equation.equation {
            rumoca_ir_ast::Equation::Simple {
                rhs: Expression::FunctionCall { comp, .. },
                ..
            } => Some(comp),
            _ => None,
        })
        .expect("mutation seed has one direct value call");
    call.set_target_def_id(None);

    let mut checker = TypeChecker::new();
    let type_table = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the nominal type graph is complete");
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a call without exact callable identity cannot issue Flat type roots");
    assert!(matches!(
        error.as_ref(),
        TypeCheckError::PhaseDiagnostic { code, message, .. }
            if code == "ET012" && message.contains("no exact resolved callable")
    ));
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn transitive_function_dimension_call_requires_checked_signature_atomically() {
    let source = r#"
        function dimension
            output Integer value;
        algorithm
            value := 1;
        end dimension;
        function consume
            input Real values[dimension()];
            output Real result;
        algorithm
            result := values[1];
        end consume;
        model Test
            Real result;
        equation
            result = consume({1.0});
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("function ABI dimension call resolves");
    let mut instanced = {
        let tree = resolved.into_inner();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let dimension = instanced
        .tree
        .get_def_id_by_name("dimension")
        .expect("dimension function declaration exists");
    let mut checker = TypeChecker::new();
    let type_table = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the nominal type graph is complete");
    checker.function_signatures.remove(&dimension);
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a transitive call in function ABI shape cannot lose its signature");
    assert!(matches!(
        error.as_ref(),
        TypeCheckError::PhaseDiagnostic { code, message, .. }
            if code == "ET012" && message.contains(&format!("{dimension:?}"))
    ));
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn unresolved_required_record_base_identity_refuses_atomic_catalog_install() {
    let source = r#"
        record Base
            Real inherited;
        end Base;
        record Derived
            extends Base;
            Boolean own;
        end Derived;
        function consume
            input Derived value;
            output Boolean result;
        algorithm
            result := value.own;
        end consume;
        model Test
            Real result;
        equation
            result = if consume(Derived(1.0, true)) then 1.0 else 0.0;
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("record inheritance resolves before mutation");
    let mut instanced = {
        let tree = resolved.into_inner();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let mut checker = TypeChecker::new();
    let type_table = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the intact record graph produces a nominal table");
    let derived = instanced
        .tree
        .get_def_id_by_name("Derived")
        .expect("derived record declaration exists");
    instanced
        .tree
        .definitions
        .classes
        .get_mut("Derived")
        .and_then(|derived| derived.extends.first_mut())
        .expect("mutation seed has one record base")
        .base_def_id = None;
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a required record base cannot lose exact declaration identity");
    assert!(
        matches!(
            error.as_ref(),
            TypeCheckError::MissingSourceContext { reason }
                if reason.contains("UnresolvedBaseIdentity")
                    && reason.contains(&format!("{derived:?}"))
        ),
        "unexpected refusal: {error:?}",
    );
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn non_record_required_base_refuses_atomic_catalog_install() {
    let source = r#"
        record Base
            Real inherited;
        end Base;
        record Derived
            extends Base;
            Boolean own;
        end Derived;
        function consume
            input Derived value;
            output Boolean result;
        algorithm
            result := value.own;
        end consume;
        model Test
            Derived value;
            Boolean result;
        equation
            result = consume(value);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("record inheritance resolves before mutation");
    let mut instanced = {
        let tree = resolved.into_inner();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let mut checker = TypeChecker::new();
    let type_table = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the intact record graph produces a nominal table");
    instanced
        .tree
        .definitions
        .classes
        .get_mut("Base")
        .expect("mutation seed retains its exact base declaration")
        .class_type = rumoca_core::ClassType::Model;

    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("record inheritance cannot silently cross a non-record declaration");
    assert!(matches!(
        error.as_ref(),
        TypeCheckError::PhaseDiagnostic { code, message, .. }
            if code == "ET000" && message.contains("not a record class")
    ));
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn missing_record_field_alias_base_fails_in_resolve() {
    let source = r#"
        package P
            type Broken = MissingLibrary.Value;
            record Payload
                Broken value;
            end Payload;
            type Alias = Payload;
            function consume
                input Alias value;
                output Real result;
            algorithm
                result := 1.0;
            end consume;
        end P;
        package Q
            record Payload
                Real value;
            end Payload;
        end Q;
        model Test
            P.Alias value;
            Real result;
        equation
            result = P.consume(value);
        end Test;
    "#;
    let diagnostics = resolve(parse(source))
        .expect_err("a missing field alias base must fail before type-root construction");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER003")
                && diagnostic.message.contains("MissingLibrary.Value")),
        "Resolve owns the exact nested alias-base refusal: {diagnostics:?}",
    );
}

#[test]
fn missing_exact_alias_record_refuses_despite_same_spelling_collision() {
    let source = r#"
        package P
            record Payload
                Real value;
            end Payload;
            type Alias = Payload;
            function consume
                input Alias value;
                output Real result;
            algorithm
                result := value.value;
            end consume;
        end P;
        package Q
            record Payload
                Real value;
            end Payload;
        end Q;
        model Test
            P.Alias value;
            Real result;
        equation
            result = P.consume(value);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("the exact alias record graph resolves");
    let mut instanced = {
        let tree = resolved.into_inner();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let selected = instanced
        .tree
        .get_def_id_by_name("P.Payload")
        .expect("selected underlying record exists");
    let collision = instanced
        .tree
        .get_def_id_by_name("Q.Payload")
        .expect("same-spelling record collision exists");
    assert_ne!(selected, collision);
    let mut checker = TypeChecker::new();
    let type_table = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the intact exact record graph produces a nominal table");
    instanced
        .tree
        .definitions
        .classes
        .get_mut("P")
        .and_then(|package| package.classes.shift_remove("Payload"))
        .expect("mutation removes only the selected underlying record");
    assert!(
        instanced.tree.get_class_by_def_id(collision).is_some(),
        "same-spelling collision remains available"
    );

    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a same-spelling record cannot replace the selected alias target");
    assert!(
        matches!(
            error.as_ref(),
            TypeCheckError::MissingSourceContext { reason }
                if reason.contains("MissingBase")
                    && reason.contains(&format!("{selected:?}"))
        ),
        "unexpected refusal: {error:?}",
    );
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn presentation_annotation_calls_are_not_runtime_signature_reachability() {
    let source = r#"
        function decoration
            output Real result;
        algorithm
            result := 1.0;
        end decoration;
        function helper
            input Real value;
            output Real result;
        algorithm
            result := decoration();
        end helper;
        model Test
            Real result;
        equation
            result = helper(value = 1.0);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("runtime and named-argument calls resolve");
    let mut tree = resolved.into_inner();
    let decoration = tree
        .get_def_id_by_name("decoration")
        .expect("annotation-only function declaration exists");
    let helper_declaration = tree
        .get_def_id_by_name("helper")
        .expect("runtime helper declaration exists");
    let helper = tree
        .definitions
        .classes
        .get_mut("helper")
        .expect("transitively used helper exists");
    let annotation = match &helper.algorithms[0][0] {
        rumoca_ir_ast::Statement::Assignment { value, .. } => value.clone(),
        other => panic!("expected one assignment call, got {other:?}"),
    };
    helper.algorithms[0][0] = rumoca_ir_ast::Statement::Empty;
    helper
        .components
        .get_mut("result")
        .expect("helper output exists")
        .annotation
        .push(annotation);
    let annotation_call = helper.components["result"]
        .annotation
        .first()
        .expect("resolved call moved into presentation metadata");
    assert!(matches!(
        annotation_call,
        Expression::FunctionCall { comp, .. } if comp.target_def_id() == Some(decoration)
    ));
    assert!(
        collect_class_function_declarations(helper)
            .expect("semantic helper body retains exact call identities")
            .iter()
            .all(|(declaration, _)| *declaration != decoration),
        "the presentation call is absent from runtime class reachability"
    );

    let mut overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
    {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    let used = collect_overlay_function_declarations(&overlay)
        .expect("instantiated runtime calls retain exact identities");
    assert!(
        used.iter()
            .any(|(declaration, _)| *declaration == helper_declaration),
        "named-argument call retains the exact helper target"
    );
    assert!(
        used.iter()
            .all(|(declaration, _)| *declaration != decoration),
        "the authoritative overlay contains no runtime decoration call"
    );

    let mut checker = TypeChecker::new();
    let type_table = checker
        .initialize_instanced_context(&tree)
        .expect("the nominal type graph is complete");
    checker.function_signatures.remove(&decoration);
    checker
        .populate_overlay_type_roots(&tree, &mut overlay, &type_table)
        .expect("presentation metadata and named-argument syntax do not invent runtime calls");
}

#[test]
fn connection_graph_operator_identities_are_predefined_call_targets() {
    let resolved = resolve(parse("model Test end Test;")).expect("minimal fixture resolves");
    let mut declarations = Vec::new();
    for role in rumoca_core::ConnectionGraphOperatorRole::ALL {
        let path = ComponentPath::from_parts(role.predefined_path());
        let declaration = resolved
            .scope_tree
            .predefined_member(&path)
            .unwrap_or_else(|| panic!("missing predefined {path}"));
        assert_eq!(
            resolved.semantic_catalogs().connections().declaration(role),
            declaration,
            "Resolve must issue the exact predefined identity for {path}"
        );
        assert!(
            !declarations.contains(&declaration),
            "connection-graph roles must have five distinct declaration identities"
        );
        declarations.push(declaration);
    }
    let tree = resolved.into_inner();
    let semantic_catalogs = semantic_catalog_projection_for_test(&tree)
        .expect("resolved fixture has exact semantic identities");
    for role in rumoca_core::ConnectionGraphOperatorRole::ALL {
        let path = ComponentPath::from_parts(role.predefined_path());
        let declaration = tree
            .scope_tree
            .predefined_member(&path)
            .unwrap_or_else(|| panic!("missing predefined {path}"));
        assert!(
            is_predefined_function_declaration(&tree, &semantic_catalogs, declaration,),
            "the exact predefined {path} identity must not require a user class body"
        );
    }
}
