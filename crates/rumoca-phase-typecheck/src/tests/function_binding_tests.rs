use super::*;

fn check_model(source: &str) -> Result<(), Diagnostics> {
    let tree = resolve(parse(source)).expect("resolve").into_inner();
    let mut overlay = InstanceOverlay::new();
    typecheck_instanced(&tree, &mut overlay, "Test")
}

fn assert_binding_mismatch(source: &str) {
    let diagnostics = check_model(source).expect_err("reached local binding must be checked");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("type mismatch")
        }),
        "{diagnostics:?}"
    );
}

#[test]
fn reached_function_binding_closure_checks_transitive_protected_locals() {
    assert_binding_mismatch(
        r#"
        package Functions
            function callee
                input Integer k;
                output Real y;
            protected
                Integer q = k ^ 3;
            algorithm
                y := q;
            end callee;
            function caller
                input Integer k;
                output Real y;
            protected
                Real q = callee(k);
            algorithm
                y := q;
            end caller;
        end Functions;
        model Test
            Real y;
        equation
            y = Functions.caller(3);
        end Test;
    "#,
    );
}

#[test]
fn reached_function_bindings_keep_lexical_input_scope() {
    check_model(
        r#"
        package Functions
            constant Real offset = 0.5;
            function f
                input Real k;
                output Real y;
            protected
                Real q = k + offset;
            algorithm
                y := q;
            end f;
        end Functions;
        model Test
            Boolean k = true;
            Boolean offset = false;
            Real y;
        equation
            y = Functions.f(2.0);
        end Test;
    "#,
    )
    .expect("caller components cannot capture original function declarations");
}

#[test]
fn reached_function_bindings_check_inherited_declarations() {
    assert_binding_mismatch(
        r#"
        function Base
            input Integer k;
            output Real y;
        protected
            Integer q = k ^ 3;
        algorithm
            y := q;
        end Base;
        function f
            extends Base;
        end f;
        model Test
            Real y;
        equation
            y = f(3);
        end Test;
    "#,
    );
}

#[test]
fn reached_function_bindings_do_not_check_unused_library_functions() {
    check_model(
        r#"
        function unused
            input Integer k;
            output Real y;
        protected
            Integer q = k ^ 3;
        algorithm
            y := q;
        end unused;
        function used
            input Integer k;
            output Real y;
        protected
            Real q = k ^ 3;
        algorithm
            y := q;
        end used;
        model Test
            Real y;
        equation
            y = used(3);
        end Test;
    "#,
    )
    .expect("unreached invalid local bindings do not reject a selected target");
}

#[test]
fn reached_function_binding_closure_terminates_on_recursive_calls() {
    check_model(
        r#"
        function f
            input Integer k;
            output Real y;
        protected
            Real q = if k > 0 then f(k - 1) else 0.0;
        algorithm
            y := q;
        end f;
        model Test
            Real y;
        equation
            y = f(3);
        end Test;
    "#,
    )
    .expect("recursive source closure has one declaration context");
}

#[test]
fn reached_function_bindings_preserve_actual_package_type_specialization() {
    let source = r#"
        partial package Base
            replaceable type T = Real;
            function f
                input T k;
                output Real y;
            protected
                Integer q = k;
            algorithm
                y := q;
            end f;
        end Base;
        package IntegerPackage
            extends Base;
            redeclare type T = Integer;
        end IntegerPackage;
        model Test
            package P = IntegerPackage;
            Real y;
        equation
            y = P.f(2);
        end Test;
    "#;
    check_model(source)
        .expect("protected bindings use the original virtual type's actual specialization");
    let both = source.replace("y = P.f(2);", "y = P.f(2) + Base.f(2.0);");
    assert_binding_mismatch(&both);
}

#[test]
fn reached_function_bindings_check_extends_value_modification_in_its_source_scope() {
    assert_binding_mismatch(
        r#"
        function Base
            input Integer k = 1;
            output Real y;
        algorithm
            y := k;
        end Base;
        function f = Base(k = 0.5);
        model Test
            Real y;
        equation
            y = f();
        end Test;
    "#,
    );
}

#[test]
fn standalone_class_checks_original_declaration_binding_compatibility() {
    let source = r#"
        function f
            input Integer k;
            output Real y;
        protected
            Integer q = k ^ 3;
        algorithm
            y := q;
        end f;
    "#;
    let resolved = resolve(parse(source)).expect("resolve");
    assert!(
        typecheck(resolved).is_err(),
        "standalone class checks the same binding compatibility"
    );
}

#[test]
fn reached_function_binding_reads_inherited_package_declarations_by_original_identity() {
    assert_binding_mismatch(
        r#"
        package Parent
            constant Real offset = 0.5;
        end Parent;
        package Child
            extends Parent;
            function f
                output Real y;
            protected
                Integer q = offset;
            algorithm
                y := q;
            end f;
        end Child;
        model Test
            Boolean offset = false;
            Real y;
        equation
            y = Child.f();
        end Test;
    "#,
    );
}

#[test]
fn reached_function_default_selects_a_nested_field_of_a_record_input() {
    check_model(
        r#"
        package D
            record Cat Real pos[2]; end Cat;
            record Loc Cat catalog; Real g; end Loc;
            function pub
                input Loc previous;
                input Real k;
                input Real p[2] = previous.catalog.pos;
                output Real y;
            algorithm
                y := k * (p[1] + p[2]) + previous.g;
            end pub;
        end D;
        model Test
            Real y = D.pub(D.Loc(D.Cat({1, 2}), 3), 2);
        end Test;
    "#,
    )
    .expect("a record input's nested field path is a known member");
}

#[test]
fn function_calls_on_either_side_of_a_reached_simple_equation_are_queued() {
    let callee = rumoca_core::DefId::new(7);
    let mut checker = TypeChecker::new();
    checker
        .function_signatures
        .insert(callee, function_signatures::FunctionSignature::default());
    checker.reachable_function_bindings = Some(Vec::new());
    let mut reference = make_comp_ref("callee");
    reference.parts[0].def_id = Some(callee);
    let call = || Expression::FunctionCall {
        comp: reference.clone(),
        args: Vec::new(),
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };
    let equation = rumoca_ir_ast::Equation::Simple {
        lhs: call(),
        rhs: call(),
    };
    let types = TypeTable::new();
    let mut calls = function_bindings::FunctionCalls(&mut checker);
    typechecker::traversal_adapter::walk_equations(&mut calls, &[equation], &types);
    let queued = checker.reachable_function_bindings.as_ref().unwrap();
    assert_eq!(queued.len(), 2, "each call in the equation is queued once");
}
