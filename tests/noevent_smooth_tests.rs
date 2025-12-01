use rumoca::Compiler;

#[test]
fn test_noevent_basic() {
    let result = Compiler::new().model("NoEventTest").compile_str(
        r#"
            model NoEventTest
                Real x(start=0.0);
            equation
                der(x) = noEvent(if x > 0 then 1.0 else -1.0);
            end NoEventTest;
            "#,
        "noevent_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    assert!(result.dae.x.contains_key("x"), "Should have state x");

    // Check that noEvent is in the JSON output
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("noEvent"), "JSON should contain noEvent");
}

#[test]
fn test_smooth_basic() {
    let result = Compiler::new().model("SmoothTest").compile_str(
        r#"
            model SmoothTest
                Real x(start=1.0);
            equation
                der(x) = smooth(0, if x > 0.5 then x else 0.5);
            end SmoothTest;
            "#,
        "smooth_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    assert!(result.dae.x.contains_key("x"), "Should have state x");

    // Check that smooth is in the JSON output
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("smooth"), "JSON should contain smooth");
}

#[test]
fn test_smooth_with_order() {
    // Test smooth with different smoothness orders
    let result = Compiler::new().model("SmoothOrderTest").compile_str(
        r#"
            model SmoothOrderTest
                Real x(start=0.0);
                Real y;
            equation
                der(x) = 1.0;
                // smooth(2, expr) means expr is 2 times continuously differentiable
                y = smooth(2, x * x * x);
            end SmoothOrderTest;
            "#,
        "smooth_order_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("smooth"), "JSON should contain smooth");
}

#[test]
fn test_noevent_nested() {
    // Test noEvent with nested expressions
    let result = Compiler::new().model("NoEventNestedTest").compile_str(
        r#"
            model NoEventNestedTest
                Real x(start=0.0);
                Real y(start=1.0);
                Real z;
            equation
                der(x) = 1.0;
                der(y) = -1.0;
                z = noEvent(abs(x - y));
            end NoEventNestedTest;
            "#,
        "noevent_nested_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    assert!(result.dae.x.contains_key("x"), "Should have state x");
    assert!(result.dae.x.contains_key("y"), "Should have state y");
    assert!(
        result.dae.y.contains_key("z"),
        "Should have algebraic variable z"
    );
}

#[test]
fn test_noevent_in_when() {
    // noEvent can be used in when conditions
    let result = Compiler::new().model("NoEventWhenTest").compile_str(
        r#"
            model NoEventWhenTest
                Real x(start=1.0);
                Real v(start=0.0);
            equation
                der(x) = v;
                der(v) = -9.81;
            when noEvent(x < 0) then
                reinit(v, -0.8 * v);
            end when;
            end NoEventWhenTest;
            "#,
        "noevent_when_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(
        json.contains("noEvent"),
        "JSON should contain noEvent in when condition"
    );
}

#[test]
fn test_combined_noevent_smooth() {
    // Test combining noEvent and smooth
    let result = Compiler::new().model("CombinedTest").compile_str(
        r#"
            model CombinedTest
                Real x(start=0.0);
            equation
                der(x) = noEvent(smooth(1, if x > 0 then x else 0));
            end CombinedTest;
            "#,
        "combined_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("noEvent"), "JSON should contain noEvent");
    assert!(json.contains("smooth"), "JSON should contain smooth");
}
