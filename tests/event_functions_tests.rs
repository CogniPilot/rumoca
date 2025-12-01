use rumoca::Compiler;

#[test]
fn test_sample_basic() {
    let result = Compiler::new().model("SampleTest").compile_str(
        r#"
            model SampleTest
                Real x(start=0.0);
            equation
                der(x) = 1.0;
            when sample(0, 0.1) then
                reinit(x, x + 0.1);
            end when;
            end SampleTest;
            "#,
        "sample_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("sample"), "JSON should contain sample");
}

#[test]
fn test_edge_basic() {
    let result = Compiler::new().model("EdgeTest").compile_str(
        r#"
            model EdgeTest
                Boolean b(start=false);
                Real x(start=0.0);
            equation
                b = time > 1.0;
            when edge(b) then
                reinit(x, 1.0);
            end when;
                der(x) = 0;
            end EdgeTest;
            "#,
        "edge_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("edge"), "JSON should contain edge");
}

#[test]
fn test_change_basic() {
    let result = Compiler::new().model("ChangeTest").compile_str(
        r#"
            model ChangeTest
                Integer n(start=0);
                Real x(start=0.0);
            equation
                n = integer(time);
            when change(n) then
                reinit(x, x + 1.0);
            end when;
                der(x) = 0;
            end ChangeTest;
            "#,
        "change_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("change"), "JSON should contain change");
}

#[test]
fn test_initial_basic() {
    let result = Compiler::new().model("InitialTest").compile_str(
        r#"
            model InitialTest
                Real x(start=0.0);
                Real y;
            equation
                der(x) = 1.0;
                y = if initial() then 0 else x;
            end InitialTest;
            "#,
        "initial_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("initial"), "JSON should contain initial");
}

#[test]
fn test_terminal_basic() {
    let result = Compiler::new().model("TerminalTest").compile_str(
        r#"
            model TerminalTest
                Real x(start=0.0);
                Real final_value;
            equation
                der(x) = 1.0;
                final_value = if terminal() then x else 0;
            end TerminalTest;
            "#,
        "terminal_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("terminal"), "JSON should contain terminal");
}

#[test]
fn test_combined_event_functions() {
    // Test combining multiple event functions
    let result = Compiler::new().model("CombinedEventTest").compile_str(
        r#"
            model CombinedEventTest
                Real x(start=0.0);
                Boolean trigger(start=false);
            equation
                der(x) = 1.0;
                trigger = x > 1.0;
            when {sample(0, 0.5), edge(trigger)} then
                reinit(x, 0.0);
            end when;
            end CombinedEventTest;
            "#,
        "combined_event_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("sample"), "JSON should contain sample");
    assert!(json.contains("edge"), "JSON should contain edge");
}

#[test]
fn test_initial_in_when() {
    // initial() can be used in when clause
    let result = Compiler::new().model("InitialWhenTest").compile_str(
        r#"
            model InitialWhenTest
                Real x(start=0.0);
            equation
                der(x) = 1.0;
            when initial() then
                reinit(x, 10.0);
            end when;
            end InitialWhenTest;
            "#,
        "initial_when_test.mo",
    );

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    let json = result.to_dae_ir_json().unwrap();
    assert!(json.contains("initial"), "JSON should contain initial");
}
