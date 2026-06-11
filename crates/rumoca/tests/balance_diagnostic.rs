//! Diagnostic tests for understanding balance issues in MSL models.

use rumoca_compile::compile::{CompiledSourceRoot, FailedPhase, PhaseResult};
use rumoca_core::VarName;
use rumoca_ir_dae::Dae;

/// Test a simple logical Not model to understand balance
#[test]
fn test_balance_simple_not() {
    let source = r#"
model Not
    input Boolean u;
    output Boolean y;
equation
    y = not u;
end Not;
"#;

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("Not") {
        PhaseResult::Success(result) => {
            let dae = &result.dae;
            println!("\n=== Simple Not Model ===");
            println!(
                "States: {:?}",
                dae.variables.states.keys().collect::<Vec<_>>()
            );
            println!(
                "Algebraics: {:?}",
                dae.variables.algebraics.keys().collect::<Vec<_>>()
            );
            println!(
                "Inputs: {:?}",
                dae.variables.inputs.keys().collect::<Vec<_>>()
            );
            println!(
                "Outputs: {:?}",
                dae.variables.outputs.keys().collect::<Vec<_>>()
            );
            println!(
                "Continuous equations (f_x): {}",
                dae.continuous.equations.len()
            );
            println!(
                "Balance: {}",
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture")
            );
            assert_eq!(
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture"),
                0,
                "Simple Not should be balanced"
            );
        }
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}

/// Test MSL-style connector-based model
#[test]
fn test_balance_connector_style() {
    let source = r#"
connector BooleanInput = input Boolean;
connector BooleanOutput = output Boolean;

model Not
    BooleanInput u;
    BooleanOutput y;
equation
    y = not u;
end Not;
"#;

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();

    // Debug: check if causality is preserved in the parsed AST
    if let Some(bool_input) = def.classes.get("BooleanInput") {
        println!("BooleanInput class causality: {:?}", bool_input.causality);
    }
    if let Some(bool_output) = def.classes.get("BooleanOutput") {
        println!("BooleanOutput class causality: {:?}", bool_output.causality);
    }

    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("Not") {
        PhaseResult::Success(result) => {
            let dae = &result.dae;
            println!("\n=== Connector-Style Not Model ===");
            println!(
                "States: {:?}",
                dae.variables.states.keys().collect::<Vec<_>>()
            );
            println!(
                "Algebraics: {:?}",
                dae.variables.algebraics.keys().collect::<Vec<_>>()
            );
            println!(
                "Inputs: {:?}",
                dae.variables.inputs.keys().collect::<Vec<_>>()
            );
            println!(
                "Outputs: {:?}",
                dae.variables.outputs.keys().collect::<Vec<_>>()
            );
            println!(
                "Continuous equations (f_x): {}",
                dae.continuous.equations.len()
            );
            println!(
                "Balance: {}",
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture")
            );
            assert_eq!(
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture"),
                0,
                "Connector-style Not should be balanced"
            );
        }
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}

/// Test output equation counting
#[test]
fn test_balance_output_equations() {
    // According to MLS, outputs don't count as unknowns when they have defining equations
    // The output equation y = ... should be counted as solving for y
    let source = r#"
model TestOutput
    input Real u;
    output Real y;
equation
    y = 2 * u;
end TestOutput;
"#;

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("TestOutput") {
        PhaseResult::Success(result) => {
            let dae = &result.dae;
            println!("\n=== Output Equation Model ===");
            println!(
                "States: {:?}",
                dae.variables.states.keys().collect::<Vec<_>>()
            );
            println!(
                "Algebraics: {:?}",
                dae.variables.algebraics.keys().collect::<Vec<_>>()
            );
            println!(
                "Inputs: {:?}",
                dae.variables.inputs.keys().collect::<Vec<_>>()
            );
            println!(
                "Outputs: {:?}",
                dae.variables.outputs.keys().collect::<Vec<_>>()
            );
            println!(
                "Continuous equations (f_x): {}",
                dae.continuous.equations.len()
            );
            println!(
                "Balance: {}",
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture")
            );
            // This model has: 0 states, 0 algebraics (y is output),
            // 1 input (u - not unknown), 1 output (y - not unknown)
            // 1 algebraic equation
            // So balance = 1 - 0 = 1 (over-determined) - but that's wrong!
            // The equation y = 2*u should define output y, not be an algebraic equation
        }
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}

/// Test partial model (intentionally unbalanced)
#[test]
fn test_balance_partial_model() {
    let source = r#"
partial model PartialModel
    Real x;
    // No equation - intentionally incomplete
end PartialModel;
"#;

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("PartialModel") {
        PhaseResult::Success(result) => {
            let dae = &result.dae;
            println!("\n=== Partial Model ===");
            println!(
                "States: {:?}",
                dae.variables.states.keys().collect::<Vec<_>>()
            );
            println!(
                "Algebraics: {:?}",
                dae.variables.algebraics.keys().collect::<Vec<_>>()
            );
            println!("ODE equations: {}", dae.continuous.equations.len());
            println!("Algebraic equations: {}", dae.continuous.equations.len());
            println!(
                "Balance: {}",
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture")
            );
            // Partial models ARE expected to be unbalanced
            // They should be excluded from balance checking
        }
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}

/// Test QuasiStatic Ground model balance issue (MLS §4.8 overconstrained connectors)
#[test]
fn test_quasistatic_ground_balance() {
    // The Ground model has:
    // - Connections.potentialRoot(pin.reference, 256) - marks as potential root
    // - if Connections.isRoot(pin.reference) then pin.reference.gamma = 0; - conditional equation
    // - pin.v = Complex(0) - sets voltage to zero
    //
    // For balance:
    // - Unknowns: pin.v.re, pin.v.im, pin.i.re, pin.i.im, pin.reference.gamma (5 algebraics)
    // - Equations:
    //   - pin.v.re = 0, pin.v.im = 0 (2 equations from pin.v = Complex(0))
    //   - pin.reference.gamma = 0 (1 equation from isRoot conditional)
    //   - interface flows: pin.i.re, pin.i.im (2 equations from MLS §4.7)
    // - Total: 5 equations, 5 unknowns -> balance = 0
    //
    // BUT if overconstrained_interface_count ALSO adds 1, we get balance = +1
    let source = r#"
package QuasiStatic
    record Complex
        Real re;
        Real im;
    end Complex;

    record Reference
        Real gamma;
    end Reference;

    connector Pin
        Complex v;
        flow Complex i;
        Reference reference;
    end Pin;

    connector PositivePin = Pin;

    model Ground
        PositivePin pin;
    equation
        Connections.potentialRoot(pin.reference, 256);
        if Connections.isRoot(pin.reference) then
            pin.reference.gamma = 0;
        end if;
        pin.v.re = 0;
        pin.v.im = 0;
    end Ground;
end QuasiStatic;
"#;

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("QuasiStatic.Ground") {
        PhaseResult::Success(result) => assert_quasistatic_ground_balanced(&result.dae),
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}

fn assert_quasistatic_ground_balanced(dae: &rumoca_ir_dae::Dae) {
    print_quasistatic_ground_balance_details(dae);
    if rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture") != 0 {
        println!(
            "\nBUG: Balance should be 0, but is {}",
            rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture")
        );
        println!("The overconstrained_interface_count is double-counting the root equation");
    }
    assert_eq!(
        rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture"),
        0,
        "QuasiStatic.Ground should be balanced"
    );
}

fn print_quasistatic_ground_balance_details(dae: &rumoca_ir_dae::Dae) {
    println!("\n=== QuasiStatic.Ground Model ===");
    println!(
        "States ({}): {:?}",
        dae.variables.states.len(),
        dae.variables.states.keys().collect::<Vec<_>>()
    );
    println!(
        "Algebraics ({}): {:?}",
        dae.variables.algebraics.len(),
        dae.variables.algebraics.keys().collect::<Vec<_>>()
    );
    println!(
        "Inputs ({}): {:?}",
        dae.variables.inputs.len(),
        dae.variables.inputs.keys().collect::<Vec<_>>()
    );
    println!(
        "Outputs ({}): {:?}",
        dae.variables.outputs.len(),
        dae.variables.outputs.keys().collect::<Vec<_>>()
    );
    let f_x_scalar = print_quasistatic_equations(dae);
    print_quasistatic_balance_calculation(dae, f_x_scalar);
}

fn print_quasistatic_equations(dae: &rumoca_ir_dae::Dae) -> usize {
    let f_x_scalar: usize = dae
        .continuous
        .equations
        .iter()
        .map(|e| e.scalar_count)
        .sum();
    println!("\nEquations:");
    println!(
        "  f_x: {} objects, {} scalar",
        dae.continuous.equations.len(),
        f_x_scalar
    );
    for eq in &dae.continuous.equations {
        println!("    [{}] {}", eq.scalar_count, eq.origin);
    }
    f_x_scalar
}

fn print_quasistatic_balance_calculation(dae: &rumoca_ir_dae::Dae, f_x_scalar: usize) {
    println!("\nInterface contributions:");
    println!(
        "  interface_flow_count: {}",
        dae.metadata.interface_flow_count
    );
    println!(
        "  overconstrained_interface_count: {}",
        dae.metadata.overconstrained_interface_count
    );
    let unknowns =
        dae.variables.states.len() + dae.variables.algebraics.len() + dae.variables.outputs.len();
    let total_eqs = (f_x_scalar + dae.metadata.interface_flow_count) as i64
        + dae.metadata.overconstrained_interface_count;
    println!("\nBalance calculation:");
    println!("  Unknowns: {}", unknowns);
    println!(
        "  Equations: {} (f_x={} + flow={} + overconstrained={})",
        total_eqs,
        f_x_scalar,
        dae.metadata.interface_flow_count,
        dae.metadata.overconstrained_interface_count
    );
    println!(
        "  Balance: {}",
        rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture")
    );
}

/// QuasiStatic model source for VoltageSource test
const QUASISTATIC_VOLTAGE_SOURCE: &str = r#"
package QuasiStatic
    record Complex Real re; Real im; end Complex;
    record Reference Real gamma; end Reference;
    connector Pin Complex v; flow Complex i; Reference reference; end Pin;
    connector PositivePin = Pin;
    connector NegativePin = Pin;
    partial model OnePort
        PositivePin pin_p; NegativePin pin_n;
        Complex v; Complex i;
    equation
        v.re = pin_p.v.re - pin_n.v.re; v.im = pin_p.v.im - pin_n.v.im;
        i.re = pin_p.i.re; i.im = pin_p.i.im;
        pin_p.i.re + pin_n.i.re = 0; pin_p.i.im + pin_n.i.im = 0;
        pin_p.reference.gamma = pin_n.reference.gamma;
    end OnePort;
    partial model Source extends OnePort;
        Real omega; Real gamma = pin_p.reference.gamma;
    equation Connections.root(pin_p.reference); end Source;
    model VoltageSource extends Source;
        parameter Real f = 1; parameter Real V = 1; parameter Real phi = 0;
    equation omega = 2 * 3.14159 * f; v.re = V * cos(phi); v.im = V * sin(phi);
    end VoltageSource;
end QuasiStatic;
"#;

/// Test QuasiStatic VoltageSource model balance issue
#[test]
fn test_quasistatic_voltage_source_balance() {
    let def = rumoca_phase_parse::parse_to_ast(QUASISTATIC_VOLTAGE_SOURCE, "test.mo").unwrap();
    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("QuasiStatic.VoltageSource") {
        PhaseResult::Success(result) => {
            let dae = &result.dae;
            println!("\n=== QuasiStatic.VoltageSource ===");
            print_dae_summary(dae);
            // Source uses Connections.root() - balance may be +1 if root eq is generated
            if rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture") != 0 {
                println!("Analysis: Source uses Connections.root() without explicit isRoot()");
            }
        }
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            if phase == FailedPhase::ToDae && error.contains("unbalanced model") {
                println!("Expected unbalanced ToDae failure: {}", error);
                return;
            }
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}

/// Print DAE summary for diagnostics
fn print_dae_summary(dae: &Dae) {
    println!(
        "States: {}, Algs: {}",
        dae.variables.states.len(),
        dae.variables.algebraics.len()
    );
    println!(
        "Inputs: {}, Outputs: {}",
        dae.variables.inputs.len(),
        dae.variables.outputs.len()
    );
    let alg_scalar: usize = dae
        .continuous
        .equations
        .iter()
        .map(|e| e.scalar_count)
        .sum();
    println!("Equations: {} scalar", alg_scalar);
    println!(
        "Interface: flow={}, overconstrained={}",
        dae.metadata.interface_flow_count, dae.metadata.overconstrained_interface_count
    );
    println!(
        "Balance: {}",
        rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture")
    );
}

/// Test inherited Real connector (like SI units)
#[test]
fn test_balance_inheritance_connector() {
    let source = r#"
connector RealInput = input Real;
connector RealOutput = output Real;

partial model SISO "Single Input Single Output"
    RealInput u;
    RealOutput y;
end SISO;

model Gain "y = k * u"
    extends SISO;
    parameter Real k = 1;
equation
    y = k * u;
end Gain;
"#;

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("Gain") {
        PhaseResult::Success(result) => {
            let dae = &result.dae;
            println!("\n=== Gain Model (SISO inheritance) ===");
            println!(
                "States: {:?}",
                dae.variables.states.keys().collect::<Vec<_>>()
            );
            println!(
                "Algebraics: {:?}",
                dae.variables.algebraics.keys().collect::<Vec<_>>()
            );
            println!(
                "Inputs: {:?}",
                dae.variables.inputs.keys().collect::<Vec<_>>()
            );
            println!(
                "Outputs: {:?}",
                dae.variables.outputs.keys().collect::<Vec<_>>()
            );
            println!(
                "Parameters: {:?}",
                dae.variables.parameters.keys().collect::<Vec<_>>()
            );
            println!(
                "Continuous equations (f_x): {}",
                dae.continuous.equations.len()
            );
            println!(
                "Balance: {}",
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture")
            );
            assert_eq!(
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture"),
                0,
                "Gain should be balanced"
            );
        }
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}

/// Test that equation-defined internal inputs are promoted to algebraic unknowns
/// when the input appears on the RHS of a residual equation.
///
/// This mirrors the MSL BaseProperties pattern where a partial model declares
/// `input Real p` as an interface input and then equates it to a state record
/// field via `state_p = p`. In the flattened model, this produces the equation
/// `medium.state_p - medium.p = 0`, where `medium.p` appears on the RHS as
/// a direct VarRef. The todae phase must detect this and promote `medium.p`
/// from input to algebraic (MLS §4.4.2.2).
///
/// In the real MSL IdealGasN2 model, `medium.p` never appears on the LHS of
/// any equation — it only appears on the RHS of `medium.state.p = medium.p`.
/// Without the RHS detection, medium.p stays as an input (not counted as
/// unknown), creating balance = +1 instead of 0.
#[test]
fn test_equation_defined_input_rhs_promoted() {
    let source = r#"
model BaseProps
    input Real p;
    Real state_p;
equation
    state_p = p;
end BaseProps;

model TestEquationDefinedInput
    BaseProps medium;
equation
    medium.state_p = 101325;
end TestEquationDefinedInput;
"#;

    // Expected flattened equations:
    //   1. medium.state_p - medium.p = 0   (from state_p = p; medium.p on RHS only)
    //   2. medium.state_p - 101325 = 0     (from enclosing model)
    //
    // Variables: medium.p (input), medium.state_p
    // Without promotion: 1 unknown (state_p), 2 equations → balance = +1
    // With promotion:    2 unknowns, 2 equations → balance = 0

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("TestEquationDefinedInput") {
        PhaseResult::Success(result) => {
            let dae = &result.dae;
            // medium.p appears only on the RHS of equation 1 (state_p = p).
            // The RHS check in find_equation_defined_inputs should detect it
            // and promote it to algebraic.
            assert!(
                !dae.variables.inputs.contains_key(&VarName::new("medium.p")),
                "medium.p should NOT be an input (equation-defined via RHS)"
            );
            assert!(
                dae.variables
                    .algebraics
                    .contains_key(&VarName::new("medium.p")),
                "medium.p should be an algebraic unknown"
            );
            assert_eq!(
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture"),
                0,
                "Model with equation-defined internal inputs should be balanced"
            );
        }
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}

/// Test that the RHS promotion does NOT fire when a container model uses a
/// sub-component's input (equation origin does not match input parent).
///
/// In the pattern `volume.vessel_ps_static = volume.medium.p`, the equation
/// originates from `volume` (the container), but `volume.medium.p` belongs
/// to `volume.medium` (the sub-component). These don't match, so `medium.p`
/// should NOT be promoted via the RHS path alone.
#[test]
fn test_rhs_promotion_skips_cross_component() {
    let source = r#"
model BaseProps
    input Real p;
    Real state_p;
equation
    state_p = p;
end BaseProps;

model Volume
    BaseProps medium;
    Real vessel_ps_static;
equation
    medium.p = 1;
    vessel_ps_static = medium.p;
end Volume;
"#;

    // Flattened equations:
    //   1. medium.state_p - medium.p = 0   (from BaseProps, origin=medium)
    //   2. medium.p - 1 = 0                (from Volume, origin=top-level)
    //   3. vessel_ps_static - medium.p = 0 (from Volume, origin=top-level)
    //
    // The RHS check promotes medium.p via equation 1 (origin matches parent)
    // but NOT via equation 2 (origin mismatch). In this simple model, medium.p
    // IS promoted because equation 1 provides the intra-component alias.

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("Volume") {
        PhaseResult::Success(result) => {
            let dae = &result.dae;
            // medium.p should be promoted via the intra-component alias
            // (equation 1: state_p = p, origin=medium matches parent=medium)
            assert!(
                dae.variables
                    .algebraics
                    .contains_key(&VarName::new("medium.p")),
                "medium.p should be algebraic (promoted via intra-component alias)"
            );
        }
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}

/// Record-level equations inside an array component instance must collapse
/// to structured record references and balance exactly (PR #202 regression:
/// `comp[k].x` stayed an Index/FieldAccess tree, so shape inference counted
/// the whole component array per equation — MSL PolyphaseInductance reported
/// balance 2010, BalancingDelta 7371).
#[test]
fn test_balance_record_equation_in_component_array() {
    let source = r#"
package P
  record R
    Real a;
    Real b;
  end R;

  model Comp
    R x;
    R y;
  equation
    x = y;
    y.a = 1;
    y.b = 2;
  end Comp;

  model Top
    Comp comp[2];
  end Top;
end P;
"#;

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
    let source_root = CompiledSourceRoot::from_stored_definition(def).unwrap();

    match source_root.compile_model_phases("P.Top") {
        PhaseResult::Success(result) => {
            let dae = &result.dae;
            assert_eq!(
                rumoca_phase_dae::balance::balance(dae).expect("valid DAE balance fixture"),
                0,
                "record equations from an array component must not inflate the balance"
            );
            assert!(
                dae.variables
                    .algebraics
                    .contains_key(&VarName::new("comp[2].x.b")),
                "record fields should scalarize per element"
            );
        }
        PhaseResult::NeedsInner { missing_inners, .. } => {
            panic!("Model needs inner declarations: {:?}", missing_inners);
        }
        PhaseResult::Failed { phase, error, .. } => {
            panic!("Compilation failed at {:?}: {}", phase, error);
        }
    }
}
