//! SPEC_0034 estimator-scope battery: `initial equation` → `Startup`
//! lowering (GAL-028/D14) and the `Matrices.solve` →
//! `solveLinearEquations` mapping with declared-by-construction escape
//! sets (GAL-029/D13). Split from `spec_0034_battery.rs` per the
//! SPEC_0021 file-size limit; the fixture section below is the minimal
//! self-contained subset that suite's fixture builders (each helper here
//! is used by these modules — the zero-dead-code discipline).

use std::collections::HashMap;

use rumoca_core::{Expression, Literal, OpBinary, Reference, Span, Subscript, VarName};
use rumoca_galec_codegen::input::ScalarTypeMap;
use rumoca_galec_codegen::{
    AlgorithmCodePackage, GalecInput, GalecOptions, GalecTargetError, lower_to_algorithm_code,
    render_algorithm_code,
};
use rumoca_ir_dae as dae;
use rumoca_ir_galec::ast::ScalarType;

// ---------------------------------------------------------------------
// Fixture: minimal admissible discrete model with one guarded update
// (mirrors spec_0034_battery.rs)
// ---------------------------------------------------------------------

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: Span::DUMMY,
    }
}

fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: Span::DUMMY,
    }
}

fn boolean(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: Span::DUMMY,
    }
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

fn indexed(name: &str, index: i64) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![Subscript::index(index, Span::DUMMY)],
        span: Span::DUMMY,
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

fn if_expr(branches: Vec<(Expression, Expression)>, else_branch: Expression) -> Expression {
    Expression::If {
        branches,
        else_branch: Box::new(else_branch),
        span: Span::DUMMY,
    }
}

fn sample_call() -> Expression {
    Expression::FunctionCall {
        name: Reference::generated(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME),
        args: vec![real(0.0), var("samplePeriod")],
        is_constructor: false,
        span: Span::DUMMY,
    }
}

fn variable(name: &str) -> dae::Variable {
    let mut variable = dae::Variable::empty_with_span(Span::DUMMY);
    variable.name = VarName::new(name);
    variable
}

fn add_pre_slot(model: &mut dae::Dae, base: &str, start: Expression, dims: Vec<i64>) {
    let mut slot = variable(&format!("__pre__.{base}"));
    slot.causality = dae::VariableCausality::CalculatedParameter;
    slot.origin = dae::VariableOrigin::Generated;
    slot.fixed = Some(true);
    slot.start = Some(start);
    slot.dims = dims;
    model.variables.parameters.insert(slot.name.clone(), slot);
}

fn add_state(model: &mut dae::Dae, name: &str) {
    let mut z = variable(name);
    z.start = Some(real(0.0));
    model.variables.discrete_reals.insert(z.name.clone(), z);
    add_pre_slot(model, name, real(0.0), Vec::new());
}

/// The canonical guarded row: fires on the sample-tick when-edge of
/// condition 1, holds `if Initial() then <target> else __pre__.<target>`
/// (byte-for-byte the `spec_0034_battery.rs` shape `guard.rs` recognizes).
fn guarded_update(target: &str, body: Expression) -> dae::Equation {
    let edge = binary(
        OpBinary::And,
        indexed("c", 1),
        Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(indexed("__pre__.c", 1)),
            span: Span::DUMMY,
        },
    );
    let hold = if_expr(
        vec![(
            Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Initial,
                args: Vec::new(),
                span: Span::DUMMY,
            },
            var(target),
        )],
        var(&format!("__pre__.{target}")),
    );
    dae::Equation {
        lhs: Some(Reference::new(target)),
        rhs: if_expr(vec![(edge, body)], hold),
        span: Span::DUMMY,
        origin: format!("when sample then {target}"),
        scalar_count: 1,
    }
}

/// Minimal real-compiler-shaped model (mirrors `spec_0034_battery.rs`):
/// inputs `u`/`x2`, Integer tunables `i1`/`i2`, constant `samplePeriod`,
/// discrete state `y` updated by `body` on the sample tick.
fn model_with_body(body: Expression) -> dae::Dae {
    let mut model = dae::Dae::default();
    for name in ["u", "x2"] {
        let mut input = variable(name);
        input.causality = dae::VariableCausality::Input;
        input.start = Some(real(0.0));
        model.variables.inputs.insert(input.name.clone(), input);
    }
    for (name, start) in [("i1", 3), ("i2", 4)] {
        let mut parameter = variable(name);
        parameter.causality = dae::VariableCausality::Parameter;
        parameter.is_tunable = true;
        parameter.start = Some(integer(start));
        model
            .variables
            .parameters
            .insert(parameter.name.clone(), parameter);
    }
    let mut sample_period = variable("samplePeriod");
    sample_period.unit = Some("s".to_owned());
    sample_period.start = Some(real(1e-3));
    model
        .variables
        .constants
        .insert(sample_period.name.clone(), sample_period);

    add_state(&mut model, "y");

    let mut condition = variable("c");
    condition.origin = dae::VariableOrigin::Generated;
    condition.dims = vec![1];
    model
        .variables
        .discrete_valued
        .insert(condition.name.clone(), condition);
    add_pre_slot(&mut model, "c", boolean(false), vec![1]);

    model.conditions.relations.push(sample_call());
    model.conditions.equations.push(dae::Equation {
        lhs: Some(Reference::new("c[1]")),
        rhs: sample_call(),
        span: Span::DUMMY,
        origin: "condition equation 1".to_owned(),
        scalar_count: 1,
    });

    model.discrete.real_updates.push(guarded_update("y", body));
    model.clocks.schedules.push(dae::ClockSchedule {
        period_seconds: 1e-3,
        phase_seconds: 0.0,
        source_span: Span::DUMMY,
    });
    model
}

fn base_types() -> ScalarTypeMap {
    let mut types = HashMap::new();
    types.insert(VarName::new("samplePeriod"), ScalarType::Real);
    types.insert(VarName::new("i1"), ScalarType::Integer);
    types.insert(VarName::new("i2"), ScalarType::Integer);
    types
}

fn lower(model: &dae::Dae, types: &ScalarTypeMap) -> AlgorithmCodePackage {
    let input = GalecInput::new(model, "Battery").with_scalar_types(types);
    match lower_to_algorithm_code(&input, &GalecOptions::default()) {
        Ok(package) => package,
        Err(errors) => panic!("lowering failed: {errors:#?}"),
    }
}

fn lower_err(model: &dae::Dae, types: &ScalarTypeMap) -> Vec<GalecTargetError> {
    let input = GalecInput::new(model, "Battery").with_scalar_types(types);
    lower_to_algorithm_code(&input, &GalecOptions::default())
        .map(|_| ())
        .expect_err("lowering must fail")
}

fn assert_unsupported(errors: &[GalecTargetError], feature: &str) {
    let marker = format!("unsupported-feature:{feature}]");
    assert!(
        errors
            .iter()
            .any(|error| error.code() == "ET017" && error.to_string().contains(&marker)),
        "expected `{marker}` among: {errors:#?}"
    );
}

fn add_real_vector(model: &mut dae::Dae, name: &str, len: i64) {
    let mut vector = variable(name);
    vector.dims = vec![len];
    vector.start = Some(real(0.0));
    model
        .variables
        .discrete_reals
        .insert(vector.name.clone(), vector);
}

fn add_real_matrix(model: &mut dae::Dae, name: &str, rows: i64, cols: i64) {
    let mut matrix = variable(name);
    matrix.dims = vec![rows, cols];
    matrix.start = Some(real(0.0));
    model
        .variables
        .discrete_reals
        .insert(matrix.name.clone(), matrix);
}

fn make_y_vector(model: &mut dae::Dae, len: i64) {
    model
        .variables
        .discrete_reals
        .get_mut(&VarName::new("y"))
        .expect("y exists")
        .dims = vec![len];
    model
        .variables
        .parameters
        .get_mut(&VarName::new("__pre__.y"))
        .expect("pre y exists")
        .dims = vec![len];
}

fn resize_y_matrix(model: &mut dae::Dae, rows: i64, cols: i64) {
    model
        .variables
        .discrete_reals
        .get_mut(&VarName::new("y"))
        .expect("y exists")
        .dims = vec![rows, cols];
    model
        .variables
        .parameters
        .get_mut(&VarName::new("__pre__.y"))
        .expect("pre y exists")
        .dims = vec![rows, cols];
}

// ---------------------------------------------------------------------
// D17: the `.alg` walking template is byte-identical to the typed printer
// ---------------------------------------------------------------------

mod alg_template_parity_d17 {
    use super::*;

    /// The strongest drift guard between the two `.alg` producers: the
    /// D17 walking template (emission) and the `rumoca-ir-galec` typed
    /// printer (the parser-facing half of the language module) must agree
    /// byte-for-byte on lowered packages — signals clauses, quoted
    /// `'previous(x)'` names, arrays, solve calls, and the GAL-019
    /// minimal parenthesization included.
    #[test]
    fn template_and_typed_printer_agree_byte_for_byte() {
        // Reads `pre(y)` (quoted `'previous(y)'` names + end-of-DoStep
        // commit), solves with a composite vector argument (call + array
        // arithmetic + cross-class parenthesization), declares the
        // GAL-029 escape.
        let mut model = model_with_body(Expression::FunctionCall {
            name: Reference::new("Modelica.Math.Matrices.solve"),
            args: vec![
                var("a"),
                binary(
                    OpBinary::Add,
                    var("b"),
                    binary(OpBinary::Mul, var("u"), var("__pre__.y")),
                ),
            ],
            is_constructor: false,
            span: Span::DUMMY,
        });
        add_real_matrix(&mut model, "a", 2, 2);
        add_real_vector(&mut model, "b", 2);
        make_y_vector(&mut model, 2);

        let package = lower(&model, &base_types());
        let template_text = render_algorithm_code(&package).expect("template renders");
        let printer_text =
            rumoca_ir_galec::print_block(&package.block).expect("typed printer renders");
        assert_eq!(template_text, printer_text);
    }
}

// ---------------------------------------------------------------------
// GAL-029/D13: Matrices.solve → solveLinearEquations
// ---------------------------------------------------------------------

mod linear_solve_gal_029 {
    use super::*;

    fn solve_call(matrix: &str, vector: Expression) -> Expression {
        Expression::FunctionCall {
            name: Reference::new("Modelica.Math.Matrices.solve"),
            args: vec![var(matrix), vector],
            is_constructor: false,
            span: Span::DUMMY,
        }
    }

    fn solve_model(call: Expression) -> dae::Dae {
        let mut model = model_with_body(call);
        add_real_matrix(&mut model, "a", 2, 2);
        add_real_vector(&mut model, "b", 2);
        make_y_vector(&mut model, 2);
        model
    }

    #[test]
    fn matrices_solve_maps_to_the_builtin_and_declares_the_escape() {
        let package = lower(&solve_model(solve_call("a", var("b"))), &base_types());
        let alg = render_algorithm_code(&package).expect("renders");
        assert!(
            alg.contains("self.y := solveLinearEquations(self.a, self.b);"),
            "{alg}"
        );
        // Declared by construction on DoStep only (GAL-029)…
        assert!(
            alg.contains("signals SOLVE_LINEAR_EQUATIONS_FAILED;"),
            "{alg}"
        );
        // …and mirrored into the manifest fragment.
        use rumoca_galec_codegen::manifest_context::algorithm_code_manifest::ErrorSignal;
        assert_eq!(
            package.manifest.do_step_signals,
            vec![ErrorSignal::SolveLinearEquationsFailed]
        );
        assert!(package.manifest.startup_signals.is_empty());
        assert!(package.manifest.recalibrate_signals.is_empty());
    }

    #[test]
    fn matrices_inv_gets_solve_guidance() {
        let mut model = model_with_body(Expression::FunctionCall {
            name: Reference::new("Modelica.Math.Matrices.inv"),
            args: vec![var("a")],
            is_constructor: false,
            span: Span::DUMMY,
        });
        add_real_matrix(&mut model, "a", 2, 2);
        resize_y_matrix(&mut model, 2, 2);
        let errors = lower_err(&model, &base_types());
        assert_unsupported(&errors, "matrix-inverse");
    }

    #[test]
    fn solve_with_mismatched_dimensions_is_a_type_error() {
        let mut model = solve_model(solve_call("a", var("b")));
        model
            .variables
            .discrete_reals
            .get_mut(&VarName::new("b"))
            .expect("b exists")
            .dims = vec![3];
        let errors = lower_err(&model, &base_types());
        assert!(
            errors.iter().any(|error| matches!(
                error,
                GalecTargetError::LoweringTypeMismatch { context, .. }
                    if context == "Matrices.solve operands"
            )),
            "{errors:?}"
        );
    }
}

// ---------------------------------------------------------------------
// GAL-028/D14: initial equation → Startup
// ---------------------------------------------------------------------

mod initialization_gal_028 {
    use super::*;

    /// A source `initial equation <target> = <value>` in DAE residual form.
    fn residual(target: &str, value: Expression) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs: binary(OpBinary::Sub, var(target), value),
            span: Span::DUMMY,
            origin: format!("initial equation for {target}"),
            scalar_count: 1,
        }
    }

    #[test]
    fn initial_equation_lowers_into_startup_and_overrides_manifest_start() {
        // The update reads `pre(y)` so the pre slot is kept and needs the
        // GAL-028 re-seed.
        let mut model = model_with_body(var("__pre__.y"));
        // y0 = 2.0 * i1 (i1 tunable Integer, default 3) => 6.0 at defaults.
        model
            .initialization
            .equations
            .push(residual("y", binary(OpBinary::Mul, real(2.0), var("i1"))));

        let alg = render_algorithm_code(&lower(&model, &base_types())).expect("renders");
        // Literal mirroring uses the GAL-028 override (6.0), then the
        // computed statement overwrites symbolically, then the pre slot
        // re-seeds from the computed value (D14 ordering).
        let mirrored = alg.find("self.y := 6.0;").expect("mirrored literal");
        let computed = alg
            .find("self.y := 2.0 * real(self.i1);")
            .expect("computed statement");
        let seeded = alg
            .find("self.'previous(y)' := self.y;")
            .expect("pre-slot re-seed");
        assert!(mirrored < computed && computed < seeded, "{alg}");
        // The pre slot's mirrored literal takes the override too.
        assert!(alg.contains("self.'previous(y)' := 6.0;"), "{alg}");
    }

    #[test]
    fn fixed_start_rows_are_skipped_as_mirroring_duplicates() {
        let mut model = model_with_body(var("u"));
        model.initialization.equations.push(dae::Equation {
            lhs: Some(Reference::new("y")),
            rhs: real(0.0),
            span: Span::DUMMY,
            origin: "fixed start initialization for y".to_owned(),
            scalar_count: 1,
        });

        let alg = render_algorithm_code(&lower(&model, &base_types())).expect("renders");
        // Exactly the mirroring assignment — no duplicate computed row.
        assert_eq!(alg.matches("self.y := 0.0;").count(), 1, "{alg}");
    }

    #[test]
    fn initial_equation_reading_an_input_is_rejected() {
        let mut model = model_with_body(var("u"));
        model.initialization.equations.push(residual("y", var("u")));
        let errors = lower_err(&model, &base_types());
        assert_unsupported(&errors, "initial-equation-reads-input");
    }

    #[test]
    fn unoriented_initial_equation_is_rejected() {
        let mut model = model_with_body(var("u"));
        model.initialization.equations.push(dae::Equation {
            lhs: None,
            rhs: binary(OpBinary::Add, var("y"), real(1.0)),
            span: Span::DUMMY,
            origin: "initial equation".to_owned(),
            scalar_count: 1,
        });
        let errors = lower_err(&model, &base_types());
        assert_unsupported(&errors, "implicit-initial-equation");
    }

    #[test]
    fn duplicate_initialization_is_rejected() {
        let mut model = model_with_body(var("u"));
        model
            .initialization
            .equations
            .push(residual("y", real(1.0)));
        model
            .initialization
            .equations
            .push(residual("y", real(2.0)));
        let errors = lower_err(&model, &base_types());
        assert_unsupported(&errors, "duplicate-initial-equation");
    }

    #[test]
    fn cyclic_initialization_is_rejected() {
        let mut model = model_with_body(var("u"));
        add_state(&mut model, "z");
        model.initialization.equations.push(residual("y", var("z")));
        model.initialization.equations.push(residual("z", var("y")));
        let errors = lower_err(&model, &base_types());
        assert_unsupported(&errors, "initialization-cycle");
    }

    #[test]
    fn parameter_initialization_target_is_rejected() {
        let mut model = model_with_body(var("u"));
        model
            .initialization
            .equations
            .push(residual("i1", integer(7)));
        let errors = lower_err(&model, &base_types());
        assert_unsupported(&errors, "initial-equation-target");
    }
}
