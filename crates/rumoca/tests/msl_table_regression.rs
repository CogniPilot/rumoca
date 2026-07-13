//! Regression tests for MSL table-driven no-state simulation behavior.

use rumoca::Compiler;
use rumoca_core::{SourceId, Span};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{LinearOp, LinearOpSliceKind, SolveVisitor};
use rumoca_sim::{SimOptions, simulate_dae};

fn fixture_span() -> Span {
    Span::from_offsets(SourceId::from_source_name("msl_table_regression.rs"), 0, 1)
}

fn real_lit(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: fixture_span(),
    }
}

fn int_lit(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: fixture_span(),
    }
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![],
        span: fixture_span(),
    }
}

fn array_expr(elements: Vec<rumoca_core::Expression>, is_matrix: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: fixture_span(),
    }
}

fn call_expr(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: false,
        span: fixture_span(),
    }
}

fn enum_ref(type_name: &str, literal: &str) -> rumoca_core::Expression {
    var_ref(format!("{type_name}.'{literal}'").as_str())
}

fn scalar_var(name: &str, start: Option<rumoca_core::Expression>) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        start,
        ..dae::Variable::new(
            rumoca_core::VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        )
    }
}

fn array_var(name: &str, dims: Vec<i64>, start: rumoca_core::Expression) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        dims,
        start: Some(start),
        ..dae::Variable::new(
            rumoca_core::VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        )
    }
}

fn ge_time_expr(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Ge,
        lhs: Box::new(var_ref("time")),
        rhs: Box::new(real_lit(value)),
        span: fixture_span(),
    }
}

fn index_expr(
    base: rumoca_core::Expression,
    subscripts: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(base),
        subscripts: subscripts
            .into_iter()
            .map(|expr| rumoca_core::Subscript::generated_expr(Box::new(expr), fixture_span()))
            .collect(),
        span: fixture_span(),
    }
}

fn explicit_eq(lhs: &str, rhs: rumoca_core::Expression, origin: &str) -> dae::Equation {
    dae::Equation::explicit(rumoca_core::VarName::new(lhs), rhs, fixture_span(), origin)
}

#[test]
fn native_no_state_simulation_recomputes_external_constant_segment_time_table() {
    let mut dae_model = dae::Dae::default();
    let span = fixture_span();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("tableID"),
        dae::Variable {
            name: rumoca_core::VarName::new("tableID"),
            start: Some(call_expr(
                "ExternalCombiTimeTable",
                vec![
                    real_lit(0.0),
                    real_lit(0.0),
                    array_expr(
                        vec![
                            array_expr(vec![real_lit(1.0), real_lit(1.0)], false),
                            array_expr(vec![real_lit(3.0), real_lit(0.0)], false),
                        ],
                        true,
                    ),
                    real_lit(0.0),
                    array_expr(vec![int_lit(2)], false),
                    int_lit(3), // ConstantSegments
                    int_lit(1), // HoldLastPoint
                ],
            )),
            ..dae::Variable::new(rumoca_core::VarName::new("tableID"), span)
        },
    );
    dae_model.variables.outputs.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(rumoca_core::VarName::new("y"), span),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable::new(rumoca_core::VarName::new("u"), span),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("b"),
        dae::Variable::new(rumoca_core::VarName::new("b"), span),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("c"),
        dae::Variable::new(rumoca_core::VarName::new("c"), span),
    );
    dae_model.continuous.equations.push(explicit_eq(
        "y",
        call_expr(
            "getTimeTableValueNoDer",
            vec![var_ref("tableID"), int_lit(1), var_ref("time")],
        ),
        "y = table(time)",
    ));
    dae_model
        .continuous
        .equations
        .push(explicit_eq("y", var_ref("u"), "y = u"));
    dae_model.continuous.equations.push(explicit_eq(
        "b",
        rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Ge,
                    lhs: Box::new(var_ref("u")),
                    rhs: Box::new(real_lit(0.5)),
                    span: fixture_span(),
                },
                real_lit(1.0),
            )],
            else_branch: Box::new(real_lit(0.0)),
            span: fixture_span(),
        },
        "b = u >= threshold",
    ));
    dae_model
        .continuous
        .equations
        .push(explicit_eq("b", var_ref("c"), "b = c"));

    let sim = simulate_dae(
        &dae_model,
        &SimOptions {
            t_end: 4.0,
            dt: Some(2.0),
            ..Default::default()
        },
    )
    .expect("MSL-style no-state table model should simulate");

    let y_idx = sim
        .names
        .iter()
        .position(|name| name == "y")
        .expect("trace should contain y");
    // MLS §12.2: pure function equations are evaluated from current inputs;
    // MSL BooleanTable/CombiTimeTable must not freeze the table value at t=0.
    assert_eq!(sim.data[y_idx], vec![1.0, 1.0, 0.0]);
    let c_idx = sim
        .names
        .iter()
        .position(|name| name == "c")
        .expect("trace should contain c");
    assert_eq!(sim.data[c_idx], vec![1.0, 1.0, 0.0]);
}

const LOGIC_TYPE: &str = "Modelica.Electrical.Digital.Interfaces.Logic";
const UX01_TYPE: &str = "Modelica.Electrical.Digital.Interfaces.UX01";
const STRENGTH_TYPE: &str = "Modelica.Electrical.Digital.Interfaces.Strength";

fn logic_ref(literal: &str) -> rumoca_core::Expression {
    enum_ref(LOGIC_TYPE, literal)
}

fn ux01_ref(literal: &str) -> rumoca_core::Expression {
    enum_ref(UX01_TYPE, literal)
}

fn insert_buf3s_enum_ordinals(dae_model: &mut dae::Dae) {
    dae_model.symbols.enum_literal_ordinals.extend([
        (format!("{LOGIC_TYPE}.'U'"), 1),
        (format!("{LOGIC_TYPE}.'X'"), 2),
        (format!("{LOGIC_TYPE}.'0'"), 3),
        (format!("{LOGIC_TYPE}.'1'"), 4),
        (format!("{LOGIC_TYPE}.'Z'"), 5),
        (format!("{UX01_TYPE}.'U'"), 1),
        (format!("{UX01_TYPE}.'X'"), 2),
        (format!("{UX01_TYPE}.'0'"), 3),
        (format!("{UX01_TYPE}.'1'"), 4),
        (format!("{STRENGTH_TYPE}.'S_X01'"), 1),
    ]);
}

fn insert_buf3s_parameters(dae_model: &mut dae::Dae) {
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("e_table.x"),
        array_var(
            "e_table.x",
            vec![3],
            array_expr(vec![logic_ref("0"), logic_ref("1"), logic_ref("Z")], false),
        ),
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("e_table.t"),
        array_var(
            "e_table.t",
            vec![3],
            array_expr(vec![real_lit(0.0), real_lit(5.0), real_lit(9.0)], false),
        ),
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("e_table.y0"),
        scalar_var("e_table.y0", Some(logic_ref("U"))),
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("x_table.x"),
        array_var(
            "x_table.x",
            vec![2],
            array_expr(vec![logic_ref("1"), logic_ref("0")], false),
        ),
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("x_table.t"),
        array_var(
            "x_table.t",
            vec![2],
            array_expr(vec![real_lit(1.0), real_lit(2.0)], false),
        ),
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("x_table.y0"),
        scalar_var("x_table.y0", Some(logic_ref("U"))),
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("bUF3S.strength"),
        scalar_var("bUF3S.strength", Some(enum_ref(STRENGTH_TYPE, "S_X01"))),
    );
}

fn insert_buf3s_discrete_vars(dae_model: &mut dae::Dae) {
    for name in [
        "e_table.y",
        "x_table.y",
        "bUF3S.enable",
        "bUF3S.x",
        "bUF3S.y",
        "bUF3S.nextstate",
        "bUF3S.yy",
        "bUF3S.inertialDelaySensitive.x",
        "bUF3S.inertialDelaySensitive.y",
    ] {
        dae_model.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            scalar_var(name, Some(logic_ref("U"))),
        );
    }
}

fn ux01_conversion_expr(input: rumoca_core::Expression) -> rumoca_core::Expression {
    index_expr(
        array_expr(
            vec![
                ux01_ref("U"),
                ux01_ref("X"),
                ux01_ref("0"),
                ux01_ref("1"),
                ux01_ref("X"),
                ux01_ref("X"),
                ux01_ref("0"),
                ux01_ref("1"),
                ux01_ref("X"),
            ],
            false,
        ),
        vec![input],
    )
}

fn buf3s_truth_table_expr() -> rumoca_core::Expression {
    let plane = array_expr(
        vec![
            array_expr(
                vec![
                    logic_ref("U"),
                    logic_ref("U"),
                    logic_ref("U"),
                    logic_ref("U"),
                ],
                false,
            ),
            array_expr(
                vec![
                    logic_ref("U"),
                    logic_ref("X"),
                    logic_ref("X"),
                    logic_ref("X"),
                ],
                false,
            ),
            array_expr(
                vec![
                    logic_ref("Z"),
                    logic_ref("Z"),
                    logic_ref("Z"),
                    logic_ref("Z"),
                ],
                false,
            ),
            array_expr(
                vec![
                    logic_ref("U"),
                    logic_ref("X"),
                    logic_ref("0"),
                    logic_ref("1"),
                ],
                false,
            ),
        ],
        true,
    );
    array_expr(std::iter::repeat_n(plane, 10).collect(), false)
}

fn buf3s_expr() -> rumoca_core::Expression {
    index_expr(
        buf3s_truth_table_expr(),
        vec![
            var_ref("bUF3S.strength"),
            ux01_conversion_expr(var_ref("bUF3S.enable")),
            ux01_conversion_expr(var_ref("bUF3S.x")),
        ],
    )
}

fn table_output_expr(
    table_name: &str,
    thresholds: &[(f64, i64)],
    fallback: &str,
) -> rumoca_core::Expression {
    rumoca_core::Expression::If {
        branches: thresholds
            .iter()
            .map(|(time, idx)| {
                (
                    ge_time_expr(*time),
                    index_expr(
                        var_ref(format!("{table_name}.x").as_str()),
                        vec![int_lit(*idx)],
                    ),
                )
            })
            .collect(),
        else_branch: Box::new(var_ref(fallback)),
        span: fixture_span(),
    }
}

fn populate_buf3s_equations(dae_model: &mut dae::Dae) {
    dae_model.discrete.valued_updates.push(explicit_eq(
        "bUF3S.yy",
        buf3s_expr(),
        "yy := Buf3sTable[strength, UX01Conv[enable], UX01Conv[x]]",
    ));
    dae_model.discrete.valued_updates.push(explicit_eq(
        "bUF3S.inertialDelaySensitive.y",
        var_ref("bUF3S.inertialDelaySensitive.x"),
        "zero-delay inertial y = x",
    ));
    dae_model.discrete.valued_updates.push(explicit_eq(
        "x_table.y",
        table_output_expr("x_table", &[(2.0, 2), (1.0, 1)], "x_table.y0"),
        "x_table output",
    ));
    dae_model.discrete.valued_updates.push(explicit_eq(
        "e_table.y",
        table_output_expr("e_table", &[(9.0, 3), (5.0, 2), (0.0, 1)], "e_table.y0"),
        "e_table output",
    ));
    dae_model.discrete.valued_updates.push(explicit_eq(
        "bUF3S.nextstate",
        buf3s_expr(),
        "nextstate truth table",
    ));
    dae_model.discrete.valued_updates.push(explicit_eq(
        "bUF3S.inertialDelaySensitive.x",
        var_ref("bUF3S.yy"),
        "connect yy to delay input",
    ));
    dae_model.discrete.valued_updates.push(explicit_eq(
        "bUF3S.y",
        var_ref("bUF3S.inertialDelaySensitive.y"),
        "connect delay output to y",
    ));
    dae_model.discrete.valued_updates.push(explicit_eq(
        "bUF3S.x",
        var_ref("x_table.y"),
        "connect x",
    ));
    dae_model.discrete.valued_updates.push(explicit_eq(
        "bUF3S.enable",
        var_ref("e_table.y"),
        "connect enable",
    ));
}

fn msl_buf3s_no_state_model() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_buf3s_enum_ordinals(&mut dae_model);
    insert_buf3s_parameters(&mut dae_model);
    insert_buf3s_discrete_vars(&mut dae_model);
    populate_buf3s_equations(&mut dae_model);
    dae_model.events.scheduled_time_events.push(0.0);
    dae_model
}

#[test]
fn native_no_state_simulation_settles_msl_buf3s_truth_table_alias_chain() {
    let dae_model = msl_buf3s_no_state_model();
    let sim = simulate_dae(
        &dae_model,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("MSL BUF3S-style no-state DAE should simulate");

    // MLS Appendix B/SIM-001: event iteration solves all discrete equations to
    // a fixed point. The truth-table equation appears before the input alias
    // equations in this DAE, matching the MSL BUF3S shape that regressed.
    let y_idx = sim
        .names
        .iter()
        .position(|name| name == "bUF3S.y")
        .expect("trace should contain bUF3S.y");
    assert_eq!(sim.data[y_idx], vec![5.0, 5.0]);
}

#[test]
fn native_no_state_simulation_refreshes_table_driven_boolean_discrete_chain() {
    let mut dae_model = dae::Dae::default();
    let span = fixture_span();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("tableID"),
        dae::Variable {
            name: rumoca_core::VarName::new("tableID"),
            start: Some(call_expr(
                "ExternalCombiTimeTable",
                vec![
                    real_lit(0.0),
                    real_lit(0.0),
                    array_expr(
                        vec![
                            array_expr(vec![real_lit(1.0), real_lit(1.0)], false),
                            array_expr(vec![real_lit(3.0), real_lit(0.0)], false),
                        ],
                        true,
                    ),
                    real_lit(0.0),
                    array_expr(vec![int_lit(2)], false),
                    int_lit(3), // ConstantSegments
                    int_lit(1), // HoldLastPoint
                ],
            )),
            ..dae::Variable::new(rumoca_core::VarName::new("tableID"), span)
        },
    );
    dae_model.variables.outputs.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(rumoca_core::VarName::new("y"), span),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable::new(rumoca_core::VarName::new("u"), span),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("b"),
        dae::Variable::new(rumoca_core::VarName::new("b"), span),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("c"),
        dae::Variable::new(rumoca_core::VarName::new("c"), span),
    );
    dae_model.continuous.equations.push(explicit_eq(
        "y",
        call_expr(
            "getTimeTableValueNoDer",
            vec![var_ref("tableID"), int_lit(1), var_ref("time")],
        ),
        "y = table(time)",
    ));
    dae_model
        .continuous
        .equations
        .push(explicit_eq("y", var_ref("u"), "y = u"));
    dae_model.discrete.valued_updates.push(explicit_eq(
        "b",
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Ge,
            lhs: Box::new(var_ref("u")),
            rhs: Box::new(real_lit(0.5)),
            span: fixture_span(),
        },
        "b = u >= threshold",
    ));
    dae_model
        .discrete
        .valued_updates
        .push(explicit_eq("c", var_ref("b"), "c = b"));

    let sim = simulate_dae(
        &dae_model,
        &SimOptions {
            t_end: 4.0,
            dt: Some(2.0),
            ..Default::default()
        },
    )
    .expect("MSL-style table-driven Boolean chain should simulate");

    // MLS Appendix B: discrete-valued equations must be settled from current
    // runtime values, not latched to the initial event value.
    let b_idx = sim
        .names
        .iter()
        .position(|name| name == "b")
        .expect("trace should contain b");
    assert_eq!(sim.data[b_idx], vec![1.0, 1.0, 0.0]);
    let c_idx = sim
        .names
        .iter()
        .position(|name| name == "c")
        .expect("trace should contain c");
    assert_eq!(sim.data[c_idx], vec![1.0, 1.0, 0.0]);
}

#[test]
fn source_table_algorithm_keeps_last_write_at_each_threshold() {
    let source = r#"
block TwoPointTable
  parameter Integer x[:] = {4};
  parameter Real t[size(x, 1)] = {1};
  parameter Integer y0 = 1;
  final parameter Integer n = size(x, 1);
  output Integer y;
algorithm
  y := y0;
  for i in 1:n loop
    if time >= t[i] then
      y := x[i];
    end if;
  end for;
end TwoPointTable;

model TwoThresholdTable
  TwoPointTable table(y0 = 3, x = {4, 3}, t = {1, 3});
end TwoThresholdTable;
"#;
    let compiled = Compiler::new()
        .model("TwoThresholdTable")
        .compile_str(source, "TwoThresholdTable.mo")
        .expect("two-threshold table fixture should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 4.0,
            dt: Some(1.0),
            ..Default::default()
        },
    )
    .expect("two-threshold table fixture should simulate");
    let y_idx = sim
        .names
        .iter()
        .position(|name| name == "table.y")
        .expect("trace should contain table.y");
    let value_at = |time: f64| {
        sim.times
            .iter()
            .zip(&sim.data[y_idx])
            .rev()
            .find(|(sample_time, _)| (**sample_time - time).abs() <= 1.0e-12)
            .map(|(_, value)| *value)
            .unwrap_or_else(|| panic!("trace should contain t={time}"))
    };

    // MLS §11.1/§11.2 and Appendix B: statements and loop iterations execute
    // sequentially, so the last active assignment owns the algorithm output.
    assert_eq!(value_at(2.0), 4.0, "first threshold should select x[1]");
    assert_eq!(value_at(3.0), 3.0, "second threshold should select x[2]");
}

const MODIFIED_TABLES_SOURCE: &str = r#"
package Modelica
package Blocks
package Types
function ExternalCombiTimeTable
  input String tableName;
  input String fileName;
  input Real table[:, :];
  input Real startTime;
  input Integer columns[:];
  input Integer smoothness;
  input Integer extrapolation;
  input Real shiftTime = 0.0;
  input Integer timeEvents = 1;
  input Boolean verboseRead = false;
  input String delimiter = ",";
  input Integer nHeaderLines = 0;
  output Real tableID;
external "C" tableID = ModelicaStandardTables_CombiTimeTable_init3(
  fileName, tableName, table, size(table, 1), size(table, 2), startTime,
  columns, size(columns, 1), smoothness, extrapolation, shiftTime, timeEvents,
  verboseRead, delimiter, nHeaderLines);
end ExternalCombiTimeTable;
end Types;

package Tables
package Internal
pure function getTimeTableValueNoDer
  input Real tableID;
  input Integer icol;
  input Real timeIn;
  input Real nextTimeEvent;
  input Real pre_nextTimeEvent;
  output Real y;
external "C" y = ModelicaStandardTables_CombiTimeTable_getValue(
  tableID, icol, timeIn, nextTimeEvent, pre_nextTimeEvent);
end getTimeTableValueNoDer;
end Internal;
end Tables;
end Blocks;
end Modelica;

block CombiTimeTable
  parameter Real table[:, :];
  parameter Integer columns[:] = {2};
  parameter Integer smoothness = 3;
  parameter Integer extrapolation = 1;
  output Real y;
protected
  parameter Real tableID = Modelica.Blocks.Types.ExternalCombiTimeTable(
    "NoName", "NoName", table, 0.0, columns, smoothness, extrapolation,
    0.0, 1, false, ",", 0);
equation
  y = Modelica.Blocks.Tables.Internal.getTimeTableValueNoDer(
    tableID, 1, time, 0.0, 0.0);
end CombiTimeTable;

block BooleanTable
  parameter Real table[:] = {0, 1};
  parameter Boolean startValue = false;
  final parameter Integer n = size(table, 1);
  output Boolean y;
  CombiTimeTable combiTimeTable(
    final table = if n > 0 then
      if startValue then
        [table[1], 1.0; table, {mod(i + 1, 2.0) for i in 1:n}]
      else
        [table[1], 0.0; table, {mod(i, 2.0) for i in 1:n}]
      else [0.0, 0.0]);
equation
  y = combiTimeTable.y >= 0.5;
end BooleanTable;

block IntegerTable
  parameter Real table[:, 2] = [0, 0];
  output Integer y;
  CombiTimeTable combiTimeTable(final table = table);
equation
  y = integer(combiTimeTable.y);
end IntegerTable;

model ModifiedTables
  BooleanTable booleanTable(table = {.05, .15});
  IntegerTable integerTable(table = [0, 1; .025, 2; .05, 0; .075, -1]);
end ModifiedTables;
"#;

struct LookupCallCounter(usize);

impl rumoca_core::ExpressionVisitor for LookupCallCounter {
    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        let segments = name.segments();
        if name.last_segment() == "getTimeTableValueNoDer"
            || (segments.last() == Some(&"y")
                && segments.get(segments.len().saturating_sub(2))
                    == Some(&"getTimeTableValueNoDer"))
        {
            self.0 += 1;
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

#[derive(Default)]
struct TableLookupCounter(usize);

impl SolveVisitor for TableLookupCounter {
    type Error = std::convert::Infallible;

    fn visit_linear_op(
        &mut self,
        _kind: LinearOpSliceKind,
        _op_index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        if matches!(op, LinearOp::TableLookup { .. }) {
            self.0 += 1;
        }
        Ok(())
    }
}

fn prepared_lookup_call_count(prepared_dae: &dae::Dae) -> usize {
    let mut counter = LookupCallCounter(0);
    for equation in &prepared_dae.continuous.equations {
        rumoca_core::ExpressionVisitor::visit_expression(&mut counter, &equation.rhs);
    }
    counter.0
}

fn solve_lookup_count(solve_model: &rumoca_ir_solve::SolveModel) -> usize {
    let mut counter = TableLookupCounter::default();
    counter
        .visit_solve_model(solve_model)
        .expect("infallible SolveModel traversal");
    counter.0
}

fn lookup_table(tables: &[rumoca_core::ExternalTableData], table_id: u64, time: f64) -> f64 {
    let row = [
        LinearOp::Const {
            dst: 0,
            value: table_id as f64,
        },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const {
            dst: 2,
            value: time,
        },
        LinearOp::TableLookup {
            dst: 3,
            table_id: 0,
            column: 1,
            input: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    rumoca_eval_solve::eval_row_with_context(
        &row,
        &[],
        &[],
        time,
        rumoca_eval_solve::RowEvalContext {
            external_tables: Some(tables),
            ..Default::default()
        },
    )
    .expect("materialized SolveModel TableLookup op should evaluate")
}

#[test]
fn source_instance_modifiers_materialize_boolean_and_integer_time_tables() {
    let compiled = Compiler::new()
        .model("ModifiedTables")
        .compile_str(MODIFIED_TABLES_SOURCE, "ModifiedTables.mo")
        .expect("real Modelica table fixture should compile");
    let options = SimOptions {
        t_end: 0.15,
        dt: Some(0.001),
        ..Default::default()
    };
    let prepared_dae =
        rumoca_sim::structurally_prepared_dae_for_simulation_artifact(&compiled.dae, &options)
            .expect("real Modelica table fixture should survive structural preparation");
    assert_eq!(
        prepared_lookup_call_count(&prepared_dae),
        2,
        "numeric table equations must not be pruned as String metadata"
    );
    let solve_model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &options)
        .expect("real Modelica table fixture should lower to simulation SolveModel");
    let tables = solve_model.external_tables.as_slice();

    assert!(
        tables
            .iter()
            .any(|table| table.data == vec![vec![0.05, 0.0], vec![0.05, 1.0], vec![0.15, 0.0]]),
        "BooleanTable modifier must materialize its duplicate-knot matrix; tables={tables:?}"
    );
    assert!(
        tables.iter().any(|table| table.data
            == vec![
                vec![0.0, 1.0],
                vec![0.025, 2.0],
                vec![0.05, 0.0],
                vec![0.075, -1.0],
            ]),
        "IntegerTable modifier must materialize its matrix; tables={tables:?}"
    );

    assert!(
        solve_lookup_count(&solve_model) >= 2,
        "both qualified getTimeTableValueNoDer equations must survive structural lowering"
    );

    let boolean_table = tables
        .iter()
        .find(|table| table.data.len() == 3)
        .expect("BooleanTable data");
    let integer_table = tables
        .iter()
        .find(|table| table.data.len() == 4)
        .expect("IntegerTable data");
    assert_eq!(lookup_table(tables, boolean_table.id, 0.049), 0.0);
    assert_eq!(lookup_table(tables, boolean_table.id, 0.05), 1.0);
    assert_eq!(lookup_table(tables, boolean_table.id, 0.149), 1.0);
    assert_eq!(lookup_table(tables, boolean_table.id, 0.15), 0.0);
    assert_eq!(lookup_table(tables, integer_table.id, 0.0), 1.0);
}
