//! End-to-end coverage for [`lower_to_algorithm_code`], the crate's only
//! public lowering entry point.
//!
//! Every other test in this crate reaches a private helper, so the suite can
//! be green while the *assembled* package — interface ordering, protected
//! state, the clock constant, `Startup`/`Recalibrate`/`DoStep` bodies, and the
//! nominal/ordinal correlations — is wrong. These tests drive the public
//! function on a checked DAE and assert on the whole returned package.
//!
//! The assertion is a rendering of the complete block rather than a spot
//! check: [`render_block`] walks every declaration and every statement of all
//! three methods and panics on any construct it does not know, so a statement
//! kind that stops being emitted, an extra statement, an inverted guard, or a
//! reordered interface all change the compared text.

use rumoca_core::{ClockLattice, ClockRational, SourceId, SourceMap, Span, TypeId, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_galec::ast as gast;
use rumoca_ir_galec::package::AlgorithmCodePackage;
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};

/// Exact provenance for `needle` inside `text`.
fn at(source: SourceId, text: &str, needle: &str) -> dae::DaeProvenance {
    let start = text.find(needle).expect("fixture text contains snippet");
    dae::DaeProvenance::source(Span::from_offsets(source, start, start + needle.len()))
        .expect("fixture provenance is exact")
}

/// The fixture Modelica this DAE stands for:
///
/// ```modelica
/// block TickOrder
///   input Real u;
///   output Real y;
/// protected
///   discrete Real level;
/// equation
///   when sample(0, 1) and level > 0.5 then   // declared FIRST
///     y = 10.0;
///   end when;
///   when sample(0, 1) then                   // declared SECOND
///     level = u;
///   end when;
/// end TickOrder;
/// ```
///
/// The guarded row for `y` is declared before the row that produces `level`,
/// and `y`'s *value* (`10.0`) reads nothing. The only same-tick dependency
/// lives in the guard `level > 0.5`. A projection that ignores reads inside a
/// guard therefore emits `y` before `level` and reads a stale value — which is
/// exactly what this fixture is shaped to catch.
fn tick_order_model() -> dae::Dae {
    guarded_model(Guard::ReadsProducer)
}

/// What the `y` row's guard is made of.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Guard {
    /// `level > 0.5` — a same-tick read of the row declared after it.
    ReadsProducer,
    /// `true` — the same block shape with no same-tick read at all. This is
    /// the counterfactual control for
    /// [`a_guard_that_reads_a_discrete_value_forces_its_producer_earlier_in_the_tick`]:
    /// with the read removed the projection falls back to declaration order,
    /// which is what proves the reordering in the other fixture is caused by
    /// the guard and not by some incidental tie-break.
    Constant,
}

#[derive(Clone, Copy)]
struct GuardedSpans {
    input: dae::DaeProvenance,
    output: dae::DaeProvenance,
    level: dae::DaeProvenance,
    clock: dae::DaeProvenance,
    guard: dae::DaeProvenance,
    output_equation: dae::DaeProvenance,
    level_equation: dae::DaeProvenance,
}

#[derive(Clone, Copy)]
struct GuardedVariables<'dae> {
    input: dae::InputId<'dae>,
    output: dae::DiscreteRealId<'dae>,
    level: dae::DiscreteRealId<'dae>,
}

#[derive(Clone, Copy)]
struct GuardedExpressions<'dae> {
    guard: dae::ExprId<'dae>,
    output_lhs: dae::ExprId<'dae>,
    output_rhs: dae::ExprId<'dae>,
    level_lhs: dae::ExprId<'dae>,
    level_rhs: dae::ExprId<'dae>,
}

fn define_guarded_variables<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    spans: GuardedSpans,
) -> Result<GuardedVariables<'dae>, dae::DaeConstructionError> {
    dae.variables(|variables| {
        let input = variables.input(
            VarName::new("u"),
            real,
            dae::InputVariability::Continuous,
            spans.input,
            dae::VariableAttributes {
                causality: dae::VariableCausality::Input,
                ..dae::VariableAttributes::default()
            },
        )?;
        let output = variables.discrete_real(
            VarName::new("y"),
            real,
            spans.output,
            dae::VariableAttributes {
                causality: dae::VariableCausality::Output,
                ..dae::VariableAttributes::default()
            },
        )?;
        let level = variables.discrete_real(
            VarName::new("level"),
            real,
            spans.level,
            dae::VariableAttributes::default(),
        )?;
        Ok(GuardedVariables {
            input,
            output,
            level,
        })
    })
}

fn define_guard_expression<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    guard: Guard,
    level: dae::DiscreteRealId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    match guard {
        Guard::ReadsProducer => {
            let level_read = expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::DiscreteReal(level))?;
            let threshold = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(0.5))?;
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Greater, level_read, threshold)
        }
        Guard::Constant => expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Boolean(true)),
    }
}

fn define_guarded_expressions<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    guard: Guard,
    variables: GuardedVariables<'dae>,
    spans: GuardedSpans,
) -> Result<GuardedExpressions<'dae>, dae::DaeConstructionError> {
    dae.expressions(|expressions| {
        Ok(GuardedExpressions {
            guard: define_guard_expression(expressions, guard, variables.level, spans.guard)?,
            output_lhs: expressions
                .at(spans.output_equation)
                .coordinate(dae::CoordinateInput::DiscreteReal(variables.output))?,
            output_rhs: expressions
                .at(spans.output_equation)
                .literal(dae::DaeLiteral::Real(10.0))?,
            level_lhs: expressions
                .at(spans.level_equation)
                .coordinate(dae::CoordinateInput::DiscreteReal(variables.level))?,
            level_rhs: expressions
                .at(spans.level_equation)
                .coordinate(dae::CoordinateInput::Input(variables.input))?,
        })
    })
}

fn define_guarded_clock<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    variables: GuardedVariables<'dae>,
    expressions: GuardedExpressions<'dae>,
    spans: GuardedSpans,
) -> Result<(), dae::DaeConstructionError> {
    let clock = dae.clocks(|clocks| {
        clocks.periodic(
            ClockLattice::new(ClockRational::ONE, ClockRational::ZERO)
                .expect("fixture lattice is valid"),
            spans.clock,
        )
    })?;
    let clock = dae::ClockId::from(clock);
    dae.clocks(|clocks| {
        clocks.own_discrete_real(clock, variables.output, spans.output)?;
        clocks.own_discrete_real(clock, variables.level, spans.level)?;
        Ok(())
    })?;
    let (tick, guarded) = dae.conditions(|conditions| {
        let tick = conditions.reserve(spans.clock)?;
        conditions.define(tick, dae::ConditionInput::Clock(clock), spans.clock)?;
        let level_above = conditions.reserve(spans.guard)?;
        conditions.define(
            level_above,
            dae::ConditionInput::Discrete(expressions.guard),
            spans.guard,
        )?;
        let guarded = conditions.reserve(spans.clock)?;
        conditions.define(
            guarded,
            dae::ConditionInput::And(tick, level_above),
            spans.clock,
        )?;
        Ok((tick, guarded))
    })?;
    dae.discrete(|discrete| {
        discrete.when_real_equation(tick, guarded, spans.output_equation, |equation| {
            equation.equal(expressions.output_lhs, expressions.output_rhs)?;
            Ok(())
        })?;
        discrete.when_real_equation(tick, tick, spans.level_equation, |equation| {
            equation.equal(expressions.level_lhs, expressions.level_rhs)?;
            Ok(())
        })?;
        Ok(())
    })
}

fn guarded_model(guard: Guard) -> dae::Dae {
    let text = "input Real u; output Real y; discrete Real level; \
                when sample(0, 1) and level > 0.5 then y = 10.0; end when; \
                when sample(0, 1) then level = u; end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("TickOrder.mo", text);
    let spans = GuardedSpans {
        input: at(source, text, "input Real u"),
        output: at(source, text, "output Real y"),
        level: at(source, text, "discrete Real level"),
        clock: at(source, text, "sample(0, 1) and level > 0.5"),
        guard: at(source, text, "level > 0.5"),
        output_equation: at(source, text, "y = 10.0"),
        level_equation: at(source, text, "level = u"),
    };
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                spans.input,
            )
        })?;
        let variables = define_guarded_variables(dae, real, spans)?;
        let expressions = define_guarded_expressions(dae, guard, variables, spans)?;
        define_guarded_clock(dae, variables, expressions, spans)
    })
    .expect("checked TickOrder fixture constructs")
}

fn dependent_parameter_model() -> dae::Dae {
    let text =
        "parameter Real gain = 2.0; parameter Real derived = 3.0 * gain; Clock c = Clock(1);";
    let mut sources = SourceMap::new();
    let source = sources.add("DependentParameter.mo", text);
    let gain_at = at(source, text, "parameter Real gain = 2.0");
    let derived_at = at(source, text, "parameter Real derived = 3.0 * gain");
    let clock_at = at(source, text, "Clock c = Clock(1)");
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                gain_at,
            )
        })?;
        let ((gain, gain_reservation), (_derived, derived_reservation)) =
            dae.variables(|variables| {
                Ok((
                    variables.reserve_parameter(VarName::new("gain"), real, gain_at)?,
                    variables.reserve_parameter(VarName::new("derived"), real, derived_at)?,
                ))
            })?;
        let (gain_default, derived_binding) = dae.expressions(|expressions| {
            let gain_default = expressions
                .at(gain_at)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let gain_value = expressions
                .at(derived_at)
                .coordinate(dae::CoordinateInput::Parameter(gain))?;
            let three = expressions
                .at(derived_at)
                .literal(dae::DaeLiteral::Real(3.0))?;
            let derived_binding = expressions.at(derived_at).binary(
                dae::BinaryOperator::Multiply,
                three,
                gain_value,
            )?;
            Ok((gain_default, derived_binding))
        })?;
        dae.variables(|variables| {
            variables.define(
                gain_reservation,
                dae::VariableAttributes {
                    binding: Some(gain_default),
                    is_tunable: true,
                    ..Default::default()
                },
                gain_at,
            )?;
            variables.define(
                derived_reservation,
                dae::VariableAttributes {
                    binding: Some(derived_binding),
                    is_tunable: true,
                    ..Default::default()
                },
                derived_at,
            )
        })?;
        dae.clocks(|clocks| {
            clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
                clock_at,
            )?;
            Ok(())
        })
    })
    .expect("checked dependent-parameter fixture constructs")
}

fn project(model: &dae::Dae, name: &str) -> AlgorithmCodePackage {
    lower_to_algorithm_code(&GalecInput::new(model, name), &GalecOptions::default())
        .unwrap_or_else(|errors| panic!("fixture must project: {errors:?}"))
}

// ---------------------------------------------------------------------------
// Total rendering of the projected block.
// ---------------------------------------------------------------------------

fn render_type(decl: &gast::VariableDeclaration) -> String {
    let scalar = match &decl.ty {
        gast::TypeRef::Primitive(scalar) => scalar.keyword().to_owned(),
        gast::TypeRef::Compartment(name) => name.lexeme().to_owned(),
    };
    if decl.dimensions.is_empty() {
        scalar
    } else {
        format!("{scalar}[{}]", decl.dimensions.len())
    }
}

fn render_reference(reference: &gast::Reference) -> String {
    let (prefix, parts) = match reference {
        gast::Reference::Local(part) => ("", std::slice::from_ref(part)),
        gast::Reference::State(parts) => ("self.", parts.as_slice()),
    };
    let rendered = parts
        .iter()
        .map(|part| {
            if part.subscripts.is_empty() {
                part.name.lexeme().to_owned()
            } else {
                format!(
                    "{}[{}]",
                    part.name.lexeme(),
                    part.subscripts
                        .iter()
                        .map(render_expression)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        })
        .collect::<Vec<_>>()
        .join(".");
    format!("{prefix}{rendered}")
}

fn render_expression(expression: &gast::Expression) -> String {
    match expression {
        gast::Expression::Bool(value) => value.to_string(),
        gast::Expression::Integer(value) => value.to_string(),
        gast::Expression::Real(value) => format!("{value:?}"),
        gast::Expression::Ref(reference) => render_reference(reference),
        gast::Expression::Neg(reference) => format!("-{}", render_reference(reference)),
        gast::Expression::Not(inner) => format!("not ({})", render_expression(inner)),
        gast::Expression::Paren(inner) => format!("({})", render_expression(inner)),
        gast::Expression::Binary { op, lhs, rhs } => format!(
            "{} {} {}",
            render_expression(lhs),
            op.token(),
            render_expression(rhs)
        ),
        // Deliberately total: a fixture that starts producing one of these is
        // a change in what the entry point emits and must be re-read, not
        // silently rendered as a placeholder.
        other => panic!("fixture rendering does not cover {other:?}"),
    }
}

fn render_statement(statement: &gast::Statement, indent: usize, out: &mut Vec<String>) {
    let pad = " ".repeat(indent);
    match statement {
        gast::Statement::Assignment { target, value } => out.push(format!(
            "{pad}{} := {};",
            render_reference(target),
            render_expression(value)
        )),
        gast::Statement::If(conditional) => {
            for (position, branch) in conditional.branches.iter().enumerate() {
                let keyword = if position == 0 { "if" } else { "elseif" };
                let gast::Condition::Expression(condition) = &branch.condition else {
                    panic!("fixture rendering does not cover signal conditions")
                };
                out.push(format!(
                    "{pad}{keyword} {} then",
                    render_expression(condition)
                ));
                for statement in &branch.body {
                    render_statement(&statement.node, indent + 2, out);
                }
            }
            if let Some(else_body) = &conditional.else_body {
                out.push(format!("{pad}else"));
                for statement in else_body {
                    render_statement(&statement.node, indent + 2, out);
                }
            }
            out.push(format!("{pad}end if;"));
        }
        other => panic!("fixture rendering does not cover {other:?}"),
    }
}

fn render_method(name: &str, method: &gast::BlockMethod, out: &mut Vec<String>) {
    out.push(format!("  {name}:"));
    for local in &method.locals {
        out.push(format!(
            "    local {} {};",
            render_type(local),
            local.name.lexeme()
        ));
    }
    for statement in &method.statements {
        render_statement(&statement.node, 4, out);
    }
}

/// Render the entire projected block: name, interface, compartments,
/// protected entities, error signals, protected functions, and all three
/// method bodies.
fn render_block(package: &AlgorithmCodePackage) -> String {
    let block = package.block();
    let mut out = vec![format!("block {}", block.name.lexeme())];
    for variable in &block.interface {
        let keyword = match variable.kind {
            gast::InterfaceKind::Input => "input",
            gast::InterfaceKind::Output => "output",
            gast::InterfaceKind::TunableParameter => "parameter",
        };
        out.push(format!(
            "  {keyword} {} {};",
            render_type(&variable.decl),
            variable.decl.name.lexeme()
        ));
    }
    for compartment in &block.compartments {
        out.push(format!("  record {}", compartment.name.lexeme()));
        for entity in &compartment.entities {
            out.push(format!(
                "    {} {};",
                render_type(&entity.decl),
                entity.decl.name.lexeme()
            ));
        }
    }
    for entity in &block.protected {
        let keyword = match entity.kind {
            gast::ProtectedKind::DependentParameter => "parameter",
            gast::ProtectedKind::Constant => "constant",
            gast::ProtectedKind::State => "state",
        };
        let start = entity
            .start
            .as_ref()
            .map(|start| format!(" = {}", render_expression(start)))
            .unwrap_or_default();
        out.push(format!(
            "  {keyword} {} {}{start};",
            render_type(&entity.decl),
            entity.decl.name.lexeme()
        ));
    }
    for signal in &block.error_signals {
        out.push(format!("  signal {};", signal.as_str()));
    }
    for function in &block.protected_functions {
        out.push(format!(
            "  {} {};",
            function.kind.keyword(),
            function.name.lexeme()
        ));
    }
    render_method("startup", &block.startup, &mut out);
    render_method("recalibrate", &block.recalibrate, &mut out);
    render_method("do_step", &block.do_step, &mut out);
    out.join("\n")
}

/// Statement lines of `DoStep` only, trimmed — used where the assertion is
/// about tick order rather than about the whole module.
fn do_step_lines(package: &AlgorithmCodePackage) -> Vec<String> {
    let mut out = Vec::new();
    for statement in &package.block().do_step.statements {
        render_statement(&statement.node, 0, &mut out);
    }
    out
}

// ---------------------------------------------------------------------------
// Tests.
// ---------------------------------------------------------------------------

#[test]
fn lower_to_algorithm_code_projects_the_whole_module() {
    let package = project(&tick_order_model(), "TickOrder");

    assert_eq!(
        render_block(&package),
        "\
block TickOrder
  input Real u;
  output Real y;
  state Real level = 0.0;
  constant Real samplePeriod = 1.0;
  startup:
    self.y := 0.0;
    self.level := 0.0;
    self.samplePeriod := 1.0;
  recalibrate:
  do_step:
    self.level := self.u;
    if self.level > 0.5 then
      self.y := 10.0;
    end if;",
        "the projected module changed; re-read the diff before updating this"
    );
}

#[test]
fn dependent_parameter_binding_is_recomputed_during_recalibrate() {
    let package = project(&dependent_parameter_model(), "DependentParameter");
    let block = package.block();

    assert!(block.interface.iter().any(|variable| {
        variable.kind == gast::InterfaceKind::TunableParameter
            && variable.decl.name.lexeme() == "gain"
    }));
    assert!(block.protected.iter().any(|variable| {
        variable.kind == gast::ProtectedKind::DependentParameter
            && variable.decl.name.lexeme() == "derived"
    }));
    assert_eq!(block.recalibrate.statements.len(), 1);
    assert!(matches!(
        &block.recalibrate.statements[0].node,
        gast::Statement::Assignment { target, value }
            if render_reference(target) == "self.derived"
                && render_expression(value).contains("self.gain")
    ));
}

#[test]
fn the_package_metadata_correlates_with_the_block_declarations() {
    let package = project(&tick_order_model(), "TickOrder");
    let block = package.block();

    let declaration_count = block.interface.len()
        + block.protected.len()
        + block
            .compartments
            .iter()
            .map(|compartment| compartment.entities.len())
            .sum::<usize>();
    assert_eq!(
        package.variable_nominals().len(),
        declaration_count,
        "one nominal slot per declared block variable"
    );

    let ordinal = package.clock_variable_ordinal();
    assert!(ordinal >= 1, "the clock ordinal is one-based");
    let clock_name = block
        .interface
        .iter()
        .map(|variable| variable.decl.name.lexeme().to_owned())
        .chain(
            block
                .protected
                .iter()
                .map(|entity| entity.decl.name.lexeme().to_owned()),
        )
        .nth(ordinal - 1)
        .expect("the clock ordinal addresses a declared variable");
    assert_eq!(clock_name, "samplePeriod");
}

#[test]
fn a_guard_that_reads_a_discrete_value_forces_its_producer_earlier_in_the_tick() {
    let package = project(&tick_order_model(), "TickOrder");
    let lines = do_step_lines(&package);

    let producer = lines
        .iter()
        .position(|line| line == "self.level := self.u;")
        .unwrap_or_else(|| panic!("the producer assignment must be emitted: {lines:?}"));
    let guard = lines
        .iter()
        .position(|line| line == "if self.level > 0.5 then")
        .unwrap_or_else(|| panic!("the guard must read the producer's current value: {lines:?}"));

    assert!(
        producer < guard,
        "`level` is produced after the guard that reads it, so the guard sees \
         the previous tick's value: {lines:?}"
    );

    // Counterfactual control: the identical block with `true` in place of the
    // read keeps the declaration order, so the reordering above is caused by
    // the guard's read and by nothing else in this fixture.
    let control = project(&guarded_model(Guard::Constant), "TickOrder");
    assert_eq!(
        do_step_lines(&control),
        [
            "if true then".to_owned(),
            "  self.y := 10.0;".to_owned(),
            "end if;".to_owned(),
            "self.level := self.u;".to_owned(),
        ],
        "without a same-tick read the rows keep their declaration order"
    );
}

#[test]
fn a_model_without_a_periodic_clock_is_rejected_by_the_entry_point() {
    let text = "Real x;";
    let mut sources = SourceMap::new();
    let source = sources.add("NoClock.mo", text);
    let declaration = at(source, text, "Real x");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        Ok(())
    })
    .expect("checked clock-free fixture constructs");

    let errors = lower_to_algorithm_code(
        &GalecInput::new(&model, "NoClock"),
        &GalecOptions::default(),
    )
    .expect_err("GALEC requires one admitted periodic clock");
    assert!(
        errors
            .iter()
            .any(|error| matches!(error, rumoca_phase_galec::GalecTargetError::NoPeriodicClock)),
        "{errors:?}"
    );
}

#[test]
fn the_block_name_option_overrides_the_model_name() {
    let model = tick_order_model();
    let package = lower_to_algorithm_code(
        &GalecInput::new(&model, "TickOrder"),
        &GalecOptions {
            block_name: Some("Renamed".to_owned()),
            ..GalecOptions::default()
        },
    )
    .expect("fixture projects");

    assert_eq!(package.block().name.lexeme(), "Renamed");
}
