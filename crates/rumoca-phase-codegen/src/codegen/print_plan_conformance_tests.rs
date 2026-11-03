//! Rule 25 executable-plan conformance for typed bodies (print-plan schema
//! v0): a typed function body prints its issued print plan and nothing else.
//! Every printed statement decodes through the one reviewed spelling of its
//! item and must equal the plan item; the whole body is consumed. An
//! operation the plan has not projected yet must name its audit ledger row
//! and print exactly what its unmigrated macro prints for it alone.

use rumoca_ir_solve as solve;
use solve::{
    SolveBodyItem as Body, SolveElementToken as Element, SolveFrameItem as Frame,
    SolveNameSpace as Space,
};

use super::codegen_test_support::builtin_template;

mod scalar;
mod tensor;

const OPENING: &str = "(const void* const in[], void* const out[], const RmcAssertionObserver* observer) {\n(void)in; (void)out; (void)observer; (void)rumoca_real64;\n";
const CLOSING: &str = "return 0;\n}\n";

/// One printed statement, decoded.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Decoded {
    ArgumentView {
        slot: usize,
        argument: usize,
        element: Element,
    },
    Frame {
        space: Space,
        id: usize,
        element: Element,
        extent: u32,
    },
    FrameFromArgument {
        slot: usize,
        argument: usize,
        element: Element,
        extent: u32,
    },
    View {
        register: usize,
        slot: usize,
        element: Element,
    },
    Copy {
        destination: (Space, usize),
        source: (Space, usize),
    },
    Output {
        slot: usize,
        output: usize,
    },
}

fn expected_frame(item: &Frame) -> Decoded {
    match *item {
        Frame::ArgumentView {
            slot,
            argument,
            element,
        } => Decoded::ArgumentView {
            slot,
            argument,
            element,
        },
        Frame::Frame {
            name,
            element,
            extent,
        } => Decoded::Frame {
            space: name.space(),
            id: name.id(),
            element,
            extent,
        },
        Frame::FrameFromArgument {
            slot,
            argument,
            element,
            extent,
        } => Decoded::FrameFromArgument {
            slot,
            argument,
            element,
            extent,
        },
    }
}

fn element(spelling: &str) -> Option<Element> {
    match spelling {
        "double" => Some(Element::Real64),
        "int64_t" => Some(Element::Int64),
        "bool" => Some(Element::Boolean),
        _ => None,
    }
}

/// A decimal token in its one canonical spelling: C reads a leading zero
/// as octal, so `010` is not the issued 10 and must not decode as it.
fn number<T: std::str::FromStr + ToString>(text: &str) -> Option<T> {
    let value: T = text.parse().ok()?;
    (value.to_string() == text).then_some(value)
}

/// `<p><id>` for prefix `s` or `r`.
fn name(text: &str) -> Option<(Space, usize)> {
    let (space, id) = match text.split_at_checked(1)? {
        ("s", id) => (Space::Slot, id),
        ("r", id) => (Space::Register, id),
        _ => return None,
    };
    Some((space, number(id)?))
}

/// One decoded declaration: element token, name and extent.
type Declaration = (Element, (Space, usize), u32);

/// `T <n>[E]; (void)<n>;` declarations, one after another.
fn declarations(mut line: &str) -> Option<Vec<Declaration>> {
    let mut found = Vec::new();
    while !line.is_empty() {
        let (spelling, rest) = line.split_once(' ')?;
        let (declared, rest) = rest.split_once('[')?;
        let (extent, rest) = rest.split_once("]; (void)")?;
        let (voided, rest) = rest.split_once(';')?;
        if voided != declared {
            return None;
        }
        found.push((element(spelling)?, name(declared)?, number(extent)?));
        line = rest;
    }
    Some(found)
}

/// `const T* <a> = <b>; (void)<a>;`
fn pointer(line: &str) -> Option<(Element, &str, &str)> {
    let rest = line.strip_prefix("const ")?;
    let (spelling, rest) = rest.split_once("* ")?;
    let (target, rest) = rest.split_once(" = ")?;
    let (source, rest) = rest.split_once("; (void)")?;
    (rest == format!("{target};")).then_some((element(spelling)?, target, source))
}

/// `memcpy(<a>, <b>, sizeof(<a>));`
fn copy(line: &str) -> Option<(&str, &str)> {
    let rest = line.strip_prefix("memcpy(")?;
    let (destination, rest) = rest.split_once(", ")?;
    let (source, rest) = rest.split_once(", sizeof(")?;
    (rest == format!("{destination}));")).then_some((destination, source))
}

fn argument(text: &str) -> Option<usize> {
    number(text.strip_prefix("in[")?.strip_suffix(']')?)
}

fn decode_slot(line: &str, next: Option<&str>) -> Option<(Decoded, bool)> {
    if let Some((element, target, source)) = pointer(line) {
        let (Space::Slot, slot) = name(target)? else {
            return None;
        };
        return Some((
            Decoded::ArgumentView {
                slot,
                argument: argument(source)?,
                element,
            },
            false,
        ));
    }
    let [(element, (Space::Slot, slot), extent)] = declarations(line)?[..] else {
        return None;
    };
    let copied = next.and_then(copy).and_then(|(destination, source)| {
        (name(destination) == Some((Space::Slot, slot)))
            .then(|| argument(source))
            .flatten()
    });
    Some(match copied {
        Some(argument) => (
            Decoded::FrameFromArgument {
                slot,
                argument,
                element,
                extent,
            },
            true,
        ),
        None => (
            Decoded::Frame {
                space: Space::Slot,
                id: slot,
                element,
                extent,
            },
            false,
        ),
    })
}

fn decode_body(line: &str) -> Option<Decoded> {
    if let Some((element, target, source)) = pointer(line) {
        let ((Space::Register, register), (Space::Slot, slot)) = (name(target)?, name(source)?)
        else {
            return None;
        };
        return Some(Decoded::View {
            register,
            slot,
            element,
        });
    }
    let (destination, source) = copy(line)?;
    Some(Decoded::Copy {
        destination: name(destination)?,
        source: name(source)?,
    })
}

fn decode_output(line: &str) -> Option<Decoded> {
    let rest = line.strip_prefix("if (out[")?;
    let (output, rest) = rest.split_once("]) memcpy(out[")?;
    let (again, rest) = rest.split_once("], ")?;
    let (slot, rest) = rest.split_once(", sizeof(")?;
    let (Space::Slot, id) = name(slot)? else {
        return None;
    };
    (again == output && rest == format!("{slot}));")).then_some(Decoded::Output {
        slot: id,
        output: number(output)?,
    })
}

struct Printer {
    environment: minijinja::Environment<'static>,
}

impl Printer {
    fn new(functions: String) -> Self {
        let mut environment = super::create_environment();
        for (name, file) in [
            ("fmi-typed-arithmetic.jinja", "typed_arithmetic.jinja"),
            ("fmi-typed-tensors.jinja", "typed_tensors.jinja"),
        ] {
            environment
                .add_template(name, builtin_template("fmi3", file))
                .expect("built-in template");
        }
        environment
            .add_template_owned("functions", functions)
            .expect("functions template");
        Self { environment }
    }

    fn builtin() -> Self {
        Self::new(builtin_template("fmi3", "typed_functions.jinja").to_owned())
    }

    fn definition(&self, program: &serde_json::Value, name: &str) -> String {
        self.environment
            .render_str(
                "{% from 'functions' import definition %}{{ definition(program, name) }}",
                minijinja::context! { program => program, name => name },
            )
            .expect("typed body renders")
    }

    fn plan_item(&self, program: &serde_json::Value, index: usize) -> String {
        self.environment
            .render_str(
                "{% from 'functions' import plan_item %}{{ plan_item(program.print.body[index]) }}",
                minijinja::context! { program => program, index => index },
            )
            .expect("plan item renders")
    }

    fn legacy(&self, program: &serde_json::Value, name: &str, index: usize) -> String {
        self.environment
            .render_str(
                "{% from 'functions' import operation %}{{ operation(program.operations[index].operation, program, region) }}",
                minijinja::context! {
                    program => program,
                    index => index,
                    region => format!("{name}_region_{index}"),
                },
            )
            .expect("legacy operation renders")
    }
}

/// Every finding of one typed function body against its issued plan.
fn check(printer: &Printer, program: &solve::TypedProgram, name: &str) -> Vec<String> {
    let wire = serde_json::to_value(program).expect("program serializes");
    let rendered = printer.definition(&wire, name);
    check_rendered(printer, program, &wire, name, &rendered)
}

/// Every finding of one whole `definition()` output against its issued
/// plans: each nested region definition in issued order, then the function
/// itself, its exact closing, and nothing after it.
fn check_rendered(
    printer: &Printer,
    program: &solve::TypedProgram,
    wire: &serde_json::Value,
    name: &str,
    rendered: &str,
) -> Vec<String> {
    let mut text = rendered;
    let mut findings = Vec::new();
    account_definition(printer, program, wire, name, &mut text, &mut findings);
    if findings.is_empty() && !text.is_empty() {
        findings.push(format!(
            "{name}: unaccounted output after the definition: {text:?}"
        ));
    }
    findings
}

/// The region bodies one operation's definition prints before its owner,
/// with the name suffix the definition gives each.
fn nested_bodies(operation: &solve::SolveOperation) -> Vec<(&solve::TypedProgram, &'static str)> {
    match operation {
        solve::SolveOperation::CheckAssertion {
            message: solve::SolveAssertionMessage::Captures { program },
            ..
        } => vec![(program.body(), "message")],
        solve::SolveOperation::Conditional {
            if_true, if_false, ..
        } => vec![(if_true.body(), "true"), (if_false.body(), "false")],
        solve::SolveOperation::Map { body, .. } => vec![(body.body(), "map")],
        solve::SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => {
            let mut bodies = vec![(transition.body(), "fold")];
            bodies.extend(
                continuation
                    .iter()
                    .map(|region| (region.body(), "continue")),
            );
            bodies
        }
        _ => Vec::new(),
    }
}

/// Consumes one definition from the front of `text`: its nested region
/// definitions, recursively, then its own function through the closing.
fn account_definition(
    printer: &Printer,
    program: &solve::TypedProgram,
    wire: &serde_json::Value,
    name: &str,
    text: &mut &str,
    findings: &mut Vec<String>,
) {
    for (index, issued) in program.operations().iter().enumerate() {
        for (body, suffix) in nested_bodies(issued.operation()) {
            let nested = serde_json::to_value(body).expect("region serializes");
            let child = format!("{name}_region_{index}_{suffix}");
            account_definition(printer, body, &nested, &child, text, findings);
        }
    }
    let opening = format!("static int {name}{OPENING}");
    let Some(rest) = text.strip_prefix(opening.as_str()) else {
        findings.push(format!("{name}: no function opening at {text:?}"));
        return;
    };
    let mut body = Cursor {
        text: rest,
        name,
        findings: Vec::new(),
    };
    let plan = program.print_plan();
    body.slots(plan.slots());
    body.registers(plan.registers());
    for item in plan.body() {
        body.item(item, printer, wire);
    }
    body.outputs(plan.outputs());
    match body.text.strip_prefix(CLOSING) {
        Some(rest) => *text = rest,
        None => {
            let unconsumed = body.text;
            body.finding(format!(
                "unconsumed body text before closing: {unconsumed:?}"
            ));
        }
    }
    findings.extend(body.findings);
}

/// The unread rest of one function body and the findings so far.
struct Cursor<'text, 'name> {
    text: &'text str,
    name: &'name str,
    findings: Vec<String>,
}

impl Cursor<'_, '_> {
    fn finding(&mut self, finding: String) {
        self.findings.push(format!("{}: {finding}", self.name));
    }

    fn line(&mut self) -> Option<String> {
        let (line, rest) = self.text.split_once('\n')?;
        self.text = rest;
        Some(line.to_owned())
    }

    fn expect_line(&mut self, expected: Decoded, decode: fn(&str) -> Option<Decoded>) {
        let printed = self.line();
        if printed.as_deref().and_then(decode) != Some(expected.clone()) {
            self.finding(format!("{expected:?} printed as {printed:?}"));
        }
    }

    fn slots(&mut self, items: &[Frame]) {
        for item in items {
            self.slot(expected_frame(item));
        }
    }

    /// One slot declaration, and its argument copy when the plan issues one.
    fn slot(&mut self, expected: Decoded) {
        let first = self.line();
        let next = self.text.split_once('\n').map(|(next, _)| next);
        let decoded = first.as_deref().and_then(|first| decode_slot(first, next));
        match decoded {
            Some((decoded, copied)) if decoded == expected => {
                if copied {
                    self.line();
                }
            }
            other => {
                self.finding(format!(
                    "slot item {expected:?} printed as {other:?} ({first:?})"
                ));
            }
        }
    }

    fn registers(&mut self, items: &[Frame]) {
        let expected: Vec<Decoded> = items.iter().map(expected_frame).collect();
        let printed = self.line();
        let decoded = printed.as_deref().and_then(declarations).map(|found| {
            found
                .into_iter()
                .map(|(element, (space, id), extent)| Decoded::Frame {
                    space,
                    id,
                    element,
                    extent,
                })
                .collect::<Vec<_>>()
        });
        if decoded.as_ref() != Some(&expected) {
            self.finding(format!(
                "register frame {expected:?} printed as {printed:?}"
            ));
        }
    }

    fn item(&mut self, item: &Body, printer: &Printer, wire: &serde_json::Value) {
        match *item {
            Body::View {
                register,
                slot,
                element,
                ..
            } => self.expect_line(
                Decoded::View {
                    register,
                    slot,
                    element,
                },
                decode_body,
            ),
            Body::Copy {
                destination,
                source,
                ..
            } => self.expect_line(
                Decoded::Copy {
                    destination: (destination.space(), destination.id()),
                    source: (source.space(), source.id()),
                },
                decode_body,
            ),
            Body::Constant { .. }
            | Body::Select { .. }
            | Body::Elementwise { .. }
            | Body::Convert { .. } => {
                let mut text = self.text;
                match scalar::check(&mut text, item) {
                    Ok(()) => self.text = text,
                    Err(finding) => self.finding(finding),
                }
            }
            Body::Tensor { .. } => {
                let mut text = self.text;
                match tensor::check(&mut text, item) {
                    Ok(()) => self.text = text,
                    Err(finding) => self.finding(finding),
                }
            }
            Body::Legacy { index, ledger } => {
                if ledger.is_none() {
                    self.finding(format!(
                        "operation {index} is not projected and has no audit ledger row"
                    ));
                }
                let expected = printer.legacy(wire, self.name, index);
                match self.text.strip_prefix(expected.as_str()) {
                    Some(rest) => self.text = rest,
                    None => self.finding(format!(
                        "operation {index} does not print as its unmigrated macro"
                    )),
                }
            }
        }
    }

    fn outputs(&mut self, items: &[solve::SolveOutputCopy]) {
        for item in items {
            self.expect_line(
                Decoded::Output {
                    slot: item.slot(),
                    output: item.output(),
                },
                decode_output,
            );
        }
    }
}

/// Projected (strictly decoded) and quarantined (legacy) body items of one
/// plan: partial coverage is reported, never counted as conformance.
fn coverage(program: &solve::TypedProgram) -> (usize, usize) {
    let body = program.print_plan().body();
    let quarantined = body
        .iter()
        .filter(|item| matches!(item, Body::Legacy { .. }))
        .count();
    (body.len() - quarantined, quarantined)
}

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("print_plan_conformance.mo"),
        0,
        1,
    )
}

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

fn real(extent: u32) -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![extent]).unwrap()
}

/// An output declared before two inputs: one read in place and negated, one
/// stored before it is read again.
fn mixed_program() -> solve::TypedProgram {
    solve::TypedProgram::construct(profile(), |builder| {
        let output = builder.declare_slot(
            real(3),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let viewed = builder.declare_slot(
            real(3),
            solve::SolveStorageClass::Input,
            solve::SolveSlotAccess::ReadOnly,
            span(),
        )?;
        let scratch = builder.declare_slot(
            real(3),
            solve::SolveStorageClass::Input,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let value = builder.load(viewed, span())?;
        let negated = builder.unary(solve::SolveUnaryOperator::Negate, value, span())?;
        builder.store(scratch, negated, span())?;
        let again = builder.load(scratch, span())?;
        builder.store(output, again, span())
    })
    .expect("checked mixed program")
}

/// Eleven inputs, so an argument index and an extent reach 10.
fn wide_program() -> solve::TypedProgram {
    solve::TypedProgram::construct(profile(), |builder| {
        let output = builder.declare_slot(
            real(10),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let mut last = None;
        for _ in 0..11 {
            last = Some(builder.declare_slot(
                real(10),
                solve::SolveStorageClass::Input,
                solve::SolveSlotAccess::ReadOnly,
                span(),
            )?);
        }
        let value = builder.load(last.expect("eleven inputs"), span())?;
        let negated = builder.unary(solve::SolveUnaryOperator::Negate, value, span())?;
        builder.store(output, negated, span())
    })
    .expect("checked wide program")
}

/// A reduction and an integer and Boolean frame.
fn reduction_program() -> solve::TypedProgram {
    let integer = solve::SolveScalarType::Integer(solve::SolveIntegerDomain::FULL);
    solve::TypedProgram::construct(profile(), |builder| {
        let input = builder.declare_slot(
            solve::SolveValueType::tensor(integer, vec![4]).unwrap(),
            solve::SolveStorageClass::Input,
            solve::SolveSlotAccess::ReadOnly,
            span(),
        )?;
        let output = builder.declare_slot(
            solve::SolveValueType::scalar(integer),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let flag = builder.declare_slot(
            solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let value = builder.load(input, span())?;
        let sum = builder.reduce(solve::SolveReductionOperator::Sum, value, span())?;
        builder.store(output, sum, span())?;
        let truth = builder.constant(solve::SolveValue::boolean(true), span())?;
        builder.store(flag, truth, span())
    })
    .expect("checked reduction program")
}

/// A lazy conditional, whose printing no audit row owns yet.
fn conditional_program() -> solve::TypedProgram {
    let arithmetic = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    solve::TypedProgram::construct(arithmetic, |builder| {
        let output = builder.declare_slot(
            real.clone(),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let condition = builder.constant(solve::SolveValue::boolean(true), span())?;
        let results = builder.conditional(
            condition,
            &[],
            vec![real],
            span(),
            |region, _, outputs| {
                let value = region.constant(solve::SolveValue::real(arithmetic, 1.0), span())?;
                region.store(outputs[0], value, span())
            },
            |region, _, outputs| {
                let value = region.constant(solve::SolveValue::real(arithmetic, 2.0), span())?;
                region.store(outputs[0], value, span())
            },
        )?;
        builder.store(output, results[0], span())
    })
    .expect("checked conditional program")
}

/// A read-only input slot of `value_type`, loaded into a register.
fn input<'p>(
    builder: &mut solve::TypedProgramBuilder<'p>,
    value_type: solve::SolveValueType,
) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError> {
    let slot = builder.declare_slot(
        value_type,
        solve::SolveStorageClass::Input,
        solve::SolveSlotAccess::ReadOnly,
        span(),
    )?;
    builder.load(slot, span())
}

fn real_tensor(dimensions: Vec<u32>) -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), dimensions).unwrap()
}

/// Every closed tensor kernel: products in each operand rank, transposes,
/// cross products, scaling, identities, diagonals, fills, aggregate
/// construction, concatenation on both axes and a static projection.
fn tensor_program() -> solve::TypedProgram {
    solve::TypedProgram::construct(profile(), |builder| {
        let m = input(builder, real_tensor(vec![2, 3]))?;
        let v = input(builder, real_tensor(vec![3]))?;
        let w = input(builder, real_tensor(vec![3]))?;
        let s = input(
            builder,
            solve::SolveValueType::scalar(solve::SolveScalarType::real(profile())),
        )?;
        let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()));
        let t = builder.transpose(m, span())?;
        let results = [
            (t, real_tensor(vec![3, 2])),
            (builder.matrix_multiply(m, v, span())?, real_tensor(vec![2])),
            (
                builder.matrix_multiply(t, m, span())?,
                real_tensor(vec![3, 3]),
            ),
            (builder.matrix_multiply(v, t, span())?, real_tensor(vec![2])),
            (builder.matrix_multiply(v, w, span())?, scalar.clone()),
            (builder.cross(v, w, span())?, real_tensor(vec![3])),
            (builder.scale(v, s, span())?, real_tensor(vec![3])),
            (
                builder.identity(solve::SolveScalarType::real(profile()), 3, span())?,
                real_tensor(vec![3, 3]),
            ),
            (builder.diagonal(v, span())?, real_tensor(vec![3, 3])),
            (
                builder.fill(s, vec![2, 2], span())?,
                real_tensor(vec![2, 2]),
            ),
            (
                builder.construct_aggregate(&[v, w], vec![2, 3], span())?,
                real_tensor(vec![2, 3]),
            ),
            (
                builder.concatenate(0, &[m, m], span())?,
                real_tensor(vec![4, 3]),
            ),
            (
                builder.concatenate(1, &[m, m, m], span())?,
                real_tensor(vec![2, 9]),
            ),
            (builder.project_element(m, vec![1, 2], span())?, scalar),
        ];
        for (result, value_type) in results {
            let slot = builder.declare_slot(
                value_type,
                solve::SolveStorageClass::Output,
                solve::SolveSlotAccess::ReadWrite,
                span(),
            )?;
            builder.store(slot, result, span())?;
        }
        Ok(())
    })
    .expect("checked tensor program")
}

/// A map whose body is its own region definition, printed before its owner.
fn map_program() -> solve::TypedProgram {
    let arithmetic = profile();
    let integer = solve::SolveScalarType::integer(arithmetic);
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    solve::TypedProgram::construct(arithmetic, |builder| {
        let output = builder.declare_slot(
            solve::SolveValueType::tensor(integer, vec![3]).unwrap(),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let mapped = builder.map(
            domain,
            &[],
            solve::SolveValueType::scalar(integer),
            span(),
            |region, _, binders, element| {
                let value = region.load(binders[0], span())?;
                let negated = region.unary(solve::SolveUnaryOperator::Negate, value, span())?;
                region.store(element, negated, span())
            },
        )?;
        builder.store(output, mapped, span())
    })
    .expect("checked map program")
}

fn narrow_profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(-100, 100).unwrap(),
    )
}

/// Checked integer arithmetic, the minimum-operand guards, comparison and
/// both conversions under a narrowed integer domain.
fn integer_program() -> solve::TypedProgram {
    let arithmetic = narrow_profile();
    let integer = solve::SolveScalarType::integer(arithmetic);
    let ints = solve::SolveValueType::tensor(integer, vec![3]).unwrap();
    let reals =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic), vec![3]).unwrap();
    solve::TypedProgram::construct(arithmetic, |builder| {
        let a = builder.declare_slot(
            ints.clone(),
            solve::SolveStorageClass::Input,
            solve::SolveSlotAccess::ReadOnly,
            span(),
        )?;
        let x = builder.declare_slot(
            reals.clone(),
            solve::SolveStorageClass::Input,
            solve::SolveSlotAccess::ReadOnly,
            span(),
        )?;
        let out = builder.declare_slot(
            ints.clone(),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let flags = builder.declare_slot(
            solve::SolveValueType::tensor(solve::SolveScalarType::Boolean, vec![3]).unwrap(),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let back = builder.declare_slot(
            reals,
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let a = builder.load(a, span())?;
        let x = builder.load(x, span())?;
        let truncated = builder.convert(
            solve::SolveConversionOperator::RealToIntegerTowardZero,
            x,
            span(),
        )?;
        let floored = builder.convert(
            solve::SolveConversionOperator::RealToIntegerTowardNegativeInfinity,
            x,
            span(),
        )?;
        let mut value = builder.binary(solve::SolveBinaryOperator::Add, a, truncated, span())?;
        for operator in [
            solve::SolveBinaryOperator::Subtract,
            solve::SolveBinaryOperator::Multiply,
            solve::SolveBinaryOperator::IntegerQuotient,
            solve::SolveBinaryOperator::IntegerModulo,
            solve::SolveBinaryOperator::IntegerRemainder,
            solve::SolveBinaryOperator::Min,
            solve::SolveBinaryOperator::Max,
        ] {
            value = builder.binary(operator, value, floored, span())?;
        }
        let negated = builder.unary(solve::SolveUnaryOperator::Negate, value, span())?;
        let magnitude = builder.unary(solve::SolveUnaryOperator::Abs, negated, span())?;
        let seven =
            builder.constant(solve::SolveValue::integer(arithmetic, -7).unwrap(), span())?;
        let shifted = builder.broadcast_binary(
            solve::SolveBinaryOperator::Add,
            magnitude,
            seven,
            true,
            span(),
        )?;
        builder.store(out, shifted, span())?;
        let less = builder.compare(solve::SolveCompareOperator::Less, a, shifted, span())?;
        builder.store(flags, less, span())?;
        let real = builder.convert(
            solve::SolveConversionOperator::IntegerToReal,
            shifted,
            span(),
        )?;
        builder.store(back, real, span())
    })
    .expect("checked integer program")
}

/// Real constants, functions, a broadcast with the scalar on the left,
/// Boolean min/and and a select.
fn real_program() -> solve::TypedProgram {
    let arithmetic = profile();
    let boolean = solve::SolveValueType::tensor(solve::SolveScalarType::Boolean, vec![2]).unwrap();
    solve::TypedProgram::construct(arithmetic, |builder| {
        let x = builder.declare_slot(
            real(2),
            solve::SolveStorageClass::Input,
            solve::SolveSlotAccess::ReadOnly,
            span(),
        )?;
        let out = builder.declare_slot(
            real(2),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let flags = builder.declare_slot(
            boolean,
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let x = builder.load(x, span())?;
        let half = builder.constant(solve::SolveValue::real(arithmetic, -0.5), span())?;
        let mut value = builder.unary(solve::SolveUnaryOperator::Sqrt, x, span())?;
        for operator in [
            solve::SolveUnaryOperator::Sign,
            solve::SolveUnaryOperator::Abs,
            solve::SolveUnaryOperator::Ceiling,
            solve::SolveUnaryOperator::Log10,
        ] {
            value = builder.unary(operator, value, span())?;
        }
        let scaled = builder.broadcast_binary(
            solve::SolveBinaryOperator::Multiply,
            value,
            half,
            true,
            span(),
        )?;
        let powered = builder.binary(solve::SolveBinaryOperator::Power, scaled, x, span())?;
        let smaller = builder.binary(solve::SolveBinaryOperator::Min, powered, x, span())?;
        let positive = builder.compare(solve::SolveCompareOperator::Greater, smaller, x, span())?;
        let negative =
            builder.compare(solve::SolveCompareOperator::LessEqual, smaller, x, span())?;
        let both = builder.binary(solve::SolveBinaryOperator::Min, positive, negative, span())?;
        let either = builder.binary(solve::SolveBinaryOperator::And, both, positive, span())?;
        builder.store(flags, either, span())?;
        let truth = builder.constant(solve::SolveValue::boolean(false), span())?;
        let chosen = builder.select(truth, smaller, x, span())?;
        builder.store(out, chosen, span())
    })
    .expect("checked real program")
}

#[test]
fn typed_bodies_print_exactly_their_issued_plans() {
    let printer = Printer::builtin();
    for (program, name) in [
        (mixed_program(), "mixed"),
        (reduction_program(), "reduction"),
        (integer_program(), "integer"),
        (real_program(), "real"),
        (map_program(), "map"),
    ] {
        assert_eq!(check(&printer, &program, name), Vec::<String>::new());
    }
}

#[test]
fn coverage_reports_projected_and_quarantined_items() {
    assert_eq!(coverage(&mixed_program()), (5, 0));
    assert_eq!(coverage(&reduction_program()), (4, 1));
    assert_eq!(coverage(&integer_program()), (21, 0));
    assert_eq!(coverage(&real_program()).1, 0);
}

#[test]
fn the_issued_plan_places_views_copies_and_argument_copies() {
    let program = mixed_program();
    let plan = program.print_plan();
    assert!(matches!(
        plan.slots(),
        [
            Frame::Frame { .. },
            Frame::ArgumentView {
                slot: 1,
                argument: 0,
                ..
            },
            Frame::FrameFromArgument {
                slot: 2,
                argument: 1,
                ..
            },
        ]
    ));
    assert!(matches!(
        plan.body(),
        [
            Body::View {
                register: 0,
                slot: 1,
                ..
            },
            Body::Elementwise { .. },
            Body::Copy { .. },
            Body::Copy { .. },
            Body::Copy { .. },
        ]
    ));
}

/// A control fails for the right reason: the body was found and decoded,
/// and an item or the closing disagreed with the plan.
fn assert_decoding_findings(label: &str, findings: &[String]) {
    assert!(!findings.is_empty(), "{label}: conformance must fail");
    assert!(
        findings
            .iter()
            .all(|finding| !finding.contains("no function opening")),
        "{label}: {findings:?}"
    );
}

/// Each injected printer deviation from the plan is a finding.
#[test]
fn printing_anything_but_the_plan_fails_conformance() {
    let template = builtin_template("fmi3", "typed_functions.jinja");
    let injections = [
        (
            "swapped copy operands",
            "memcpy({{ storage_name(item.destination) }}, {{ storage_name(item.source) }}, sizeof({{ storage_name(item.destination) }}));",
            "memcpy({{ storage_name(item.source) }}, {{ storage_name(item.destination) }}, sizeof({{ storage_name(item.source) }}));",
        ),
        (
            "argument number from the slot id",
            "= in[{{ item.argument }}]; (void)s{{ item.slot }};",
            "= in[{{ item.slot }}]; (void)s{{ item.slot }};",
        ),
        (
            "dropped view",
            "const {{ element(item.element) }}* r{{ item.register }} = s{{ item.slot }}; (void)r{{ item.register }};\n",
            "",
        ),
        (
            "duplicated output copy",
            "if (out[{{ item.output }}]) memcpy(out[{{ item.output }}], s{{ item.slot }}, sizeof(s{{ item.slot }}));\n",
            "if (out[{{ item.output }}]) memcpy(out[{{ item.output }}], s{{ item.slot }}, sizeof(s{{ item.slot }}));\nif (out[{{ item.output }}]) memcpy(out[{{ item.output }}], s{{ item.slot }}, sizeof(s{{ item.slot }}));\n",
        ),
        (
            "literal loop pasted after the outputs",
            "{% for item in plan.outputs -%}",
            "for (size_t k=0; k<1; ++k) {}\n{% for item in plan.outputs -%}",
        ),
        (
            "extent recomputed in the template",
            "r{{ item.name.id }}[{{ item.extent }}];",
            "r{{ item.name.id }}[{{ item.extent + 1 }}];",
        ),
    ];
    let program = mixed_program();
    for (label, original, injected) in injections {
        assert_eq!(template.matches(original).count(), 1, "{label}: anchor");
        let printer = Printer::new(template.replacen(original, injected, 1));
        assert_decoding_findings(label, &check(&printer, &program, "mixed"));
    }
}

/// A plan item substituted under the same id prints a valid-looking body
/// that is still not the issued plan.
#[test]
fn a_same_id_substitution_in_the_printed_plan_fails_conformance() {
    let printer = Printer::builtin();
    let program = mixed_program();
    let mut wire = serde_json::to_value(&program).unwrap();
    let body = wire["print"]["body"].as_array_mut().unwrap();
    let copy = body.iter_mut().find(|item| item["kind"] == "copy").unwrap();
    let destination = copy["destination"].clone();
    copy["destination"] = copy["source"].clone();
    copy["source"] = destination;
    let rendered = printer.definition(&wire, "mixed");
    assert_decoding_findings(
        "swapped copy",
        &check_rendered(&printer, &program, &wire, "mixed", &rendered),
    );
}

/// An operation no audit row owns cannot pass as unmigrated printing.
#[test]
fn an_unledgered_legacy_operation_fails_conformance() {
    let findings = check(&Printer::builtin(), &conditional_program(), "conditional");
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("no audit ledger row")),
        "{findings:?}"
    );
}

/// Each injected deviation from an issued scalar item is a finding.
#[test]
fn printing_a_scalar_item_but_its_plan_fails_conformance() {
    let template = builtin_template("fmi3", "typed_functions.jinja");
    let injections = [
        (
            "multiply spelled as add",
            "{%- elif token == \"multiply\" -%}({{ a }} * {{ b }})",
            "{%- elif token == \"multiply\" -%}({{ a }} + {{ b }})",
            "real",
        ),
        (
            "dropped overflow check",
            "if (({{ b }}>0 && {{ a }}>INT64_MAX-{{ b }}) || ({{ b }}<0 && {{ a }}<INT64_MIN-{{ b }})) return 1;\n",
            "",
            "integer",
        ),
        (
            "changed constant bits",
            "rumoca_real64(UINT64_C({{ item.value.bits }}))",
            "rumoca_real64(UINT64_C({{ item.value.bits }}0))",
            "real",
        ),
        (
            "broadcast read at the running element",
            "[{% if value.broadcast %}0{% else %}k{% endif %}]",
            "[k]",
            "real",
        ),
        (
            "dropped domain check",
            "{% if item.domain -%}",
            "{% if false -%}",
            "integer",
        ),
        (
            "dropped minimum-operand check",
            "{% if item.kernel.min_operand_check -%}",
            "{% if false -%}",
            "integer",
        ),
    ];
    for (label, original, injected, fixture) in injections {
        assert_eq!(template.matches(original).count(), 1, "{label}: anchor");
        let printer = Printer::new(template.replacen(original, injected, 1));
        let program = if fixture == "real" {
            real_program()
        } else {
            integer_program()
        };
        assert_decoding_findings(label, &check(&printer, &program, fixture));
    }
}

/// Whole-output controls: text before, after or duplicating an admitted
/// definition is never ignored.
#[test]
fn injected_text_outside_the_named_function_must_not_be_ignored() {
    let printer = Printer::builtin();
    let program = mixed_program();
    let wire = serde_json::to_value(&program).unwrap();
    let rendered = printer.definition(&wire, "mixed");
    assert_eq!(
        check_rendered(&printer, &program, &wire, "mixed", &rendered),
        Vec::<String>::new()
    );
    for (label, text) in [
        (
            "prefix",
            format!("static int injected_decision(void) {{ return 42; }}\n{rendered}"),
        ),
        (
            "suffix",
            format!("{rendered}static int injected_decision(void) {{ return 42; }}\n"),
        ),
        ("duplicate definition", format!("{rendered}{rendered}")),
    ] {
        let findings = check_rendered(&printer, &program, &wire, "mixed", &text);
        assert!(
            !findings.is_empty(),
            "{label}: all output must be accounted for"
        );
    }
}

/// A nested region definition printed under a different owner name is not
/// the issued region, and nothing before the owner is skipped.
#[test]
fn a_region_definition_under_another_owner_fails_conformance() {
    let template = builtin_template("fmi3", "typed_functions.jinja");
    let original = "{{ definition(op.body.body, child ~ \"_map\", prefix, assertion_outputs, source_predicate_outputs) -}}";
    assert_eq!(template.matches(original).count(), 1, "anchor");
    let printer = Printer::new(template.replacen(
        original,
        "{{ definition(op.body.body, child ~ \"_mapped\", prefix, assertion_outputs, source_predicate_outputs) -}}",
        1,
    ));
    let findings = check(&printer, &map_program(), "map");
    assert!(!findings.is_empty(), "wrong region owner must fail");
}

/// A decimal 10 printed as `010` is octal 8 in C: an extent or argument
/// index in any but its canonical spelling is not the issued one.
#[test]
fn a_non_canonical_decimal_spelling_fails_conformance() {
    let program = wide_program();
    assert_eq!(
        check(&Printer::builtin(), &program, "wide"),
        Vec::<String>::new()
    );
    let template = builtin_template("fmi3", "typed_functions.jinja");
    for (label, original, injected) in [
        (
            "extent 10 printed as 010",
            "r{{ item.name.id }}[{{ item.extent }}];",
            "r{{ item.name.id }}[{% if item.extent == 10 %}0{% endif %}{{ item.extent }}];",
        ),
        (
            "argument 10 printed as 010",
            "= in[{{ item.argument }}]; (void)s{{ item.slot }};",
            "= in[{% if item.argument == 10 %}0{% endif %}{{ item.argument }}]; (void)s{{ item.slot }};",
        ),
    ] {
        assert_eq!(template.matches(original).count(), 1, "{label}: anchor");
        let printer = Printer::new(template.replacen(original, injected, 1));
        let rendered = printer.definition(&serde_json::to_value(&program).unwrap(), "wide");
        assert!(rendered.contains("[010]"), "{label}: control prints 010");
        assert_decoding_findings(label, &check(&printer, &program, "wide"));
    }
}

/// The tensor items of a program, by body position.
fn tensor_items(program: &solve::TypedProgram) -> Vec<(usize, usize)> {
    program
        .print_plan()
        .body()
        .iter()
        .enumerate()
        .filter_map(|(position, item)| match item {
            Body::Tensor { index, .. } => Some((position, *index)),
            _ => None,
        })
        .collect()
}

#[test]
fn tensor_bodies_print_exactly_their_issued_plans() {
    let program = tensor_program();
    assert_eq!(
        check(&Printer::builtin(), &program, "tensor"),
        Vec::<String>::new()
    );
    assert_eq!(tensor_items(&program).len(), 14);
    assert_eq!(coverage(&program).1, 0);
}

/// Each tensor item prints the bytes its unmigrated macro printed for the
/// same operation, so moving it to the plan changes no generated code.
#[test]
fn tensor_items_print_what_their_legacy_route_printed() {
    let printer = Printer::builtin();
    let program = tensor_program();
    let wire = serde_json::to_value(&program).unwrap();
    for (position, index) in tensor_items(&program) {
        assert_eq!(
            printer.plan_item(&wire, position),
            printer.legacy(&wire, "tensor", index),
            "operation {index}"
        );
    }
}

/// Each injected deviation from an issued tensor item is a finding.
#[test]
fn printing_a_tensor_item_but_its_plan_fails_conformance() {
    let template = builtin_template("fmi3", "typed_functions.jinja");
    let injections = [
        (
            "product operands swapped",
            "sum+=r{{ item.kernel.lhs }}[i*{{ item.kernel.inner }}+k]*r{{ item.kernel.rhs }}[k*{{ item.kernel.columns }}+j]",
            "sum+=r{{ item.kernel.rhs }}[i*{{ item.kernel.inner }}+k]*r{{ item.kernel.lhs }}[k*{{ item.kernel.columns }}+j]",
        ),
        (
            "transpose stride from the wrong axis",
            "=r{{ item.kernel.operand }}[(i*{{ item.kernel.columns }}+j)",
            "=r{{ item.kernel.operand }}[(i*{{ item.kernel.rows }}+j)",
        ),
        (
            "fill extent recomputed",
            "k<{{ item.kernel.extent }}; ++k) r{{ item.destination }}[k]=r{{ item.kernel.value }}[0];",
            "k<{{ item.kernel.extent + 1 }}; ++k) r{{ item.destination }}[k]=r{{ item.kernel.value }}[0];",
        ),
        (
            "extent in a non-canonical spelling",
            "for (size_t k=0; k<{{ item.kernel.trailing }}; ++k)",
            "for (size_t k=0; k<0{{ item.kernel.trailing }}; ++k)",
        ),
        (
            "dropped concatenation part",
            "{% for part in item.kernel.parts -%}\nfor (size_t k=0;",
            "{% for part in item.kernel.parts[1:] -%}\nfor (size_t k=0;",
        ),
        (
            "aggregate offset from the loop",
            "memcpy(&r{{ item.destination }}[{{ part.offset }}]",
            "memcpy(&r{{ item.destination }}[{{ loop.index0 }}]",
        ),
        (
            "projection coordinate dropped",
            "index=index*{{ axis.extent }}+{{ axis.coordinate }};",
            "index=index*{{ axis.extent }}+0;",
        ),
        (
            "cross product term sign",
            "r{{ item.destination }}[0]=r{{ item.kernel.lhs }}[1]*r{{ item.kernel.rhs }}[2]-r{{ item.kernel.lhs }}[2]*r{{ item.kernel.rhs }}[1];",
            "r{{ item.destination }}[0]=r{{ item.kernel.lhs }}[1]*r{{ item.kernel.rhs }}[2]+r{{ item.kernel.lhs }}[2]*r{{ item.kernel.rhs }}[1];",
        ),
        (
            "scale scalar read at the running element",
            "=r{{ item.kernel.aggregate }}[k]*r{{ item.kernel.scalar }}[0];",
            "=r{{ item.kernel.aggregate }}[k]*r{{ item.kernel.scalar }}[k];",
        ),
        (
            "identity diagonal value",
            "(i==j)?1.0:0.0;",
            "(i==j)?0.0:1.0;",
        ),
        (
            "diagonal read at a fixed element",
            "(i==j)?r{{ item.kernel.operand }}[i]:0.0;",
            "(i==j)?r{{ item.kernel.operand }}[0]:0.0;",
        ),
        (
            "dropped tensor item",
            "{% elif item.kind == \"tensor\" %}{{ tensor_item(item) }}",
            "{% elif item.kind == \"tensor\" %}",
        ),
    ];
    let program = tensor_program();
    for (label, original, injected) in injections {
        assert_eq!(template.matches(original).count(), 1, "{label}: anchor");
        let printer = Printer::new(template.replacen(original, injected, 1));
        assert_decoding_findings(label, &check(&printer, &program, "tensor"));
    }
}

/// A tensor item whose operands are exchanged under the same id prints a
/// valid-looking product that is still not the issued plan.
#[test]
fn a_same_id_tensor_substitution_fails_conformance() {
    let printer = Printer::builtin();
    let program = tensor_program();
    let mut wire = serde_json::to_value(&program).unwrap();
    let body = wire["print"]["body"].as_array_mut().unwrap();
    let product = body
        .iter_mut()
        .find(|item| item["kind"] == "tensor" && item["kernel"]["kind"] == "matrix_multiply")
        .unwrap();
    let lhs = product["kernel"]["lhs"].clone();
    product["kernel"]["lhs"] = product["kernel"]["rhs"].clone();
    product["kernel"]["rhs"] = lhs;
    let rendered = printer.definition(&wire, "tensor");
    assert_decoding_findings(
        "swapped product",
        &check_rendered(&printer, &program, &wire, "tensor", &rendered),
    );
}
