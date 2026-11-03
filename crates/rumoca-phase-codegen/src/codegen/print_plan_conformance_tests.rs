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

fn number<T: std::str::FromStr>(text: &str) -> Option<T> {
    text.parse().ok()
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

fn check_rendered(
    printer: &Printer,
    program: &solve::TypedProgram,
    wire: &serde_json::Value,
    name: &str,
    rendered: &str,
) -> Vec<String> {
    let opening = format!("static int {name}{OPENING}");
    let Some(start) = rendered.find(&opening) else {
        return vec![format!("{name}: no function opening in {rendered:?}")];
    };
    let mut body = Cursor {
        text: &rendered[start + opening.len()..],
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
    if !body.text.starts_with(CLOSING) {
        let text = body.text;
        body.finding(format!("unconsumed body text before closing: {text:?}"));
    }
    body.findings
}

/// The unread rest of one function body and the findings so far.
struct Cursor<'a> {
    text: &'a str,
    name: &'a str,
    findings: Vec<String>,
}

impl Cursor<'_> {
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

/// An identity, whose printing no audit row owns yet.
fn identity_program() -> solve::TypedProgram {
    solve::TypedProgram::construct(profile(), |builder| {
        let output = builder.declare_slot(
            solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![2, 2])
                .unwrap(),
            solve::SolveStorageClass::Output,
            solve::SolveSlotAccess::ReadWrite,
            span(),
        )?;
        let identity = builder.identity(solve::SolveScalarType::real(profile()), 2, span())?;
        builder.store(output, identity, span())
    })
    .expect("checked identity program")
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
    let findings = check(&Printer::builtin(), &identity_program(), "identity");
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
