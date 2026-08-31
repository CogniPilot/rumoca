//! Evaluable function bodies, their declarations, and the compaction driver.
//!
//! One fixed declaration table backs every program: two Real inputs, one
//! Integer input, two Real array inputs, Real/Integer/Boolean scalar locals,
//! one Real array local, and a Real scalar plus Real array output. Fixing the
//! table is what lets a generated body be run: every name it can mention has a
//! declared type, a declared extent, and a concrete entry value.
//!
//! Subscripts are in bounds by construction. Only loop binders, whose literal
//! `1:N` ranges stay inside the array extent, and literal coordinates in the
//! same range are ever written, so no generated program depends on the
//! interpreter's out-of-range refusal.
//!
//! A collected Flat function exposes exactly one source declaration, which it
//! carries as its exposure identity. The harness declaration is written here
//! rather than resolved from a class tree, so `64_101` names it.

use super::preservation_values::{Environment, Value};
use super::*;
use rumoca_core::{
    ComponentRefPart, ComponentReference, DefId, EffectiveType, Literal, OpBinary, SourceId,
    Subscript, TypeId,
};

pub(super) const REAL_INPUTS: [&str; 2] = ["u", "v"];
pub(super) const INTEGER_INPUT: &str = "n";
pub(super) const REAL_ARRAY_INPUTS: [&str; 2] = ["r", "s"];
pub(super) const REAL_LOCALS: [&str; 2] = ["a", "b"];
pub(super) const INTEGER_LOCAL: &str = "k";
pub(super) const BOOLEAN_LOCAL: &str = "f";
pub(super) const REAL_ARRAY_LOCAL: &str = "t";
pub(super) const REAL_OUTPUT: &str = "y";
pub(super) const REAL_ARRAY_OUTPUT: &str = "w";
pub(super) const BINDERS: [&str; 2] = ["i", "j"];

/// Declared extent of every array in the table.
pub(super) const ARRAY_EXTENT: i64 = 3;

const REAL_TYPE: TypeId = TypeId(1);
const INTEGER_TYPE: TypeId = TypeId(2);
const BOOLEAN_TYPE: TypeId = TypeId(3);
const STRING_TYPE: TypeId = TypeId(4);
const CLOCK_TYPE: TypeId = TypeId(5);

pub(super) fn span() -> Span {
    Span::from_offsets(SourceId::DUMMY, 0, 1)
}

/// The names whose final values the preservation theorem compares.
pub(super) fn observable_names() -> Vec<VarName> {
    [REAL_OUTPUT, REAL_ARRAY_OUTPUT]
        .iter()
        .map(|name| VarName::new(*name))
        .collect()
}

/// The function locals, which the pass is free to leave in any final state.
pub(super) fn local_names() -> Vec<VarName> {
    REAL_LOCALS
        .iter()
        .chain([&INTEGER_LOCAL, &BOOLEAN_LOCAL, &REAL_ARRAY_LOCAL])
        .map(|name| VarName::new(*name))
        .collect()
}

fn param(name: &str, canonical: TypeId, dimensions: &[i64]) -> rumoca_core::FunctionParam {
    let type_name = match canonical {
        INTEGER_TYPE => "Integer",
        BOOLEAN_TYPE => "Boolean",
        _ => "Real",
    };
    let effective = EffectiveType::new(canonical, canonical, dimensions.to_vec())
        .expect("the harness declaration table names resolved predefined types");
    rumoca_core::FunctionParam::new(name, type_name, effective, span())
}

fn real_scalar(name: &str) -> rumoca_core::FunctionParam {
    param(name, REAL_TYPE, &[])
}

fn real_array(name: &str) -> rumoca_core::FunctionParam {
    param(name, REAL_TYPE, &[ARRAY_EXTENT])
}

/// The one function declaration every generated body is placed into.
pub(super) fn harness_function(body: Vec<rumoca_core::Statement>) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("preservation_probe", DefId::new(64_101), span());
    function.inputs = REAL_INPUTS
        .iter()
        .map(|name| real_scalar(name))
        .chain(std::iter::once(param(INTEGER_INPUT, INTEGER_TYPE, &[])))
        .chain(REAL_ARRAY_INPUTS.iter().map(|name| real_array(name)))
        .collect();
    function.locals = REAL_LOCALS
        .iter()
        .map(|name| real_scalar(name))
        .chain([
            param(INTEGER_LOCAL, INTEGER_TYPE, &[]),
            param(BOOLEAN_LOCAL, BOOLEAN_TYPE, &[]),
            real_array(REAL_ARRAY_LOCAL),
        ])
        .collect();
    function.outputs = vec![real_scalar(REAL_OUTPUT), real_array(REAL_ARRAY_OUTPUT)];
    function.body = body;
    function
}

fn harness_flat() -> flat::Model {
    flat::Model {
        predefined_types: flat::PredefinedTypeIds {
            real: REAL_TYPE,
            integer: INTEGER_TYPE,
            boolean: BOOLEAN_TYPE,
            string: STRING_TYPE,
            clock: CLOCK_TYPE,
        },
        ..flat::Model::default()
    }
}

/// The proven shapes of the declaration table, as the production path supplies
/// them for a function whose formals all have literal extents.
fn harness_shapes(function: &rumoca_core::Function) -> ShapeEnvironment {
    let mut shapes = ShapeEnvironment::default();
    for value in function
        .inputs
        .iter()
        .chain(&function.locals)
        .chain(&function.outputs)
    {
        let shape = value
            .effective_type
            .dimensions()
            .iter()
            .map(|extent| *extent as u32)
            .collect();
        shapes.insert(VarName::new(&value.name), shape);
    }
    shapes
}

/// Run the pass pipeline exactly as `validate_statement_function` runs it.
pub(super) fn compact(
    body: &[rumoca_core::Statement],
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let function = harness_function(body.to_vec());
    let flat = harness_flat();
    let shapes = harness_shapes(&function);
    compact_function_loops(body, &HashMap::new(), &shapes, &function, &flat, false)
}

/// One concrete assignment of entry values to every declared name.
pub(super) struct EntryValues {
    pub(super) label: &'static str,
    reals: [f64; 2],
    integer: i64,
    arrays: [[f64; 3]; 2],
    locals: [f64; 2],
    local_integer: i64,
    local_boolean: bool,
    local_array: [f64; 3],
}

/// Four entry environments, run against every program.
///
/// A single entry vector can hide a divergence behind an arithmetic
/// coincidence; four with distinct signs, zeros and orderings do not.
pub(super) const ENTRY_VALUES: [EntryValues; 4] = [
    EntryValues {
        label: "ascending",
        reals: [2.0, 3.0],
        integer: 2,
        arrays: [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]],
        locals: [-7.0, 5.0],
        local_integer: 1,
        local_boolean: false,
        local_array: [2.0, -3.0, 4.0],
    },
    EntryValues {
        label: "signed",
        reals: [-1.0, 4.0],
        integer: 3,
        arrays: [[-2.0, 5.0, -1.0], [3.0, -4.0, 2.0]],
        locals: [6.0, -2.0],
        local_integer: 2,
        local_boolean: true,
        local_array: [-5.0, 1.0, -6.0],
    },
    EntryValues {
        label: "zeroed",
        reals: [0.0, 0.0],
        integer: 1,
        arrays: [[0.0, 1.0, 0.0], [0.0, 0.0, 2.0]],
        locals: [0.0, 3.0],
        local_integer: 0,
        local_boolean: false,
        local_array: [0.0, 0.0, 0.0],
    },
    EntryValues {
        label: "descending",
        reals: [5.0, -3.0],
        integer: 3,
        arrays: [[7.0, -6.0, 1.0], [-8.0, 2.0, 9.0]],
        locals: [-4.0, -9.0],
        local_integer: 3,
        local_boolean: true,
        local_array: [8.0, 7.0, -1.0],
    },
];

fn real_array_value(values: &[f64; 3]) -> Value {
    Value::Array(values.iter().copied().map(Value::Real).collect())
}

impl EntryValues {
    /// The complete entry environment, including the entry values of locals.
    ///
    /// Locals enter with distinct values on purpose: a program that reads a
    /// local before writing it observes the entry value, and both executions
    /// must observe the same one.
    pub(super) fn environment(&self) -> Environment {
        let mut environment = Environment::new();
        for (name, value) in REAL_INPUTS.iter().zip(self.reals) {
            environment.insert(VarName::new(*name), Value::Real(value));
        }
        environment.insert(VarName::new(INTEGER_INPUT), Value::Integer(self.integer));
        for (name, values) in REAL_ARRAY_INPUTS.iter().zip(&self.arrays) {
            environment.insert(VarName::new(*name), real_array_value(values));
        }
        for (name, value) in REAL_LOCALS.iter().zip(self.locals) {
            environment.insert(VarName::new(*name), Value::Real(value));
        }
        environment.insert(
            VarName::new(INTEGER_LOCAL),
            Value::Integer(self.local_integer),
        );
        environment.insert(
            VarName::new(BOOLEAN_LOCAL),
            Value::Boolean(self.local_boolean),
        );
        environment.insert(
            VarName::new(REAL_ARRAY_LOCAL),
            real_array_value(&self.local_array),
        );
        environment.insert(VarName::new(REAL_OUTPUT), Value::Real(0.0));
        environment.insert(
            VarName::new(REAL_ARRAY_OUTPUT),
            real_array_value(&[0.0, 0.0, 0.0]),
        );
        environment
    }
}

pub(super) fn component(name: &str, subs: Vec<Subscript>) -> ComponentReference {
    ComponentReference::construct(
        false,
        span(),
        vec![ComponentRefPart {
            ident: name.to_string(),
            span: span(),
            subs,
            def_id: DefId::new(1),
        }],
    )
    .expect("the harness component reference has exact identity")
}

pub(super) fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: span(),
    }
}

pub(super) fn element(name: &str, subscript: Expression) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![Subscript::Expr {
            expr: Box::new(subscript),
            span: span(),
        }],
        span: span(),
    }
}

pub(super) fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: span(),
    }
}

pub(super) fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: span(),
    }
}

pub(super) fn boolean(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: span(),
    }
}

pub(super) fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: span(),
    }
}

pub(super) fn assign(target: &str, value: Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: component(target, Vec::new()),
        value,
        span: span(),
    }
}

pub(super) fn assign_element(
    target: &str,
    subscript: Expression,
    value: Expression,
) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: component(
            target,
            vec![Subscript::Expr {
                expr: Box::new(subscript),
                span: span(),
            }],
        ),
        value,
        span: span(),
    }
}

pub(super) fn for_loop(
    binder: &str,
    count: i64,
    body: Vec<rumoca_core::Statement>,
) -> rumoca_core::Statement {
    rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: binder.to_string(),
            range: Expression::Range {
                start: Box::new(integer(1)),
                step: None,
                end: Box::new(integer(count)),
                span: span(),
            },
        }],
        equations: body,
        span: span(),
    }
}

pub(super) fn branch(
    conditions: Vec<(Expression, Vec<rumoca_core::Statement>)>,
    fallback: Option<Vec<rumoca_core::Statement>>,
) -> rumoca_core::Statement {
    rumoca_core::Statement::If {
        cond_blocks: conditions
            .into_iter()
            .map(|(cond, stmts)| rumoca_core::StatementBlock { cond, stmts })
            .collect(),
        else_block: fallback,
        span: span(),
    }
}
