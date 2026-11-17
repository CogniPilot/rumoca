//! Match the actual SSA uses, never observed register numbers or function names.
use crate::{SolveBinaryOperator as B, SolveOperation as O, SolveRegisterId, TypedProgram};
#[derive(Clone, Copy)]
enum Pattern {
    Input(usize),
    Product(usize, usize),
    Binary(B, usize, usize),
    Outer(usize),
    DivideArray(usize, usize),
    Scale(usize, usize),
    Output(usize, usize),
}
use Pattern::{
    Binary as A, DivideArray as D, Input as I, Outer as U, Output as W, Product as P, Scale as S,
};
const SOURCE: &[Pattern] = &[
    I(0),
    I(1),
    I(2),
    I(3),
    I(4),
    I(5),
    I(6),
    P(0, 1),
    P(1, 7),
    A(B::Add, 8, 2),
    A(B::Add, 9, 3),
    P(1, 5),
    A(B::Subtract, 4, 11),
    U(7),
    D(13, 10),
    A(B::Subtract, 0, 14),
    P(15, 6),
    A(B::Add, 5, 16),
    A(B::Divide, 12, 10),
    S(7, 18),
    A(B::Add, 17, 19),
    W(0, 7),
    W(1, 10),
    W(2, 12),
    W(3, 15),
    W(4, 20),
];

pub(super) fn matches(body: &TypedProgram) -> bool {
    if body.operations().len() != SOURCE.len() {
        return false;
    }
    let mut registers = Vec::new();
    for (spanned, pattern) in body.operations().iter().zip(SOURCE) {
        let op = spanned.operation();
        if let Pattern::Output(output, source) = *pattern {
            if !matches!(op,O::Store { slot, source:actual }
                if slot.index()==7+output && registers.get(source)==Some(actual))
            {
                return false;
            }
        } else {
            let Some(destination) = destination(op, *pattern, &registers) else {
                return false;
            };
            registers.push(destination);
        }
    }
    true
}
fn destination(op: &O, pattern: Pattern, r: &[SolveRegisterId]) -> Option<SolveRegisterId> {
    let matched = match (op, pattern) {
        (O::Load { destination, slot }, Pattern::Input(input)) if slot.index() == input => {
            *destination
        }
        (
            O::MatrixMultiply {
                destination,
                lhs,
                rhs,
            },
            Pattern::Product(a, b),
        ) if r.get(a) == Some(lhs) && r.get(b) == Some(rhs) => *destination,
        (
            O::Binary {
                destination,
                operator,
                lhs,
                rhs,
            },
            Pattern::Binary(want, a, b),
        ) if *operator == want && r.get(a) == Some(lhs) && r.get(b) == Some(rhs) => *destination,
        (
            O::Scale {
                destination,
                aggregate,
                scalar,
            },
            Pattern::Scale(a, b),
        ) if r.get(a) == Some(aggregate) && r.get(b) == Some(scalar) => *destination,
        (
            O::BroadcastBinary {
                destination,
                operator: B::Divide,
                aggregate,
                scalar,
                scalar_on_lhs: false,
            },
            Pattern::DivideArray(a, b),
        ) if r.get(a) == Some(aggregate) && r.get(b) == Some(scalar) => *destination,
        (
            O::Map {
                destination,
                domain,
                captures,
                body,
            },
            Pattern::Outer(a),
        ) if captures.len() == 1
            && r.get(a) == captures.first()
            && super::outer::matches(domain, body) =>
        {
            *destination
        }
        _ => return None,
    };
    Some(matched)
}
