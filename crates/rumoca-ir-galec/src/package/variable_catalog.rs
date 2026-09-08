//! Construction-derived block declaration catalog facts.

use crate::ast::{InterfaceKind, ProtectedKind, ScalarType, TypeRef};
use crate::validate::{
    BlockDeclarationStartLiteral, EvaluatedLiteral, EvaluatedScalar, RetainedValidation,
};

use super::{AlgorithmCodeEvaluatedScalar, AlgorithmCodeEvaluatedStart};

pub(super) fn block_declarations(block: &crate::Block) -> Vec<&crate::ast::VariableDeclaration> {
    block
        .interface
        .iter()
        .map(|variable| &variable.decl)
        .chain(block.protected.iter().map(|variable| &variable.decl))
        .collect()
}

pub(super) fn block_variable_starts(
    block: &crate::Block,
    retained: &RetainedValidation,
) -> Vec<AlgorithmCodeEvaluatedStart> {
    block_declarations(block)
        .into_iter()
        .enumerate()
        .map(|(index, _declaration)| {
            let index = u32::try_from(index)
                .expect("checked Algorithm Code block declaration count fits u32");
            let (literal, shape) = retained.block_declaration_start_literal(index);
            match literal {
                BlockDeclarationStartLiteral::NotApplicable => AlgorithmCodeEvaluatedStart::Missing,
                BlockDeclarationStartLiteral::Exact(literal) => {
                    evaluated_start(literal, shape.extents.is_empty())
                }
            }
        })
        .collect()
}

fn evaluated_start(literal: EvaluatedLiteral, scalar_shape: bool) -> AlgorithmCodeEvaluatedStart {
    match (scalar_shape, literal) {
        (true, EvaluatedLiteral::Scalar(scalar)) => {
            AlgorithmCodeEvaluatedStart::Scalar(evaluated_scalar(scalar))
        }
        (true, _) => AlgorithmCodeEvaluatedStart::UnsupportedScalarExpression,
        (false, EvaluatedLiteral::UniformTensorFill(scalar)) => {
            AlgorithmCodeEvaluatedStart::UniformTensorFill(evaluated_scalar(scalar))
        }
        (false, EvaluatedLiteral::NonUniformTensor) => {
            AlgorithmCodeEvaluatedStart::UnsupportedNonUniformTensor
        }
        (false, EvaluatedLiteral::Symbolic | EvaluatedLiteral::Scalar(_)) => {
            AlgorithmCodeEvaluatedStart::UnsupportedSymbolicTensor
        }
    }
}

const fn evaluated_scalar(scalar: EvaluatedScalar) -> AlgorithmCodeEvaluatedScalar {
    match scalar {
        EvaluatedScalar::RealBits(bits) => AlgorithmCodeEvaluatedScalar::RealBits(bits),
        EvaluatedScalar::Integer(value) => AlgorithmCodeEvaluatedScalar::Integer(value),
        EvaluatedScalar::Boolean(value) => AlgorithmCodeEvaluatedScalar::Boolean(value),
    }
}

pub(super) fn clock_ordinal(block: &crate::Block, name: &str) -> Option<usize> {
    block
        .interface
        .iter()
        .map(|variable| {
            (
                &variable.decl,
                matches!(variable.kind, InterfaceKind::TunableParameter),
            )
        })
        .chain(block.protected.iter().map(|variable| {
            (
                &variable.decl,
                matches!(variable.kind, ProtectedKind::Constant),
            )
        }))
        .enumerate()
        .find_map(|(index, (declaration, constant))| {
            let scalar_real = matches!(declaration.ty, TypeRef::Primitive(ScalarType::Real))
                && declaration.dimensions.is_empty();
            (constant && scalar_real && declaration.name.lexeme() == name).then_some(index + 1)
        })
}
