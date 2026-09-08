use super::{ConstantFoldedParameter, PackageError};
use crate::ast::{ProtectedKind, ScalarType, TypeRef};

pub(super) fn validate_variable_nominals(
    declarations: &[&crate::ast::VariableDeclaration],
    nominals: &[Option<f64>],
) -> Result<(), PackageError> {
    for (index, (declaration, nominal)) in declarations.iter().zip(nominals).enumerate() {
        let Some(nominal) = nominal else {
            continue;
        };
        let ordinal = index + 1;
        if !matches!(declaration.ty, TypeRef::Primitive(ScalarType::Real)) {
            return Err(PackageError::InvalidVariableNominal {
                ordinal,
                detail: "only Real declarations admit a nominal",
            });
        }
        if !nominal.is_finite() || *nominal <= 0.0 {
            return Err(PackageError::InvalidVariableNominal {
                ordinal,
                detail: "a Real nominal must be finite and positive",
            });
        }
    }
    Ok(())
}

pub(super) fn validate_constant_folds(
    block: &crate::Block,
    folds: &[ConstantFoldedParameter],
) -> Result<(), PackageError> {
    let mut variables = std::collections::BTreeSet::new();
    for fold in folds {
        if !variables.insert(fold.variable.as_str()) {
            return Err(invalid_fold(fold, "the variable appears more than once"));
        }
        if fold.folded_from.trim().is_empty() {
            return Err(invalid_fold(fold, "the source function is empty"));
        }
        let Some(entity) = block.protected.iter().find(|entity| {
            entity.kind == ProtectedKind::DependentParameter
                && entity.decl.name.lexeme() == fold.variable
        }) else {
            return Err(invalid_fold(
                fold,
                "the variable is not a protected dependent parameter",
            ));
        };
        let Some(scalars) = literal_scalar_count(&entity.decl) else {
            return Err(invalid_fold(
                fold,
                "the dependent parameter has no fixed literal shape",
            ));
        };
        if fold.scalars != scalars {
            return Err(invalid_fold(
                fold,
                "the scalar count disagrees with the checked declaration shape",
            ));
        }
    }
    Ok(())
}

fn literal_scalar_count(declaration: &crate::ast::VariableDeclaration) -> Option<usize> {
    declaration
        .dimensions
        .iter()
        .try_fold(1usize, |count, dimension| {
            let crate::ast::Dimension::Expr(crate::ast::Expression::Integer(extent)) = dimension
            else {
                return None;
            };
            usize::try_from(*extent)
                .ok()
                .and_then(|extent| count.checked_mul(extent))
        })
}

fn invalid_fold(fold: &ConstantFoldedParameter, detail: &'static str) -> PackageError {
    PackageError::InvalidConstantFold {
        variable: fold.variable.clone(),
        detail,
    }
}
