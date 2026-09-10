use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringConversionFormatInput<'dae> {
    Options {
        minimum_length: Option<ExprId<'dae>>,
        left_justified: Option<ExprId<'dae>>,
        significant_digits: Option<ExprId<'dae>>,
    },
    Format {
        value: ExprId<'dae>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CheckedStringConversionFormat<'dae> {
    minimum_length: Option<ExprId<'dae>>,
    left_justified: Option<ExprId<'dae>>,
    significant_digits: Option<ExprId<'dae>>,
    explicit: Option<ExprId<'dae>>,
}
impl<'dae> ExpressionAt<'_, 'dae> {
    /// Construct one MLS §3.7.1 predefined scalar-to-String conversion.
    ///
    /// The declaration must match the Resolve-proven identity registered by
    /// the DAE semantic owner before expression construction.
    pub fn string_conversion(
        self,
        declaration: rumoca_core::DefId,
        value: ExprId<'dae>,
        format: StringConversionFormatInput<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        match self.storage.predefined_string_declaration {
            Some(expected) if expected != declaration => {
                return Err(DaeConstructionError::ConflictingPredefinedString {
                    expected,
                    found: declaration,
                    span: self.provenance.span(),
                });
            }
            Some(_) => {}
            None => {
                return Err(DaeConstructionError::MissingPredefinedString {
                    span: self.provenance.span(),
                });
            }
        }
        let value_type = self.storage.expr_type(value, self.provenance)?;
        if !value_type.is_scalar() {
            return Err(DaeConstructionError::ExpectedScalar {
                span: self.provenance.span(),
            });
        }
        let source = value_type.scalar_type();
        if matches!(
            source,
            ScalarType::Enumeration | ScalarType::String | ScalarType::Record
        ) {
            return Err(DaeConstructionError::InvalidStringConversionSource {
                found: source,
                span: self.provenance.span(),
            });
        }

        let checked =
            check_string_conversion_format(self.storage, source, format, self.provenance)?;
        let operands = [
            Some(value),
            checked.minimum_length,
            checked.left_justified,
            checked.significant_digits,
            checked.explicit,
        ];
        let mut variability = ExpressionVariability::Constant;
        for operand in operands.into_iter().flatten() {
            variability = variability.max(self.storage.expr_variability(operand, self.provenance)?);
        }
        let binder_domain = merged_binder_domain(
            self.storage,
            operands.into_iter().flatten(),
            self.provenance,
        )?;
        let ty = self
            .storage
            .intern_type(ValueType::scalar(ScalarType::String), self.provenance)?;
        self.insert(
            ExprNode::StringConversion {
                declaration,
                value: value.index(),
                minimum_length: checked.minimum_length.map(ExprId::index),
                left_justified: checked.left_justified.map(ExprId::index),
                significant_digits: checked.significant_digits.map(ExprId::index),
                format: checked.explicit.map(ExprId::index),
            },
            ty,
            variability,
            binder_domain,
        )
    }
}

fn check_string_conversion_format<'dae>(
    storage: &Storage,
    source: ScalarType,
    format: StringConversionFormatInput<'dae>,
    at: DaeProvenance,
) -> Result<CheckedStringConversionFormat<'dae>, DaeConstructionError> {
    match format {
        StringConversionFormatInput::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } => {
            expect_optional_scalar_type(storage, minimum_length, ScalarType::Integer, at)?;
            expect_optional_scalar_type(storage, left_justified, ScalarType::Boolean, at)?;
            expect_optional_scalar_type(storage, significant_digits, ScalarType::Integer, at)?;
            if significant_digits.is_some() && source != ScalarType::Real {
                return Err(DaeConstructionError::InvalidSignificantDigitsSource {
                    found: source,
                    span: at.span(),
                });
            }
            Ok(CheckedStringConversionFormat {
                minimum_length,
                left_justified,
                significant_digits,
                explicit: None,
            })
        }
        StringConversionFormatInput::Format { value } => {
            expect_optional_scalar_type(storage, Some(value), ScalarType::String, at)?;
            if !matches!(source, ScalarType::Real | ScalarType::Integer) {
                return Err(DaeConstructionError::InvalidStringFormatSource {
                    found: source,
                    span: at.span(),
                });
            }
            Ok(CheckedStringConversionFormat {
                minimum_length: None,
                left_justified: None,
                significant_digits: None,
                explicit: Some(value),
            })
        }
    }
}

fn expect_optional_scalar_type(
    storage: &Storage,
    expression: Option<ExprId<'_>>,
    expected: ScalarType,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let Some(expression) = expression else {
        return Ok(());
    };
    let found = storage.expr_type(expression, at)?;
    if !found.is_scalar() {
        return Err(DaeConstructionError::ExpectedScalar { span: at.span() });
    }
    if found.scalar_type() != expected {
        return Err(DaeConstructionError::TypeMismatch {
            expected,
            found: found.scalar_type(),
            span: at.span(),
        });
    }
    Ok(())
}
