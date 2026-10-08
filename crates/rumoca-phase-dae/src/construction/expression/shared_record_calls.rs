//! One lowered occurrence per record-valued call in one argument list.
//!
//! Flatten passes a record-valued expression to a function whose record
//! parameter it decomposed as one field projection per leaf field, and every
//! projection owns a copy of the expression. When that expression is a call,
//! lowering each copy separately issues one call occurrence per leaf field,
//! and every occurrence is a full evaluation of the callee. All arguments of
//! one call are evaluated at one point, in one scope, so copies of the same
//! call among them denote one evaluation: the first lowering is the
//! occurrence, and each later copy projects its field from it.

use super::calls::{RecordBaseLowering, lower_record_array_field_access_with};
use super::*;

pub(super) struct SharedRecordCalls<'e, 'dae> {
    calls: Vec<(&'e Expression, dae::ExprId<'dae>)>,
}

impl<'e, 'dae> SharedRecordCalls<'e, 'dae> {
    pub(super) const fn new() -> Self {
        Self { calls: Vec::new() }
    }

    /// Lower one argument of the call, sharing the record-valued calls its
    /// field projections read with the arguments already lowered.
    pub(super) fn lower_argument(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        symbols: LoweringSymbols<'_, 'dae>,
        binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
        argument: &'e Expression,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let Expression::FieldAccess { span, .. } = argument else {
            return lower_expression_scoped(construction, symbols, binders, argument, None);
        };
        let provenance = expression_provenance(*span, None)?;
        lower_record_array_field_access_with(
            construction,
            symbols,
            binders,
            argument,
            provenance,
            self,
        )
    }
}

impl<'e, 'dae> RecordBaseLowering<'e, 'dae> for SharedRecordCalls<'e, 'dae> {
    fn lower_base(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        symbols: LoweringSymbols<'_, 'dae>,
        binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
        base: &'e Expression,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        match base {
            Expression::FieldAccess { .. } => {
                self.lower_argument(construction, symbols, binders, base)
            }
            Expression::FunctionCall { .. } => {
                if let Some((_, lowered)) = self.calls.iter().find(|(shared, _)| *shared == base) {
                    return Ok(*lowered);
                }
                let lowered = lower_expression_scoped(construction, symbols, binders, base, None)?;
                self.calls.push((base, lowered));
                Ok(lowered)
            }
            _ => lower_expression_scoped(construction, symbols, binders, base, None),
        }
    }
}
