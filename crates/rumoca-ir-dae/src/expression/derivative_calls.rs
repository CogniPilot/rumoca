use super::*;
use crate::FunctionDerivativeId;

struct SourceCall {
    function: u32,
    output: u32,
    operands: OperandRange,
    previous: Option<(u32, u32)>,
}

impl<'dae> ExpressionAt<'_, 'dae> {
    /// Preserve the MLS §12.7.1 differentiation history of this exact call.
    /// Arguments contain the unchanged primal prefix followed by checked tangents.
    pub fn differentiated_call(
        self,
        source: ExprId<'dae>,
        ordinal: u32,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        self.replay_differentiated_call(source, ordinal, arguments, None)
    }

    pub(crate) fn replay_differentiated_call(
        self,
        source: ExprId<'dae>,
        ordinal: u32,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
        expected: Option<(FunctionId<'dae>, u32)>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let SourceCall {
            function,
            output,
            operands,
            previous,
        } = source_call(self.storage, source, self.provenance)?;
        let link = FunctionDerivativeId::from_raw(function, ordinal);
        let entry = self.storage.function_derivative(link, self.provenance)?;
        if entry.previous().is_some()
            && entry
                .previous()
                .map(|id| (id.function().index(), id.ordinal()))
                != previous
        {
            return Err(invalid(
                self.provenance,
                "higher-order call lacks its derivative predecessor",
            ));
        }
        let prefix = &self.storage.expressions.operands[operands.indices()];
        if arguments.len() < prefix.len()
            || !arguments.iter().zip(prefix).all(|(a, b)| a.index() == *b)
        {
            return Err(invalid(
                self.provenance,
                "derivative call changed the original argument prefix",
            ));
        }
        let result = entry
            .result(output as usize)
            .ok_or_else(|| invalid(self.provenance, "source output has no derivative result"))?;
        let target = FunctionId::from_raw(entry.target().index());
        if expected
            .is_some_and(|(function, output)| function != target || output as usize != result)
        {
            return Err(invalid(
                self.provenance,
                "derivative call target or output differs from its link",
            ));
        }
        self.insert_call_results(
            target,
            [result],
            arguments,
            Some((source.index(), link.ordinal())),
        )
        .map(|mut results| results.pop().expect("one differentiated result"))
    }
}

fn source_call(
    storage: &Storage,
    source: ExprId<'_>,
    at: DaeProvenance,
) -> Result<SourceCall, DaeConstructionError> {
    let Some(ExprNode::Call {
        function,
        output,
        operands,
        derivative,
        ..
    }) = storage.expressions.nodes.get(source.index() as usize)
    else {
        return Err(invalid(
            at,
            "derivative source is not an earlier function call",
        ));
    };
    let previous = derivative.map(|(prior, ordinal)| {
        let ExprNode::Call { function, .. } = storage.expressions.nodes[prior as usize] else {
            unreachable!("construction checked the derivative source")
        };
        (function, ordinal)
    });
    Ok(SourceCall {
        function: *function,
        output: *output,
        operands: *operands,
        previous,
    })
}

fn invalid(at: DaeProvenance, reason: &'static str) -> DaeConstructionError {
    DaeConstructionError::InvalidFunctionDerivative {
        reason,
        span: at.span(),
    }
}
