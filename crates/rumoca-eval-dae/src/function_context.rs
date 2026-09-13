//! Source-bound function substitutions shared by DAE proof consumers.

mod scalar;
#[cfg(test)]
mod tests;

use rumoca_ir_dae as dae;

/// Arguments and call identity for one checked source function instantiation.
#[derive(Clone, Copy)]
struct FunctionCallFrame<'dae> {
    function: dae::FunctionId<'dae>,
    arguments: dae::ExpressionOperands<'dae>,
    call: dae::ExprId<'dae>,
}

/// One exact instantiation context for expressions read from checked Modelica
/// function bodies.
///
/// A function-body expression is not a whole-model value until each of its
/// parameter coordinates has been substituted by the corresponding caller
/// argument. Keeping that environment explicit lets structural proofs inspect
/// a body without cloning it into the source DAE or confusing two call sites.
#[derive(Clone, Default)]
pub struct FunctionCallContext<'dae> {
    frames: Vec<FunctionCallFrame<'dae>>,
}

impl<'dae> FunctionCallContext<'dae> {
    /// Exact outer-to-inner call identities for a source-bound proof consumer.
    pub fn call_path(&self) -> impl Iterator<Item = dae::ExprId<'dae>> + '_ {
        self.frames.iter().map(|frame| frame.call)
    }

    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    /// Restrict substitutions to the lexical owner of one checked expression.
    ///
    /// Following a parameter argument or a model-level causal definition can
    /// leave the current callee. Keeping deeper frames active there would let
    /// an unrelated call site capture record projections or nested calls.
    pub fn scoped_to_expression(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Self {
        let Some(owner) = view
            .expression(expression)
            .and_then(|node| node.function_scope())
        else {
            return Self::default();
        };
        let Some(frame) = self
            .frames
            .iter()
            .rposition(|frame| frame.function == owner)
        else {
            return Self::default();
        };
        Self {
            frames: self.frames[..=frame].to_vec(),
        }
    }

    pub fn parameter_argument(
        &self,
        parameter: dae::FunctionParameterId<'dae>,
    ) -> Option<dae::ExprId<'dae>> {
        self.frames
            .iter()
            .rev()
            .find(|frame| frame.function == parameter.function())
            .and_then(|frame| frame.arguments.get(parameter.ordinal() as usize))
    }

    /// Enter the selected result of a call only when its checked Modelica body
    /// is one straight-line assignment to that result.
    pub fn call_result(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Option<(dae::ExprId<'dae>, Self)> {
        let node = view.expression(expression)?;
        let dae::ExpressionOperation::Call {
            function,
            output,
            arguments,
            ..
        } = node.operation()
        else {
            return None;
        };
        if self.frames.iter().any(|frame| frame.function == function) {
            return None;
        }
        let result = single_assignment_result(view, function, output)?;
        let mut nested = self.clone();
        nested.frames.push(FunctionCallFrame {
            function,
            arguments,
            call: expression,
        });
        Some((result, nested))
    }

    /// Resolve a field projection through records, caller arguments, and
    /// straight-line function results until it names an existing expression.
    pub fn projected_field(
        &self,
        view: dae::DaeView<'dae>,
        mut base: dae::ExprId<'dae>,
        field: u32,
    ) -> Option<(dae::ExprId<'dae>, Self)> {
        let mut context = self.clone();
        loop {
            context = context.scoped_to_expression(view, base);
            if let Some(branch) = context.selected_branch(view, base) {
                base = branch;
                continue;
            }
            let node = view.expression(base)?;
            match node.operation() {
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                    parameter,
                )) => base = context.parameter_argument(parameter)?,
                dae::ExpressionOperation::Record(fields) => {
                    return Some((fields.get(field as usize)?, context));
                }
                dae::ExpressionOperation::Call { .. } => {
                    (base, context) = context.call_result(view, base)?;
                }
                _ => return None,
            }
        }
    }
}

fn single_assignment_result<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionId<'dae>,
    output: u32,
) -> Option<dae::ExprId<'dae>> {
    let function = view.function(function)?;
    if function.is_external() {
        return None;
    }
    let result = function.result_values().get(output as usize)?;
    let mut statements = function.statements();
    let dae::FunctionStatementView::Assignment { definition } = statements.next()? else {
        return None;
    };
    if statements.next().is_some()
        || definition.target() != result.target()
        || definition.rhs() != result.rhs()
    {
        return None;
    }
    Some(result.rhs())
}
