//! The definedness check of one expression read.

use super::*;

pub(super) struct DefinedValueReadChecker<'scope> {
    pub(super) definitions: &'scope FunctionDefinitions,
    /// The loop binder region of the expression being read: narrowed inside
    /// an if-expression branch by the conditions that select it.
    pub(super) folds: std::borrow::Cow<'scope, FoldScopes>,
    /// The if-expression conditions that select the expression being read.
    pub(super) selected_by: Vec<Expression>,
    pub(super) context: FunctionValidationContext<'scope>,
    pub(super) span: Span,
}

impl<'scope> DefinedValueReadChecker<'scope> {
    fn narrowed(&self, folds: FoldScopes) -> DefinedValueReadChecker<'scope> {
        DefinedValueReadChecker {
            definitions: self.definitions,
            folds: std::borrow::Cow::Owned(folds),
            selected_by: self.selected_by.clone(),
            context: self.context,
            span: self.span,
        }
    }

    /// The checker for a branch value that `condition` selects.
    fn selected(mut self, condition: &Expression) -> Self {
        self.selected_by.push(condition.clone());
        self
    }
}

impl FallibleExpressionVisitor for DefinedValueReadChecker<'_> {
    type Error = ToDaeError;

    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
    ) -> Result<(), Self::Error> {
        for subscript in subscripts {
            self.visit_subscript(subscript)?;
        }
        self.definitions.require_reference_readable(
            (&self.folds, &self.selected_by),
            name,
            subscripts,
            self.context,
            self.span,
        )
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> Result<(), Self::Error> {
        for arg in function.value_args(args) {
            self.visit_expression(arg)?;
        }
        Ok(())
    }

    /// MLS §3.6.5: a branch value is evaluated only when its condition, and
    /// the failure of every earlier one, select it; inside a loop iteration
    /// its reads are judged on the binder values that select it.
    fn visit_if(
        &mut self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
    ) -> Result<(), Self::Error> {
        if !self.folds.is_active() {
            for (condition, value) in branches {
                self.visit_expression(condition)?;
                DefinedValueReadChecker {
                    definitions: self.definitions,
                    folds: std::borrow::Cow::Borrowed(&*self.folds),
                    selected_by: self.selected_by.clone(),
                    context: self.context,
                    span: self.span,
                }
                .selected(condition)
                .visit_expression(value)?;
            }
            return self.visit_expression(else_branch);
        }
        let mut remaining = self.folds.clone().into_owned();
        for (condition, value) in branches {
            self.narrowed(remaining.clone())
                .visit_expression(condition)?;
            let mut selected = remaining.clone();
            let reached = selected.assume(condition, true, self.context);
            remaining.assume(condition, false, self.context);
            if reached {
                self.narrowed(selected)
                    .selected(condition)
                    .visit_expression(value)?;
            }
        }
        if remaining.reaches() {
            self.narrowed(remaining).visit_expression(else_branch)?;
        }
        Ok(())
    }

    fn visit_index(
        &mut self,
        base: &Expression,
        subscripts: &[Subscript],
    ) -> Result<(), Self::Error> {
        let mut selected = Vec::new();
        if let Some(name) = indexed_reference(base, &mut selected) {
            selected.extend_from_slice(subscripts);
            for subscript in &selected {
                self.visit_subscript(subscript)?;
            }
            return self.definitions.require_reference_readable(
                (&self.folds, &self.selected_by),
                name,
                &selected,
                self.context,
                self.span,
            );
        }
        self.visit_expression(base)?;
        for subscript in subscripts {
            self.visit_subscript(subscript)?;
        }
        Ok(())
    }

    /// Check a comprehension body once per point of its iteration domain.
    ///
    /// MLS §10.4.2 binds each iterator to every value of its range in turn, so
    /// `{u[i] * V[i, j + 1] for i in 1:size(u, 1)}` reads exactly the elements
    /// those points name. When every range settles, each point is checked with
    /// its iterators as static integers, which names the read indices; a range
    /// that does not settle keeps the conservative whole-body check.
    fn visit_array_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&Expression>,
    ) -> Result<(), Self::Error> {
        for index in indices.iter().rev() {
            self.visit_expression(&index.range)?;
        }
        let Some(points) = comprehension_points(indices, self.context) else {
            self.visit_expression(expr)?;
            return filter.map_or(Ok(()), |filter| self.visit_expression(filter));
        };
        for point in points {
            let mut checker = DefinedValueReadChecker {
                definitions: self.definitions,
                folds: std::borrow::Cow::Borrowed(&self.folds),
                selected_by: self.selected_by.clone(),
                context: FunctionValidationContext {
                    comprehension_values: Some(&point),
                    ..self.context
                },
                span: self.span,
            };
            if let Some(filter) = filter {
                checker.visit_expression(filter)?;
            }
            checker.visit_expression(expr)?;
        }
        Ok(())
    }
}

/// Enumerate every iterator binding of a comprehension whose ranges all
/// settle to static integers, opening the last textual iterator first.
fn comprehension_points(
    indices: &[rumoca_core::ComprehensionIndex],
    context: FunctionValidationContext<'_>,
) -> Option<Vec<ComprehensionValues>> {
    let mut points = vec![context.comprehension_values.cloned().unwrap_or_default()];
    for index in indices.iter().rev() {
        let mut extended = Vec::new();
        for prefix in &points {
            let point_context = FunctionValidationContext {
                comprehension_values: Some(prefix),
                ..context
            };
            for value in static_subscript_indices(&index.range, point_context)? {
                let mut point = prefix.clone();
                point.bind(index, value);
                extended.push(point);
            }
        }
        points = extended;
    }
    Some(points)
}

fn indexed_reference<'expression>(
    expression: &'expression Expression,
    subscripts: &mut Vec<Subscript>,
) -> Option<&'expression rumoca_core::Reference> {
    match expression {
        Expression::VarRef {
            name,
            subscripts: direct,
            ..
        } => {
            subscripts.extend_from_slice(direct);
            Some(name)
        }
        Expression::Index {
            base,
            subscripts: selected,
            ..
        } => {
            let name = indexed_reference(base, subscripts)?;
            subscripts.extend_from_slice(selected);
            Some(name)
        }
        _ => None,
    }
}
