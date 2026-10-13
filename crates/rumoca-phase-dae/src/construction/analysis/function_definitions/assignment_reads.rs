use super::*;
use rumoca_core::ExpressionVisitor;

impl FunctionDefinitions {
    /// Whole carried values readable on this exact source entry path.
    pub(in crate::construction::analysis) fn proven_carried_entry_reads(
        &self,
        targets: &[VarName],
        context: FunctionValidationContext<'_>,
        span: Span,
    ) -> Vec<(VarName, Span)> {
        if self.folds.is_learning() {
            return Vec::new();
        }
        targets
            .iter()
            .filter(|name| {
                self.require_named_readable((&self.folds, &[]), name, &[], context, span)
                    .is_ok()
            })
            .map(|name| (name.clone(), span))
            .collect()
    }

    /// Eager mutable reads already proven total on this selected source path.
    pub(in crate::construction::analysis) fn proven_assignment_reads(
        &self,
        expression: &Expression,
        context: FunctionValidationContext<'_>,
        span: Span,
    ) -> Vec<(VarName, Span)> {
        if self.folds.is_learning() {
            return Vec::new();
        }
        let mut reads = ProvenAssignmentReads {
            definitions: self,
            context,
            reads: Vec::new(),
            span,
        };
        reads.visit_expression(expression);
        reads.reads
    }
}

struct ProvenAssignmentReads<'scope> {
    definitions: &'scope FunctionDefinitions,
    context: FunctionValidationContext<'scope>,
    reads: Vec<(VarName, Span)>,
    span: Span,
}

impl ProvenAssignmentReads<'_> {
    /// Prove the read of `name`. A record field the function assigns on its
    /// own is a function value of its own, so its read is proven as that field
    /// and as the record that owns it.
    fn prove_reference(&mut self, name: &VarName, span: Option<Span>) {
        if self.context.loop_binders.contains(name) {
            return;
        }
        let field = self.definitions.tracks_value(name).then(|| name.clone());
        let span = span.unwrap_or(self.span);
        for root in field
            .into_iter()
            .chain(mutable_read_root(name, self.context))
        {
            self.prove_read(root, span);
        }
    }

    /// Record `root` as proven readable on this path, once.
    fn prove_read(&mut self, root: VarName, span: Span) {
        let readable = self
            .definitions
            .require_named_readable(
                (&self.definitions.folds, &[]),
                &root,
                &[],
                self.context,
                self.span,
            )
            .is_ok();
        if readable && !self.reads.iter().any(|(read, _)| read == &root) {
            self.reads.push((root, span));
        }
    }
}

impl ExpressionVisitor for ProvenAssignmentReads<'_> {
    fn visit_expression(&mut self, expression: &Expression) {
        if let Expression::VarRef {
            name: reference, ..
        } = expression
        {
            self.prove_reference(reference.var_name(), expression.span());
        }
        self.walk_expression(expression);
    }

    fn visit_if(&mut self, branches: &[(Expression, Expression)], _fallback: &Expression) {
        if let Some((condition, _)) = branches.first() {
            self.visit_expression(condition);
        }
    }

    fn visit_array_comprehension(
        &mut self,
        _body: &Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        _filter: Option<&Expression>,
    ) {
        if let Some(index) = indices.first() {
            self.visit_expression(&index.range);
        }
    }
}

fn mutable_read_root(name: &VarName, context: FunctionValidationContext<'_>) -> Option<VarName> {
    if declared_value(name, context).is_some() {
        return Some(name.clone());
    }
    context.record_roots.get(name).cloned()
}
