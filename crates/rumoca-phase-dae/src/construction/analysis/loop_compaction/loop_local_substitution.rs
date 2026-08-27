//! Eliminate loop-local prefixes only under exact dominance and use proofs.
//!
//! Two decisions here drop a definition: [`inline_dominated_loop_locals`]
//! substitutes one forward inside a loop body, and [`loop_local_substitutions`]
//! folds a whole prefix into the body's trailing statement. Both require a
//! [`liveness::LoopLocalStoreUnobserved`] for the name, which is the one place
//! the question "is this value observed again?" is answered, and both keep the
//! obligations that are not that question as explicit guards of their own.

use super::*;

/// Replace a perfect inner element loop with its tensor-native slice write.
///
/// `for j in r loop A[i,j] := B[i,j]; end for` owns exactly the same ordered
/// coordinate map as `A[i,r] := B[i,r]`. Keeping the enclosing loop intact
/// preserves source order while removing a nested transition the DAE would
/// otherwise have to scalarize.
///
/// `shapes` is the function scope's proven shape environment, which is what
/// makes the rank questions [`binder_slice_gain`] asks answerable: MLS §10.6.4
/// picks which product `*` denotes from its operands' ranks, so the rewrite
/// cannot be shown meaning-preserving from the expression tree alone.
pub(super) fn compact_perfect_inner_element_loops(
    statements: &[rumoca_core::Statement],
    shapes: &ShapeEnvironment,
) -> Vec<rumoca_core::Statement> {
    compact_element_loops(statements, false, shapes)
}

fn compact_element_loops(
    statements: &[rumoca_core::Statement],
    nested: bool,
    shapes: &ShapeEnvironment,
) -> Vec<rumoca_core::Statement> {
    statements
        .iter()
        .map(|statement| match statement {
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => rumoca_core::Statement::For {
                indices: indices.clone(),
                equations: compact_element_loops(
                    equations,
                    true,
                    &loop_body_shapes(indices, shapes),
                ),
                span: *span,
            },
            _ => statement.clone(),
        })
        .map(|statement| {
            if nested {
                compact_one_perfect_inner_loop(statement, shapes)
            } else {
                statement
            }
        })
        .collect()
}

/// The enclosing scope's shapes extended with this loop's binders.
///
/// MLS §11.2.2 makes a `for` index a scalar of the range's element type inside
/// the body, so binding it as a scalar is what lets the body's reads be shaped
/// at all — and it is the *per-iteration* shape environment, which is exactly
/// the one the rank questions below must be asked in.
fn loop_body_shapes(
    indices: &[rumoca_core::ForIndex],
    shapes: &ShapeEnvironment,
) -> ShapeEnvironment {
    let mut body = shapes.clone();
    for index in indices {
        body.insert(VarName::new(&index.ident), Vec::new());
    }
    body
}

/// Inline loop-local prefix definitions into one trailing statement.
///
/// Both decisions below drop a definition only while holding a
/// [`liveness::LoopLocalStoreUnobserved`] for its target, so nothing the
/// dataflow answer cannot certify is folded away. The prefix definition
/// dominates each substituted use, so a conditional or nested loop can retain
/// one compact owner without manufacturing a scalar scratch transition.
pub(super) fn inline_loop_local_prefixes(
    statements: &[rumoca_core::Statement],
    locals: &HashSet<VarName>,
    outputs: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    inline_loop_local_prefixes_in_scope(
        statements,
        &[],
        &[],
        locals,
        outputs,
        false,
        &HashSet::new(),
    )
}

fn inline_loop_local_prefixes_in_scope(
    statements: &[rumoca_core::Statement],
    enclosing_suffix: &[rumoca_core::Statement],
    back_edges: &[&[rumoca_core::Statement]],
    locals: &HashSet<VarName>,
    outputs: &HashSet<VarName>,
    preserve_shared: bool,
    enclosing_binders: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    statements
        .iter()
        .enumerate()
        .map(|(ordinal, statement)| {
            let mut suffix = statements[ordinal + 1..].to_vec();
            suffix.extend_from_slice(enclosing_suffix);
            inline_one_loop_local_prefix(
                statement,
                &suffix,
                back_edges,
                locals,
                outputs,
                preserve_shared,
                enclosing_binders,
            )
        })
        .collect()
}

/// The names the function's caller observes once the body returns.
///
/// MLS §12.4.1 makes a function's outputs its result, so a value left in an
/// output is observed even though no statement of the body reads it. Handing
/// them to the liveness prover as the set live on exit is what lets it answer
/// the question a membership test can only refuse: an output the exit path
/// definitely rewrites before returning is not observed at the store under
/// proof.
fn observed_by_the_caller(outputs: &HashSet<VarName>) -> liveness::LiveSet {
    outputs.iter().cloned().collect()
}

fn inline_one_loop_local_prefix(
    statement: &rumoca_core::Statement,
    suffix: &[rumoca_core::Statement],
    back_edges: &[&[rumoca_core::Statement]],
    locals: &HashSet<VarName>,
    outputs: &HashSet<VarName>,
    preserve_shared: bool,
    enclosing_binders: &HashSet<VarName>,
) -> rumoca_core::Statement {
    if let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    {
        let cond_blocks = cond_blocks
            .iter()
            .map(|block| rumoca_core::StatementBlock {
                cond: block.cond.clone(),
                stmts: inline_loop_local_prefixes_in_scope(
                    &block.stmts,
                    suffix,
                    back_edges,
                    locals,
                    outputs,
                    preserve_shared,
                    enclosing_binders,
                ),
            })
            .collect();
        let else_block = else_block.as_ref().map(|statements| {
            inline_loop_local_prefixes_in_scope(
                statements,
                suffix,
                back_edges,
                locals,
                outputs,
                preserve_shared,
                enclosing_binders,
            )
        });
        return rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span: *span,
        };
    }
    let rumoca_core::Statement::For {
        indices,
        equations,
        span,
    } = statement
    else {
        return statement.clone();
    };
    let mut body_back_edges = Vec::with_capacity(back_edges.len() + 1);
    body_back_edges.push(equations.as_slice());
    body_back_edges.extend_from_slice(back_edges);
    let preserve_shared = independent_loop_domain(indices, enclosing_binders);
    let mut body_binders = enclosing_binders.clone();
    body_binders.extend(indices.iter().map(|index| VarName::new(&index.ident)));
    let equations = inline_loop_local_prefixes_in_scope(
        equations,
        suffix,
        &body_back_edges,
        locals,
        outputs,
        preserve_shared,
        &body_binders,
    );
    let equations = inline_dominated_loop_locals(
        equations,
        suffix,
        back_edges,
        locals,
        outputs,
        preserve_shared,
    );
    let Some((trailing, prefix)) = equations.split_last() else {
        return statement.clone();
    };
    if prefix.is_empty() {
        return statement_with_equations(indices, equations, *span);
    }
    let Some(substitutions) = loop_local_substitutions(
        prefix,
        suffix,
        back_edges,
        std::slice::from_ref(trailing),
        outputs,
        preserve_shared,
    ) else {
        return statement_with_equations(indices, equations, *span);
    };
    let mut rewriter = LocalSubstitution {
        values: &substitutions,
    };
    rumoca_core::Statement::For {
        indices: indices.clone(),
        equations: vec![rewriter.rewrite_statement(trailing)],
        span: *span,
    }
}

/// Replace a definition that dominates its uses with its value at those uses,
/// inside one loop body.
///
/// A definition is dropped only while holding a
/// [`liveness::LoopLocalStoreUnobserved`] for its target, so a name the
/// dataflow answer cannot certify keeps every store it has. The witness owns
/// the four questions about whether the value is observed again: by the caller
/// through an output, by the statements that run once the loop is left, by this
/// body's own back edge, and by an enclosing loop's. The guards left here are
/// the ones that are not liveness questions at all: whether the stored
/// expression can be reproduced at each later use, and the shared-scratch
/// retention policy.
fn inline_dominated_loop_locals(
    mut statements: Vec<rumoca_core::Statement>,
    outer_suffix: &[rumoca_core::Statement],
    back_edges: &[&[rumoca_core::Statement]],
    locals: &HashSet<VarName>,
    outputs: &HashSet<VarName>,
    preserve_shared: bool,
) -> Vec<rumoca_core::Statement> {
    let live_on_exit = observed_by_the_caller(outputs);
    let mut ordinal = 0usize;
    while ordinal < statements.len() {
        let Some((target, value)) = scalar_local_definition(&statements[ordinal]) else {
            ordinal += 1;
            continue;
        };
        if !locals.contains(&target) {
            ordinal += 1;
            continue;
        }
        let suffix = &statements[ordinal + 1..];
        let substituted = [suffix];
        let witness = liveness::prove_unobserved_loop_local_store(
            &target,
            liveness::LoopLocalStoreRegions {
                substituted: &substituted,
                body_prefix: &statements[..ordinal],
                enclosing_bodies: back_edges,
                exit: outer_suffix,
                live_on_exit: &live_on_exit,
            },
        );
        let Some(witness) = witness else {
            ordinal += 1;
            continue;
        };
        if statements_assign_name(suffix, &target)
            || (preserve_shared
                && statements_read_count(suffix, &target) > 1
                && !statements_loop_ranges_read_name(suffix, &target))
            || expression_reads_name(&value, &target)
            // Substituting the value at a later use only reproduces this
            // definition while the value still reads the same thing. A later
            // write to any of its dependencies would silently move the read
            // forward in the algorithm's order (MLS §11.1).
            || expression_dependencies_change(&value, suffix)
        {
            ordinal += 1;
            continue;
        }
        let substitutions = HashMap::from([(witness.name().clone(), value)]);
        let mut rewriter = LocalSubstitution {
            values: &substitutions,
        };
        let rewritten = rewriter.rewrite_statements(suffix);
        statements.splice(ordinal.., rewritten);
    }
    statements
}

fn scalar_local_definition(statement: &rumoca_core::Statement) -> Option<(VarName, Expression)> {
    let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
        return None;
    };
    let [part] = comp.parts() else {
        return None;
    };
    part.subs
        .is_empty()
        .then(|| (VarName::new(&part.ident), value.clone()))
}

/// Whether a later statement moves a value `expression` reads.
///
/// Substituting an expression at a later use reproduces the definition only
/// while everything the expression reads still holds what it held where the
/// definition ran (MLS §11.1). A later write reaches that value whenever the
/// written name and the read name denote the same storage or one is nested in
/// the other: `t := e` defines every field beneath `t`, and `t.f := e`
/// redefines part of `t`. Comparing the two names for equality alone lets both
/// of those writes through, and the substituted expression then reads the new
/// value at the old position.
pub(super) fn expression_dependencies_change(
    expression: &Expression,
    statements: &[rumoca_core::Statement],
) -> bool {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references
        .iter()
        .any(|reference| statements_write_value_named(statements, reference))
}

fn statements_write_value_named(
    statements: &[rumoca_core::Statement],
    reference: &VarName,
) -> bool {
    statements_write_where(statements, &|component| {
        component_writes_value_named(component, reference)
    })
}

/// Whether writing through `component` reaches the value `reference` names.
///
/// Both names are read as dotted paths, and they reach the same storage when
/// one path is a prefix of the other. Subscripts are dropped from both sides: a
/// write to one element is a write to part of the array, which is what a read
/// of the array observes.
fn component_writes_value_named(
    component: &rumoca_core::ComponentReference,
    reference: &VarName,
) -> bool {
    let read = value_path(reference.as_str());
    let written = component
        .parts()
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>();
    let shared = read.len().min(written.len());
    read[..shared] == written[..shared]
}

/// The dotted path a value name spells, with each segment's subscripts dropped.
///
/// A name reaches this analysis as flat text that may carry subscripts inside
/// it, as in `a[i].b`, so the split has to ignore a dot that sits inside a
/// subscript.
fn value_path(text: &str) -> Vec<&str> {
    let mut path = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (offset, byte) in text.bytes().enumerate() {
        match byte {
            b'[' => depth += 1,
            b']' => depth = depth.saturating_sub(1),
            b'.' if depth == 0 => {
                path.push(&text[start..offset]);
                start = offset + 1;
            }
            _ => {}
        }
    }
    path.push(&text[start..]);
    path.into_iter().map(strip_subscripts).collect()
}

fn strip_subscripts(segment: &str) -> &str {
    match segment.find('[') {
        Some(offset) => &segment[..offset],
        None => segment,
    }
}

/// The value each loop-local prefix definition contributes to the trailing
/// statement, or nothing when the prefix cannot be folded away.
///
/// The whole prefix disappears when this succeeds, so every one of its
/// definitions needs its own [`liveness::LoopLocalStoreUnobserved`]; a single
/// name the dataflow answer cannot certify keeps the prefix as it stands. The
/// witness owns whether the value is observed again; the guards here own
/// whether the value can be reproduced inside the trailing statement.
///
/// The trailing statement is rewritten in one pass, so a read it performs after
/// writing the name would be replaced with the prefix's value rather than the
/// one it just wrote. `statements_assign_name` is therefore a refusal: the
/// substitution has no way to stop at the write.
fn loop_local_substitutions(
    prefix: &[rumoca_core::Statement],
    exit: &[rumoca_core::Statement],
    enclosing_bodies: &[&[rumoca_core::Statement]],
    trailing: &[rumoca_core::Statement],
    outputs: &HashSet<VarName>,
    preserve_shared: bool,
) -> Option<HashMap<VarName, Expression>> {
    let live_on_exit = observed_by_the_caller(outputs);
    let mut substitutions = HashMap::new();
    for (ordinal, statement) in prefix.iter().enumerate() {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            return None;
        };
        let [part] = comp.parts() else {
            return None;
        };
        if !part.subs.is_empty() {
            return None;
        }
        let target = VarName::new(&part.ident);
        // The values of the later prefix definitions are rewritten too, so
        // their reads of this name are substituted reads like the trailing
        // statement's and carry the same obligations.
        let substituted = [&prefix[ordinal + 1..], trailing];
        let witness = liveness::prove_unobserved_loop_local_store(
            &target,
            liveness::LoopLocalStoreRegions {
                substituted: &substituted,
                body_prefix: &prefix[..ordinal],
                enclosing_bodies,
                exit,
                live_on_exit: &live_on_exit,
            },
        )?;
        let mut rewriter = LocalSubstitution {
            values: &substitutions,
        };
        // The expression that actually lands in the trailing statement is the
        // one already folded through the earlier definitions, so it is the one
        // whose dependencies have to hold still.
        let replacement = rewriter.rewrite_expression(value);
        if statements_assign_name(trailing, &target)
            || (preserve_shared
                && statement_segments_read_count(&substituted, &target) > 1
                && !statements_loop_ranges_read_name(trailing, &target))
            || expression_reads_name(value, &target)
            || expression_dependencies_change(&replacement, &prefix[ordinal + 1..])
            || expression_dependencies_change(&replacement, trailing)
        {
            return None;
        }
        substitutions.insert(witness.name().clone(), replacement);
    }
    Some(substitutions)
}

/// The deliberately narrow domain where retaining shared scratch cannot block
/// dependent-domain normalization later in the pipeline.
pub(super) fn independent_loop_domain(
    indices: &[rumoca_core::ForIndex],
    enclosing_binders: &HashSet<VarName>,
) -> bool {
    let mut visible_binders = enclosing_binders.clone();
    for index in indices {
        if visible_binders
            .iter()
            .any(|binder| expression_reads_name(&index.range, binder))
        {
            return false;
        }
        visible_binders.insert(VarName::new(&index.ident));
    }
    true
}

/// Whether a value feeds a nested loop domain and therefore must remain
/// available to dependent-domain normalization.
///
/// Shared scratch is normally retained to avoid duplicating its expression at
/// every use.  A loop bound is the exception: substituting its dominating
/// scalar definition exposes the affine binder relation that rectangularization
/// must prove, whereas retaining the temporary would hide that relation behind
/// mutable function storage.
fn statements_loop_ranges_read_name(statements: &[rumoca_core::Statement], name: &VarName) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            indices
                .iter()
                .any(|index| expression_reads_name(&index.range, name))
                || statements_loop_ranges_read_name(equations, name)
        }
        rumoca_core::Statement::While { block, .. } => {
            statements_loop_ranges_read_name(&block.stmts, name)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_loop_ranges_read_name(&block.stmts, name))
                || else_block
                    .as_deref()
                    .is_some_and(|branch| statements_loop_ranges_read_name(branch, name))
        }
        rumoca_core::Statement::When { blocks, .. } => blocks
            .iter()
            .any(|block| statements_loop_ranges_read_name(&block.stmts, name)),
        _ => false,
    })
}

pub(super) fn statement_with_equations(
    indices: &[rumoca_core::ForIndex],
    equations: Vec<rumoca_core::Statement>,
    span: Span,
) -> rumoca_core::Statement {
    rumoca_core::Statement::For {
        indices: indices.to_vec(),
        equations,
        span,
    }
}

pub(super) struct LocalSubstitution<'a> {
    pub(super) values: &'a HashMap<VarName, Expression>,
}

impl ExpressionRewriter for LocalSubstitution<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if subscripts.is_empty()
            && let Some(value) = self.values.get(name.var_name())
        {
            return value.clone();
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

impl StatementRewriter for LocalSubstitution<'_> {}

pub(in crate::construction::analysis) fn statements_read_name(
    statements: &[rumoca_core::Statement],
    name: &VarName,
) -> bool {
    struct Finder<'a> {
        name: &'a VarName,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            self.found |= reference_reads_name(reference, self.name);
            self.walk_var_ref(reference, subscripts);
        }
    }
    impl rumoca_ir_flat::visitor::StatementVisitor for Finder<'_> {}

    let mut finder = Finder { name, found: false };
    for statement in statements {
        rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut finder, statement);
        if finder.found {
            return true;
        }
    }
    false
}

/// Count reads only up to two: scratch substitution needs to distinguish a
/// single use from duplication, and no larger cardinality changes that proof.
pub(super) fn statements_read_count(
    statements: &[rumoca_core::Statement],
    name: &VarName,
) -> usize {
    statement_segments_read_count(&[statements], name)
}

fn statement_segments_read_count(segments: &[&[rumoca_core::Statement]], name: &VarName) -> usize {
    struct Counter<'a> {
        name: &'a VarName,
        count: usize,
    }
    impl rumoca_core::ExpressionVisitor for Counter<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            if self.count < 2 && reference_reads_name(reference, self.name) {
                self.count += 1;
            }
            if self.count < 2 {
                self.walk_var_ref(reference, subscripts);
            }
        }
    }
    impl rumoca_ir_flat::visitor::StatementVisitor for Counter<'_> {}

    let mut counter = Counter { name, count: 0 };
    for statements in segments {
        for statement in *statements {
            rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut counter, statement);
            if counter.count == 2 {
                return 2;
            }
        }
    }
    counter.count
}

fn reference_reads_name(reference: &Reference, name: &VarName) -> bool {
    let reference = reference.var_name().as_str();
    let name = name.as_str();
    reference == name
        || reference
            .strip_prefix(name)
            .is_some_and(|suffix| suffix.starts_with('.'))
}

pub(super) fn statements_read_nonrewritable_name(
    statements: &[rumoca_core::Statement],
    name: &VarName,
) -> bool {
    struct Finder<'a> {
        name: &'a VarName,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            self.found |= reference_reads_name(reference, self.name)
                && (reference.var_name() != self.name || !subscripts.is_empty());
            self.walk_var_ref(reference, subscripts);
        }
    }
    impl rumoca_ir_flat::visitor::StatementVisitor for Finder<'_> {}

    let mut finder = Finder { name, found: false };
    for statement in statements {
        rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut finder, statement);
    }
    finder.found
}

pub(super) fn expression_reads_name(expression: &Expression, name: &VarName) -> bool {
    struct Finder<'a> {
        name: &'a VarName,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            self.found |= reference_reads_name(reference, self.name);
            self.walk_var_ref(reference, subscripts);
        }
    }
    let mut finder = Finder { name, found: false };
    rumoca_core::ExpressionVisitor::visit_expression(&mut finder, expression);
    finder.found
}

fn statements_assign_name(statements: &[rumoca_core::Statement], name: &VarName) -> bool {
    statements_write_where(statements, &|component| {
        component_targets_name(component, name)
    })
}

/// Whether any statement writes through a component reference `writes` accepts.
///
/// The two write questions this module asks differ only in which references
/// count, so the walk over the statement tree is stated once here.
fn statements_write_where(
    statements: &[rumoca_core::Statement],
    writes: &dyn Fn(&rumoca_core::ComponentReference) -> bool,
) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Assignment { comp, .. } => writes(comp),
        // A multi-output call `(a, b) := f(...)` writes through its receiving
        // list (MLS §12.4.4); missing those writes would let a substitution
        // read a value the call has already replaced.
        rumoca_core::Statement::FunctionCall { outputs, .. } => {
            outputs.iter().flatten().any(writes)
        }
        rumoca_core::Statement::For { equations, .. } => statements_write_where(equations, writes),
        rumoca_core::Statement::While { block, .. } => statements_write_where(&block.stmts, writes),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_write_where(&block.stmts, writes))
                || else_block
                    .as_deref()
                    .is_some_and(|block| statements_write_where(block, writes))
        }
        rumoca_core::Statement::When { blocks, .. } => blocks
            .iter()
            .any(|block| statements_write_where(&block.stmts, writes)),
        _ => false,
    })
}

fn component_targets_name(component: &rumoca_core::ComponentReference, name: &VarName) -> bool {
    matches!(component.parts(), [part] if part.ident == name.as_str())
}

pub(in crate::construction::analysis) fn statements_partially_assign_name(
    statements: &[rumoca_core::Statement],
    name: &VarName,
) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Assignment { comp, .. } => {
            component_targets_name(comp, name) && scalar_assignment_target(comp).is_none()
        }
        rumoca_core::Statement::FunctionCall { outputs, .. } => {
            outputs.iter().flatten().any(|output| {
                component_targets_name(output, name) && scalar_assignment_target(output).is_none()
            })
        }
        rumoca_core::Statement::For { equations, .. } => {
            statements_partially_assign_name(equations, name)
        }
        rumoca_core::Statement::While { block, .. } => {
            statements_partially_assign_name(&block.stmts, name)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_partially_assign_name(&block.stmts, name))
                || else_block
                    .as_deref()
                    .is_some_and(|branch| statements_partially_assign_name(branch, name))
        }
        _ => false,
    })
}

fn compact_one_perfect_inner_loop(
    statement: rumoca_core::Statement,
    shapes: &ShapeEnvironment,
) -> rumoca_core::Statement {
    let rumoca_core::Statement::For {
        indices,
        equations,
        span: _,
    } = &statement
    else {
        return statement;
    };
    let [index] = indices.as_slice() else {
        return statement;
    };
    let [
        rumoca_core::Statement::Assignment {
            comp,
            value,
            span: assignment_span,
        },
    ] = equations.as_slice()
    else {
        return statement;
    };
    let body = loop_body_shapes(indices, shapes);
    // Both sides own the same claim: the write and the value must each stack
    // the loop's iterations along one new *leading* dimension, or they are not
    // stacking them along the same one.
    if binder_slice_target_gain(comp, &index.ident, &body) != Some(SliceGain::Leading)
        || binder_slice_gain(value, &index.ident, &body) != Some(SliceGain::Leading)
    {
        return statement;
    }
    let mut replacement = BinderSliceReplacement {
        binder: &index.ident,
        range: &index.range,
        replacements: 0,
        unsupported: false,
    };
    let parts = comp
        .parts()
        .iter()
        .cloned()
        .map(|mut part| {
            part.subs = replacement.rewrite_subscripts(&part.subs);
            part
        })
        .collect();
    let Ok(comp) = comp.with_replaced_parts(parts) else {
        return statement;
    };
    let target_replacements = replacement.replacements;
    let value = replacement.rewrite_expression(value);
    if replacement.unsupported
        || target_replacements != 1
        || replacement.replacements == target_replacements
    {
        return statement;
    }
    rumoca_core::Statement::Assignment {
        comp,
        value,
        span: *assignment_span,
    }
}

/// Prove that turning every `binder` subscript into the loop's whole range
/// leaves each operator computing the same elements, in the same order.
///
/// [`compact_perfect_inner_element_loops`] owns the claim that
/// `for j in r loop A[i,j] := e; end for` and `A[i,r] := e[j := r]` write the
/// same ordered coordinate map. That claim is a claim about *e* as much as
/// about the write, and it fails in two distinct ways.
///
/// **Which operator the expression denotes.** MLS §10.6.3 and §10.6.4 give
/// `+`, `-`, `*`, `/` and `^` meanings that read their operands' *ranks*, so
/// raising an operand's rank by one can silently change the operator. The
/// binder-sliced operand of a scalar product is the case: MLS §10.6.4 makes
/// `v * w` on two vectors the scalar product, so `s[j] * a[i,j]` — two
/// per-element reads under the loop — becomes the *contraction* `s[r] * a[i,r]`
/// rather than the row of products the source wrote.
///
/// **Where the raised dimension lands.** A rank is not a position. `a[i,:] *
/// b[j,:]` is a scalar product of two *vectors*, and slicing raises `b[j,:]` to
/// the matrix `b[r,:]` whose leading dimension is the sliced one — so MLS
/// §10.6.4's vector-matrix product contracts the dimension the slice added and
/// returns `a * b` where the source wrote `a * transpose(b)`. Equally,
/// MLS §10.4 stacks an array construction along a leading dimension of its own,
/// so `{a[i,j], 10*a[i,j]}` compacts to the transpose of the matrix the loop
/// wrote. Both type-check at exactly the shape the target gained, so neither is
/// reported: the proof must read rank *position*, not rank alone, which is why
/// it needs the scope's shapes rather than the expression tree alone.
///
/// Returns `Some(gain)` when the rewrite is meaning-preserving, where `gain`
/// says *where* this subtree's compacted value carries the sliced dimension,
/// and `None` when it is not — in which case the caller keeps the scalar loop,
/// which is always a legal owner for the same coordinates.
fn binder_slice_gain(
    expression: &Expression,
    binder: &str,
    shapes: &ShapeEnvironment,
) -> Option<SliceGain> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            if name.as_str() == binder && subscripts.is_empty() {
                return None;
            }
            let mut scan = SliceScan::new(binder, shapes);
            scan.subscripts(subscripts)?;
            scan.gain(expression)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            // A base that already gained the sliced dimension shifts every
            // later subscript onto a different dimension than the source read.
            if binder_slice_gain(base, binder, shapes)?.gains() {
                return None;
            }
            let mut scan = SliceScan::new(binder, shapes);
            scan.subscripts(subscripts)?;
            scan.gain(expression)
        }
        // MLS §10.6.1 negation is element-wise, so it keeps its operand's gain
        // — including a trailing one, which `reject_trailing` then refuses
        // rather than carry outward as the loop's stack.
        Expression::Unary { rhs, .. } => {
            binder_slice_gain(rhs, binder, shapes).and_then(reject_trailing)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs_gain = binder_slice_gain(lhs, binder, shapes)?;
            let rhs_gain = binder_slice_gain(rhs, binder, shapes)?;
            binder_slice_binary(op, (lhs, lhs_gain), (rhs, rhs_gain), shapes)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let gain = reject_trailing(binder_slice_gain(else_branch, binder, shapes)?)?;
            for (condition, value) in branches {
                if binder_slice_gain(condition, binder, shapes)?.gains()
                    || binder_slice_gain(value, binder, shapes)? != gain
                {
                    return None;
                }
            }
            Some(gain)
        }
        Expression::Literal { .. } | Expression::Empty { .. } => Some(SliceGain::Invariant),
        Expression::FieldAccess { base, .. } => {
            binder_slice_gain(base, binder, shapes).and_then(reject_trailing)
        }
        // MLS §10.6.1 applies these operators element-wise to an array
        // argument, so an argument that gained the sliced dimension keeps the
        // per-element meaning the scalar loop wrote.
        Expression::BuiltinCall { function, args, .. }
            if builtin_maps_elementwise(*function, args.len()) =>
        {
            binder_slice_broadcast(args, binder, shapes)
        }
        // Everything below builds its result along a dimension of its own —
        // MLS §10.4 stacks an array construction's elements along a new
        // *leading* dimension, a range and a comprehension index own the
        // dimension they generate, an argument's rank is what selects a call's
        // MLS §10.6.1/§12.4.6 reading, and `String` has no array form at all.
        // A gained dimension therefore lands somewhere other than where the
        // scalar loop stacked it. Reading the binder as a plain subscript
        // underneath is still fine; gaining a dimension is not.
        Expression::StringConversion { value, .. } => {
            binder_slice_invariant(std::slice::from_ref(&**value), binder, shapes)
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            binder_slice_invariant(args, binder, shapes)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            binder_slice_invariant(elements, binder, shapes)
        }
        Expression::Range {
            start, step, end, ..
        } => {
            binder_slice_invariant(std::slice::from_ref(&**start), binder, shapes)?;
            if let Some(step) = step {
                binder_slice_invariant(std::slice::from_ref(&**step), binder, shapes)?;
            }
            binder_slice_invariant(std::slice::from_ref(&**end), binder, shapes)
        }
        Expression::ArrayComprehension { expr, filter, .. } => {
            binder_slice_invariant(std::slice::from_ref(&**expr), binder, shapes)?;
            match filter {
                Some(filter) => {
                    binder_slice_invariant(std::slice::from_ref(&**filter), binder, shapes)
                }
                None => Some(SliceGain::Invariant),
            }
        }
    }
}

/// Where a compacted subtree carries the dimension the binder slice added.
///
/// `for j in r loop A[..,j,..] := e; end for` stacks its per-iteration values
/// along one *new leading* dimension — that is precisely what the compacted
/// write `A[..,r,..]` means once every preceding subscript is a scalar index.
/// A subtree whose compacted value carries the added dimension anywhere else,
/// or carries more than one, is not that stack however exactly its shape lines
/// up with the target's: MLS §10.4 builds an array construction along a
/// leading dimension of its own, so `{f(x[j]), g(x[j])}` compacts to the
/// *transpose* of the matrix the loop wrote, at identical shape whenever the
/// constructor's element count equals the sliced extent.
///
/// A two-valued "leading or nothing" lattice cannot express the textbook matrix
/// product. `y[i,j] := a[i,:] * b[:,j]` slices `b`'s *trailing* dimension, and
/// MLS §10.6.4's vector-matrix product then contracts `b`'s leading one — so the
/// sliced dimension is the compacted result's only dimension, which is the very
/// leading stack the loop wrote. "Gained trailing, then contracted back to
/// leading" is a legal path through the rewrite and needs a name of its own;
/// [`SliceGain::Trailing`] is that name, and [`binder_slice_product`] is the one
/// place it is allowed to reach an answer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SliceGain {
    /// The subtree never reads the binder. The rewrite leaves it — and, which
    /// is what the operator rules read, its rank — exactly as the scalar loop
    /// read it.
    Invariant,
    /// The subtree gains the sliced dimension as its leading dimension.
    Leading,
    /// The subtree gains the sliced dimension as its *trailing* dimension:
    /// every dimension the per-iteration read produces comes ahead of it.
    ///
    /// This is not a stack the scalar loop wrote, so it is never an answer on
    /// its own — [`compact_one_perfect_inner_loop`] demands
    /// [`SliceGain::Leading`] of the whole value. It is admitted only where MLS
    /// §10.6.4 contracts it back to leading, and refused by every other
    /// operator, which is what keeps the single admitted rule from widening
    /// into the transposition defects this proof exists to catch.
    Trailing,
}

impl SliceGain {
    fn gains(self) -> bool {
        matches!(self, Self::Leading | Self::Trailing)
    }
}

/// Refuse a trailing gain, which only MLS §10.6.4's vector-matrix product may
/// consume.
///
/// Every operator that propagates its operand's gain unchanged — negation, a
/// record field projection, the branches of an `if` — would carry a trailing
/// gain outward as if it were the loop's stack. Refusing here costs a
/// compaction the two-valued lattice already refused, and keeps the new value
/// confined to the one rule that proves it back into a leading one.
fn reject_trailing(gain: SliceGain) -> Option<SliceGain> {
    (gain != SliceGain::Trailing).then_some(gain)
}

/// One left-to-right scan of a reference's subscripts.
///
/// MLS §10.5.3 keeps a slice's dimensions in subscript order and appends the
/// unsubscripted dimensions after them, so the sliced subscript contributes the
/// read's *leading* dimension exactly when every subscript ahead of it is a
/// scalar index. That is the whole question this scan answers, and it is asked
/// of an assignment target's whole part chain as well as of a value's reads.
struct SliceScan<'scope> {
    binder: &'scope str,
    shapes: &'scope ShapeEnvironment,
    /// Dimensions the read already produces ahead of the sliced subscript.
    leading: usize,
    sliced: bool,
}

impl<'scope> SliceScan<'scope> {
    fn new(binder: &'scope str, shapes: &'scope ShapeEnvironment) -> Self {
        Self {
            binder,
            shapes,
            leading: 0,
            sliced: false,
        }
    }

    fn subscripts(&mut self, subscripts: &[Subscript]) -> Option<()> {
        for subscript in subscripts {
            self.subscript(subscript)?;
        }
        Some(())
    }

    fn subscript(&mut self, subscript: &Subscript) -> Option<()> {
        if is_binder_subscript(subscript, self.binder) {
            // Naming the binder twice in one reference — `a[j,j]` — slices two
            // dimensions of the same read, which is a submatrix rather than the
            // diagonal the scalar loop walked.
            if self.sliced {
                return None;
            }
            self.sliced = true;
            return Some(());
        }
        if let Subscript::Expr { expr, .. } = subscript
            && binder_slice_gain(expr, self.binder, self.shapes)?.gains()
        {
            return None;
        }
        if !self.sliced {
            self.leading += subscript_dimensions(subscript, self.shapes)?;
        }
        Some(())
    }

    /// Where this read carries the sliced dimension, given the rank the read
    /// has *per iteration*.
    ///
    /// The scan counts the dimensions produced ahead of the sliced subscript
    /// directly. It cannot count the ones produced after it the same way: MLS
    /// §10.5.3 appends every *unsubscripted* dimension behind the subscripted
    /// ones, so `b[:,j]` on a `b[3,4,5]` puts the sliced dimension in the
    /// middle while naming nothing after it. The per-iteration rank closes
    /// that gap — it counts every dimension the read produces, so the ones
    /// behind the slice are exactly `rank - leading`, however they were
    /// spelled. The sliced dimension is therefore trailing precisely when
    /// `leading == rank`, and a slice with dimensions on both sides is neither
    /// gain and is refused.
    fn gain(&self, read: &Expression) -> Option<SliceGain> {
        if !self.sliced {
            return Some(SliceGain::Invariant);
        }
        if self.leading == 0 {
            return Some(SliceGain::Leading);
        }
        (self.leading == binder_slice_rank(read, self.shapes)?).then_some(SliceGain::Trailing)
    }

    /// The gain of a read that may only ever be the loop's own leading stack.
    ///
    /// An assignment target is the one such read: `A[..,r,..]` names the
    /// coordinates the loop wrote, and only a leading slice makes the compacted
    /// write cover them in the order the iterations produced. Answering without
    /// the per-iteration rank keeps the target scan independent of whether the
    /// target's shape happens to be provable.
    fn leading_gain(&self) -> Option<SliceGain> {
        match (self.sliced, self.leading) {
            (false, _) => Some(SliceGain::Invariant),
            (true, 0) => Some(SliceGain::Leading),
            (true, _) => None,
        }
    }
}

/// How many dimensions one subscript contributes to the read it appears in.
fn subscript_dimensions(subscript: &Subscript, shapes: &ShapeEnvironment) -> Option<usize> {
    match subscript {
        Subscript::Index { .. } => Some(0),
        Subscript::Colon { .. } => Some(1),
        Subscript::Expr { expr, .. } => Some(call_free_expression_shape(expr, shapes)?.len()),
    }
}

/// The gain one assignment target carries, scanned across its whole part chain.
///
/// MLS §11.2.1 writes the target as a component reference, so the dimensions of
/// an earlier part's subscripts come ahead of a later part's — `a[:].b[j]` puts
/// the sliced dimension second exactly as `a[:, j]` does.
fn binder_slice_target_gain(
    component: &rumoca_core::ComponentReference,
    binder: &str,
    shapes: &ShapeEnvironment,
) -> Option<SliceGain> {
    let mut scan = SliceScan::new(binder, shapes);
    for part in component.parts() {
        scan.subscripts(&part.subs)?;
    }
    scan.leading_gain()
}

fn binder_slice_binary(
    op: &rumoca_core::OpBinary,
    lhs: (&Expression, SliceGain),
    rhs: (&Expression, SliceGain),
    shapes: &ShapeEnvironment,
) -> Option<SliceGain> {
    use rumoca_core::OpBinary;
    let ((lhs_value, lhs_gain), (rhs_value, rhs_gain)) = (lhs, rhs);
    if !matches!(op, OpBinary::Mul) {
        // `*` is the only operator whose MLS §10.6.4 reading can contract a
        // trailing gain back into the loop's leading stack. Every other
        // operator here either requires equal shapes or broadcasts a scalar,
        // and both of those carry the sliced dimension outward exactly where
        // the operand put it — which is not where the loop stacked it.
        reject_trailing(lhs_gain)?;
        reject_trailing(rhs_gain)?;
    }
    match op {
        OpBinary::Mul => binder_slice_product((lhs_value, lhs_gain), (rhs_value, rhs_gain), shapes),
        // MLS §10.6.4 admits only a scalar divisor and a scalar exponent. A
        // loop-invariant one keeps the rank it was checked at whatever the
        // numerator's rank becomes, so only a *gaining* one changes the
        // operator.
        OpBinary::Div | OpBinary::Exp => (!rhs_gain.gains()).then_some(lhs_gain),
        // MLS §10.6.3: these require operands of the same shape, so slicing
        // exactly one side makes the two disagree.
        OpBinary::Add | OpBinary::Sub | OpBinary::And | OpBinary::Or => {
            (lhs_gain == rhs_gain).then_some(lhs_gain)
        }
        OpBinary::Eq
        | OpBinary::Neq
        | OpBinary::Lt
        | OpBinary::Le
        | OpBinary::Gt
        | OpBinary::Ge => (lhs_gain == rhs_gain).then_some(lhs_gain),
        // MLS §10.6.5 element-wise operators broadcast a *scalar* against an
        // array, so a sliced operand keeps its per-element meaning only where
        // the operand it broadcasts against is that scalar, or gained the same
        // dimension.
        OpBinary::AddElem
        | OpBinary::SubElem
        | OpBinary::MulElem
        | OpBinary::DivElem
        | OpBinary::ExpElem => {
            if lhs_gain == rhs_gain {
                return Some(lhs_gain);
            }
            let broadcast = if lhs_gain.gains() {
                rhs_value
            } else {
                lhs_value
            };
            (binder_slice_rank(broadcast, shapes)? == 0).then_some(SliceGain::Leading)
        }
        OpBinary::Empty | OpBinary::Assign => None,
    }
}

/// MLS §10.6.4 reads both operands' *ranks* to decide which product `*` is, so
/// raising one operand's rank by one can silently select a different one.
///
/// Writing `n` for the sliced extent and taking a gained dimension as leading,
/// exactly these configurations still compute the stack the scalar loop wrote:
///
/// * neither operand sliced, which is the scalar loop's own product;
/// * a scalar factor against a sliced operand, which MLS §10.6.4 broadcasts
///   over whatever rank the sliced operand reached;
/// * a sliced *left* factor whose per-iteration value is a vector, against a
///   vector or a matrix — `[n, k] * [k]` and `[n, k] * [k, m]` contract exactly
///   the dimension the source contracted and leave `n` leading.
///
/// The refusal that most looks right is the defect this proof exists for. A
/// sliced right factor that gained its dimension *leading* is contracted on the
/// dimension the slice added: `a[i,:] * b[j,:]` is a scalar product of two rows,
/// and `a[i,:] * b[r,:]` reads `n` as `b`'s inner dimension and returns `a * b`.
/// Slicing both operands likewise contracts the loop's own dimension away.
///
/// A right factor that gained its dimension *trailing* is the opposite case,
/// and it is the ordinary matrix product. `y[i,j] := a[i,:] * b[:,j]` slices
/// `b`'s second dimension, so `b[:,r]` is the `[k, n]` matrix whose *leading*
/// dimension is the one the source contracted. MLS §10.6.4's vector-matrix
/// product contracts exactly that one, leaving `[n]` — the sliced dimension,
/// alone and therefore leading, which is the stack the scalar loop wrote.
/// Element for element, `(a[i,:] * b[:,r])[j] = sum(k) a[i,k] * b[k,j]` is the
/// `y[i,j]` the loop assigned. Both operands must be vectors per iteration for
/// that reading to be the one MLS picks, which is what the ranks below check.
fn binder_slice_product(
    lhs: (&Expression, SliceGain),
    rhs: (&Expression, SliceGain),
    shapes: &ShapeEnvironment,
) -> Option<SliceGain> {
    let ((lhs_value, lhs_gain), (rhs_value, rhs_gain)) = (lhs, rhs);
    match (lhs_gain, rhs_gain) {
        (SliceGain::Invariant, SliceGain::Invariant) => Some(SliceGain::Invariant),
        (SliceGain::Leading, SliceGain::Invariant) => {
            let factor = binder_slice_rank(rhs_value, shapes)?;
            let sliced = binder_slice_rank(lhs_value, shapes)?;
            (factor == 0 || (sliced == 1 && (factor == 1 || factor == 2)))
                .then_some(SliceGain::Leading)
        }
        (SliceGain::Invariant, SliceGain::Leading) => {
            (binder_slice_rank(lhs_value, shapes)? == 0).then_some(SliceGain::Leading)
        }
        // The one rule that consumes a trailing gain: two per-iteration vectors
        // whose scalar product becomes the vector-matrix product contracting
        // the dimension the slice did *not* add.
        (SliceGain::Invariant, SliceGain::Trailing) => (binder_slice_rank(lhs_value, shapes)? == 1
            && binder_slice_rank(rhs_value, shapes)? == 1)
            .then_some(SliceGain::Leading),
        // A trailing gain on the *left* is contracted away or transposed: with
        // `a[:,j] * v` the compacted `a[:,r]` is `[k, n]`, whose second
        // dimension MLS §10.6.4 contracts against `v` — so the surviving `[k]`
        // is not the loop's stack at all, and against a scalar the result is
        // `[k, n]` where the loop wrote `[n, k]`. Slicing both operands
        // contracts the loop's own dimension away whichever end it landed on.
        _ => None,
    }
}

/// The rank the operand has *per iteration* — the rank the scalar loop
/// type-checked it at, since the binder is a scalar in this scope.
///
/// `None` is "not proven here", and refusing on it costs a compaction rather
/// than correctness: the scalar loop remains a legal owner of the coordinates.
fn binder_slice_rank(expression: &Expression, shapes: &ShapeEnvironment) -> Option<usize> {
    Some(call_free_expression_shape(expression, shapes)?.len())
}

/// Admit a form only where no operand gains the sliced dimension.
fn binder_slice_invariant(
    expressions: &[Expression],
    binder: &str,
    shapes: &ShapeEnvironment,
) -> Option<SliceGain> {
    for expression in expressions {
        if binder_slice_gain(expression, binder, shapes)?.gains() {
            return None;
        }
    }
    Some(SliceGain::Invariant)
}

/// Admit an element-wise operator's arguments, which broadcast a scalar against
/// the argument that gained the sliced dimension.
fn binder_slice_broadcast(
    expressions: &[Expression],
    binder: &str,
    shapes: &ShapeEnvironment,
) -> Option<SliceGain> {
    let mut gains = Vec::with_capacity(expressions.len());
    for expression in expressions {
        // An element-wise builtin maps over whatever shape it is handed, so it
        // would report a trailing gain as the loop's leading stack. Only
        // MLS §10.6.4's vector-matrix product may consume one.
        gains.push(reject_trailing(binder_slice_gain(
            expression, binder, shapes,
        )?)?);
    }
    if !gains.iter().any(|gain| gain.gains()) {
        return Some(SliceGain::Invariant);
    }
    for (expression, gain) in expressions.iter().zip(&gains) {
        if !gain.gains() && binder_slice_rank(expression, shapes)? != 0 {
            return None;
        }
    }
    Some(SliceGain::Leading)
}

/// MLS §10.6.1 applies these built-in operators element-wise to an array.
///
/// `min` and `max` appear only in their two-argument form: MLS §10.3.4 makes
/// the one-argument form a *reduction*, which reads the rank it is given rather
/// than mapping over it.
fn builtin_maps_elementwise(function: BuiltinFunction, arity: usize) -> bool {
    match function {
        BuiltinFunction::Abs
        | BuiltinFunction::Sign
        | BuiltinFunction::Sqrt
        | BuiltinFunction::Ceil
        | BuiltinFunction::Floor
        | BuiltinFunction::Integer
        | BuiltinFunction::Div
        | BuiltinFunction::Mod
        | BuiltinFunction::Rem
        | BuiltinFunction::Sin
        | BuiltinFunction::Cos
        | BuiltinFunction::Tan
        | BuiltinFunction::Asin
        | BuiltinFunction::Acos
        | BuiltinFunction::Atan
        | BuiltinFunction::Atan2
        | BuiltinFunction::Sinh
        | BuiltinFunction::Cosh
        | BuiltinFunction::Tanh
        | BuiltinFunction::Exp
        | BuiltinFunction::Log
        | BuiltinFunction::Log10 => true,
        BuiltinFunction::Min | BuiltinFunction::Max => arity == 2,
        // MLS §3.7.4 passes the expression through unchanged.
        BuiltinFunction::NoEvent => arity == 1,
        BuiltinFunction::Smooth => arity == 2,
        _ => false,
    }
}

fn is_binder_subscript(subscript: &Subscript, binder: &str) -> bool {
    matches!(
        subscript,
        Subscript::Expr { expr, .. }
            if matches!(
                expr.as_ref(),
                Expression::VarRef { name, subscripts, .. }
                    if name.as_str() == binder && subscripts.is_empty()
            )
    )
}

struct BinderSliceReplacement<'a> {
    binder: &'a str,
    range: &'a Expression,
    replacements: usize,
    unsupported: bool,
}

impl ExpressionRewriter for BinderSliceReplacement<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        if matches!(
            expression,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == self.binder && subscripts.is_empty()
        ) {
            self.unsupported = true;
        }
        self.walk_expression(expression)
    }

    fn rewrite_subscript(&mut self, subscript: &Subscript) -> Subscript {
        if let Subscript::Expr { expr, span } = subscript
            && matches!(
                expr.as_ref(),
                Expression::VarRef { name, subscripts, .. }
                    if name.as_str() == self.binder && subscripts.is_empty()
            )
        {
            self.replacements += 1;
            return Subscript::Expr {
                expr: Box::new(self.range.clone()),
                span: *span,
            };
        }
        match subscript {
            Subscript::Index { value, span } => Subscript::Index {
                value: *value,
                span: *span,
            },
            Subscript::Colon { span } => Subscript::Colon { span: *span },
            Subscript::Expr { expr, span } => Subscript::Expr {
                expr: Box::new(self.rewrite_expression(expr)),
                span: *span,
            },
        }
    }
}

pub(super) fn first_dependent_loop_range(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Option<Span>, ToDaeError> {
    for statement in statements {
        let found = match statement {
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                let range_span = first_unsettled_range(indices, static_integers, shapes)?;
                range_span.or(first_dependent_loop_range(
                    equations,
                    static_integers,
                    shapes,
                )?)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => first_dependent_block_range(cond_blocks, static_integers, shapes)?.or(else_block
                .as_ref()
                .map_or(Ok(None), |block| {
                    first_dependent_loop_range(block, static_integers, shapes)
                })?),
            rumoca_core::Statement::While { block, .. } => {
                first_dependent_loop_range(&block.stmts, static_integers, shapes)?
            }
            rumoca_core::Statement::When { blocks, .. } => {
                first_dependent_block_range(blocks, static_integers, shapes)?
            }
            _ => None,
        };
        if found.is_some() {
            return Ok(found);
        }
    }
    Ok(None)
}

fn first_unsettled_range(
    indices: &[rumoca_core::ForIndex],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Option<Span>, ToDaeError> {
    for index in indices {
        if static_function_range(&index.range, static_integers, shapes)?.is_none() {
            return expression_span(&index.range).map(Some);
        }
    }
    Ok(None)
}

fn first_dependent_block_range(
    blocks: &[rumoca_core::StatementBlock],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Option<Span>, ToDaeError> {
    for block in blocks {
        if let Some(span) = first_dependent_loop_range(&block.stmts, static_integers, shapes)? {
            return Ok(Some(span));
        }
    }
    Ok(None)
}
