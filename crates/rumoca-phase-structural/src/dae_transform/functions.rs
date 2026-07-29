use rumoca_ir_dae as dae;

use super::{DirectStateConstraint, ExpressionRebuilder, RebuiltIdentities};

pub(super) struct RebuiltFunction<'dae> {
    pub(super) id: dae::FunctionId<'dae>,
    pub(super) parameters: Vec<dae::FunctionParameterId<'dae>>,
    pub(super) values: Vec<dae::FunctionValueId<'dae>>,
}

pub(super) fn reserve_functions<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    types: &[dae::ValueTypeId<'target>],
) -> Result<
    (
        Vec<RebuiltFunction<'target>>,
        Vec<Option<dae::FunctionReservation<'target>>>,
    ),
    dae::DaeConstructionError,
> {
    let mut rebuilt = Vec::with_capacity(source.function_count());
    let mut reservations = Vec::with_capacity(source.function_count());
    for index in 0..source.function_count() {
        let source_id = source
            .function_id(index)
            .expect("finalized function ordinal resolves");
        let function = source
            .function(source_id)
            .expect("finalized function identity resolves");
        let parameters = function
            .parameter_types()
            .iter()
            .map(|value_type| types[value_type.index() as usize])
            .collect::<Vec<_>>();
        let results = function
            .result_types()
            .iter()
            .map(|value_type| types[value_type.index() as usize])
            .collect::<Vec<_>>();
        let (id, reservation) = target.functions(|functions| {
            functions.reserve_recursive(
                function.name().clone(),
                parameters,
                results,
                function.declaration(),
            )
        })?;
        let parameters = declare_parameters(target, function, &reservation)?;
        let values = declare_values(target, function, types, &reservation)?;
        rebuilt.push(RebuiltFunction {
            id,
            parameters,
            values,
        });
        reservations.push(Some(reservation));
    }
    Ok((rebuilt, reservations))
}

fn declare_parameters<'target>(
    target: &mut dae::DaeConstruction<'target>,
    function: dae::FunctionView<'_>,
    reservation: &dae::FunctionReservation<'target>,
) -> Result<Vec<dae::FunctionParameterId<'target>>, dae::DaeConstructionError> {
    function
        .parameters()
        .enumerate()
        .map(|(ordinal, parameter)| {
            target.functions(|functions| {
                functions.parameter(
                    reservation,
                    parameter.name().clone(),
                    ordinal,
                    parameter.declaration(),
                )
            })
        })
        .collect()
}

fn declare_values<'target>(
    target: &mut dae::DaeConstruction<'target>,
    function: dae::FunctionView<'_>,
    types: &[dae::ValueTypeId<'target>],
    reservation: &dae::FunctionReservation<'target>,
) -> Result<Vec<dae::FunctionValueId<'target>>, dae::DaeConstructionError> {
    let source_values = function.values().collect::<Vec<_>>();
    let mut rebuilt = vec![None; source_values.len()];
    for (ordinal, value) in source_values
        .iter()
        .copied()
        .filter(|value| value.role() == dae::FunctionValueRole::Output)
        .enumerate()
    {
        let id = target.functions(|functions| {
            functions.output(
                reservation,
                value.name().clone(),
                ordinal,
                value.declaration(),
            )
        })?;
        rebuilt[value.id().ordinal() as usize] = Some(id);
    }
    for value in source_values
        .iter()
        .copied()
        .filter(|value| value.role() == dae::FunctionValueRole::Local)
    {
        let id = target.functions(|functions| {
            functions.local(
                reservation,
                value.name().clone(),
                types[value.value_type().index() as usize],
                value.declaration(),
            )
        })?;
        rebuilt[value.id().ordinal() as usize] = Some(id);
    }
    Ok(rebuilt
        .into_iter()
        .map(|value| value.expect("every checked function value has a supported role"))
        .collect())
}

pub(super) fn rebuild_functions<'source, 'target>(
    source: dae::DaeView<'source>,
    target: &mut dae::DaeConstruction<'target>,
    identities: RebuiltIdentities<'_, 'target>,
    derivative_definitions: &[Option<u32>],
    candidate: Option<DirectStateConstraint>,
    rebuilt: &mut [Option<dae::ExprId<'target>>],
    reservations: &mut [Option<dae::FunctionReservation<'target>>],
) -> Result<(), dae::DaeConstructionError> {
    let (function_use_groups, function_uses) = index_function_uses(source);
    let function_definitions = (0..source.function_count())
        .map(|index| {
            let id = source
                .function_id(index)
                .expect("finalized function ordinal resolves");
            let function = source
                .function(id)
                .expect("finalized function identity resolves");
            vec![None; function.definition_count()]
        })
        .collect();
    FunctionRebuilder {
        source,
        target,
        identities,
        derivative_definitions,
        candidate,
        rebuilt,
        pending: Vec::new(),
        function_use_groups,
        function_uses,
        function_definitions,
    }
    .rebuild_all(reservations)
}

#[derive(Clone, Copy)]
struct IndexedFunctionUse {
    function: u32,
    definition: u32,
    value: u32,
    expression: u32,
}

struct FunctionUseGroup {
    function: u32,
    definition: u32,
    value: u32,
    range: std::ops::Range<usize>,
    materialized: bool,
}

fn index_function_uses(source: dae::DaeView<'_>) -> (Vec<FunctionUseGroup>, Vec<u32>) {
    let mut indexed = Vec::new();
    for index in 0..source.expression_count() {
        let source_id = source
            .expression_id(index)
            .expect("finalized expression ordinal resolves");
        let expression = source
            .expression(source_id)
            .expect("finalized expression identity resolves");
        if let dae::ExpressionOperation::FunctionValue { value, definition } =
            expression.operation()
        {
            assert_eq!(
                definition.target(),
                value,
                "checked function occurrence definition targets its value"
            );
            indexed.push(IndexedFunctionUse {
                function: definition.id().function().index(),
                definition: definition.id().ordinal(),
                value: value.ordinal(),
                expression: source_id.index(),
            });
        }
    }
    indexed.sort_unstable_by_key(|entry| {
        (
            entry.function,
            entry.definition,
            entry.value,
            entry.expression,
        )
    });
    let mut groups = Vec::new();
    let mut expressions = Vec::with_capacity(indexed.len());
    for entry in indexed {
        let key = (entry.function, entry.definition, entry.value);
        let starts_group = groups.last().is_none_or(|group: &FunctionUseGroup| {
            (group.function, group.definition, group.value) != key
        });
        if starts_group {
            let start = expressions.len();
            groups.push(FunctionUseGroup {
                function: entry.function,
                definition: entry.definition,
                value: entry.value,
                range: start..start,
                materialized: false,
            });
        }
        expressions.push(entry.expression);
        groups
            .last_mut()
            .expect("indexed function use creates its group")
            .range
            .end = expressions.len();
    }
    (groups, expressions)
}

struct FunctionRebuilder<'source, 'borrow, 'target> {
    source: dae::DaeView<'source>,
    target: &'borrow mut dae::DaeConstruction<'target>,
    identities: RebuiltIdentities<'borrow, 'target>,
    derivative_definitions: &'borrow [Option<u32>],
    candidate: Option<DirectStateConstraint>,
    rebuilt: &'borrow mut [Option<dae::ExprId<'target>>],
    pending: Vec<(dae::ExprId<'source>, bool)>,
    function_use_groups: Vec<FunctionUseGroup>,
    function_uses: Vec<u32>,
    function_definitions: Vec<Vec<Option<dae::FunctionDefinitionId<'target>>>>,
}

impl<'source, 'target> FunctionRebuilder<'source, '_, 'target> {
    fn rebuild_all(
        &mut self,
        reservations: &mut [Option<dae::FunctionReservation<'target>>],
    ) -> Result<(), dae::DaeConstructionError> {
        for (index, reservation) in reservations.iter_mut().enumerate() {
            let source_id = self
                .source
                .function_id(index)
                .expect("finalized function ordinal resolves");
            let function = self
                .source
                .function(source_id)
                .expect("finalized function identity resolves");
            let reservation = reservation
                .take()
                .expect("each recursive function reservation is consumed once");
            let body = self
                .target
                .functions(|functions| functions.begin(reservation, function.declaration()))?;
            let body = self.rebuild_statements(
                &self.identities.functions[index],
                body,
                function.statements(),
            )?;
            self.seed_results(function, &self.identities.functions[index], &body)?;
            self.target
                .functions(|functions| functions.define(body, function.declaration()))?;
        }
        self.expect_all_definitions_mapped()?;
        self.rebuild_orphaned_scoped_expressions()
    }

    fn expect_all_definitions_mapped(&self) -> Result<(), dae::DaeConstructionError> {
        let missing =
            self.function_definitions
                .iter()
                .enumerate()
                .find_map(|(function, definitions)| {
                    self.first_missing_definition(function, definitions)
                });
        let Some(source) = missing else {
            return Ok(());
        };
        Err(dae::DaeConstructionError::IncompleteDefinition {
            kind: "rebuilt function definition",
            index: source.id().ordinal(),
            span: source.provenance().span(),
        })
    }

    fn first_missing_definition(
        &self,
        function_index: usize,
        definitions: &[Option<dae::FunctionDefinitionId<'target>>],
    ) -> Option<dae::FunctionDefinitionView<'source>> {
        let ordinal = definitions.iter().position(Option::is_none)?;
        let function = self
            .source
            .function_id(function_index)
            .and_then(|id| self.source.function(id))
            .expect("finalized function ordinal resolves");
        function
            .definition_id(ordinal)
            .and_then(|id| self.source.function_definition(id))
    }

    fn rebuild_statements(
        &mut self,
        function: &RebuiltFunction<'target>,
        mut body: dae::FunctionBody<'target>,
        statements: dae::FunctionStatements<'source>,
    ) -> Result<dae::FunctionBody<'target>, dae::DaeConstructionError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::Assignment { definition } => {
                    self.assign_statement(function, &mut body, definition)?
                }
                dae::FunctionStatementView::For {
                    fold,
                    statements,
                    provenance,
                } => body = self.rebuild_loop(function, body, fold, statements, provenance)?,
            }
        }
        Ok(body)
    }

    fn assign_statement(
        &mut self,
        function: &RebuiltFunction<'target>,
        body: &mut dae::FunctionBody<'target>,
        source_definition: dae::FunctionDefinitionView<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        let source_target = source_definition.target();
        let source_rhs = source_definition.rhs();
        let provenance = source_definition.provenance();
        let value = self.rebuild_expression(body, source_rhs)?;
        let target_value = function.values[source_target.ordinal() as usize];
        self.target
            .functions(|functions| functions.assign(body, target_value, value, provenance))?;
        let target_definition = self.target.functions(|functions| {
            functions.current_definition_id(body, target_value, provenance)
        })?;
        self.materialize_definition_uses(
            body,
            source_definition,
            source_target,
            target_value,
            target_definition,
        )
    }

    fn rebuild_loop(
        &mut self,
        function: &RebuiltFunction<'target>,
        body: dae::FunctionBody<'target>,
        source_fold_id: dae::FunctionFoldId<'source>,
        statements: dae::FunctionStatements<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::FunctionBody<'target>, dae::DaeConstructionError> {
        let source_fold = self
            .source
            .function_fold(source_fold_id)
            .expect("checked function fold identity resolves");
        let targets = source_fold
            .targets()
            .map(|target| function.values[target.ordinal() as usize])
            .collect::<Vec<_>>();
        self.seed_current(
            &body,
            source_fold.targets(),
            &targets,
            source_fold.initial_values(),
        )?;
        let domain = self.identities.domains[source_fold.domain().index() as usize].id;
        let mut loop_body = self.target.functions(|functions| {
            functions.begin_loop(body, domain, targets.clone(), source_fold.provenance())
        })?;
        self.seed_current(
            loop_body.body(),
            source_fold.targets(),
            &targets,
            source_fold.parameter_values(),
        )?;
        self.rebuild_loop_statements(function, &mut loop_body, statements)?;
        self.seed_current(
            loop_body.body(),
            source_fold.targets(),
            &targets,
            source_fold.update_values(),
        )?;
        let body = self
            .target
            .functions(|functions| functions.finish_loop(loop_body, provenance))?;
        self.seed_current(
            &body,
            source_fold.targets(),
            &targets,
            source_fold.output_values(),
        )?;
        Ok(body)
    }

    fn rebuild_loop_statements(
        &mut self,
        function: &RebuiltFunction<'target>,
        loop_body: &mut dae::FunctionLoop<'target>,
        statements: dae::FunctionStatements<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        for statement in statements {
            let dae::FunctionStatementView::Assignment { definition } = statement else {
                unreachable!("checked function loops cannot contain nested folds")
            };
            let source_target = definition.target();
            let source_rhs = definition.rhs();
            let provenance = definition.provenance();
            let target_value = function.values[source_target.ordinal() as usize];
            let value = self.rebuild_expression(loop_body.body(), source_rhs)?;
            self.target.functions(|functions| {
                functions.assign_loop(loop_body, target_value, value, provenance)
            })?;
            let target_definition = self.target.functions(|functions| {
                functions.current_definition_id(loop_body.body(), target_value, provenance)
            })?;
            self.materialize_definition_uses(
                loop_body.body(),
                definition,
                source_target,
                target_value,
                target_definition,
            )?;
        }
        Ok(())
    }

    fn rebuild_expression(
        &mut self,
        body: &dae::FunctionBody<'target>,
        source_id: dae::ExprId<'source>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        self.rebuild_postorder(source_id, Some(body))
    }

    fn rebuild_orphaned_scoped_expressions(&mut self) -> Result<(), dae::DaeConstructionError> {
        for index in 0..self.source.expression_count() {
            let source_id = self
                .source
                .expression_id(index)
                .expect("finalized expression ordinal resolves");
            let expression = self
                .source
                .expression(source_id)
                .expect("finalized expression identity resolves");
            if expression.function_scope().is_some()
                && self.rebuilt[source_id.index() as usize].is_none()
            {
                self.rebuild_postorder(source_id, None)?;
            }
        }
        Ok(())
    }

    fn rebuild_postorder(
        &mut self,
        source_id: dae::ExprId<'source>,
        body: Option<&dae::FunctionBody<'target>>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        self.pending.clear();
        self.pending.push((source_id, false));
        while let Some((current, expanded)) = self.pending.pop() {
            let index = current.index() as usize;
            if self.rebuilt[index].is_some() {
                continue;
            }
            let expression = self
                .source
                .expression(current)
                .expect("checked function expression resolves");
            let provenance = expression.provenance();
            match expression.operation() {
                dae::ExpressionOperation::FunctionValue { value, definition } => {
                    self.rebuild_function_value(
                        current, value, definition, expanded, body, provenance,
                    )?;
                }
                dae::ExpressionOperation::FunctionFoldParameter { .. }
                | dae::ExpressionOperation::FunctionFoldOutput { .. } => {
                    return Err(dae::DaeConstructionError::IncompleteDefinition {
                        kind: "function fold expression",
                        index: current.index(),
                        span: provenance.span(),
                    });
                }
                _operation if expanded => self.rebuild_expanded_expression(current)?,
                operation => {
                    self.pending.push((current, true));
                    Self::push_dependencies(&mut self.pending, operation);
                }
            }
        }
        self.rebuilt[source_id.index() as usize].ok_or(
            dae::DaeConstructionError::IncompleteDefinition {
                kind: "function expression",
                index: source_id.index(),
                span: self
                    .source
                    .expression(source_id)
                    .expect("checked function expression resolves")
                    .provenance()
                    .span(),
            },
        )
    }

    fn rebuild_function_value(
        &mut self,
        current: dae::ExprId<'source>,
        value: dae::FunctionValueId<'source>,
        source_definition: dae::FunctionDefinitionView<'source>,
        expanded: bool,
        body: Option<&dae::FunctionBody<'target>>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        if !expanded {
            self.pending.push((current, true));
            return Ok(());
        }
        let target_value = self.identities.functions[value.function().index() as usize].values
            [value.ordinal() as usize];
        let body = body.ok_or(dae::DaeConstructionError::IncompleteDefinition {
            kind: "function value occurrence",
            index: current.index(),
            span: provenance.span(),
        })?;
        let target_definition = self.mapped_definition(source_definition.id(), provenance)?;
        let current_definition = self.target.functions(|functions| {
            functions.current_definition_id(body, target_value, provenance)
        })?;
        if current_definition != target_definition {
            return Err(dae::DaeConstructionError::InvalidFunctionValueRead {
                value: target_value.ordinal(),
                expected_definition: Some(target_definition.ordinal()),
                found_definition: current_definition.ordinal(),
                span: provenance.span(),
            });
        }
        let rebuilt = self
            .target
            .functions(|functions| functions.read(body, target_value, provenance))?;
        self.rebuilt[current.index() as usize] = Some(rebuilt);
        Ok(())
    }

    fn rebuild_expanded_expression(
        &mut self,
        current: dae::ExprId<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        let rebuilt = self.target.expressions(|expressions| {
            ExpressionRebuilder::new(
                self.source,
                expressions,
                self.identities,
                self.derivative_definitions,
                self.candidate,
                self.rebuilt,
            )
            .rebuild(current)
        })?;
        self.rebuilt[current.index() as usize] = Some(rebuilt);
        Ok(())
    }

    fn push_dependencies(
        pending: &mut Vec<(dae::ExprId<'source>, bool)>,
        operation: dae::ExpressionOperation<'source>,
    ) {
        let mut push = |expression| pending.push((expression, false));
        match operation {
            dae::ExpressionOperation::Literal(_)
            | dae::ExpressionOperation::Coordinate(_)
            | dae::ExpressionOperation::Range { .. }
            | dae::ExpressionOperation::FunctionFoldParameter { .. }
            | dae::ExpressionOperation::FunctionFoldOutput { .. }
            | dae::ExpressionOperation::FunctionValue { .. } => {}
            dae::ExpressionOperation::Unary { operand, .. } => push(operand),
            dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
                push(rhs);
                push(lhs);
            }
            dae::ExpressionOperation::Conditional(operands)
            | dae::ExpressionOperation::Array(operands)
            | dae::ExpressionOperation::Record(operands)
            | dae::ExpressionOperation::Builtin {
                arguments: operands,
                ..
            }
            | dae::ExpressionOperation::Call {
                arguments: operands,
                ..
            } => {
                for operand in operands.iter() {
                    push(operand);
                }
            }
            dae::ExpressionOperation::Field { base, .. }
            | dae::ExpressionOperation::Comprehension { body: base, .. } => push(base),
            dae::ExpressionOperation::Index { base, subscripts } => {
                Self::push_subscripts(pending, subscripts);
                pending.push((base, false));
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                Self::push_subscripts(pending, subscripts);
                pending.push((value, false));
                pending.push((base, false));
            }
        }
    }

    fn push_subscripts(
        pending: &mut Vec<(dae::ExprId<'source>, bool)>,
        subscripts: dae::SubscriptsView<'source>,
    ) {
        for subscript in subscripts.iter() {
            match subscript {
                dae::SubscriptView::Index { expression, .. }
                | dae::SubscriptView::Slice { expression, .. } => {
                    pending.push((expression, false));
                }
                dae::SubscriptView::Whole { .. } => {}
            }
        }
    }

    fn seed_results(
        &mut self,
        source: dae::FunctionView<'source>,
        function: &RebuiltFunction<'target>,
        body: &dae::FunctionBody<'target>,
    ) -> Result<(), dae::DaeConstructionError> {
        let outputs = source
            .values()
            .filter(|value| value.role() == dae::FunctionValueRole::Output);
        for (source_result, source_output) in source.result_values().iter().zip(outputs) {
            let target_output = function.values[source_output.id().ordinal() as usize];
            let target_definition = self.target.functions(|functions| {
                functions.current_definition_id(body, target_output, source_result.provenance())
            })?;
            let target_rhs = self.target.functions(|functions| {
                functions.current_definition(body, target_output, source_result.provenance())
            })?;
            self.seed(source_result.rhs(), target_rhs, source_result.provenance())?;
            self.materialize_definition_uses(
                body,
                source_result,
                source_output.id(),
                target_output,
                target_definition,
            )?;
        }
        Ok(())
    }

    fn seed_current(
        &mut self,
        body: &dae::FunctionBody<'target>,
        source_targets: impl IntoIterator<Item = dae::FunctionValueId<'source>>,
        targets: &[dae::FunctionValueId<'target>],
        source_definitions: dae::FunctionDefinitionValues<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        for ((source_definition, source_target), target_value) in source_definitions
            .iter()
            .zip(source_targets)
            .zip(targets.iter().copied())
        {
            if source_definition.target() != source_target {
                return Err(dae::DaeConstructionError::ShapeMismatch {
                    span: source_definition.provenance().span(),
                });
            }
            let target_definition = self.target.functions(|functions| {
                functions.current_definition_id(body, target_value, source_definition.provenance())
            })?;
            let target_rhs = self.target.functions(|functions| {
                functions.current_definition(body, target_value, source_definition.provenance())
            })?;
            self.seed(
                source_definition.rhs(),
                target_rhs,
                source_definition.provenance(),
            )?;
            self.materialize_definition_uses(
                body,
                source_definition,
                source_target,
                target_value,
                target_definition,
            )?;
        }
        Ok(())
    }

    fn materialize_definition_uses(
        &mut self,
        body: &dae::FunctionBody<'target>,
        source_definition: dae::FunctionDefinitionView<'source>,
        source_value: dae::FunctionValueId<'source>,
        target_value: dae::FunctionValueId<'target>,
        target_definition: dae::FunctionDefinitionId<'target>,
    ) -> Result<(), dae::DaeConstructionError> {
        if source_definition.target() != source_value
            || source_definition.id().function() != source_value.function()
            || target_definition.function() != target_value.function()
        {
            return Err(dae::DaeConstructionError::ShapeMismatch {
                span: source_definition.provenance().span(),
            });
        }
        self.record_definition_mapping(
            source_definition.id(),
            target_definition,
            source_definition.provenance(),
        )?;
        let key = (
            source_definition.id().function().index(),
            source_definition.id().ordinal(),
            source_value.ordinal(),
        );
        let Ok(group_index) = self
            .function_use_groups
            .binary_search_by_key(&key, |group| {
                (group.function, group.definition, group.value)
            })
        else {
            return Ok(());
        };
        if self.function_use_groups[group_index].materialized {
            return Ok(());
        }
        let range = self.function_use_groups[group_index].range.clone();
        for raw in self.function_uses[range].iter().copied() {
            let source_use = self
                .source
                .expression_id(raw as usize)
                .expect("indexed function-value occurrence resolves");
            if self.rebuilt[raw as usize].is_some() {
                continue;
            }
            let expression = self
                .source
                .expression(source_use)
                .expect("indexed function-value occurrence resolves");
            let current_definition = self.target.functions(|functions| {
                functions.current_definition_id(body, target_value, expression.provenance())
            })?;
            if current_definition != target_definition {
                return Err(dae::DaeConstructionError::InvalidFunctionValueRead {
                    value: target_value.ordinal(),
                    expected_definition: Some(target_definition.ordinal()),
                    found_definition: current_definition.ordinal(),
                    span: expression.provenance().span(),
                });
            }
            let rebuilt = self.target.functions(|functions| {
                functions.read(body, target_value, expression.provenance())
            })?;
            self.rebuilt[raw as usize] = Some(rebuilt);
        }
        self.function_use_groups[group_index].materialized = true;
        Ok(())
    }

    fn record_definition_mapping(
        &mut self,
        source: dae::FunctionDefinitionId<'source>,
        target: dae::FunctionDefinitionId<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        if source.function().index() != target.function().index()
            || source.ordinal() != target.ordinal()
        {
            return Err(dae::DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            });
        }
        let slot = self
            .function_definitions
            .get_mut(source.function().index() as usize)
            .and_then(|definitions| definitions.get_mut(source.ordinal() as usize))
            .ok_or(dae::DaeConstructionError::IncompleteDefinition {
                kind: "source function definition",
                index: source.ordinal(),
                span: provenance.span(),
            })?;
        if slot.is_some_and(|existing| existing != target) {
            return Err(dae::DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            });
        }
        *slot = Some(target);
        Ok(())
    }

    fn mapped_definition(
        &self,
        source: dae::FunctionDefinitionId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::FunctionDefinitionId<'target>, dae::DaeConstructionError> {
        self.function_definitions
            .get(source.function().index() as usize)
            .and_then(|definitions| definitions.get(source.ordinal() as usize))
            .copied()
            .flatten()
            .ok_or(dae::DaeConstructionError::IncompleteDefinition {
                kind: "rebuilt function definition",
                index: source.ordinal(),
                span: provenance.span(),
            })
    }

    fn seed(
        &mut self,
        source: dae::ExprId<'source>,
        target: dae::ExprId<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        let slot = &mut self.rebuilt[source.index() as usize];
        if slot.is_some_and(|existing| existing != target) {
            return Err(dae::DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            });
        }
        *slot = Some(target);
        Ok(())
    }
}
