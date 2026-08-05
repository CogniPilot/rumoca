use rumoca_ir_dae as dae;

use super::DirectStateConstraint;
use super::constraints::DifferentiationFacts;
use super::expressions::{ExpressionRebuilder, RebuiltBaseIdentities, RebuiltIdentities};

#[derive(Clone)]
pub(super) struct RebuiltFunction<'dae> {
    pub(super) id: dae::FunctionId<'dae>,
    pub(super) parameters: Vec<dae::FunctionParameterId<'dae>>,
    pub(super) values: Vec<dae::FunctionValueId<'dae>>,
}

fn function_signature<'target>(
    function: dae::FunctionView<'_>,
    types: &[dae::ValueTypeId<'target>],
) -> dae::FunctionSignature<'target> {
    let parameters = function
        .parameter_types()
        .iter()
        .map(|value_type| types[value_type.index() as usize]);
    let results = function
        .result_types()
        .iter()
        .map(|value_type| types[value_type.index() as usize]);
    dae::FunctionSignature::new(
        function.name().clone(),
        parameters,
        results,
        function.declaration(),
    )
}

fn declare_parameters<'target>(
    target: &mut dae::DaeConstruction<'target>,
    function: dae::FunctionView<'_>,
    reservation: &dae::FunctionReservation<'_, 'target>,
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
    reservation: &dae::FunctionReservation<'_, 'target>,
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
    identities: RebuiltBaseIdentities<'_, 'target>,
    facts: &DifferentiationFacts,
    candidate: Option<DirectStateConstraint>,
    rebuilt: &mut [Option<dae::ExprId<'target>>],
) -> Result<Vec<RebuiltFunction<'target>>, dae::DaeConstructionError> {
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
    let mut rebuilder = FunctionRebuilder {
        source,
        identities,
        functions: Vec::with_capacity(source.function_count()),
        facts,
        candidate,
        rebuilt,
        pending: Vec::new(),
        function_use_groups,
        function_uses,
        function_definitions,
    };
    rebuilder.rebuild_all(target)?;
    Ok(rebuilder.functions)
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

fn function_components(source: dae::DaeView<'_>) -> Vec<rumoca_core::DependencyScc> {
    let mut dependencies = vec![Vec::new(); source.function_count()];
    for index in 0..source.expression_count() {
        let id = source
            .expression_id(index)
            .expect("finalized expression ordinal resolves");
        let expression = source
            .expression(id)
            .expect("finalized expression identity resolves");
        let Some(owner) = expression.function_scope() else {
            continue;
        };
        let dae::ExpressionOperation::Call { function, .. } = expression.operation() else {
            continue;
        };
        dependencies[owner.index() as usize].push(function.index() as usize);
    }
    for callees in &mut dependencies {
        callees.sort_unstable();
        callees.dedup();
    }
    rumoca_core::dependency_first_sccs(&dependencies)
        .expect("checked DAE calls reference known functions")
}

struct FunctionRebuilder<'source, 'borrow, 'target> {
    source: dae::DaeView<'source>,
    identities: RebuiltBaseIdentities<'borrow, 'target>,
    functions: Vec<RebuiltFunction<'target>>,
    facts: &'borrow DifferentiationFacts,
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
        target: &mut dae::DaeConstruction<'target>,
    ) -> Result<(), dae::DaeConstructionError> {
        for component in function_components(self.source) {
            if component.recursive {
                self.rebuild_recursive_component(target, &component.members)?;
            } else {
                self.rebuild_acyclic_function(target, component.members[0])?;
            }
        }
        self.expect_all_definitions_mapped()?;
        self.expect_all_scoped_expressions_mapped()
    }

    fn rebuild_acyclic_function(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        index: usize,
    ) -> Result<(), dae::DaeConstructionError> {
        self.expect_next_function(index);
        let function = self.source_function(index);
        let signature = function_signature(function, self.identities.types);
        target.function(signature, |target, reservation| {
            self.declare_function(target, function, &reservation)?;
            self.rebuild_function(target, index, reservation)
        })?;
        Ok(())
    }

    fn rebuild_recursive_component(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        members: &[usize],
    ) -> Result<(), dae::DaeConstructionError> {
        let first_index = self.functions.len();
        let mut signatures = members
            .iter()
            .enumerate()
            .map(|(offset, &index)| {
                assert_eq!(
                    first_index + offset,
                    index,
                    "checked DAE recursive SCCs occupy contiguous function ordinals"
                );
                function_signature(self.source_function(index), self.identities.types)
            })
            .collect::<Vec<_>>()
            .into_iter();
        let first = signatures
            .next()
            .expect("function components are constructor-proven nonempty");
        target.recursive_functions(first, signatures, |target, reservations| {
            for (&index, reservation) in members.iter().zip(&reservations) {
                let function = self.source_function(index);
                self.declare_function(target, function, reservation)?;
            }
            for (&index, reservation) in members.iter().zip(reservations) {
                self.rebuild_function(target, index, reservation)?;
            }
            Ok(())
        })?;
        Ok(())
    }

    fn declare_function(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        function: dae::FunctionView<'source>,
        reservation: &dae::FunctionReservation<'_, 'target>,
    ) -> Result<(), dae::DaeConstructionError> {
        let parameters = declare_parameters(target, function, reservation)?;
        let values = declare_values(target, function, self.identities.types, reservation)?;
        self.functions.push(RebuiltFunction {
            id: reservation.function(),
            parameters,
            values,
        });
        Ok(())
    }

    fn rebuild_function(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        index: usize,
        reservation: dae::FunctionReservation<'_, 'target>,
    ) -> Result<(), dae::DaeConstructionError> {
        let function = self.source_function(index);
        let rebuilt_function = self.functions[index].clone();
        if let Some(external) = function.external() {
            return self.rebuild_external_body(
                target,
                function,
                &rebuilt_function,
                reservation,
                external,
            );
        }
        let body =
            target.functions(|functions| functions.begin(reservation, function.declaration()))?;
        let body =
            self.rebuild_statements(target, &rebuilt_function, body, function.statements())?;
        self.seed_results(target, function, &rebuilt_function, &body)?;
        self.rebuild_orphaned_scoped_expressions(target, index, &body)?;
        target.functions(|functions| functions.define(body, function.declaration()))
    }

    /// Reconstruct one MLS §12.9 external interface through the same checked
    /// construction operation production lowering uses.
    ///
    /// Argument expressions rebuild without a body capability: a checked
    /// external argument provably reads no function value, so there is no
    /// definition for a body to supply.
    fn rebuild_external_body(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        function: dae::FunctionView<'source>,
        rebuilt_function: &RebuiltFunction<'target>,
        reservation: dae::FunctionReservation<'_, 'target>,
        external: dae::ExternalFunctionView<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        let source_arguments = external.arguments().collect::<Vec<_>>();
        let mut arguments = Vec::with_capacity(source_arguments.len());
        for argument in source_arguments {
            arguments.push(match argument {
                dae::ExternalArgumentView::Input(expression) => {
                    dae::ExternalArgument::Input(self.rebuild_postorder(target, expression, None)?)
                }
                dae::ExternalArgumentView::Output(value) => dae::ExternalArgument::Output(
                    self.rebuilt_value(rebuilt_function, value, function.declaration())?,
                ),
            });
        }
        let result = match external.result() {
            Some(value) => {
                Some(self.rebuilt_value(rebuilt_function, value, function.declaration())?)
            }
            None => None,
        };
        let body = dae::ExternalFunctionBody::new(
            external.purity(),
            external.language(),
            external.symbol().clone(),
            arguments,
            result,
            external.linkage().clone(),
        );
        target.functions(|functions| {
            functions.define_external(reservation, body, function.declaration())
        })
    }

    fn rebuilt_value(
        &self,
        function: &RebuiltFunction<'target>,
        value: dae::FunctionValueId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::FunctionValueId<'target>, dae::DaeConstructionError> {
        function
            .values
            .get(value.ordinal() as usize)
            .copied()
            .ok_or(dae::DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            })
    }

    fn source_function(&self, index: usize) -> dae::FunctionView<'source> {
        let id = self
            .source
            .function_id(index)
            .expect("finalized function ordinal resolves");
        self.source
            .function(id)
            .expect("finalized function identity resolves")
    }

    fn expect_next_function(&self, index: usize) {
        assert_eq!(
            self.functions.len(),
            index,
            "checked DAE functions are stored in dependency-first SCC order"
        );
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
        target: &mut dae::DaeConstruction<'target>,
        function: &RebuiltFunction<'target>,
        mut body: dae::FunctionBody<'target>,
        statements: dae::FunctionStatements<'source>,
    ) -> Result<dae::FunctionBody<'target>, dae::DaeConstructionError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::Assignment { definition } => {
                    self.assign_statement(target, function, &mut body, definition)?
                }
                dae::FunctionStatementView::Assertion {
                    condition,
                    message,
                    provenance,
                } => self.rebuild_assertion(target, &mut body, condition, message, provenance)?,
                dae::FunctionStatementView::For {
                    fold,
                    statements,
                    provenance,
                } => {
                    body =
                        self.rebuild_loop(target, function, body, fold, statements, provenance)?
                }
            }
        }
        Ok(body)
    }

    fn rebuild_assertion(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        body: &mut dae::FunctionBody<'target>,
        condition: dae::ExprId<'source>,
        message: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        let condition = self.rebuild_expression(target, body, condition)?;
        let message = self.rebuild_expression(target, body, message)?;
        target.functions(|functions| functions.assertion(body, condition, message, provenance))?;
        Ok(())
    }

    fn assign_statement(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        function: &RebuiltFunction<'target>,
        body: &mut dae::FunctionBody<'target>,
        source_definition: dae::FunctionDefinitionView<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        let source_target = source_definition.target();
        let source_rhs = source_definition.rhs();
        let provenance = source_definition.provenance();
        let value = self.rebuild_expression(target, body, source_rhs)?;
        let target_value = function.values[source_target.ordinal() as usize];
        target.functions(|functions| functions.assign(body, target_value, value, provenance))?;
        let target_definition = target.functions(|functions| {
            functions.current_definition_id(body, target_value, provenance)
        })?;
        self.materialize_definition_uses(
            target,
            body,
            source_definition,
            source_target,
            target_value,
            target_definition,
        )
    }

    fn rebuild_loop(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
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
            target,
            &body,
            source_fold.targets(),
            &targets,
            source_fold.initial_values(),
        )?;
        let domain = self.identities.domains[source_fold.domain().index() as usize].id;
        let mut loop_body = target.functions(|functions| {
            functions.begin_loop(body, domain, targets.clone(), source_fold.provenance())
        })?;
        self.seed_current(
            target,
            loop_body.body(),
            source_fold.targets(),
            &targets,
            source_fold.parameter_values(),
        )?;
        self.rebuild_loop_statements(target, function, &mut loop_body, statements)?;
        self.seed_current(
            target,
            loop_body.body(),
            source_fold.targets(),
            &targets,
            source_fold.update_values(),
        )?;
        let body = target.functions(|functions| functions.finish_loop(loop_body, provenance))?;
        self.seed_current(
            target,
            &body,
            source_fold.targets(),
            &targets,
            source_fold.output_values(),
        )?;
        Ok(body)
    }

    fn rebuild_loop_statements(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        function: &RebuiltFunction<'target>,
        loop_body: &mut dae::FunctionLoop<'target>,
        statements: dae::FunctionStatements<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        for statement in statements {
            let dae::FunctionStatementView::Assignment { definition } = statement else {
                self.rebuild_loop_assertion(target, loop_body, statement)?;
                continue;
            };
            let source_target = definition.target();
            let source_rhs = definition.rhs();
            let provenance = definition.provenance();
            let target_value = function.values[source_target.ordinal() as usize];
            let value = self.rebuild_expression(target, loop_body.body(), source_rhs)?;
            target.functions(|functions| {
                functions.assign_loop(loop_body, target_value, value, provenance)
            })?;
            let target_definition = target.functions(|functions| {
                functions.current_definition_id(loop_body.body(), target_value, provenance)
            })?;
            self.materialize_definition_uses(
                target,
                loop_body.body(),
                definition,
                source_target,
                target_value,
                target_definition,
            )?;
        }
        Ok(())
    }

    fn rebuild_loop_assertion(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        loop_body: &mut dae::FunctionLoop<'target>,
        statement: dae::FunctionStatementView<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        let dae::FunctionStatementView::Assertion {
            condition,
            message,
            provenance,
        } = statement
        else {
            unreachable!("checked function loops cannot contain nested loops")
        };
        let condition = self.rebuild_expression(target, loop_body.body(), condition)?;
        let message = self.rebuild_expression(target, loop_body.body(), message)?;
        target.functions(|functions| {
            functions.assertion_loop(loop_body, condition, message, provenance)
        })
    }

    fn rebuild_expression(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        body: &dae::FunctionBody<'target>,
        source_id: dae::ExprId<'source>,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        self.rebuild_postorder(target, source_id, Some(body))
    }

    fn rebuild_orphaned_scoped_expressions(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        function: usize,
        body: &dae::FunctionBody<'target>,
    ) -> Result<(), dae::DaeConstructionError> {
        for index in 0..self.source.expression_count() {
            let source_id = self
                .source
                .expression_id(index)
                .expect("finalized expression ordinal resolves");
            let expression = self
                .source
                .expression(source_id)
                .expect("finalized expression identity resolves");
            if expression
                .function_scope()
                .is_some_and(|owner| owner.index() as usize == function)
                && self.rebuilt[source_id.index() as usize].is_none()
            {
                self.rebuild_postorder(target, source_id, Some(body))?;
            }
        }
        Ok(())
    }

    fn expect_all_scoped_expressions_mapped(&self) -> Result<(), dae::DaeConstructionError> {
        for index in 0..self.source.expression_count() {
            let source_id = self
                .source
                .expression_id(index)
                .expect("finalized expression ordinal resolves");
            let expression = self
                .source
                .expression(source_id)
                .expect("finalized expression identity resolves");
            if expression.function_scope().is_some() && self.rebuilt[index].is_none() {
                return Err(dae::DaeConstructionError::IncompleteDefinition {
                    kind: "function expression",
                    index: source_id.index(),
                    span: expression.provenance().span(),
                });
            }
        }
        Ok(())
    }

    fn rebuild_postorder(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
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
                        target, current, value, definition, expanded, body,
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
                _operation if expanded => self.rebuild_expanded_expression(target, current)?,
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
        target: &mut dae::DaeConstruction<'target>,
        current: dae::ExprId<'source>,
        value: dae::FunctionValueId<'source>,
        source_definition: dae::FunctionDefinitionView<'source>,
        expanded: bool,
        body: Option<&dae::FunctionBody<'target>>,
    ) -> Result<(), dae::DaeConstructionError> {
        let provenance = self
            .source
            .expression(current)
            .expect("checked function expression resolves")
            .provenance();
        if !expanded {
            self.pending.push((current, true));
            return Ok(());
        }
        let target_value =
            self.functions[value.function().index() as usize].values[value.ordinal() as usize];
        let body = body.ok_or(dae::DaeConstructionError::IncompleteDefinition {
            kind: "function value occurrence",
            index: current.index(),
            span: provenance.span(),
        })?;
        let target_definition = self.mapped_definition(source_definition.id(), provenance)?;
        let current_definition = target.functions(|functions| {
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
        let rebuilt =
            target.functions(|functions| functions.read(body, target_value, provenance))?;
        self.rebuilt[current.index() as usize] = Some(rebuilt);
        Ok(())
    }

    fn rebuild_expanded_expression(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        current: dae::ExprId<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        let identities = RebuiltIdentities {
            base: self.identities,
            functions: &self.functions,
        };
        let source = self.source;
        let facts = self.facts;
        let candidate = self.candidate;
        let rebuilt_state = &mut *self.rebuilt;
        let rebuilt = target.expressions(|expressions| {
            ExpressionRebuilder::new(
                source,
                expressions,
                identities,
                facts,
                candidate,
                rebuilt_state,
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
            | dae::ExpressionOperation::FunctionFoldParameter { .. }
            | dae::ExpressionOperation::FunctionFoldOutput { .. }
            | dae::ExpressionOperation::FunctionValue { .. } => {}
            dae::ExpressionOperation::Range(range) => {
                push(range.stop().expression());
                if let Some(step) = range.explicit_step() {
                    push(step.expression());
                }
                push(range.start().expression());
            }
            dae::ExpressionOperation::Unary { operand, .. } => push(operand),
            dae::ExpressionOperation::ClockTransfer { source, .. } => push(source),
            dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
                push(rhs);
                push(lhs);
            }
            dae::ExpressionOperation::StringConversion { value, format, .. } => {
                Self::push_string_conversion_dependencies(pending, value, format);
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

    fn push_string_conversion_dependencies(
        pending: &mut Vec<(dae::ExprId<'source>, bool)>,
        value: dae::ExprId<'source>,
        format: dae::StringConversionFormatView<'source>,
    ) {
        pending.push((value, false));
        match format {
            dae::StringConversionFormatView::Options {
                minimum_length,
                left_justified,
                significant_digits,
            } => {
                for option in [minimum_length, left_justified, significant_digits]
                    .into_iter()
                    .flatten()
                {
                    pending.push((option, false));
                }
            }
            dae::StringConversionFormatView::Format { value } => pending.push((value, false)),
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
        target: &mut dae::DaeConstruction<'target>,
        source: dae::FunctionView<'source>,
        function: &RebuiltFunction<'target>,
        body: &dae::FunctionBody<'target>,
    ) -> Result<(), dae::DaeConstructionError> {
        let outputs = source
            .values()
            .filter(|value| value.role() == dae::FunctionValueRole::Output);
        for (source_result, source_output) in source.result_values().iter().zip(outputs) {
            let target_output = function.values[source_output.id().ordinal() as usize];
            let target_definition = target.functions(|functions| {
                functions.current_definition_id(body, target_output, source_result.provenance())
            })?;
            let target_rhs = target.functions(|functions| {
                functions.current_definition(body, target_output, source_result.provenance())
            })?;
            self.seed(source_result.rhs(), target_rhs, source_result.provenance())?;
            self.materialize_definition_uses(
                target,
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
        target: &mut dae::DaeConstruction<'target>,
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
            let target_definition = target.functions(|functions| {
                functions.current_definition_id(body, target_value, source_definition.provenance())
            })?;
            let target_rhs = target.functions(|functions| {
                functions.current_definition(body, target_value, source_definition.provenance())
            })?;
            self.seed(
                source_definition.rhs(),
                target_rhs,
                source_definition.provenance(),
            )?;
            self.materialize_definition_uses(
                target,
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
        target: &mut dae::DaeConstruction<'target>,
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
            let current_definition = target.functions(|functions| {
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
            let rebuilt = target.functions(|functions| {
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
