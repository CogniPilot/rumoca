use std::{
    collections::{BTreeMap, BTreeSet},
    marker::PhantomData,
};

use super::*;

#[derive(Debug, PartialEq, Eq)]
pub(super) struct CallableSourceInventory {
    pub(super) regions: CallableRegionInventory,
    functions: Box<[CallableFunctionEntry]>,
    expressions: Box<[CallableExpressionEntry]>,
    definitions: Box<[CallableDefinitionEntry]>,
    assertions: Box<[CallableAssertionEntry]>,
    conditionals: Box<[CallableConditionalEntry]>,
    calls: Box<[CallableCallEntry]>,
    call_offsets: Box<[usize]>,
    folds: Box<[CallableFoldEntry]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableFunctionEntry {
    function: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableExpressionEntry {
    function: u32,
    expression: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableDefinitionEntry {
    function: u32,
    definition: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableAssertionEntry {
    function: u32,
    assertion: u32,
    condition: u32,
    message: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, PartialEq, Eq)]
struct CallableConditionalEntry {
    function: u32,
    conditional: u32,
    definitions: Box<[u32]>,
    conditions: Box<[u32]>,
    branches: Box<[Box<[u32]>]>,
    fallback: Box<[u32]>,
    provenance: DaeProvenance,
}

#[derive(Debug, PartialEq, Eq)]
struct CallableCallEntry {
    function: u32,
    call: u32,
    owner: u32,
    projections: Box<[CallableCallProjectionEntry]>,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableCallProjectionEntry {
    expression: u32,
    output: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CallableFoldEntry {
    function: u32,
    fold: u32,
    provenance: DaeProvenance,
}

/// One generative lending of the exact reachable callable-source inventory.
///
/// The inventory belongs to the retained DAE and is reissued by current-schema
/// replay. It contains identities and provenance only; no operation-contract
/// classification is stored here.
pub struct CallableSourceInventoryView<'inventory, 'dae> {
    pub(super) inventory: &'dae CallableSourceInventory,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

pub struct CallableFunctionOccurrence<'inventory, 'dae> {
    function: FunctionId<'dae>,
    provenance: DaeProvenance,
    calls: &'dae [CallableCallEntry],
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

pub struct CallableExpressionOccurrence<'inventory, 'dae> {
    function: FunctionId<'dae>,
    expression: ExprId<'dae>,
    provenance: DaeProvenance,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

pub struct CallableDefinitionOccurrence<'inventory, 'dae> {
    definition: FunctionDefinitionId<'dae>,
    provenance: DaeProvenance,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

pub struct CallableAssertionOccurrence<'inventory, 'dae> {
    assertion: FunctionAssertionId<'dae>,
    condition: ExprId<'dae>,
    message: ExprId<'dae>,
    provenance: DaeProvenance,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

pub struct CallableConditionalOccurrence<'inventory, 'dae> {
    conditional: FunctionConditionalId<'dae>,
    entry: &'dae CallableConditionalEntry,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

pub struct CallableCallOccurrence<'inventory, 'dae> {
    call: FunctionCallId<'dae>,
    entry: &'dae CallableCallEntry,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

#[derive(Clone, Copy)]
pub struct CallableCallProjectionOccurrence<'inventory, 'dae> {
    expression: ExprId<'dae>,
    output: u32,
    provenance: DaeProvenance,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

pub struct CallableFoldOccurrence<'inventory, 'dae> {
    fold: FunctionFoldId<'dae>,
    provenance: DaeProvenance,
    marker: PhantomData<&'inventory mut &'inventory ()>,
}

impl<'dae> DaeView<'dae> {
    pub fn with_callable_source_inventory<R>(
        self,
        inspect: impl for<'inventory> FnOnce(CallableSourceInventoryView<'inventory, 'dae>) -> R,
    ) -> R {
        fn lend<'inventory, 'dae, R>(
            inventory: &'dae CallableSourceInventory,
            inspect: impl FnOnce(CallableSourceInventoryView<'inventory, 'dae>) -> R,
        ) -> R {
            inspect(CallableSourceInventoryView {
                inventory,
                marker: PhantomData,
            })
        }

        lend(&self.dae.storage.callable_inventory, inspect)
    }
}

impl<'inventory, 'dae> CallableSourceInventoryView<'inventory, 'dae> {
    pub fn functions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableFunctionOccurrence<'inventory, 'dae>> + 'dae {
        let inventory = self.inventory;
        self.inventory.functions.iter().map(move |entry| {
            let ordinal = entry.function as usize;
            let start = inventory.call_offsets[ordinal];
            let end = inventory.call_offsets[ordinal + 1];
            CallableFunctionOccurrence {
                function: FunctionId::from_raw(entry.function),
                provenance: entry.provenance,
                calls: &inventory.calls[start..end],
                marker: PhantomData,
            }
        })
    }

    pub fn expressions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableExpressionOccurrence<'inventory, 'dae>> + 'dae {
        self.inventory
            .expressions
            .iter()
            .map(|entry| CallableExpressionOccurrence {
                function: FunctionId::from_raw(entry.function),
                expression: ExprId::from_raw(entry.expression),
                provenance: entry.provenance,
                marker: PhantomData,
            })
    }

    pub fn definitions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableDefinitionOccurrence<'inventory, 'dae>> + 'dae {
        self.inventory
            .definitions
            .iter()
            .map(|entry| CallableDefinitionOccurrence {
                definition: FunctionDefinitionId::from_raw(entry.function, entry.definition),
                provenance: entry.provenance,
                marker: PhantomData,
            })
    }

    pub fn assertions(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableAssertionOccurrence<'inventory, 'dae>> + 'dae {
        self.inventory
            .assertions
            .iter()
            .map(|entry| CallableAssertionOccurrence {
                assertion: FunctionAssertionId::from_raw(entry.function, entry.assertion),
                condition: ExprId::from_raw(entry.condition),
                message: ExprId::from_raw(entry.message),
                provenance: entry.provenance,
                marker: PhantomData,
            })
    }

    pub fn conditionals(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableConditionalOccurrence<'inventory, 'dae>> + 'dae {
        self.inventory
            .conditionals
            .iter()
            .map(|entry| CallableConditionalOccurrence {
                conditional: FunctionConditionalId::from_raw(entry.function, entry.conditional),
                entry,
                marker: PhantomData,
            })
    }

    pub fn calls(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableCallOccurrence<'inventory, 'dae>> + 'dae {
        self.inventory
            .calls
            .iter()
            .map(|entry| CallableCallOccurrence {
                call: FunctionCallId::from_raw(entry.function, entry.call),
                entry,
                marker: PhantomData,
            })
    }

    pub fn folds(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableFoldOccurrence<'inventory, 'dae>> + 'dae {
        self.inventory
            .folds
            .iter()
            .map(|entry| CallableFoldOccurrence {
                fold: FunctionFoldId::from_raw(entry.function, entry.fold),
                provenance: entry.provenance,
                marker: PhantomData,
            })
    }
}

impl<'inventory, 'dae> CallableFunctionOccurrence<'inventory, 'dae> {
    pub const fn function(&self) -> FunctionId<'dae> {
        self.function
    }

    pub const fn provenance(&self) -> DaeProvenance {
        self.provenance
    }

    pub fn calls(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableCallOccurrence<'inventory, 'dae>> + 'dae {
        self.calls.iter().map(|entry| CallableCallOccurrence {
            call: FunctionCallId::from_raw(entry.function, entry.call),
            entry,
            marker: PhantomData,
        })
    }

    pub fn call(&self, ordinal: usize) -> Option<CallableCallOccurrence<'inventory, 'dae>> {
        let entry = self.calls.get(ordinal)?;
        Some(CallableCallOccurrence {
            call: FunctionCallId::from_raw(entry.function, entry.call),
            entry,
            marker: PhantomData,
        })
    }
}

impl<'dae> CallableExpressionOccurrence<'_, 'dae> {
    pub const fn function(&self) -> FunctionId<'dae> {
        self.function
    }

    pub const fn expression(&self) -> ExprId<'dae> {
        self.expression
    }

    pub const fn provenance(&self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> CallableDefinitionOccurrence<'_, 'dae> {
    pub const fn definition(&self) -> FunctionDefinitionId<'dae> {
        self.definition
    }

    pub const fn provenance(&self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> CallableAssertionOccurrence<'_, 'dae> {
    pub const fn assertion(&self) -> FunctionAssertionId<'dae> {
        self.assertion
    }

    pub const fn condition(&self) -> ExprId<'dae> {
        self.condition
    }

    pub const fn message(&self) -> ExprId<'dae> {
        self.message
    }

    pub const fn provenance(&self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> CallableConditionalOccurrence<'_, 'dae> {
    pub const fn conditional(&self) -> FunctionConditionalId<'dae> {
        self.conditional
    }

    pub fn definitions(&self) -> impl ExactSizeIterator<Item = FunctionDefinitionId<'dae>> + 'dae {
        let function = self.conditional.function().index();
        self.entry
            .definitions
            .iter()
            .copied()
            .map(move |ordinal| FunctionDefinitionId::from_raw(function, ordinal))
    }

    pub fn conditions(&self) -> impl ExactSizeIterator<Item = ExprId<'dae>> + 'dae {
        self.entry.conditions.iter().copied().map(ExprId::from_raw)
    }

    pub fn branch_count(&self) -> usize {
        self.entry.branches.len()
    }

    pub fn branches(
        &self,
    ) -> impl ExactSizeIterator<Item = impl ExactSizeIterator<Item = ExprId<'dae>> + 'dae> + 'dae
    {
        self.entry
            .branches
            .iter()
            .map(|branch| branch.iter().copied().map(ExprId::from_raw))
    }

    pub fn branch(
        &self,
        ordinal: usize,
    ) -> Option<impl ExactSizeIterator<Item = ExprId<'dae>> + 'dae> {
        self.entry
            .branches
            .get(ordinal)
            .map(|branch| branch.iter().copied().map(ExprId::from_raw))
    }

    pub fn fallback(&self) -> impl ExactSizeIterator<Item = ExprId<'dae>> + 'dae {
        self.entry.fallback.iter().copied().map(ExprId::from_raw)
    }

    pub const fn provenance(&self) -> DaeProvenance {
        self.entry.provenance
    }
}

impl<'dae> CallableCallOccurrence<'_, 'dae> {
    pub const fn call(&self) -> FunctionCallId<'dae> {
        self.call
    }

    pub const fn owner(&self) -> ExprId<'dae> {
        ExprId::from_raw(self.entry.owner)
    }

    pub fn projections(
        &self,
    ) -> impl ExactSizeIterator<Item = CallableCallProjectionOccurrence<'_, 'dae>> + '_ {
        self.entry
            .projections
            .iter()
            .map(|projection| CallableCallProjectionOccurrence {
                expression: ExprId::from_raw(projection.expression),
                output: projection.output,
                provenance: projection.provenance,
                marker: PhantomData,
            })
    }

    pub const fn provenance(&self) -> DaeProvenance {
        self.entry.provenance
    }
}

impl<'dae> CallableCallProjectionOccurrence<'_, 'dae> {
    pub const fn expression(self) -> ExprId<'dae> {
        self.expression
    }

    pub const fn output(self) -> u32 {
        self.output
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> CallableFoldOccurrence<'_, 'dae> {
    pub const fn fold(&self) -> FunctionFoldId<'dae> {
        self.fold
    }

    pub const fn provenance(&self) -> DaeProvenance {
        self.provenance
    }
}

pub(super) fn issue_callable_source_inventory(
    storage: &Storage,
) -> Result<CallableSourceInventory, DaeConstructionError> {
    let regions = issue_callable_region_inventory(storage)?;
    let mut functions = Vec::with_capacity(storage.functions.len());
    let mut expressions = Vec::new();
    let mut definitions = Vec::new();
    let mut assertions = Vec::new();
    let mut conditionals = Vec::new();
    let mut calls = Vec::new();
    let mut call_offsets = Vec::with_capacity(storage.functions.len() + 1);
    let mut folds = Vec::new();
    let mut marks = vec![None; storage.expressions.nodes.len()];

    for (function_index, function) in storage.functions.iter().enumerate() {
        call_offsets.push(calls.len());
        let function_raw =
            u32::try_from(function_index).map_err(|_| DaeConstructionError::CapacityExceeded {
                arena: "callable function inventory",
                attempted_index: function_index,
                span: function.declaration.span(),
            })?;
        functions.push(CallableFunctionEntry {
            function: function_raw,
            provenance: function.declaration,
        });
        let mut roots = Vec::new();
        let mut definition_owners = vec![false; function.definitions.len()];
        let mut fold_owners = vec![false; function.folds.len()];
        let Some(body) = function.definition.as_ref() else {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function",
                index: function_raw,
                span: function.declaration.span(),
            });
        };
        match body {
            FunctionBodyEntry::Modelica(body) => StatementOccurrenceCollector {
                storage,
                function: function_raw,
                function_provenance: function.declaration,
                roots: &mut roots,
                definitions: &mut definitions,
                assertions: &mut assertions,
                conditionals: &mut conditionals,
                folds: &mut folds,
                definition_owners: &mut definition_owners,
                fold_owners: &mut fold_owners,
                next_assertion: 0,
                next_conditional: 0,
            }
            .collect(&body.statements)?,
            FunctionBodyEntry::External(body) => {
                roots.extend(body.arguments.iter().filter_map(|argument| match argument {
                    ExternalArgumentEntry::Input(expression) => Some(*expression),
                    ExternalArgumentEntry::Output(_) => None,
                }));
            }
        }
        check_complete_owners(&definition_owners, &fold_owners, function.declaration)?;
        let expression_start = expressions.len();
        let mut requested_call_projections = BTreeSet::new();
        collect_reachable_expressions(
            storage,
            function_raw,
            &mut roots,
            &mut marks,
            &mut expressions,
            &mut requested_call_projections,
            function.declaration,
        )?;
        collect_call_occurrences(
            storage,
            function_raw,
            &expressions[expression_start..],
            &requested_call_projections,
            &mut calls,
            function.declaration,
        )?;
    }
    call_offsets.push(calls.len());

    Ok(CallableSourceInventory {
        regions,
        functions: functions.into_boxed_slice(),
        expressions: expressions.into_boxed_slice(),
        definitions: definitions.into_boxed_slice(),
        assertions: assertions.into_boxed_slice(),
        conditionals: conditionals.into_boxed_slice(),
        calls: calls.into_boxed_slice(),
        call_offsets: call_offsets.into_boxed_slice(),
        folds: folds.into_boxed_slice(),
    })
}

fn check_complete_owners(
    definition_owners: &[bool],
    fold_owners: &[bool],
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if let Some(definition) = definition_owners.iter().position(|owned| !owned) {
        return Err(DaeConstructionError::IncompleteDefinition {
            kind: "callable function definition owner",
            index: checked_u32(definition, "callable definition inventory", provenance)?,
            span: provenance.span(),
        });
    }
    if let Some(fold) = fold_owners.iter().position(|owned| !owned) {
        return Err(DaeConstructionError::IncompleteDefinition {
            kind: "callable function fold owner",
            index: checked_u32(fold, "callable fold inventory", provenance)?,
            span: provenance.span(),
        });
    }
    Ok(())
}

fn collect_call_occurrences(
    storage: &Storage,
    function: u32,
    expressions: &[CallableExpressionEntry],
    requested_call_projections: &BTreeSet<u32>,
    calls: &mut Vec<CallableCallEntry>,
    function_provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let mut projections_by_owner = BTreeMap::<u32, Vec<CallableCallProjectionEntry>>::new();
    for expression in expressions {
        let Some(ExprNode::Call { owner, output, .. }) = storage
            .expressions
            .nodes
            .get(expression.expression as usize)
        else {
            continue;
        };
        if !requested_call_projections.contains(&expression.expression) {
            continue;
        }
        projections_by_owner
            .entry(*owner)
            .or_default()
            .push(CallableCallProjectionEntry {
                expression: expression.expression,
                output: *output,
                provenance: expression.provenance,
            });
    }
    for (ordinal, (owner, mut projections)) in projections_by_owner.into_iter().enumerate() {
        projections.sort_unstable_by_key(|projection| projection.expression);
        let call = checked_u32(ordinal, "callable call inventory", function_provenance)?;
        let provenance = storage
            .expressions
            .provenance
            .get(owner as usize)
            .copied()
            .ok_or_else(|| unknown("callable call owner", owner, function_provenance))?;
        calls.push(CallableCallEntry {
            function,
            call,
            owner,
            projections: projections.into_boxed_slice(),
            provenance,
        });
    }
    Ok(())
}

struct StatementOccurrenceCollector<'storage, 'output> {
    storage: &'storage Storage,
    function: u32,
    function_provenance: DaeProvenance,
    roots: &'output mut Vec<u32>,
    definitions: &'output mut Vec<CallableDefinitionEntry>,
    assertions: &'output mut Vec<CallableAssertionEntry>,
    conditionals: &'output mut Vec<CallableConditionalEntry>,
    folds: &'output mut Vec<CallableFoldEntry>,
    definition_owners: &'output mut [bool],
    fold_owners: &'output mut [bool],
    next_assertion: u32,
    next_conditional: u32,
}

impl StatementOccurrenceCollector<'_, '_> {
    fn collect(
        &mut self,
        statements: &[FunctionStatementWire],
    ) -> Result<(), DaeConstructionError> {
        let mut pending = statements.iter().rev().collect::<Vec<_>>();
        while let Some(statement) = pending.pop() {
            self.collect_statement(statement, &mut pending)?;
        }
        Ok(())
    }

    fn collect_statement<'statement>(
        &mut self,
        statement: &'statement FunctionStatementWire,
        pending: &mut Vec<&'statement FunctionStatementWire>,
    ) -> Result<(), DaeConstructionError> {
        match statement {
            FunctionStatementWire::Assignment { definition } => {
                self.claim_definition(*definition, self.function_provenance)
            }
            FunctionStatementWire::AssignmentGroup {
                definitions,
                conditional,
            } => self.collect_assignment_group(definitions, conditional.as_ref()),
            FunctionStatementWire::Assertion {
                condition,
                message,
                provenance,
            } => self.collect_assertion(*condition, *message, *provenance),
            FunctionStatementWire::For {
                fold,
                statements,
                provenance,
            } => {
                self.collect_fold(*fold, *provenance)?;
                pending.extend(statements.iter().rev());
                Ok(())
            }
        }
    }

    fn collect_assignment_group(
        &mut self,
        definitions: &[u32],
        conditional: Option<&FunctionConditionalWire>,
    ) -> Result<(), DaeConstructionError> {
        for definition in definitions {
            self.claim_definition(*definition, self.function_provenance)?;
        }
        let Some(conditional) = conditional else {
            return Ok(());
        };
        let provenance = self.group_provenance(definitions)?;
        self.conditionals.push(CallableConditionalEntry {
            function: self.function,
            conditional: self.next_conditional,
            definitions: definitions.to_vec().into_boxed_slice(),
            conditions: conditional.conditions.clone().into_boxed_slice(),
            branches: conditional
                .branches
                .iter()
                .cloned()
                .map(Vec::into_boxed_slice)
                .collect(),
            fallback: conditional.fallback.clone().into_boxed_slice(),
            provenance,
        });
        self.next_conditional =
            self.next_conditional
                .checked_add(1)
                .ok_or(DaeConstructionError::CapacityExceeded {
                    arena: "callable conditional inventory",
                    attempted_index: self.conditionals.len(),
                    span: provenance.span(),
                })?;
        self.roots.extend(conditional.conditions.iter().copied());
        self.roots
            .extend(conditional.branches.iter().flatten().copied());
        self.roots.extend(conditional.fallback.iter().copied());
        Ok(())
    }

    fn group_provenance(&self, definitions: &[u32]) -> Result<DaeProvenance, DaeConstructionError> {
        let Some(definition) = definitions.first().copied() else {
            return Err(self.incomplete_conditional());
        };
        let owner = self
            .storage
            .functions
            .get(self.function as usize)
            .ok_or_else(|| unknown("callable function", self.function, self.function_provenance))?;
        owner
            .definitions
            .get(definition as usize)
            .map(|entry| entry.provenance)
            .ok_or_else(|| self.incomplete_conditional())
    }

    fn incomplete_conditional(&self) -> DaeConstructionError {
        DaeConstructionError::IncompleteDefinition {
            kind: "callable conditional assignment group",
            index: self.next_conditional,
            span: self.function_provenance.span(),
        }
    }

    fn collect_assertion(
        &mut self,
        condition: u32,
        message: u32,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        self.assertions.push(CallableAssertionEntry {
            function: self.function,
            assertion: self.next_assertion,
            condition,
            message,
            provenance,
        });
        self.next_assertion =
            self.next_assertion
                .checked_add(1)
                .ok_or(DaeConstructionError::CapacityExceeded {
                    arena: "callable assertion inventory",
                    attempted_index: self.assertions.len(),
                    span: provenance.span(),
                })?;
        self.roots.extend([condition, message]);
        Ok(())
    }

    fn collect_fold(
        &mut self,
        fold: u32,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let owner = self
            .fold_owners
            .get_mut(fold as usize)
            .ok_or_else(|| unknown("callable function fold", fold, provenance))?;
        if std::mem::replace(owner, true) {
            return Err(DaeConstructionError::DuplicateDefinition {
                kind: "callable function fold owner",
                index: fold,
                span: provenance.span(),
            });
        }
        self.folds.push(CallableFoldEntry {
            function: self.function,
            fold,
            provenance,
        });
        let definitions = self.fold_definitions(fold, provenance)?;
        for definition in definitions {
            self.claim_definition(definition, provenance)?;
        }
        Ok(())
    }

    fn fold_definitions(
        &self,
        fold: u32,
        provenance: DaeProvenance,
    ) -> Result<Vec<u32>, DaeConstructionError> {
        let entry = self
            .storage
            .functions
            .get(self.function as usize)
            .and_then(|function| function.folds.get(fold as usize))
            .and_then(|raw| self.storage.function_folds.get(*raw as usize))
            .ok_or_else(|| unknown("callable function fold", fold, provenance))?;
        Ok(entry
            .parameter_definitions
            .iter()
            .chain(&entry.output_definitions)
            .copied()
            .collect())
    }

    fn claim_definition(
        &mut self,
        definition: u32,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let owner = self
            .definition_owners
            .get_mut(definition as usize)
            .ok_or_else(|| unknown("callable function definition", definition, provenance))?;
        if std::mem::replace(owner, true) {
            return Err(DaeConstructionError::DuplicateDefinition {
                kind: "callable function definition owner",
                index: definition,
                span: provenance.span(),
            });
        }
        let function = self
            .storage
            .functions
            .get(self.function as usize)
            .ok_or_else(|| unknown("callable function", self.function, provenance))?;
        let entry = function
            .definitions
            .get(definition as usize)
            .ok_or_else(|| unknown("callable function definition", definition, provenance))?;
        self.definitions.push(CallableDefinitionEntry {
            function: self.function,
            definition,
            provenance: entry.provenance,
        });
        self.roots.push(entry.rhs);
        Ok(())
    }
}

fn collect_reachable_expressions(
    storage: &Storage,
    function: u32,
    roots: &mut Vec<u32>,
    marks: &mut [Option<u32>],
    expressions: &mut Vec<CallableExpressionEntry>,
    requested_call_projections: &mut BTreeSet<u32>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let start = expressions.len();
    let mut pending = roots
        .drain(..)
        .map(|expression| (expression, false))
        .collect::<Vec<_>>();
    while let Some((expression, owner_only)) = pending.pop() {
        let Some(node) = storage.expressions.nodes.get(expression as usize) else {
            return Err(unknown("function expression", expression, provenance));
        };
        if matches!(node, ExprNode::Call { .. }) && !owner_only {
            requested_call_projections.insert(expression);
        }
        let Some(mark) = marks.get_mut(expression as usize) else {
            return Err(unknown("function expression", expression, provenance));
        };
        if *mark == Some(function) {
            continue;
        }
        *mark = Some(function);
        expressions.push(CallableExpressionEntry {
            function,
            expression,
            provenance: storage.expressions.provenance[expression as usize],
        });
        if let ExprNode::Call { owner, .. } = node
            && *owner != expression
        {
            pending.push((*owner, true));
        }
        node.for_each_child(&storage.expressions, |child| pending.push((child, false)));
    }
    expressions[start..].sort_unstable_by_key(|entry| entry.expression);
    Ok(())
}
