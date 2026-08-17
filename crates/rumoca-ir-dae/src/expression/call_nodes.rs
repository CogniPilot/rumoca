use super::*;

impl<'dae> ExpressionAt<'_, 'dae> {
    pub fn builtin(
        self,
        builtin: PureBuiltin,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let result = builtin_result(self.storage, builtin, &arguments, self.provenance)?;
        if matches!(
            builtin,
            PureBuiltin::Div | PureBuiltin::Mod | PureBuiltin::Rem
        ) {
            validate_static_quotient(self.storage, builtin, &arguments, self.provenance)?;
        }
        self.insert_builtin(builtin, arguments, result)
    }

    pub(crate) fn checked_runtime_quotient(
        self,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let result = builtin_result(self.storage, builtin, &arguments, self.provenance)?;
        validate_runtime_quotient(self.storage, builtin, &arguments, self.provenance)?;
        self.insert_builtin(builtin, arguments.into(), result)
    }

    /// Construct a runtime quotient with no event surface.
    ///
    /// MLS §3.7.2 exempts calls inside function bodies from event
    /// generation: the quotient's value semantics stand under the same
    /// time-invariant divisor admission, but no discontinuity root exists to
    /// own — a function body is event-free, and a root would smuggle a
    /// function-scope expression into the model's condition system.
    pub fn function_runtime_quotient(
        self,
        builtin: PureBuiltin,
        arguments: [ExprId<'dae>; 2],
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        self.checked_runtime_quotient(builtin, arguments)
    }

    fn insert_builtin(
        self,
        builtin: PureBuiltin,
        arguments: Vec<ExprId<'dae>>,
        result: ValueType,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let variability = max_variability(self.storage, &arguments, self.provenance)?;
        let binder_domain =
            merged_binder_domain(self.storage, arguments.iter().copied(), self.provenance)?;
        let ty = self.storage.intern_type(result, self.provenance)?;
        let operands = self
            .storage
            .expressions
            .push_operands(arguments.into_iter().map(ExprId::index), self.provenance)?;
        self.insert(
            ExprNode::Builtin { builtin, operands },
            ty,
            variability,
            binder_domain,
        )
    }

    pub fn call(
        self,
        function: FunctionId<'dae>,
        output: usize,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        self.call_results(function, [output], arguments)
            .map(|mut results| {
                results
                    .pop()
                    .expect("one requested call result constructs one projection")
            })
    }

    /// Construct all requested result projections of one exact call occurrence.
    ///
    /// The first projection issues the occurrence identity and every remaining
    /// projection shares both that identity and the one packed argument range.
    /// No later phase has to rediscover that the projections name one call.
    pub fn call_results(
        mut self,
        function: FunctionId<'dae>,
        outputs: impl IntoIterator<Item = usize>,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<Vec<ExprId<'dae>>, DaeConstructionError> {
        let outputs = outputs.into_iter().collect::<Vec<_>>();
        if outputs.is_empty() {
            return Ok(Vec::new());
        }
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let variability = max_variability(self.storage, &arguments, self.provenance)?;
        let binder_domain =
            merged_binder_domain(self.storage, arguments.iter().copied(), self.provenance)?;
        let (parameters, results) = self.storage.function_signature(function, self.provenance)?;
        let parameters = parameters.to_vec();
        let results = results.to_vec();
        if arguments.len() != parameters.len() {
            return Err(invalid_arity(
                parameters.len(),
                arguments.len(),
                self.provenance,
            ));
        }
        for (argument, expected) in arguments.iter().zip(parameters) {
            let found = self
                .storage
                .expressions
                .value_types
                .get(argument.index() as usize)
                .copied()
                .ok_or_else(|| DaeConstructionError::UnknownId {
                    kind: "expression",
                    index: argument.index(),
                    span: self.provenance.span(),
                })?;
            self.storage
                .expect_value_type_compatible(expected, found, self.provenance)?;
        }
        let operands = self
            .storage
            .expressions
            .push_operands(arguments.into_iter().map(ExprId::index), self.provenance)?;
        let owner = checked_u32(
            self.storage.expressions.nodes.len(),
            "function call owner",
            self.provenance,
        )?;
        outputs
            .into_iter()
            .map(|output| {
                let Some(&ty) = results.get(output) else {
                    return Err(invalid_arity(results.len(), output + 1, self.provenance));
                };
                let output = checked_u32(output, "function output", self.provenance)?;
                self.insert_borrowed(
                    ExprNode::Call {
                        owner,
                        function: function.index(),
                        output,
                        operands,
                    },
                    ValueTypeId::from_raw(ty),
                    variability,
                    binder_domain,
                )
            })
            .collect()
    }

    /// Rebuild one additional projection of an already issued call owner.
    ///
    /// Construction accepts it only when provenance, callee, and every
    /// argument identity exactly match the owner, so transformations and wire
    /// replay can preserve identity without an equivalence search.
    pub fn call_projection(
        self,
        owner: ExprId<'dae>,
        function: FunctionId<'dae>,
        output: usize,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let owner_index = owner.index() as usize;
        let (owner_function, operands) = match self.storage.expressions.nodes.get(owner_index) {
            Some(ExprNode::Call {
                owner: root,
                function,
                operands,
                ..
            }) if *root == owner.index() => (*function, *operands),
            _ => {
                return Err(DaeConstructionError::InvalidCallProjectionOwner {
                    span: self.provenance.span(),
                });
            }
        };
        if owner_function != function.index()
            || self.storage.expressions.provenance[owner_index] != self.provenance
            || self.storage.expressions.operands[operands.indices()]
                != arguments
                    .iter()
                    .map(|argument| argument.index())
                    .collect::<Vec<_>>()
        {
            return Err(DaeConstructionError::InvalidCallProjectionOwner {
                span: self.provenance.span(),
            });
        }
        self.insert_call_projection(owner, function, output)
    }

    pub(crate) fn replay_call_projection(
        self,
        owner: ExprId<'dae>,
        function: FunctionId<'dae>,
        output: usize,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        self.insert_call_projection(owner, function, output)
    }

    fn insert_call_projection(
        mut self,
        owner: ExprId<'dae>,
        function: FunctionId<'dae>,
        output: usize,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let owner_index = owner.index() as usize;
        let operands = match self.storage.expressions.nodes.get(owner_index) {
            Some(ExprNode::Call {
                owner: root,
                function: owner_function,
                operands,
                ..
            }) if *root == owner.index() && *owner_function == function.index() => *operands,
            _ => {
                return Err(DaeConstructionError::InvalidCallProjectionOwner {
                    span: self.provenance.span(),
                });
            }
        };
        if self.storage.expressions.provenance[owner_index] != self.provenance {
            return Err(DaeConstructionError::InvalidCallProjectionOwner {
                span: self.provenance.span(),
            });
        }
        let arguments = self.storage.expressions.operands[operands.indices()]
            .iter()
            .copied()
            .map(ExprId::from_raw)
            .collect::<Vec<_>>();
        let variability = max_variability(self.storage, &arguments, self.provenance)?;
        let binder_domain =
            merged_binder_domain(self.storage, arguments.iter().copied(), self.provenance)?;
        let (parameters, results) = self.storage.function_signature(function, self.provenance)?;
        if arguments.len() != parameters.len() {
            return Err(invalid_arity(
                parameters.len(),
                arguments.len(),
                self.provenance,
            ));
        }
        let Some(&ty) = results.get(output) else {
            return Err(invalid_arity(results.len(), output + 1, self.provenance));
        };
        let output = checked_u32(output, "function output", self.provenance)?;
        self.insert_borrowed(
            ExprNode::Call {
                owner: owner.index(),
                function: function.index(),
                output,
                operands,
            },
            ValueTypeId::from_raw(ty),
            variability,
            binder_domain,
        )
    }
}
