use super::*;

impl<'dae> ExpressionAt<'_, 'dae> {
    pub fn builtin(
        self,
        builtin: PureBuiltin,
        arguments: impl IntoIterator<Item = ExprId<'dae>>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let arguments = arguments.into_iter().collect::<Vec<_>>();
        let variability = max_variability(self.storage, &arguments, self.provenance)?;
        let result = builtin_result(self.storage, builtin, &arguments, self.provenance)?;
        if matches!(
            builtin,
            PureBuiltin::Div | PureBuiltin::Mod | PureBuiltin::Rem
        ) {
            validate_static_quotient(self.storage, builtin, &arguments, self.provenance)?;
        }
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
        let arguments = arguments.into_iter().collect::<Vec<_>>();
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
                .expect_value_type_compatible(*expected, found, self.provenance)?;
        }
        let Some(&ty) = results.get(output) else {
            return Err(invalid_arity(results.len(), output + 1, self.provenance));
        };
        let operands = self
            .storage
            .expressions
            .push_operands(arguments.into_iter().map(ExprId::index), self.provenance)?;
        let output = checked_u32(output, "function output", self.provenance)?;
        self.insert(
            ExprNode::Call {
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
