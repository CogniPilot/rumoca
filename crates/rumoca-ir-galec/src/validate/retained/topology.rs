//! Sole exhaustive construction traversal for retained semantic subjects.

use super::*;

/// A complete topology built before validator facts may mutate retained state.
///
/// The private lifetime records the one AST root whose addresses populate the
/// build-local lookup. The plan owns every dense locator and parent/role edge;
/// installation only consumes these completed arenas and never walks the AST.
pub(super) struct TopologyPlan<'ast> {
    retained: RetainedValidation,
    lookup: AddressLookup,
    declaration_starts: super::super::DeclarationStartContract,
    arithmetic: Option<crate::package::AlgorithmCodeArithmeticProfile>,
    real_matrix_multiply_outstanding: usize,
    _root: std::marker::PhantomData<&'ast Block>,
}

impl<'ast> TopologyPlan<'ast> {
    pub(super) fn construct(
        block: &'ast Block,
        declaration_starts: super::super::DeclarationStartContract,
        arithmetic: Option<crate::package::AlgorithmCodeArithmeticProfile>,
    ) -> Result<Self, RetainedValidationError> {
        let mut plan = Self {
            retained: RetainedValidation {
                declarations: Vec::new(),
                methods: Vec::new(),
                functions: Vec::new(),
                binders: Vec::new(),
                statements: Vec::new(),
                expressions: Vec::new(),
                references: Vec::new(),
                calls: Vec::new(),
                builtin_results: Vec::new(),
                call_result_projections: Vec::new(),
                real_matrix_multiply_occurrences: Vec::new(),
                root_children: Vec::new(),
                user_call_graph: RequiredFact::Pending,
                fixed_shapes: FixedShapeClosure::ALL_PROVEN,
            },
            lookup: AddressLookup::new(),
            declaration_starts,
            arithmetic,
            real_matrix_multiply_outstanding: 0,
            _root: std::marker::PhantomData,
        };
        plan.block(block)?;
        plan.finish_real_matrix_multiply_occurrences()?;
        Ok(plan)
    }

    pub(super) fn into_parts(self) -> (RetainedValidation, AddressLookup) {
        (self.retained, self.lookup)
    }

    fn block(&mut self, block: &'ast Block) -> Result<(), RetainedValidationError> {
        let Block {
            name: _,
            interface,
            compartments,
            protected,
            error_signals: _,
            protected_functions,
            startup,
            recalibrate,
            do_step,
            public_functions,
            span: _,
        } = block;
        for (index, variable) in interface.iter().enumerate() {
            let crate::ast::InterfaceVariable { kind, decl, start } = variable;
            self.declaration(
                decl,
                match kind {
                    crate::ast::InterfaceKind::Input => DeclarationClass::Input,
                    crate::ast::InterfaceKind::Output => DeclarationClass::Output,
                    crate::ast::InterfaceKind::TunableParameter => {
                        DeclarationClass::TunableParameter
                    }
                },
                SubjectParent::Block(ChildRole::BlockDeclaration(to_u32(index)?)),
                start.as_ref(),
            )?;
        }
        let declaration_offset = interface.len();
        for (index, entity) in protected.iter().enumerate() {
            let crate::ast::ProtectedEntity { kind, decl, start } = entity;
            self.declaration(
                decl,
                protected_declaration_class(*kind),
                SubjectParent::Block(ChildRole::BlockDeclaration(to_u32(
                    declaration_offset + index,
                )?)),
                start.as_ref(),
            )?;
        }
        for compartment in compartments {
            let crate::ast::StateCompartment {
                name: _,
                entities,
                span: _,
            } = compartment;
            for (index, entity) in entities.iter().enumerate() {
                let crate::ast::ProtectedEntity { kind, decl, start } = entity;
                self.declaration(
                    decl,
                    compartment_declaration_class(*kind),
                    SubjectParent::Block(ChildRole::CompartmentMember(to_u32(index)?)),
                    start.as_ref(),
                )?;
            }
        }
        self.method(MethodOwner::Startup, startup)?;
        self.method(MethodOwner::Recalibrate, recalibrate)?;
        self.method(MethodOwner::DoStep, do_step)?;
        for (index, function) in protected_functions
            .iter()
            .chain(public_functions)
            .enumerate()
        {
            self.function(function, to_u32(index)?)?;
        }
        Ok(())
    }

    fn declaration(
        &mut self,
        declaration: &'ast VariableDeclaration,
        class: DeclarationClass,
        parent: SubjectParent,
        start: Option<&'ast Expression>,
    ) -> Result<DeclarationLoc, RetainedValidationError> {
        let VariableDeclaration {
            ty,
            name: _,
            dimensions,
            range,
            span,
        } = declaration;
        let crate::ast::RangeAttributes { min, max } = range;
        let loc = DeclarationLoc(to_u32(self.retained.declarations.len())?);
        let address = std::ptr::from_ref(declaration);
        if self.lookup.declarations.contains_key(&address) {
            return Err(RetainedValidationError::DuplicateSubject);
        }
        self.lookup.declarations.insert(address, loc);
        self.retained.declarations.push(DeclarationSubject {
            parent,
            provenance: exact_or(*span, || {
                SubjectProvenance::Generated(GeneratedOrigin::Declaration)
            }),
            class,
            primitive: matches!(ty, crate::ast::TypeRef::Primitive(_)),
            fixed_shape: fixed_declaration_shape(declaration),
            start: match (
                class.requires_start() && matches!(ty, crate::ast::TypeRef::Primitive(_)),
                start.is_some(),
                self.declaration_starts,
            ) {
                (false, _, _) => RequiredFact::Checked(DeclarationStartFact::NotApplicable),
                (true, true, _) => RequiredFact::Pending,
                (true, false, super::super::DeclarationStartContract::ParsedSyntax) => {
                    RequiredFact::Checked(DeclarationStartFact::NotRepresentedInSyntax)
                }
                (true, false, super::super::DeclarationStartContract::GeneratedPackage) => {
                    RequiredFact::Pending
                }
            },
            children: Vec::new(),
        });
        self.link(parent, SubjectLoc::Declaration(loc))?;
        let owner = SubjectLoc::Declaration(loc);
        for (index, dimension) in dimensions.iter().enumerate() {
            match dimension {
                Dimension::Derived => {}
                Dimension::Expr(expression) => {
                    self.expression(
                        expression,
                        SubjectParent::child(
                            owner,
                            ChildRole::DeclarationDimension(to_u32(index)?),
                        ),
                        ProvenanceContext::NONE,
                        None,
                    )?;
                }
            }
        }
        if let Some(minimum) = min {
            self.expression(
                minimum,
                SubjectParent::child(owner, ChildRole::DeclarationMinimum),
                ProvenanceContext::NONE,
                None,
            )?;
        }
        if let Some(maximum) = max {
            self.expression(
                maximum,
                SubjectParent::child(owner, ChildRole::DeclarationMaximum),
                ProvenanceContext::NONE,
                None,
            )?;
        }
        if let Some(start) = start {
            self.expression(
                start,
                SubjectParent::child(owner, ChildRole::DeclarationStart),
                ProvenanceContext::NONE,
                None,
            )?;
        }
        Ok(loc)
    }

    fn method(
        &mut self,
        owner: MethodOwner,
        method: &'ast BlockMethod,
    ) -> Result<(), RetainedValidationError> {
        let BlockMethod {
            signals: _,
            locals,
            statements,
            span,
        } = method;
        let loc = owner.loc();
        if self.retained.methods.len() != usize::from(loc.0) {
            return Err(RetainedValidationError::DuplicateSubject);
        }
        self.retained.methods.push(MethodSubject {
            provenance: exact_or(*span, || {
                SubjectProvenance::Generated(GeneratedOrigin::LifecycleMethod)
            }),
            children: Vec::new(),
        });
        self.link(
            SubjectParent::Block(ChildRole::BlockMethod(owner)),
            SubjectLoc::Method(loc),
        )?;
        let subject = SubjectLoc::Method(loc);
        let mut owner_declarations = Vec::new();
        owner_declarations.try_reserve(locals.len()).map_err(|_| {
            RetainedValidationError::AllocationFailed {
                family: "topology-owner-declarations",
            }
        })?;
        for (index, local) in locals.iter().enumerate() {
            let declaration = self.declaration(
                local,
                DeclarationClass::MethodLocal,
                SubjectParent::child(subject, ChildRole::MethodLocal(to_u32(index)?)),
                None,
            )?;
            owner_declarations.push((declaration, local));
        }
        self.statements(
            statements,
            subject,
            ChildRole::MethodAction,
            &owner_declarations,
        )
    }

    fn function(
        &mut self,
        function: &'ast UserFunction,
        order: u32,
    ) -> Result<(), RetainedValidationError> {
        let UserFunction {
            kind,
            name: _,
            signals: _,
            parameters,
            locals,
            statements,
            span,
        } = function;
        let loc = FunctionLoc(to_u32(self.retained.functions.len())?);
        let address = std::ptr::from_ref(function);
        if self.lookup.functions.contains_key(&address) {
            return Err(RetainedValidationError::DuplicateSubject);
        }
        let parent = SubjectParent::Block(ChildRole::BlockFunction(order));
        self.retained.functions.push(FunctionSubject {
            parent,
            provenance: exact_or(*span, || {
                SubjectProvenance::Generated(GeneratedOrigin::UserFunction)
            }),
            kind: *kind,
            results: Vec::new(),
            children: Vec::new(),
        });
        self.lookup.functions.insert(address, loc);
        self.link(parent, SubjectLoc::Function(loc))?;
        let owner = SubjectLoc::Function(loc);
        let mut owner_declarations = Vec::new();
        owner_declarations
            .try_reserve(parameters.len().saturating_add(locals.len()))
            .map_err(|_| RetainedValidationError::AllocationFailed {
                family: "topology-owner-declarations",
            })?;
        for (index, parameter) in parameters.iter().enumerate() {
            let crate::ast::Parameter { direction, decl } = parameter;
            let declaration = self.declaration(
                decl,
                match direction {
                    Direction::Input => DeclarationClass::FunctionInput,
                    Direction::Output => DeclarationClass::FunctionOutput,
                },
                SubjectParent::child(
                    owner,
                    ChildRole::FunctionParameter {
                        direction: *direction,
                        index: to_u32(index)?,
                    },
                ),
                None,
            )?;
            if *direction == Direction::Output {
                self.retained.functions[loc.0 as usize]
                    .results
                    .push(declaration);
            }
            owner_declarations.push((declaration, decl));
        }
        for (index, local) in locals.iter().enumerate() {
            let declaration = self.declaration(
                local,
                DeclarationClass::FunctionLocal,
                SubjectParent::child(owner, ChildRole::FunctionLocal(to_u32(index)?)),
                None,
            )?;
            owner_declarations.push((declaration, local));
        }
        self.statements(
            statements,
            owner,
            ChildRole::FunctionAction,
            &owner_declarations,
        )
    }

    fn statements(
        &mut self,
        statements: &'ast [Spanned<Statement>],
        owner: SubjectLoc,
        role: impl Fn(u32) -> ChildRole + Copy,
        owner_declarations: &[(DeclarationLoc, &'ast VariableDeclaration)],
    ) -> Result<(), RetainedValidationError> {
        for (index, statement) in statements.iter().enumerate() {
            self.statement(
                statement,
                SubjectParent::child(owner, role(to_u32(index)?)),
                owner_declarations,
            )?;
        }
        Ok(())
    }

    fn statement(
        &mut self,
        statement: &'ast Spanned<Statement>,
        parent: SubjectParent,
        owner_declarations: &[(DeclarationLoc, &'ast VariableDeclaration)],
    ) -> Result<StatementLoc, RetainedValidationError> {
        let Spanned { node, span } = statement;
        let loc = StatementLoc(to_u32(self.retained.statements.len())?);
        let address = std::ptr::from_ref(node);
        if self.lookup.statements.contains_key(&address) {
            return Err(RetainedValidationError::DuplicateSubject);
        }
        self.retained.statements.push(StatementSubject {
            parent,
            provenance: exact_or(*span, || {
                SubjectProvenance::Generated(GeneratedOrigin::OrderedStatement)
            }),
            kind: statement_kind(node),
            effect: RequiredFact::Checked(statement_effect(node)),
            real_matrix_multiply: None,
            children: Vec::new(),
        });
        self.lookup.statements.insert(address, loc);
        self.link(parent, SubjectLoc::Statement(loc))?;
        self.statement_children(
            loc,
            node,
            ProvenanceContext::for_statement(*span),
            owner_declarations,
        )?;
        Ok(loc)
    }

    fn statement_children(
        &mut self,
        loc: StatementLoc,
        statement: &'ast Statement,
        context: ProvenanceContext,
        owner_declarations: &[(DeclarationLoc, &'ast VariableDeclaration)],
    ) -> Result<(), RetainedValidationError> {
        let owner = SubjectLoc::Statement(loc);
        match statement {
            Statement::Assignment { target, value } => {
                self.reference(
                    target,
                    SubjectParent::child(owner, ChildRole::AssignmentTarget),
                    context,
                    None,
                )?;
                self.expression(
                    value,
                    SubjectParent::child(owner, ChildRole::AssignmentValue),
                    context,
                    None,
                )?;
            }
            Statement::MultiAssignment { targets, call } => {
                let mut target_locs = Vec::new();
                target_locs.try_reserve(targets.len()).map_err(|_| {
                    RetainedValidationError::AllocationFailed {
                        family: "topology-multi-assignment",
                    }
                })?;
                for (index, target) in targets.iter().enumerate() {
                    target_locs.push(self.reference(
                        target,
                        SubjectParent::child(
                            owner,
                            ChildRole::MultiAssignmentTarget(to_u32(index)?),
                        ),
                        context,
                        None,
                    )?);
                }
                self.call(
                    call,
                    SubjectParent::child(owner, ChildRole::MultiAssignmentCall),
                    context,
                    None,
                    CallUseLoc::MultiAssignment {
                        action: loc,
                        targets: target_locs.into_boxed_slice(),
                    },
                )?;
            }
            Statement::Call(call) => {
                self.call(
                    call,
                    SubjectParent::child(owner, ChildRole::CallStatement),
                    context,
                    None,
                    CallUseLoc::DiscardedBy(loc),
                )?;
            }
            Statement::If(value) => self.if_statement(loc, value, context, owner_declarations)?,
            Statement::For(value) => self.for_loop(loc, value, context, owner_declarations)?,
            Statement::Limit(targets) => self.limit_targets(owner, targets, context)?,
            Statement::Signal(_) => {}
        }
        Ok(())
    }

    fn limit_targets(
        &mut self,
        owner: SubjectLoc,
        targets: &'ast [LimitTarget],
        context: ProvenanceContext,
    ) -> Result<(), RetainedValidationError> {
        for (index, target) in targets.iter().enumerate() {
            match target {
                LimitTarget::SelfState => {}
                LimitTarget::Reference(reference) => {
                    self.reference(
                        reference,
                        SubjectParent::child(owner, ChildRole::LimitTarget(to_u32(index)?)),
                        context,
                        None,
                    )?;
                }
            }
        }
        Ok(())
    }

    fn if_statement(
        &mut self,
        loc: StatementLoc,
        value: &'ast IfStatement,
        context: ProvenanceContext,
        owner_declarations: &[(DeclarationLoc, &'ast VariableDeclaration)],
    ) -> Result<(), RetainedValidationError> {
        let IfStatement {
            branches,
            else_body,
        } = value;
        for (branch_index, branch) in branches.iter().enumerate() {
            let crate::ast::IfBranch {
                condition,
                body,
                span: _,
            } = branch;
            let branch_index = to_u32(branch_index)?;
            match condition {
                Condition::Expression(expression) => {
                    self.expression(
                        expression,
                        SubjectParent::child(
                            SubjectLoc::Statement(loc),
                            ChildRole::IfCondition(branch_index),
                        ),
                        context,
                        None,
                    )?;
                }
                Condition::SignalCheck(check) => {
                    self.signal_check_fallback(loc, branch_index, check, context)?;
                }
            }
            self.statements(
                body,
                SubjectLoc::Statement(loc),
                |index| ChildRole::IfAction {
                    branch: branch_index,
                    index,
                },
                owner_declarations,
            )?;
        }
        if let Some(body) = else_body {
            self.statements(
                body,
                SubjectLoc::Statement(loc),
                ChildRole::ElseAction,
                owner_declarations,
            )?;
        }
        Ok(())
    }

    fn signal_check_fallback(
        &mut self,
        loc: StatementLoc,
        branch: u32,
        check: &'ast crate::ast::SignalCheck,
        context: ProvenanceContext,
    ) -> Result<(), RetainedValidationError> {
        let crate::ast::SignalCheck {
            closure: _,
            test: _,
            fallback,
        } = check;
        let Some(expression) = fallback else {
            return Ok(());
        };
        self.expression(
            expression,
            SubjectParent::child(
                SubjectLoc::Statement(loc),
                ChildRole::SignalFallback(branch),
            ),
            context,
            None,
        )?;
        Ok(())
    }

    fn for_loop(
        &mut self,
        loc: StatementLoc,
        value: &'ast ForLoop,
        context: ProvenanceContext,
        owner_declarations: &[(DeclarationLoc, &'ast VariableDeclaration)],
    ) -> Result<(), RetainedValidationError> {
        let ForLoop {
            iterator,
            start,
            step,
            stop,
            body,
            correlation: _,
        } = value;
        let owner = SubjectLoc::Statement(loc);
        if let Some(iterator) = iterator {
            let binder = BinderLoc(to_u32(self.retained.binders.len())?);
            let address = std::ptr::from_ref(iterator);
            if self.lookup.binders.contains_key(&address) {
                return Err(RetainedValidationError::DuplicateSubject);
            }
            let parent = SubjectParent::child(owner, ChildRole::LoopBinder);
            self.retained.binders.push(BinderSubject {
                parent,
                provenance: context.name(iterator),
            });
            self.lookup.binders.insert(address, binder);
            self.link(parent, SubjectLoc::Binder(binder))?;
        }
        self.expression(
            start,
            SubjectParent::child(owner, ChildRole::LoopStart),
            context,
            None,
        )?;
        if let Some(step) = step {
            self.expression(
                step,
                SubjectParent::child(owner, ChildRole::LoopStep),
                context,
                None,
            )?;
        }
        self.expression(
            stop,
            SubjectParent::child(owner, ChildRole::LoopStop),
            context,
            None,
        )?;
        self.statements(body, owner, ChildRole::LoopAction, owner_declarations)?;
        self.issue_real_matrix_multiply_occurrence(
            loc,
            value,
            context.statement,
            owner_declarations,
        )
    }

    fn issue_real_matrix_multiply_occurrence(
        &mut self,
        owner: StatementLoc,
        loop_: &'ast ForLoop,
        statement_span: Option<rumoca_core::Span>,
        declarations: &[(DeclarationLoc, &'ast VariableDeclaration)],
    ) -> Result<(), RetainedValidationError> {
        let Some(contract) = loop_.real_matrix_multiply_occurrence() else {
            return Ok(());
        };
        let Some(arithmetic) = self.arithmetic else {
            return Err(RetainedValidationError::UnexpectedRealMatrixMultiplyOccurrence);
        };
        self.real_matrix_multiply_outstanding = self
            .real_matrix_multiply_outstanding
            .checked_add(1)
            .ok_or(RetainedValidationError::LocatorOverflow)?;
        let provenance = exact_or(statement_span.unwrap_or(rumoca_core::Span::DUMMY), || {
            SubjectProvenance::Generated(GeneratedOrigin::OrderedStatement)
        });
        let validated = crate::package::matrix_multiply::validate_real_matrix_multiply_occurrence(
            loop_,
            contract,
            declarations,
            arithmetic,
        )
        .map_err(|detail| {
            RetainedValidationError::InvalidRealMatrixMultiplyOccurrence { detail, provenance }
        })?;
        #[cfg(not(test))]
        let _ = validated;
        let locator = RealMatrixMultiplyOccurrenceLoc(to_u32(
            self.retained.real_matrix_multiply_occurrences.len(),
        )?);
        let statement = self
            .retained
            .statements
            .get_mut(owner.0 as usize)
            .ok_or(RetainedValidationError::MissingResolvedSubject)?;
        if statement.real_matrix_multiply.replace(locator).is_some() {
            return Err(RetainedValidationError::DuplicateFact {
                family: "real-matrix-multiply-occurrence",
                index: owner.0,
            });
        }
        self.retained
            .real_matrix_multiply_occurrences
            .push(RealMatrixMultiplyOccurrenceSubject {
                #[cfg(test)]
                owner,
                #[cfg(test)]
                provenance,
                #[cfg(test)]
                target: validated.target,
                #[cfg(test)]
                source: validated.source,
                #[cfg(test)]
                contract: contract.clone(),
            });
        self.real_matrix_multiply_outstanding = self
            .real_matrix_multiply_outstanding
            .checked_sub(1)
            .ok_or(RetainedValidationError::InconsistentFact {
                family: "real-matrix-multiply-occurrence",
                index: owner.0,
            })?;
        Ok(())
    }

    fn finish_real_matrix_multiply_occurrences(&self) -> Result<(), RetainedValidationError> {
        if self.real_matrix_multiply_outstanding == 0 {
            return Ok(());
        }
        Err(RetainedValidationError::MissingFact {
            family: "real-matrix-multiply-occurrence",
            index: 0,
        })
    }

    fn expression(
        &mut self,
        expression: &'ast Expression,
        parent: SubjectParent,
        context: ProvenanceContext,
        gap: Option<CoverageGap>,
    ) -> Result<ExpressionLoc, RetainedValidationError> {
        if let (Expression::Integer(value), Some(format)) = (
            expression,
            self.arithmetic
                .map(crate::package::AlgorithmCodeArithmeticProfile::source_integer),
        ) {
            let minimum = format.minimum();
            let maximum = format.maximum();
            if *value < minimum || *value > maximum {
                return Err(RetainedValidationError::IntegerLiteralOutOfDomain {
                    value: *value,
                    minimum,
                    maximum,
                    provenance: context.expression(),
                });
            }
        }
        let loc = ExpressionLoc(to_u32(self.retained.expressions.len())?);
        let address = std::ptr::from_ref(expression);
        if self.lookup.expressions.contains_key(&address) {
            return Err(RetainedValidationError::DuplicateSubject);
        }
        let ty = gap.map_or(RequiredFact::Pending, RequiredFact::NotChecked);
        let fixed_shape = gap.map_or(RequiredFact::Pending, RequiredFact::NotChecked);
        let evaluated_literal = gap.map_or(RequiredFact::Pending, RequiredFact::NotChecked);
        self.retained.expressions.push(ExpressionSubject {
            parent,
            provenance: context.expression(),
            kind: expression_kind(expression),
            effect: RequiredFact::Checked(expression_effect(expression)),
            ty,
            fixed_shape,
            evaluated_literal,
            children: Vec::new(),
        });
        self.lookup.expressions.insert(address, loc);
        self.link(parent, SubjectLoc::Expression(loc))?;
        self.expression_children(loc, expression, context, gap)?;
        Ok(loc)
    }

    fn expression_children(
        &mut self,
        loc: ExpressionLoc,
        expression: &'ast Expression,
        context: ProvenanceContext,
        gap: Option<CoverageGap>,
    ) -> Result<(), RetainedValidationError> {
        let owner = SubjectLoc::Expression(loc);
        match expression {
            Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {}
            Expression::Ref(reference) => {
                self.reference(
                    reference,
                    SubjectParent::child(owner, ChildRole::ExpressionReference),
                    context,
                    gap,
                )?;
            }
            Expression::Size { array, dimension } => {
                self.reference(
                    array,
                    SubjectParent::child(owner, ChildRole::SizeArray),
                    context,
                    gap,
                )?;
                self.expression(
                    dimension,
                    SubjectParent::child(owner, ChildRole::SizeDimension),
                    context,
                    gap,
                )?;
            }
            Expression::Call(call) => {
                self.call(
                    call,
                    SubjectParent::child(owner, ChildRole::ExpressionCall),
                    context,
                    gap,
                    CallUseLoc::ExpressionValue(loc),
                )?;
            }
            Expression::Paren(inner) => {
                self.expression(
                    inner,
                    SubjectParent::child(owner, ChildRole::Parenthesized),
                    context,
                    gap,
                )?;
            }
            Expression::If(value) => self.if_expression_children(owner, value, context, gap)?,
            Expression::Array(elements) => {
                for (index, element) in elements.iter().enumerate() {
                    self.expression(
                        element,
                        SubjectParent::child(owner, ChildRole::ArrayElement(to_u32(index)?)),
                        context,
                        gap,
                    )?;
                }
            }
            Expression::Neg(reference) => {
                self.reference(
                    reference,
                    SubjectParent::child(owner, ChildRole::NegatedReference),
                    context,
                    gap,
                )?;
            }
            Expression::Not(inner) => {
                self.expression(
                    inner,
                    SubjectParent::child(owner, ChildRole::NotOperand),
                    context,
                    gap,
                )?;
            }
            Expression::Binary { op: _, lhs, rhs } => {
                self.expression(
                    lhs,
                    SubjectParent::child(owner, ChildRole::BinaryLeft),
                    context,
                    gap,
                )?;
                self.expression(
                    rhs,
                    SubjectParent::child(owner, ChildRole::BinaryRight),
                    context,
                    gap,
                )?;
            }
        }
        Ok(())
    }

    fn if_expression_children(
        &mut self,
        owner: SubjectLoc,
        value: &'ast crate::ast::IfExpression,
        context: ProvenanceContext,
        gap: Option<CoverageGap>,
    ) -> Result<(), RetainedValidationError> {
        let crate::ast::IfExpression {
            branches,
            else_value,
            correlation: _,
        } = value;
        for (index, (condition, result)) in branches.iter().enumerate() {
            let index = to_u32(index)?;
            self.expression(
                condition,
                SubjectParent::child(owner, ChildRole::IfExpressionCondition(index)),
                context,
                gap,
            )?;
            self.expression(
                result,
                SubjectParent::child(owner, ChildRole::IfExpressionValue(index)),
                context,
                gap,
            )?;
        }
        self.expression(
            else_value,
            SubjectParent::child(owner, ChildRole::IfExpressionElse),
            context,
            gap,
        )?;
        if let Some(selection) = value.bounded_selection_correlation() {
            self.reference(
                selection.reference(),
                SubjectParent::child(owner, ChildRole::AggregateProjectionSource),
                context,
                None,
            )?;
        }
        Ok(())
    }

    fn reference(
        &mut self,
        reference: &'ast Reference,
        parent: SubjectParent,
        context: ProvenanceContext,
        gap: Option<CoverageGap>,
    ) -> Result<ReferenceLoc, RetainedValidationError> {
        let loc = ReferenceLoc(to_u32(self.retained.references.len())?);
        let address = std::ptr::from_ref(reference);
        if self.lookup.references.contains_key(&address) {
            return Err(RetainedValidationError::DuplicateSubject);
        }
        let parts = match reference {
            Reference::Local(part) => std::slice::from_ref(part),
            Reference::State(parts) => parts,
        };
        let provenance = parts.first().map_or_else(
            || context.expression(),
            |part| {
                let crate::ast::RefPart {
                    name,
                    subscripts: _,
                    span: _,
                } = part;
                context.name(name)
            },
        );
        let resolution = gap.map_or(RequiredFact::Pending, RequiredFact::NotChecked);
        let fixed_shape = gap.map_or(RequiredFact::Pending, RequiredFact::NotChecked);
        self.retained.references.push(ReferenceSubject {
            parent,
            provenance,
            resolution,
            fixed_shape,
            children: Vec::new(),
        });
        self.lookup.references.insert(address, loc);
        self.link(parent, SubjectLoc::Reference(loc))?;
        for (part_index, part) in parts.iter().enumerate() {
            let crate::ast::RefPart {
                name: _,
                subscripts,
                span: _,
            } = part;
            for (index, subscript) in subscripts.iter().enumerate() {
                self.expression(
                    subscript,
                    SubjectParent::child(
                        SubjectLoc::Reference(loc),
                        ChildRole::ReferenceSubscript {
                            part: to_u32(part_index)?,
                            index: to_u32(index)?,
                        },
                    ),
                    context,
                    gap,
                )?;
            }
        }
        Ok(loc)
    }

    fn call(
        &mut self,
        call: &'ast FunctionCall,
        parent: SubjectParent,
        context: ProvenanceContext,
        gap: Option<CoverageGap>,
        use_site: CallUseLoc,
    ) -> Result<CallLoc, RetainedValidationError> {
        let FunctionCall {
            function,
            arguments,
        } = call;
        let loc = CallLoc(to_u32(self.retained.calls.len())?);
        let address = std::ptr::from_ref(call);
        if self.lookup.calls.contains_key(&address) {
            return Err(RetainedValidationError::DuplicateSubject);
        }
        let resolution = gap.map_or(RequiredFact::Pending, RequiredFact::NotChecked);
        let result_set = gap.map_or(RequiredFact::Pending, RequiredFact::NotChecked);
        self.retained.calls.push(CallSubject {
            parent,
            provenance: context.name(function),
            use_site,
            effect: RequiredFact::Checked(expression_effect(&Expression::Call(call.clone()))),
            resolution,
            result_set,
            children: Vec::new(),
        });
        self.lookup.calls.insert(address, loc);
        self.link(parent, SubjectLoc::Call(loc))?;
        for (index, argument) in arguments.iter().enumerate() {
            self.expression(
                argument,
                SubjectParent::child(
                    SubjectLoc::Call(loc),
                    ChildRole::CallArgument(to_u32(index)?),
                ),
                context,
                gap,
            )?;
        }
        Ok(loc)
    }

    fn link(
        &mut self,
        parent: SubjectParent,
        child: SubjectLoc,
    ) -> Result<(), RetainedValidationError> {
        require_provenance(self.retained.subject_provenance(child)?)?;
        let role = match parent {
            SubjectParent::Block(role) | SubjectParent::Subject { owner: _, role } => role,
        };
        match parent {
            SubjectParent::Block(_) => self.retained.root_children.push((child, role)),
            SubjectParent::Subject { owner, role: _ } => {
                self.children_mut(owner)?.push((child, role));
            }
        }
        Ok(())
    }

    fn children_mut(
        &mut self,
        owner: SubjectLoc,
    ) -> Result<&mut Vec<(SubjectLoc, ChildRole)>, RetainedValidationError> {
        match owner {
            SubjectLoc::Declaration(loc) => self
                .retained
                .declarations
                .get_mut(loc.0 as usize)
                .map(|subject| &mut subject.children),
            SubjectLoc::Method(loc) => self
                .retained
                .methods
                .get_mut(loc.0 as usize)
                .map(|subject| &mut subject.children),
            SubjectLoc::Function(loc) => self
                .retained
                .functions
                .get_mut(loc.0 as usize)
                .map(|subject| &mut subject.children),
            SubjectLoc::Statement(loc) => self
                .retained
                .statements
                .get_mut(loc.0 as usize)
                .map(|subject| &mut subject.children),
            SubjectLoc::Expression(loc) => self
                .retained
                .expressions
                .get_mut(loc.0 as usize)
                .map(|subject| &mut subject.children),
            SubjectLoc::Reference(loc) => self
                .retained
                .references
                .get_mut(loc.0 as usize)
                .map(|subject| &mut subject.children),
            SubjectLoc::Call(loc) => self
                .retained
                .calls
                .get_mut(loc.0 as usize)
                .map(|subject| &mut subject.children),
            SubjectLoc::Binder(_) | SubjectLoc::CallResultProjection(_) => None,
        }
        .ok_or(RetainedValidationError::MissingResolvedSubject)
    }
}

const fn statement_kind(statement: &Statement) -> StatementKind {
    match statement {
        Statement::Assignment {
            target: _,
            value: _,
        } => StatementKind::Assignment,
        Statement::MultiAssignment {
            targets: _,
            call: _,
        } => StatementKind::MultiAssignment,
        Statement::Call(_) => StatementKind::Call,
        Statement::If(_) => StatementKind::If,
        Statement::For(_) => StatementKind::For,
        Statement::Limit(_) => StatementKind::Limit,
        Statement::Signal(_) => StatementKind::Signal,
    }
}

const fn expression_kind(expression: &Expression) -> ExpressionKind {
    match expression {
        Expression::Bool(value) => ExpressionKind::Boolean(*value),
        Expression::Integer(value) => ExpressionKind::Integer(*value),
        Expression::Real(value) => ExpressionKind::Real(value.to_bits()),
        Expression::Ref(_) => ExpressionKind::Reference,
        Expression::Size {
            array: _,
            dimension: _,
        } => ExpressionKind::Size,
        Expression::Call(_) => ExpressionKind::Call,
        Expression::Paren(_) => ExpressionKind::Parenthesized,
        Expression::If(_) => ExpressionKind::If,
        Expression::Array(_) => ExpressionKind::Array,
        Expression::Neg(_) => ExpressionKind::NegatedReference,
        Expression::Not(_) => ExpressionKind::Not,
        Expression::Binary { op, lhs: _, rhs: _ } => ExpressionKind::Binary(*op),
    }
}

const fn protected_declaration_class(kind: crate::ast::ProtectedKind) -> DeclarationClass {
    match kind {
        crate::ast::ProtectedKind::DependentParameter => DeclarationClass::DependentParameter,
        crate::ast::ProtectedKind::Constant => DeclarationClass::Constant,
        crate::ast::ProtectedKind::State => DeclarationClass::PersistentState,
    }
}

const fn compartment_declaration_class(kind: crate::ast::ProtectedKind) -> DeclarationClass {
    match kind {
        crate::ast::ProtectedKind::DependentParameter => {
            DeclarationClass::CompartmentDependentParameter
        }
        crate::ast::ProtectedKind::Constant => DeclarationClass::CompartmentConstant,
        crate::ast::ProtectedKind::State => DeclarationClass::CompartmentPersistentState,
    }
}
