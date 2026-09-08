//! Expression and function-call type inference for the late typecheck pass:
//! the value type of an expression, builtin and user-function result types,
//! and the function definitions those results are read from.

use super::*;

impl TypeChecker {
    /// The sole issuer of an expression's value type.
    ///
    /// The result is closed: `Known` carries a canonical identity, `Unknown`
    /// abstains to another owner with a required reason, and `Invalid` records
    /// a definite absence of a value type together with the checker that owns
    /// its diagnostic. No consumer may substitute a type for the latter two.
    pub(crate) fn infer_expression_type(
        &self,
        expr: &Expression,
        type_table: &TypeTable,
    ) -> ExpressionType {
        // A `for` binder shadows a component of the same name, so it is
        // consulted first. Its type is whatever its range domain issued.
        if let Expression::ComponentReference(comp) = expr
            && let Some(binder) = self.iterator_binder_type(comp)
        {
            return binder;
        }
        match self.lookup_instance_expression(expr) {
            // The overlay's identity is not guaranteed resolved, so it goes
            // through the checked constructor like every other identity.
            SemanticLookup::Found(semantics) => return ExpressionType::known(semantics.type_id),
            SemanticLookup::Missing
            | SemanticLookup::Ambiguous
            | SemanticLookup::InvalidAstSubscript => {}
        }
        match expr {
            Expression::Terminal { terminal_type, .. } => {
                Self::terminal_value_type(terminal_type, type_table)
            }
            Expression::ComponentReference(cr) => Self::from_optional_identity(
                self.infer_component_ref_type(cr, type_table),
                "component reference type is owned by name resolution",
            ),
            Expression::FunctionCall {
                comp,
                is_partial_application: true,
                ..
            } => self.infer_partial_application_type(comp, type_table),
            Expression::FunctionCall {
                comp,
                args,
                is_partial_application: false,
                ..
            } => self.infer_function_call_result_type(comp, args, type_table),
            Expression::DerivativeCall { args, .. } => {
                self.infer_builtin_result_type(rumoca_core::BuiltinFunction::Der, args, type_table)
            }
            Expression::Unary { op, rhs, .. } => match op {
                rumoca_core::OpUnary::Not => ExpressionType::known(type_table.boolean()),
                rumoca_core::OpUnary::Minus
                | rumoca_core::OpUnary::Plus
                | rumoca_core::OpUnary::DotMinus
                | rumoca_core::OpUnary::DotPlus => self.infer_expression_type(rhs, type_table),
                rumoca_core::OpUnary::Empty => {
                    ExpressionType::Unknown("empty unary operator has no result type")
                }
            },
            Expression::Binary { op, lhs, rhs, .. } => {
                self.infer_binary_expression_type(op, lhs, rhs, type_table)
            }
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.infer_if_expression_type(branches, else_branch, type_table),
            Expression::FieldAccess { base, field, .. } => Self::from_optional_identity(
                self.infer_field_access_type(base, field, type_table),
                "field access owner type is owned by name resolution",
            ),
            Expression::Parenthesized { inner, .. } => {
                self.infer_expression_type(inner, type_table)
            }
            // MLS §10.4 / ARR-020: every argument of an array constructor must
            // be type compatible, and the constructor's type is the maximally
            // expanded type of *all* of them. Element compatibility is a type
            // property, not a shape property; no shape check inspects it.
            Expression::Array { elements, .. } => {
                self.infer_array_constructor_type(elements, type_table)
            }
            // A tuple is an output-expression-list, not a value. It carries one
            // type per position, so it has no single value type; the
            // output-list checker compares those positions.
            Expression::Tuple { .. } => ExpressionType::Invalid(TypeErrorReason::MultiValue(
                MultiValueForm::OutputExpressionList,
            )),
            Expression::Range {
                start, step, end, ..
            } => self.infer_range_element_type(start, step.as_deref(), end, type_table),
            Expression::NamedArgument { value, .. }
            | Expression::Modification {
                value: Some(value), ..
            }
            | Expression::ArrayIndex { base: value, .. } => {
                self.infer_expression_type(value, type_table)
            }
            Expression::Modification { value: None, .. } => {
                ExpressionType::Unknown("value-less modification has no value")
            }
            Expression::ArrayComprehension { expr, .. } => {
                self.infer_expression_type(expr, type_table)
            }
            Expression::Empty { .. } => ExpressionType::Unknown("empty expression has no value"),
            Expression::ClassModification { .. } => {
                ExpressionType::Unknown("class modification is not a value expression")
            }
        }
    }

    fn terminal_value_type(
        terminal_type: &rumoca_ir_ast::TerminalType,
        type_table: &TypeTable,
    ) -> ExpressionType {
        match terminal_type {
            rumoca_ir_ast::TerminalType::UnsignedReal => ExpressionType::known(type_table.real()),
            rumoca_ir_ast::TerminalType::UnsignedInteger => {
                ExpressionType::known(type_table.integer())
            }
            rumoca_ir_ast::TerminalType::Bool => ExpressionType::known(type_table.boolean()),
            rumoca_ir_ast::TerminalType::String => ExpressionType::known(type_table.string()),
            _ => ExpressionType::Unknown("terminal token has no value type"),
        }
    }

    /// MLS §12.4.2: a partially applied function has the callee's function type.
    fn infer_partial_application_type(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) -> ExpressionType {
        let dotted_name = Self::component_ref_name(comp);
        let type_id = self.resolve_type_name(&dotted_name, comp.root_def_id(), type_table);
        Self::from_optional_identity(
            matches!(
                type_table.get(type_id),
                Some(Type::Class(class_type)) if class_type.kind == ClassKind::Function
            )
            .then_some(type_id),
            "partial application target is not a resolved function class",
        )
    }

    /// MLS §3.6.5 / EXPR-017: every branch participates.
    ///
    /// Branch incompatibility is diagnosed by
    /// `check_if_expression_branch_types`, so this composition only records the
    /// outcome and never emits a second time.
    fn infer_if_expression_type(
        &self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        type_table: &TypeTable,
    ) -> ExpressionType {
        let mut result = self.infer_expression_type(else_branch, type_table);
        for (_, value) in branches {
            let branch = self.infer_expression_type(value, type_table);
            result = self.promote_value_types(
                result,
                branch,
                ValueCompositionContext::ConditionalBranches,
                type_table,
            );
        }
        result
    }

    /// MLS §6.7 / §10.4 / §10.4.3: report an array constructor or a range
    /// whose own positions are not type compatible.
    ///
    /// Only a node that *composes* the positions can mint this error, and only
    /// when none of its own positions is already invalid, so the innermost
    /// offending constructor reports exactly once and no enclosing expression
    /// re-proves it. Every other `Invalid` reason names a different owner and
    /// is deliberately silent here.
    pub(in crate::typechecker) fn check_expression_type_validity(
        &mut self,
        expression: &Expression,
        type_table: &TypeTable,
    ) {
        let ExpressionType::Invalid(reason) = self.infer_expression_type(expression, type_table)
        else {
            return;
        };
        // `reported` is `Some` only for the compositions this phase owns; the
        // renderer below cannot even name the delegated ones.
        let Some(error) = reason.reported() else {
            return;
        };
        let composed = match expression {
            Expression::Array { elements, .. } => elements.iter().collect::<Vec<_>>(),
            Expression::Range {
                start, step, end, ..
            } => std::iter::once(&**start)
                .chain(step.as_deref())
                .chain(std::iter::once(&**end))
                .collect(),
            // Some inner constructor produced this; that node owns the report.
            _ => return,
        };
        if composed.into_iter().any(|position| {
            matches!(
                self.infer_expression_type(position, type_table),
                ExpressionType::Invalid(_)
            )
        }) {
            return;
        }
        let message = Self::describe_type_error(&error, type_table);
        let Some(location) = expression.get_location() else {
            return;
        };
        let Some(span) = self.diagnostic_location_span(location, "expression type validity") else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET002",
            message,
            "incompatible element type",
            span,
        ));
    }

    /// Render an error this phase owns.
    ///
    /// `ReportedTypeError` cannot carry a delegated composition or an
    /// unresolved identity, so every arm below is reachable and every operand
    /// formats as a Modelica type name.
    fn describe_type_error(error: &ReportedTypeError, type_table: &TypeTable) -> String {
        let composition = match error.composition {
            ReportedComposition::ArrayConstructor => "array constructor elements",
            ReportedComposition::RangeBounds => "range bounds",
        };
        let relation = match error.kind {
            IncompatibilityKind::ValueTypes => "are not type compatible",
            IncompatibilityKind::EnumerationIdentity => "use two different enumeration types",
        };
        format!(
            "{composition} {relation}: `{}` and `{}` (MLS §6.7)",
            Self::format_type_name(type_table, error.left.get()),
            Self::format_type_name(type_table, error.right.get())
        )
    }

    /// Adapt a helper that still answers with a bare identity. The `None` case
    /// is an abstention with a named owner, never an ill-typedness.
    fn from_optional_identity(type_id: Option<TypeId>, reason: &'static str) -> ExpressionType {
        match type_id {
            Some(type_id) => ExpressionType::known(type_id),
            None => ExpressionType::Unknown(reason),
        }
    }

    /// The value type bound by a lexically active `for` iterator, if this
    /// reference names one.
    ///
    /// The binder type comes from the iterator's checked range domain, so an
    /// iterator over a Real range binds Real and an iterator over an
    /// enumeration binds that exact enumeration.
    pub(crate) fn iterator_binder_type(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
    ) -> Option<ExpressionType> {
        let [part] = comp.parts.as_slice() else {
            return None;
        };
        if part.subs.iter().flatten().next().is_some() {
            return None;
        }
        self.current_iterator_binders
            .iter()
            .rev()
            .find(|(name, _)| name.as_str() == part.ident.text.as_ref())
            .map(|(_, binder)| binder.clone())
    }

    /// MLS §10.4: fold every position of an array constructor.
    fn infer_array_constructor_type(
        &self,
        elements: &[Expression],
        type_table: &TypeTable,
    ) -> ExpressionType {
        let Some((first, rest)) = elements.split_first() else {
            // SPEC_0022 ARR-006: `{}` is not defined.
            return ExpressionType::Invalid(TypeErrorReason::EmptyArrayConstructor);
        };
        let mut result = self.infer_expression_type(first, type_table);
        for element in rest {
            let position = self.infer_expression_type(element, type_table);
            result = self.promote_value_types(
                result,
                position,
                ValueCompositionContext::ArrayConstructor,
                type_table,
            );
        }
        result
    }

    /// MLS §10.4.3: `j : d : k` is a Real vector if any of `j`, `d` or `k` is
    /// Real, so the element type is the promotion of every bound present.
    fn infer_range_element_type(
        &self,
        start: &Expression,
        step: Option<&Expression>,
        end: &Expression,
        type_table: &TypeTable,
    ) -> ExpressionType {
        let mut result = self.infer_expression_type(start, type_table);
        if let Some(step) = step {
            let step = self.infer_expression_type(step, type_table);
            result = self.promote_value_types(
                result,
                step,
                ValueCompositionContext::RangeBounds,
                type_table,
            );
        }
        let end = self.infer_expression_type(end, type_table);
        self.promote_value_types(
            result,
            end,
            ValueCompositionContext::RangeBounds,
            type_table,
        )
    }

    /// The declared output types of a user function, in declaration order.
    ///
    /// Every output position is produced; nothing selects the first one.
    pub(in crate::typechecker) fn user_function_output_types(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) -> Option<Vec<TypeId>> {
        if let Some(signature) = comp
            .root_def_id()
            .and_then(|def_id| self.function_signatures.get(&def_id))
        {
            return Some(
                signature
                    .outputs
                    .iter()
                    .map(|(_, output)| {
                        self.resolve_function_signature_component_type(comp, output, type_table)
                    })
                    .collect(),
            );
        }
        let dotted_name = Self::component_ref_name(comp);
        let function = self.user_function_definition(comp, &dotted_name)?;
        Some(
            function
                .components
                .values()
                .filter(|component| {
                    matches!(component.causality, rumoca_core::Causality::Output(_))
                })
                .map(|output| {
                    self.resolve_type_name(
                        &output.type_name.to_string(),
                        output.type_def_id,
                        type_table,
                    )
                })
                .collect(),
        )
    }

    pub(crate) fn infer_function_call_result_type(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) -> ExpressionType {
        let dotted_name = Self::component_ref_name(comp);
        let Some(last) = comp.parts.last() else {
            return ExpressionType::Unknown("call has no callee name");
        };
        let leaf = last.ident.text.as_ref();
        if leaf == "String" {
            return ExpressionType::known(type_table.string());
        }
        // MLS §12.3 `pure(f(…))` wraps one call to bypass purity checking of
        // that callee; it changes nothing else, so the wrapper has exactly the
        // type of what it wraps.
        if leaf == rumoca_core::PURITY_WRAPPER && comp.parts.len() == 1 {
            let Some(wrapped) = args.first() else {
                return ExpressionType::Unknown("purity wrapper has no wrapped call");
            };
            return self.infer_expression_type(wrapped, type_table);
        }
        if let Some(function) = self.resolved_builtin_function(comp) {
            return self.infer_builtin_result_type(function, args, type_table);
        }
        if comp.root_def_id().is_some()
            && self.user_function_definition(comp, &dotted_name).is_some()
        {
            return self.infer_user_function_output_type(comp, type_table);
        }

        // Record constructors use call syntax (`Payload(...)`) but semantically
        // evaluate to the record type. Resolve through the type table so
        // equation compatibility checks can reject mismatched record identities.
        let type_id = self.resolve_type_name(&dotted_name, comp.root_def_id(), type_table);
        if type_id.is_unknown() {
            return self.infer_user_function_output_type(comp, type_table);
        }
        match type_table.get(type_id) {
            Some(Type::Builtin(rumoca_ir_ast::BuiltinType::Clock)) if leaf == "Clock" => {
                ExpressionType::known(type_id)
            }
            Some(Type::Class(class_ty)) if class_ty.kind == ClassKind::Record => {
                ExpressionType::known(type_id)
            }
            Some(Type::Alias(_)) | Some(Type::Enumeration(_)) => ExpressionType::known(type_id),
            Some(Type::Class(class_ty)) if class_ty.kind == ClassKind::Function => {
                self.infer_user_function_output_type(comp, type_table)
            }
            // MLS §12.4.1: a call in a value position is a single value only
            // when the callee declares exactly one output. Every declared
            // output is counted; none is selected by position.
            Some(Type::Function(function)) => Self::sole_output_type(
                function
                    .outputs
                    .iter()
                    .map(|(_, ty)| *ty)
                    .collect::<Vec<_>>(),
            ),
            _ => self.infer_user_function_output_type(comp, type_table),
        }
    }

    /// MLS §12.4.1: the value type of a call is the callee's only output.
    ///
    /// More than one output is not an ill-typed callee; it is a call that
    /// belongs in an output-expression-list, so it carries no single value
    /// type and the output-list checker owns it.
    fn sole_output_type(outputs: Vec<TypeId>) -> ExpressionType {
        match outputs.as_slice() {
            [] => ExpressionType::Unknown("callee declares no output"),
            [single] => ExpressionType::known(*single),
            _ => ExpressionType::Invalid(TypeErrorReason::MultiValue(
                MultiValueForm::MultiOutputCall,
            )),
        }
    }

    fn infer_binary_expression_type(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) -> ExpressionType {
        use rumoca_core::OpBinary;
        match op {
            OpBinary::Eq
            | OpBinary::Neq
            | OpBinary::Lt
            | OpBinary::Le
            | OpBinary::Gt
            | OpBinary::Ge
            | OpBinary::And
            | OpBinary::Or => ExpressionType::known(type_table.boolean()),
            OpBinary::Div | OpBinary::DivElem => {
                let lhs = self.infer_expression_type(lhs, type_table);
                let rhs = self.infer_expression_type(rhs, type_table);
                let common = self.promote_value_types(
                    lhs,
                    rhs,
                    ValueCompositionContext::BinaryOperands,
                    type_table,
                );
                let common = match common {
                    ExpressionType::Known(common) => common.get(),
                    unknown_or_invalid => return unknown_or_invalid,
                };
                let root = self.resolve_type_root(common);
                match type_table.get(root) {
                    // MLS §10.6.5 / SPEC_0022 TYPE-034: division of numeric
                    // operands is always Real, even Integer / Integer.
                    Some(Type::Builtin(
                        rumoca_ir_ast::BuiltinType::Real | rumoca_ir_ast::BuiltinType::Integer,
                    )) => ExpressionType::known(type_table.real()),
                    // MLS §14: an operator record's `'/'` yields that record's
                    // type. Whether the record actually declares `'/'` is
                    // operand legality, owned by `require_numeric_expression`.
                    Some(Type::Class(_)) => ExpressionType::known(common),
                    // Boolean, String and enumeration operands were already
                    // rejected with ET002 by `require_numeric_expression`, the
                    // single owner of operand legality for `/`. Re-deriving a
                    // type here would re-prove an issued fact, so this abstains
                    // to that owner instead of inventing the operand type.
                    _ => ExpressionType::Unknown(
                        "non-numeric division operand is owned by the operator operand check",
                    ),
                }
            }
            OpBinary::Add
            | OpBinary::Sub
            | OpBinary::Mul
            | OpBinary::Exp
            | OpBinary::ExpElem
            | OpBinary::AddElem
            | OpBinary::SubElem
            | OpBinary::MulElem
            | OpBinary::Assign => {
                let lhs = self.infer_expression_type(lhs, type_table);
                let rhs = self.infer_expression_type(rhs, type_table);
                self.promote_value_types(
                    lhs,
                    rhs,
                    ValueCompositionContext::BinaryOperands,
                    type_table,
                )
            }
            OpBinary::Empty => ExpressionType::Unknown("empty binary operator has no result type"),
        }
    }

    /// The single promotion rule for two composed value types.
    ///
    /// The table is total: every input pair yields a decision, and no row falls
    /// back to a plausible type. Numeric widening is applied only for the exact
    /// operand class MLS licenses it for; Boolean, String and enumeration are
    /// decided by identity and never folded into the numeric lattice.
    pub(in crate::typechecker) fn promote_value_types(
        &self,
        lhs: ExpressionType,
        rhs: ExpressionType,
        context: ValueCompositionContext,
        type_table: &TypeTable,
    ) -> ExpressionType {
        // Ill-typedness wins over abstention: an error already found must not
        // be downgraded to "not inferable". Abstention is contagious in turn;
        // an error is never invented out of ignorance.
        let (lhs, rhs) = match (lhs, rhs) {
            (ExpressionType::Invalid(reason), _) | (_, ExpressionType::Invalid(reason)) => {
                return ExpressionType::Invalid(reason);
            }
            (ExpressionType::Unknown(reason), _) | (_, ExpressionType::Unknown(reason)) => {
                return ExpressionType::Unknown(reason);
            }
            (ExpressionType::Known(lhs), ExpressionType::Known(rhs)) => (lhs, rhs),
        };
        let lhs_root = self.resolve_type_root(lhs.get());
        let rhs_root = self.resolve_type_root(rhs.get());
        // Identity first. This is what keeps `Colors` distinct from `Sizes`
        // while keeping `Colors` compatible with itself, and it covers Boolean,
        // String, every enumeration and every record without a domain enum.
        if lhs_root == rhs_root {
            return ExpressionType::Known(lhs);
        }
        if Self::is_unresolved_alias_root(type_table, lhs_root)
            || Self::is_unresolved_alias_root(type_table, rhs_root)
        {
            return ExpressionType::Unknown("composed operand type alias is unresolved");
        }
        use rumoca_ir_ast::BuiltinType;
        match (type_table.get(lhs_root), type_table.get(rhs_root)) {
            (
                Some(Type::Builtin(BuiltinType::Real | BuiltinType::Integer)),
                Some(Type::Builtin(BuiltinType::Real | BuiltinType::Integer)),
            ) => {
                // MLS §6.7 / §10.6.13 / SPEC_0022 TYPE-033, ARR-009: the
                // maximally expanded numeric type. This is the only licensed
                // implicit conversion in the scalar domain.
                if lhs_root == type_table.real() || rhs_root == type_table.real() {
                    ExpressionType::known(type_table.real())
                } else {
                    ExpressionType::known(type_table.integer())
                }
            }
            // MLS §6.7: enumeration expressions are compatible only when they
            // are the same enumeration. The roots already differ here, so
            // spelling cannot rescue them.
            (Some(Type::Enumeration(_)), Some(Type::Enumeration(_))) => {
                ExpressionType::Invalid(TypeErrorReason::Incompatible {
                    context,
                    kind: IncompatibilityKind::EnumerationIdentity,
                    left: lhs,
                    right: rhs,
                })
            }
            // MLS §3.7.1: `Integer()` and `EnumTypeName()` are the only bridges
            // between an enumeration and the numeric domain; there is no
            // implicit conversion either way.
            (Some(Type::Enumeration(_)), Some(Type::Builtin(_)))
            | (Some(Type::Builtin(_)), Some(Type::Enumeration(_)))
            | (Some(Type::Builtin(_)), Some(Type::Builtin(_))) => {
                ExpressionType::Invalid(TypeErrorReason::Incompatible {
                    context,
                    kind: IncompatibilityKind::ValueTypes,
                    left: lhs,
                    right: rhs,
                })
            }
            // Record and operator-record composition identity has no owner in
            // this phase yet; abstain rather than mint a rejection this cut
            // ships no acceptance contract for.
            _ => ExpressionType::Unknown(
                "composed operand types have no scalar-domain compatibility owner",
            ),
        }
    }

    fn infer_builtin_result_type(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
        type_table: &TypeTable,
    ) -> ExpressionType {
        use rumoca_core::BuiltinFunction;
        match function {
            BuiltinFunction::Initial
            | BuiltinFunction::Terminal
            | BuiltinFunction::Edge
            | BuiltinFunction::Change => ExpressionType::known(type_table.boolean()),
            BuiltinFunction::Integer
            | BuiltinFunction::Ndims
            | BuiltinFunction::Size
            | BuiltinFunction::Identity => ExpressionType::known(type_table.integer()),
            BuiltinFunction::Sqrt
            | BuiltinFunction::Floor
            | BuiltinFunction::Ceil
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
            | BuiltinFunction::Log10
            | BuiltinFunction::Zeros
            | BuiltinFunction::Ones
            | BuiltinFunction::Linspace
            | BuiltinFunction::Interval => ExpressionType::known(type_table.real()),
            BuiltinFunction::Clock => Self::from_optional_identity(
                type_table.lookup("Clock"),
                "the Clock type is not present in this type table",
            ),
            BuiltinFunction::Smooth => self.builtin_argument_result_type(
                args.get(1),
                "smooth() has no value argument",
                type_table,
            ),
            BuiltinFunction::Fill => self.builtin_argument_result_type(
                args.first(),
                "fill() has no value argument",
                type_table,
            ),
            BuiltinFunction::Cross => self.infer_cross_result_type(args, type_table),
            BuiltinFunction::Sample => self.infer_sample_result_type(args, type_table),
            BuiltinFunction::Reinit => {
                ExpressionType::Unknown("reinit() is a statement, not a value")
            }
            BuiltinFunction::Homotopy
            | BuiltinFunction::SemiLinear
            | BuiltinFunction::Der
            | BuiltinFunction::Pre
            | BuiltinFunction::Abs
            | BuiltinFunction::Sign
            | BuiltinFunction::Min
            | BuiltinFunction::Max
            | BuiltinFunction::Div
            | BuiltinFunction::Mod
            | BuiltinFunction::Rem
            | BuiltinFunction::NoEvent
            | BuiltinFunction::Delay
            | BuiltinFunction::Sum
            | BuiltinFunction::Product
            | BuiltinFunction::Scalar
            | BuiltinFunction::Vector
            | BuiltinFunction::Matrix
            | BuiltinFunction::Diagonal
            | BuiltinFunction::Transpose
            | BuiltinFunction::OuterProduct
            | BuiltinFunction::Symmetric
            | BuiltinFunction::Skew
            | BuiltinFunction::Cat
            | BuiltinFunction::Hold
            | BuiltinFunction::Previous
            | BuiltinFunction::SubSample
            | BuiltinFunction::SuperSample
            | BuiltinFunction::ShiftSample
            | BuiltinFunction::BackSample
            | BuiltinFunction::NoClock => self.builtin_argument_result_type(
                args.first(),
                "builtin call has no value argument",
                type_table,
            ),
        }
    }

    /// MLS §10.3.5: `cross(x, y)` has the promoted numeric type of its two
    /// arguments. Argument legality is owned by the builtin argument checks.
    fn infer_cross_result_type(
        &self,
        args: &[Expression],
        type_table: &TypeTable,
    ) -> ExpressionType {
        let [lhs, rhs] = args else {
            return ExpressionType::Unknown("cross() does not have two arguments");
        };
        let lhs = self.infer_expression_type(lhs, type_table);
        let rhs = self.infer_expression_type(rhs, type_table);
        let common = self.promote_value_types(
            lhs,
            rhs,
            ValueCompositionContext::BinaryOperands,
            type_table,
        );
        let identity = match common {
            ExpressionType::Known(identity) => identity,
            unknown_or_invalid => return unknown_or_invalid,
        };
        if Self::is_numeric_type(self.resolve_type_root(identity.get()), type_table) {
            ExpressionType::Known(identity)
        } else {
            ExpressionType::Unknown("cross() argument type is owned by its argument check")
        }
    }

    /// MLS §16.5.1: `sample(u, c)` has the type of `u` when `c` is a clock, and
    /// is a Boolean sampling trigger otherwise.
    fn infer_sample_result_type(
        &self,
        args: &[Expression],
        type_table: &TypeTable,
    ) -> ExpressionType {
        let Some(sampled_value) = args.first() else {
            return ExpressionType::Unknown("sample() has no value argument");
        };
        let clocked = args.len() == 1
            || args.get(1).is_some_and(|clock| {
                matches!(
                    self.infer_expression_type(clock, type_table)
                        .value_identity()
                        .map(|ty| self.resolve_type_root(ty))
                        .and_then(|ty| type_table.get(ty)),
                    Some(Type::Builtin(rumoca_ir_ast::BuiltinType::Clock))
                )
            });
        if clocked {
            self.infer_expression_type(sampled_value, type_table)
        } else {
            ExpressionType::known(type_table.boolean())
        }
    }

    /// A builtin whose result type is the type of one of its arguments.
    fn builtin_argument_result_type(
        &self,
        argument: Option<&Expression>,
        missing: &'static str,
        type_table: &TypeTable,
    ) -> ExpressionType {
        match argument {
            Some(argument) => self.infer_expression_type(argument, type_table),
            None => ExpressionType::Unknown(missing),
        }
    }

    /// MLS §12.4.1: the value type of a user-function call.
    ///
    /// Every declared output position is produced and then counted; the result
    /// is a value type only when there is exactly one. Shape stays a separate
    /// property, so an array-typed sole output keeps its scalar identity here.
    fn infer_user_function_output_type(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) -> ExpressionType {
        match self.user_function_output_types(comp, type_table) {
            Some(outputs) => Self::sole_output_type(outputs),
            None => ExpressionType::Unknown("callee has no resolved function definition"),
        }
    }

    pub(super) fn resolve_function_signature_component_type(
        &self,
        call: &rumoca_ir_ast::ComponentReference,
        component: &Component,
        type_table: &TypeTable,
    ) -> TypeId {
        let specialized_def_id = (component.type_name.name.len() == 1)
            .then_some(component.type_def_id)
            .flatten()
            .and_then(|def_id| {
                self.current_call_type_overrides
                    .specialized_type(call, def_id)
            });
        self.resolve_type_name(
            &component.type_name.to_string(),
            specialized_def_id.or(component.type_def_id),
            type_table,
        )
    }

    pub(in crate::typechecker) fn user_function_definition<'a>(
        &'a self,
        comp: &rumoca_ir_ast::ComponentReference,
        dotted_name: &str,
    ) -> Option<&'a ClassDef> {
        if let Some(def_id) = comp.root_def_id() {
            // A resolved DefId is authoritative. The evaluator's function map
            // also contains scope-local import aliases, so a name-only fallback
            // here can bind a predefined call such as `sum(...)` to an
            // unrelated imported `ComplexMath.sum` from another class.
            let canonical_name = self.def_qualified_names.get(&def_id)?;
            return self
                .eval_ctx
                .functions
                .get(canonical_name)
                .filter(|function| function.def_id == Some(def_id));
        }

        self.eval_ctx.functions.get(dotted_name).or_else(|| {
            comp.parts
                .last()
                .and_then(|part| self.eval_ctx.functions.get(part.ident.text.as_ref()))
        })
    }

    pub(in crate::typechecker) fn resolved_builtin_function(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
    ) -> Option<rumoca_core::BuiltinFunction> {
        let dotted_name = Self::component_ref_name(comp);
        if let Some(def_id) = comp.root_def_id()
            && (self.function_signatures.contains_key(&def_id)
                || self.user_function_definition(comp, &dotted_name).is_some())
        {
            return None;
        }
        if let Some(function) = comp
            .target_def_id()
            .and_then(|identity| self.predefined_intrinsics.get(&identity))
        {
            return Some(*function);
        }
        let leaf = comp.parts.last()?.ident.text.as_ref();
        let resolves_to_predefined = comp
            .root_def_id()
            .is_none_or(|def_id| !self.type_ids_by_def_id.contains_key(&def_id));
        resolves_to_predefined
            .then_some(leaf)
            .and_then(rumoca_core::BuiltinFunction::from_name)
    }
}
