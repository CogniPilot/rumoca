//! Expression and function-call type inference for the late typecheck pass:
//! the value type of an expression, builtin and user-function result types,
//! and the function definitions those results are read from.

use super::*;

impl TypeChecker {
    pub(crate) fn infer_expression_type(
        &self,
        expr: &Expression,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        if let Expression::ComponentReference(comp) = expr
            && self.is_integer_iterator_reference(comp)
        {
            return Some(type_table.integer());
        }
        match self.lookup_instance_expression(expr) {
            SemanticLookup::Found(semantics) => return Some(semantics.type_id),
            SemanticLookup::Missing | SemanticLookup::Ambiguous => {}
        }
        match expr {
            Expression::Terminal { terminal_type, .. } => match terminal_type {
                rumoca_ir_ast::TerminalType::UnsignedReal => Some(type_table.real()),
                rumoca_ir_ast::TerminalType::UnsignedInteger => Some(type_table.integer()),
                rumoca_ir_ast::TerminalType::Bool => Some(type_table.boolean()),
                rumoca_ir_ast::TerminalType::String => Some(type_table.string()),
                _ => None,
            },
            Expression::ComponentReference(cr) => self.infer_component_ref_type(cr, type_table),
            Expression::FunctionCall {
                comp,
                is_partial_application: true,
                ..
            } => {
                let dotted_name = Self::component_ref_name(comp);
                let type_id = self.resolve_type_name(&dotted_name, comp.root_def_id(), type_table);
                matches!(
                    type_table.get(type_id),
                    Some(Type::Class(class_type)) if class_type.kind == ClassKind::Function
                )
                .then_some(type_id)
            }
            Expression::FunctionCall {
                comp,
                args,
                is_partial_application: false,
                ..
            } => self.infer_function_call_result_type(comp, args, type_table),
            Expression::Unary { op, rhs, .. } => match op {
                rumoca_core::OpUnary::Not => Some(type_table.boolean()),
                rumoca_core::OpUnary::Minus
                | rumoca_core::OpUnary::Plus
                | rumoca_core::OpUnary::DotMinus
                | rumoca_core::OpUnary::DotPlus => self.infer_expression_type(rhs, type_table),
                rumoca_core::OpUnary::Empty => None,
            },
            Expression::Binary { op, lhs, rhs, .. } => {
                self.infer_binary_expression_type(op, lhs, rhs, type_table)
            }
            Expression::If {
                branches,
                else_branch,
                ..
            } => {
                let mut result = self.infer_expression_type(else_branch, type_table)?;
                for (_, value) in branches {
                    let branch = self.infer_expression_type(value, type_table)?;
                    result = self.common_value_type(result, branch, type_table)?;
                }
                Some(result)
            }
            Expression::FieldAccess { base, field, .. } => {
                self.infer_field_access_type(base, field, type_table)
            }
            Expression::Parenthesized { inner, .. } => {
                self.infer_expression_type(inner, type_table)
            }
            // An array's value type is its element type (uniformity of the
            // elements is the shape check's concern).
            Expression::Array { elements, .. } => elements
                .first()
                .and_then(|element| self.infer_expression_type(element, type_table)),
            Expression::Tuple { elements, .. } => elements
                .first()
                .and_then(|element| self.infer_expression_type(element, type_table)),
            Expression::Range { start, .. } => self.infer_expression_type(start, type_table),
            Expression::NamedArgument { value, .. }
            | Expression::Modification { value, .. }
            | Expression::ArrayIndex { base: value, .. } => {
                self.infer_expression_type(value, type_table)
            }
            Expression::ArrayComprehension { expr, .. } => {
                self.infer_expression_type(expr, type_table)
            }
            Expression::Empty { .. } | Expression::ClassModification { .. } => None,
        }
    }

    pub(crate) fn is_integer_iterator_reference(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
    ) -> bool {
        comp.parts.len() == 1
            && comp.parts[0].subs.iter().flatten().next().is_none()
            && self
                .current_integer_iterators
                .iter()
                .rev()
                .any(|name| name.as_str() == comp.parts[0].ident.text.as_ref())
    }

    pub(crate) fn infer_function_call_result_type(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
        args: &[Expression],
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        let dotted_name = Self::component_ref_name(comp);
        let leaf = comp.parts.last()?.ident.text.as_ref();
        if leaf == "String" {
            return Some(type_table.string());
        }
        // MLS §12.3 `pure(f(…))` wraps one call to bypass purity checking of
        // that callee; it changes nothing else, so the wrapper has exactly the
        // type of what it wraps.
        if leaf == rumoca_core::PURITY_WRAPPER && comp.parts.len() == 1 {
            return self.infer_expression_type(args.first()?, type_table);
        }
        if comp.root_def_id().is_some()
            && self.user_function_definition(comp, &dotted_name).is_some()
        {
            return self.infer_user_function_output_type(comp, &dotted_name, type_table);
        }
        if let Some(function) = comp
            .target_def_id()
            .and_then(|identity| self.predefined_intrinsics.get(&identity))
        {
            return self.infer_builtin_result_type(*function, args, type_table);
        }
        let resolves_to_predefined = comp
            .root_def_id()
            .is_none_or(|def_id| !self.type_ids_by_def_id.contains_key(&def_id));
        if resolves_to_predefined
            && let Some(function) = rumoca_core::BuiltinFunction::from_name(leaf)
        {
            // An unknown builtin result remains unknown. Falling through to
            // leaf-name user-function lookup can capture an unrelated library
            // overload such as ComplexMath.sum.
            return self.infer_builtin_result_type(function, args, type_table);
        }

        // Record constructors use call syntax (`Payload(...)`) but semantically
        // evaluate to the record type. Resolve through the type table so
        // equation compatibility checks can reject mismatched record identities.
        let type_id = self.resolve_type_name(&dotted_name, comp.root_def_id(), type_table);
        if type_id.is_unknown() {
            return self.infer_user_function_output_type(comp, &dotted_name, type_table);
        }
        match type_table.get(type_id) {
            Some(Type::Builtin(rumoca_ir_ast::BuiltinType::Clock)) if leaf == "Clock" => {
                Some(type_id)
            }
            Some(Type::Class(class_ty)) if class_ty.kind == ClassKind::Record => Some(type_id),
            Some(Type::Alias(_)) | Some(Type::Enumeration(_)) => Some(type_id),
            Some(Type::Class(class_ty)) if class_ty.kind == ClassKind::Function => {
                self.infer_user_function_output_type(comp, &dotted_name, type_table)
            }
            Some(Type::Function(function)) => function.outputs.first().map(|(_, ty)| *ty),
            _ => self.infer_user_function_output_type(comp, &dotted_name, type_table),
        }
    }

    fn infer_binary_expression_type(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        use rumoca_core::OpBinary;
        match op {
            OpBinary::Eq
            | OpBinary::Neq
            | OpBinary::Lt
            | OpBinary::Le
            | OpBinary::Gt
            | OpBinary::Ge
            | OpBinary::And
            | OpBinary::Or => Some(type_table.boolean()),
            OpBinary::Div | OpBinary::DivElem => {
                let lhs = self.infer_expression_type(lhs, type_table)?;
                let rhs = self.infer_expression_type(rhs, type_table)?;
                let common = self.common_value_type(lhs, rhs, type_table)?;
                let root = self.resolve_type_root(type_table, common);
                matches!(
                    type_table.get(root),
                    Some(Type::Builtin(
                        rumoca_ir_ast::BuiltinType::Real | rumoca_ir_ast::BuiltinType::Integer
                    ))
                )
                .then_some(type_table.real())
                .or(Some(common))
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
                let lhs = self.infer_expression_type(lhs, type_table)?;
                let rhs = self.infer_expression_type(rhs, type_table)?;
                self.common_value_type(lhs, rhs, type_table)
            }
            OpBinary::Empty => None,
        }
    }

    fn common_value_type(
        &self,
        lhs: TypeId,
        rhs: TypeId,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        let lhs_root = self.resolve_type_root(type_table, lhs);
        let rhs_root = self.resolve_type_root(type_table, rhs);
        if lhs_root == rhs_root {
            return Some(lhs);
        }
        let numeric = |ty| {
            matches!(
                type_table.get(ty),
                Some(Type::Builtin(
                    rumoca_ir_ast::BuiltinType::Real | rumoca_ir_ast::BuiltinType::Integer
                ))
            )
        };
        if numeric(lhs_root) && numeric(rhs_root) {
            if lhs_root == type_table.real() || rhs_root == type_table.real() {
                Some(type_table.real())
            } else {
                Some(type_table.integer())
            }
        } else {
            None
        }
    }

    fn infer_builtin_result_type(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        use rumoca_core::BuiltinFunction;
        match function {
            BuiltinFunction::Initial
            | BuiltinFunction::Terminal
            | BuiltinFunction::Edge
            | BuiltinFunction::Change => Some(type_table.boolean()),
            BuiltinFunction::Integer | BuiltinFunction::Ndims | BuiltinFunction::Size => {
                Some(type_table.integer())
            }
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
            | BuiltinFunction::Identity
            | BuiltinFunction::Linspace
            | BuiltinFunction::Interval => Some(type_table.real()),
            BuiltinFunction::Clock => type_table.lookup("Clock"),
            BuiltinFunction::Smooth => args
                .get(1)
                .and_then(|arg| self.infer_expression_type(arg, type_table)),
            BuiltinFunction::Fill => args
                .first()
                .and_then(|arg| self.infer_expression_type(arg, type_table)),
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
            | BuiltinFunction::Cross
            | BuiltinFunction::Skew
            | BuiltinFunction::Cat
            | BuiltinFunction::Hold
            | BuiltinFunction::Previous
            | BuiltinFunction::SubSample
            | BuiltinFunction::SuperSample
            | BuiltinFunction::ShiftSample
            | BuiltinFunction::BackSample
            | BuiltinFunction::NoClock => args
                .first()
                .and_then(|arg| self.infer_expression_type(arg, type_table)),
            BuiltinFunction::Sample => {
                let sampled_value = args.first()?;
                if args.len() == 1
                    || args.get(1).is_some_and(|clock| {
                        matches!(
                            self.infer_expression_type(clock, type_table)
                                .map(|ty| self.resolve_type_root(type_table, ty))
                                .and_then(|ty| type_table.get(ty)),
                            Some(Type::Builtin(rumoca_ir_ast::BuiltinType::Clock))
                        )
                    })
                {
                    self.infer_expression_type(sampled_value, type_table)
                } else {
                    Some(type_table.boolean())
                }
            }
            BuiltinFunction::Reinit => None,
        }
    }

    fn infer_user_function_output_type(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
        dotted_name: &str,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        if let Some(output) = comp
            .root_def_id()
            .and_then(|def_id| self.function_signatures.get(&def_id))
            .and_then(|signature| signature.outputs.first())
            .map(|(_, output)| output)
        {
            let type_id = self.resolve_function_signature_component_type(comp, output, type_table);
            return (!type_id.is_unknown()).then_some(type_id);
        }
        let function = self.user_function_definition(comp, dotted_name)?;
        // Function result type is the first declared output even when that
        // output is an array. Shape is a separate property; skipping shaped
        // outputs here can accidentally select a later scalar scratch output.
        let output = function
            .components
            .values()
            .find(|component| matches!(component.causality, rumoca_core::Causality::Output(_)))?;
        let name = output.type_name.to_string();
        let type_id = self.resolve_type_name(&name, output.type_def_id, type_table);
        (!type_id.is_unknown()).then_some(type_id)
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
}
