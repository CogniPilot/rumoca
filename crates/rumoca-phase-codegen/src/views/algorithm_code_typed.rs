//! Target-neutral expression shape evidence for checked Algorithm Code.

use std::collections::{HashMap, HashSet};

use rumoca_ir_galec::ast;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub(super) struct TypedBlockView<'a> {
    name: &'a ast::Name,
    interface: &'a [ast::InterfaceVariable],
    compartments: &'a [ast::StateCompartment],
    protected: &'a [ast::ProtectedEntity],
    error_signals: &'a [ast::Identifier],
    protected_functions: &'a [ast::UserFunction],
    startup: TypedMethodView<'a>,
    recalibrate: TypedMethodView<'a>,
    do_step: TypedMethodView<'a>,
    public_functions: &'a [ast::UserFunction],
}

#[derive(Debug, Clone, Serialize)]
struct TypedMethodView<'a> {
    signals: &'a [ast::PredefinedSignal],
    locals: &'a [ast::VariableDeclaration],
    statements: Vec<TypedSpannedStatement<'a>>,
}

#[derive(Debug, Clone, Serialize)]
struct TypedSpannedStatement<'a> {
    node: TypedStatementView<'a>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedStatementView<'a> {
    Assignment {
        target: TypedReferenceView<'a>,
        value: TypedExpressionView<'a>,
    },
    MultiAssignment {
        targets: Vec<TypedReferenceView<'a>>,
        call: TypedCallView<'a>,
    },
    Call(TypedCallView<'a>),
    If(TypedIfStatementView<'a>),
    For(TypedForView<'a>),
    Limit(Vec<TypedLimitTargetView<'a>>),
    Signal(&'a [ast::Identifier]),
}

#[derive(Debug, Clone, Serialize)]
struct TypedIfStatementView<'a> {
    branches: Vec<TypedIfBranchView<'a>>,
    else_body: Option<Vec<TypedSpannedStatement<'a>>>,
}

#[derive(Debug, Clone, Serialize)]
struct TypedIfBranchView<'a> {
    condition: TypedConditionView<'a>,
    body: Vec<TypedSpannedStatement<'a>>,
}

#[derive(Debug, Clone, Serialize)]
struct TypedForView<'a> {
    iterator: &'a Option<ast::Name>,
    start: TypedExpressionView<'a>,
    step: Option<TypedExpressionView<'a>>,
    stop: TypedExpressionView<'a>,
    body: Vec<TypedSpannedStatement<'a>>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedLimitTargetView<'a> {
    SelfState,
    Reference(TypedReferenceView<'a>),
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedConditionView<'a> {
    Expression(TypedExpressionView<'a>),
    SignalCheck(&'a ast::SignalCheck),
}

#[derive(Debug, Clone, Serialize)]
struct TypedCallView<'a> {
    function: &'a ast::Name,
    lifted_base: Option<&'static str>,
    arguments: Vec<TypedExpressionView<'a>>,
}

#[derive(Debug, Clone, Serialize)]
struct TypedReferenceView<'a> {
    rank: usize,
    extents: Option<Vec<usize>>,
    #[serde(flatten)]
    node: TypedReferenceNodeView<'a>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedReferenceNodeView<'a> {
    Local(&'a ast::RefPart),
    State(&'a [ast::RefPart]),
}

#[derive(Debug, Clone, Serialize)]
struct TypedExpressionView<'a> {
    rank: usize,
    extents: Option<Vec<usize>>,
    #[serde(flatten)]
    node: TypedExpressionNodeView<'a>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedExpressionNodeView<'a> {
    Bool(bool),
    Integer(i64),
    Real(f64),
    Ref(TypedReferenceView<'a>),
    Size {
        array: TypedReferenceView<'a>,
        dimension: Box<TypedExpressionView<'a>>,
    },
    Call(TypedCallView<'a>),
    Paren(Box<TypedExpressionView<'a>>),
    If(TypedIfExpressionView<'a>),
    Array(Vec<TypedExpressionView<'a>>),
    Neg(TypedReferenceView<'a>),
    Not(Box<TypedExpressionView<'a>>),
    Binary {
        op: ast::BinaryOp,
        lhs: Box<TypedExpressionView<'a>>,
        rhs: Box<TypedExpressionView<'a>>,
    },
}

#[derive(Debug, Clone, Serialize)]
struct TypedIfExpressionView<'a> {
    branches: Vec<(TypedExpressionView<'a>, TypedExpressionView<'a>)>,
    else_value: Box<TypedExpressionView<'a>>,
}

pub(super) fn block(block: &ast::Block) -> Result<TypedBlockView<'_>, String> {
    let shapes = BlockShapes::new(block);
    Ok(TypedBlockView {
        name: &block.name,
        interface: &block.interface,
        compartments: &block.compartments,
        protected: &block.protected,
        error_signals: &block.error_signals,
        protected_functions: &block.protected_functions,
        startup: shapes.method(&block.startup)?,
        recalibrate: shapes.method(&block.recalibrate)?,
        do_step: shapes.method(&block.do_step)?,
        public_functions: &block.public_functions,
    })
}

struct BlockShapes<'a> {
    state: HashMap<&'a str, &'a ast::VariableDeclaration>,
    compartments: HashMap<&'a str, &'a ast::StateCompartment>,
    functions: HashMap<&'a str, &'a ast::UserFunction>,
}

impl<'a> BlockShapes<'a> {
    fn new(block: &'a ast::Block) -> Self {
        let state = block
            .interface
            .iter()
            .map(|variable| (variable.decl.name.lexeme(), &variable.decl))
            .chain(
                block
                    .protected
                    .iter()
                    .map(|variable| (variable.decl.name.lexeme(), &variable.decl)),
            )
            .collect();
        let compartments = block
            .compartments
            .iter()
            .map(|compartment| (compartment.name.lexeme(), compartment))
            .collect();
        let functions = block
            .protected_functions
            .iter()
            .chain(&block.public_functions)
            .map(|function| (function.name.lexeme(), function))
            .collect();
        Self {
            state,
            compartments,
            functions,
        }
    }

    fn method(&self, method: &'a ast::BlockMethod) -> Result<TypedMethodView<'a>, String> {
        let scope = ScopeShapes::new(self, &method.locals);
        Ok(TypedMethodView {
            signals: &method.signals,
            locals: &method.locals,
            statements: scope.statements(&method.statements)?,
        })
    }

    fn state_reference_shape(&self, parts: &[ast::RefPart]) -> Result<ShapeEvidence, String> {
        let Some(first) = parts.first() else {
            return Err("checked state reference has no parts".to_owned());
        };
        let mut declaration = self
            .state
            .get(first.name.lexeme())
            .copied()
            .ok_or_else(|| {
                format!(
                    "checked state reference `{}` is unresolved",
                    first.name.lexeme()
                )
            })?;
        let mut remaining = reference_shape(declaration, first)?;
        for part in &parts[1..] {
            if remaining.rank != 0 {
                return Err(format!(
                    "checked component array `{}` is not fully subscripted",
                    declaration.name.lexeme()
                ));
            }
            let ast::TypeRef::Compartment(compartment_name) = &declaration.ty else {
                return Err(format!(
                    "checked multipart reference traverses primitive `{}`",
                    declaration.name.lexeme()
                ));
            };
            let compartment = self
                .compartments
                .get(compartment_name.lexeme())
                .copied()
                .ok_or_else(|| {
                    format!(
                        "checked compartment type `{}` is unresolved",
                        compartment_name.lexeme()
                    )
                })?;
            declaration = compartment
                .entities
                .iter()
                .map(|entity| &entity.decl)
                .find(|candidate| candidate.name.lexeme() == part.name.lexeme())
                .ok_or_else(|| {
                    format!(
                        "checked compartment field `{}` is unresolved",
                        part.name.lexeme()
                    )
                })?;
            remaining = reference_shape(declaration, part)?;
        }
        Ok(remaining)
    }

    fn call_shape(
        &self,
        call: &ast::FunctionCall,
        arguments: &[TypedExpressionView<'_>],
    ) -> Result<ShapeEvidence, String> {
        if let Some(function) = self.functions.get(call.function.lexeme()) {
            let output = function
                .parameters
                .iter()
                .find(|parameter| parameter.direction == ast::Direction::Output)
                .ok_or_else(|| {
                    format!(
                        "checked expression call `{}` has no output",
                        call.function.lexeme()
                    )
                })?;
            return Ok(declaration_shape(&output.decl));
        }
        if let Some(base) = rumoca_ir_galec::builtins::find_lifted_base(call.function.lexeme()) {
            let rank = usize::from(call.function.lexeme().ends_with("2D")) + 1;
            let mut argument_shapes = arguments
                .iter()
                .map(ShapeEvidence::of_expression)
                .filter(|shape| shape.rank > 0);
            let shape = argument_shapes.next().ok_or_else(|| {
                format!(
                    "checked lifted call `{}` has no array argument",
                    call.function.lexeme()
                )
            })?;
            if shape.rank != rank {
                return Err(format!(
                    "checked lifted call `{}` has rank {}, expected {rank}",
                    call.function.lexeme(),
                    shape.rank
                ));
            }
            for argument in argument_shapes {
                require_equal_shape(shape.clone(), argument, "checked lifted-call arguments")?;
            }
            if base.outputs.len() == 1 {
                return Ok(shape);
            }
        }
        let builtin = rumoca_ir_galec::builtins::find_builtin(call.function.lexeme())
            .ok_or_else(|| format!("checked call `{}` is unresolved", call.function.lexeme()))?;
        let [output] = builtin.outputs else {
            return Err(format!(
                "checked expression call `{}` does not have one output",
                call.function.lexeme()
            ));
        };
        Ok(ShapeEvidence {
            rank: builtin_rank(output.ty),
            extents: (builtin_rank(output.ty) == 0).then(Vec::new),
        })
    }
}

struct ScopeShapes<'a, 'block> {
    block: &'block BlockShapes<'a>,
    locals: HashMap<&'a str, &'a ast::VariableDeclaration>,
    iterators: HashSet<&'a str>,
}

impl<'a, 'block> ScopeShapes<'a, 'block> {
    fn new(block: &'block BlockShapes<'a>, locals: &'a [ast::VariableDeclaration]) -> Self {
        Self {
            block,
            locals: locals
                .iter()
                .map(|declaration| (declaration.name.lexeme(), declaration))
                .collect(),
            iterators: HashSet::new(),
        }
    }

    fn with_iterator(&self, iterator: Option<&'a ast::Name>) -> Self {
        let mut iterators = self.iterators.clone();
        if let Some(iterator) = iterator {
            iterators.insert(iterator.lexeme());
        }
        Self {
            block: self.block,
            locals: self.locals.clone(),
            iterators,
        }
    }

    fn statements(
        &self,
        statements: &'a [ast::Spanned<ast::Statement>],
    ) -> Result<Vec<TypedSpannedStatement<'a>>, String> {
        statements
            .iter()
            .map(|statement| {
                Ok(TypedSpannedStatement {
                    node: self.statement(&statement.node)?,
                })
            })
            .collect()
    }

    fn statement(&self, statement: &'a ast::Statement) -> Result<TypedStatementView<'a>, String> {
        Ok(match statement {
            ast::Statement::Assignment { target, value } => {
                let target = self.reference(target)?;
                let value = self.expression(value)?;
                require_equal_shape(
                    ShapeEvidence::of_reference(&target),
                    ShapeEvidence::of_expression(&value),
                    "checked assignment",
                )?;
                TypedStatementView::Assignment { target, value }
            }
            ast::Statement::MultiAssignment { targets, call } => {
                TypedStatementView::MultiAssignment {
                    targets: targets
                        .iter()
                        .map(|target| self.reference(target))
                        .collect::<Result<_, _>>()?,
                    call: self.call(call)?,
                }
            }
            ast::Statement::Call(call) => TypedStatementView::Call(self.call(call)?),
            ast::Statement::If(statement) => TypedStatementView::If(TypedIfStatementView {
                branches: statement
                    .branches
                    .iter()
                    .map(|branch| {
                        Ok(TypedIfBranchView {
                            condition: self.condition(&branch.condition)?,
                            body: self.statements(&branch.body)?,
                        })
                    })
                    .collect::<Result<_, String>>()?,
                else_body: statement
                    .else_body
                    .as_deref()
                    .map(|body| self.statements(body))
                    .transpose()?,
            }),
            ast::Statement::For(for_loop) => {
                let body_scope = self.with_iterator(for_loop.iterator.as_ref());
                TypedStatementView::For(TypedForView {
                    iterator: &for_loop.iterator,
                    start: self.expression(&for_loop.start)?,
                    step: for_loop
                        .step
                        .as_ref()
                        .map(|step| self.expression(step))
                        .transpose()?,
                    stop: self.expression(&for_loop.stop)?,
                    body: body_scope.statements(&for_loop.body)?,
                })
            }
            ast::Statement::Limit(targets) => TypedStatementView::Limit(
                targets
                    .iter()
                    .map(|target| match target {
                        ast::LimitTarget::SelfState => Ok(TypedLimitTargetView::SelfState),
                        ast::LimitTarget::Reference(reference) => {
                            Ok(TypedLimitTargetView::Reference(self.reference(reference)?))
                        }
                    })
                    .collect::<Result<_, String>>()?,
            ),
            ast::Statement::Signal(signals) => TypedStatementView::Signal(signals),
        })
    }

    fn condition(&self, condition: &'a ast::Condition) -> Result<TypedConditionView<'a>, String> {
        Ok(match condition {
            ast::Condition::Expression(expression) => {
                TypedConditionView::Expression(self.expression(expression)?)
            }
            ast::Condition::SignalCheck(check) => TypedConditionView::SignalCheck(check),
        })
    }

    fn call(&self, call: &'a ast::FunctionCall) -> Result<TypedCallView<'a>, String> {
        Ok(TypedCallView {
            function: &call.function,
            lifted_base: rumoca_ir_galec::builtins::find_lifted_base(call.function.lexeme())
                .map(|builtin| builtin.name),
            arguments: call
                .arguments
                .iter()
                .map(|argument| self.expression(argument))
                .collect::<Result<_, _>>()?,
        })
    }

    fn reference(&self, reference: &'a ast::Reference) -> Result<TypedReferenceView<'a>, String> {
        let shape = match reference {
            ast::Reference::Local(part) if self.iterators.contains(part.name.lexeme()) => {
                ShapeEvidence::scalar()
            }
            ast::Reference::Local(part) => self
                .locals
                .get(part.name.lexeme())
                .copied()
                .ok_or_else(|| {
                    format!(
                        "checked local reference `{}` is unresolved",
                        part.name.lexeme()
                    )
                })
                .and_then(|declaration| reference_shape(declaration, part))?,
            ast::Reference::State(parts) => self.block.state_reference_shape(parts)?,
        };
        Ok(TypedReferenceView {
            rank: shape.rank,
            extents: shape.extents,
            node: match reference {
                ast::Reference::Local(part) => TypedReferenceNodeView::Local(part),
                ast::Reference::State(parts) => TypedReferenceNodeView::State(parts),
            },
        })
    }

    fn expression(
        &self,
        expression: &'a ast::Expression,
    ) -> Result<TypedExpressionView<'a>, String> {
        let (shape, node) = match expression {
            ast::Expression::Bool(value) => (
                ShapeEvidence::scalar(),
                TypedExpressionNodeView::Bool(*value),
            ),
            ast::Expression::Integer(value) => (
                ShapeEvidence::scalar(),
                TypedExpressionNodeView::Integer(*value),
            ),
            ast::Expression::Real(value) => (
                ShapeEvidence::scalar(),
                TypedExpressionNodeView::Real(*value),
            ),
            ast::Expression::Ref(reference) => {
                let reference = self.reference(reference)?;
                (
                    ShapeEvidence::of_reference(&reference),
                    TypedExpressionNodeView::Ref(reference),
                )
            }
            ast::Expression::Size { array, dimension } => (
                ShapeEvidence::scalar(),
                TypedExpressionNodeView::Size {
                    array: self.reference(array)?,
                    dimension: Box::new(self.expression(dimension)?),
                },
            ),
            ast::Expression::Call(call) => {
                let call_view = self.call(call)?;
                (
                    self.block.call_shape(call, &call_view.arguments)?,
                    TypedExpressionNodeView::Call(call_view),
                )
            }
            ast::Expression::Paren(value) => {
                let value = Box::new(self.expression(value)?);
                (
                    ShapeEvidence::of_expression(&value),
                    TypedExpressionNodeView::Paren(value),
                )
            }
            ast::Expression::If(value) => self.if_expression(value)?,
            ast::Expression::Array(values) => self.array_expression(values)?,
            ast::Expression::Neg(reference) => {
                let reference = self.reference(reference)?;
                (
                    ShapeEvidence::of_reference(&reference),
                    TypedExpressionNodeView::Neg(reference),
                )
            }
            ast::Expression::Not(value) => (
                ShapeEvidence::scalar(),
                TypedExpressionNodeView::Not(Box::new(self.expression(value)?)),
            ),
            ast::Expression::Binary { op, lhs, rhs } => self.binary_expression(*op, lhs, rhs)?,
        };
        Ok(TypedExpressionView {
            rank: shape.rank,
            extents: shape.extents,
            node,
        })
    }

    fn if_expression(
        &self,
        value: &'a ast::IfExpression,
    ) -> Result<(ShapeEvidence, TypedExpressionNodeView<'a>), String> {
        let branches = value
            .branches
            .iter()
            .map(|(condition, branch)| Ok((self.expression(condition)?, self.expression(branch)?)))
            .collect::<Result<Vec<_>, String>>()?;
        let else_value = Box::new(self.expression(&value.else_value)?);
        let shape = ShapeEvidence::of_expression(&else_value);
        for (_, branch) in &branches {
            require_equal_shape(
                shape.clone(),
                ShapeEvidence::of_expression(branch),
                "checked if-expression branches",
            )?;
        }
        Ok((
            shape,
            TypedExpressionNodeView::If(TypedIfExpressionView {
                branches,
                else_value,
            }),
        ))
    }

    fn array_expression(
        &self,
        elements: &'a [ast::Expression],
    ) -> Result<(ShapeEvidence, TypedExpressionNodeView<'a>), String> {
        let values = elements
            .iter()
            .map(|value| self.expression(value))
            .collect::<Result<Vec<_>, _>>()?;
        let element_shape = values
            .first()
            .map(ShapeEvidence::of_expression)
            .unwrap_or_else(ShapeEvidence::scalar);
        for value in &values[1..] {
            require_equal_shape(
                element_shape.clone(),
                ShapeEvidence::of_expression(value),
                "checked array constructor",
            )?;
        }
        let mut extents = element_shape.extents.clone();
        if let Some(extents) = &mut extents {
            extents.insert(0, values.len());
        }
        Ok((
            ShapeEvidence {
                rank: element_shape.rank + 1,
                extents,
            },
            TypedExpressionNodeView::Array(values),
        ))
    }

    fn binary_expression(
        &self,
        op: ast::BinaryOp,
        lhs: &'a ast::Expression,
        rhs: &'a ast::Expression,
    ) -> Result<(ShapeEvidence, TypedExpressionNodeView<'a>), String> {
        let lhs = Box::new(self.expression(lhs)?);
        let rhs = Box::new(self.expression(rhs)?);
        let shape = if matches!(
            op.precedence_class(),
            ast::PrecedenceClass::Power
                | ast::PrecedenceClass::Multiplicative
                | ast::PrecedenceClass::Additive
        ) {
            broadcast_shape(
                ShapeEvidence::of_expression(&lhs),
                ShapeEvidence::of_expression(&rhs),
            )?
        } else {
            ShapeEvidence::scalar()
        };
        Ok((shape, TypedExpressionNodeView::Binary { op, lhs, rhs }))
    }
}

#[derive(Clone)]
struct ShapeEvidence {
    rank: usize,
    extents: Option<Vec<usize>>,
}

impl ShapeEvidence {
    fn scalar() -> Self {
        Self {
            rank: 0,
            extents: Some(Vec::new()),
        }
    }

    fn of_reference(reference: &TypedReferenceView<'_>) -> Self {
        Self {
            rank: reference.rank,
            extents: reference.extents.clone(),
        }
    }

    fn of_expression(expression: &TypedExpressionView<'_>) -> Self {
        Self {
            rank: expression.rank,
            extents: expression.extents.clone(),
        }
    }
}

fn declaration_shape(declaration: &ast::VariableDeclaration) -> ShapeEvidence {
    ShapeEvidence {
        rank: declaration.dimensions.len(),
        extents: literal_extents(&declaration.dimensions),
    }
}

fn literal_extents(dimensions: &[ast::Dimension]) -> Option<Vec<usize>> {
    dimensions
        .iter()
        .map(|dimension| match dimension {
            ast::Dimension::Expr(ast::Expression::Integer(value)) if *value > 0 => {
                usize::try_from(*value).ok()
            }
            ast::Dimension::Derived | ast::Dimension::Expr(_) => None,
        })
        .collect()
}

const fn builtin_rank(ty: rumoca_ir_galec::builtins::BuiltinType) -> usize {
    use rumoca_ir_galec::builtins::BuiltinType;
    match ty {
        BuiltinType::Boolean | BuiltinType::Integer | BuiltinType::Real => 0,
        BuiltinType::IntegerVector | BuiltinType::RealVector => 1,
        BuiltinType::RealMatrix => 2,
        BuiltinType::RealArray3 => 3,
    }
}

fn require_equal_shape(
    expected: ShapeEvidence,
    found: ShapeEvidence,
    context: &str,
) -> Result<(), String> {
    if expected.rank != found.rank {
        return Err(format!(
            "{context} rank mismatch ({} != {})",
            expected.rank, found.rank
        ));
    }
    if matches!(
        (&expected.extents, &found.extents),
        (Some(expected), Some(found)) if expected != found
    ) {
        return Err(format!("{context} extent mismatch"));
    }
    Ok(())
}

fn broadcast_shape(lhs: ShapeEvidence, rhs: ShapeEvidence) -> Result<ShapeEvidence, String> {
    if lhs.rank == 0 {
        return Ok(rhs);
    }
    if rhs.rank == 0 {
        return Ok(lhs);
    }
    require_equal_shape(lhs.clone(), rhs, "checked binary operands")?;
    Ok(lhs)
}

fn reference_shape(
    declaration: &ast::VariableDeclaration,
    part: &ast::RefPart,
) -> Result<ShapeEvidence, String> {
    let rank = declaration
        .dimensions
        .len()
        .checked_sub(part.subscripts.len())
        .ok_or_else(|| {
            format!(
                "checked reference `{}` has too many subscripts",
                declaration.name.lexeme()
            )
        })?;
    let extents = if part.subscripts.is_empty() {
        literal_extents(&declaration.dimensions)
    } else if rank == 0 {
        Some(Vec::new())
    } else {
        None
    };
    Ok(ShapeEvidence { rank, extents })
}
