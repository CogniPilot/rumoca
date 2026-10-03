//! MLS 3.7 §14 operator overloading, resolved at flatten.
//!
//! An operator applied to an operator-record operand (`a + b`, `-a`, `2*a`,
//! `a == b` with `Complex a, b`) is a call of one of the record's operator
//! functions (MLS §14.5). This pass replaces each such operator by that call
//! before function collection, so the called operator functions are collected
//! and lowered like any other call and the record-valued result reaches DAE
//! construction through the existing record-call owners.
//!
//! Resolution follows MLS §14.5 for scalar operands:
//!
//! * the functions of operator `'op'` declared by either operand's record are
//!   candidates, and a candidate matches when its first inputs accept the
//!   operands (a record input accepts the same operator record, a Real input
//!   any numeric operand, an Integer input only an Integer) and every further
//!   input has a default; exactly one match is called, and two are an
//!   ambiguity error (MLS §14.2, OPREC-007);
//! * when no function matches and one operand is numeric, the numeric operand
//!   is converted by the unique matching `'constructor'` function of the other
//!   operand's record (`2*c` is `c'.constructor'(2)*c`), and matching repeats;
//! * `'0'` is the record's zero element, called where the operator record
//!   needs a neutral value (an empty `sum`).
//!
//! An equation whose sides are record values that no record-equation owner
//! reads directly (`a + b = Complex(0, 0)`) is stated field by field, so each
//! field is one scalar equation, as the record equation denotes (MLS §8.4).

mod arrays;
mod catalog;

use rumoca_core::{
    DefId, Expression, ExpressionRewriter, OpBinary, OpUnary, Span, StatementRewriter,
};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

use crate::errors::FlattenError;
use arrays::record_arrays;
use catalog::{OperandType, OperatorCatalog, OperatorFunction};

/// Resolve every operator-record operator in the model's expressions and in
/// the bodies of the collected functions.
pub(crate) fn resolve_operator_overloads(
    flat: &mut flat::Model,
    classes: &ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let mut catalog = OperatorCatalog::new(classes);
    let scope = ModelScope::from_model(flat);
    let mut resolver = Resolver {
        catalog: &mut catalog,
        scope: &scope,
        error: None,
    };
    rewrite_model(flat, &scope, &mut resolver);
    if let Some(error) = resolver.error.take() {
        return Err(error);
    }
    let names: Vec<_> = flat.functions.keys().cloned().collect();
    for name in names {
        let Some(function) = flat.functions.get(&name) else {
            continue;
        };
        let scope = FunctionScope::from_function(function, &flat.predefined_types);
        let mut resolver = Resolver {
            catalog: &mut catalog,
            scope: &scope,
            error: None,
        };
        let body = resolver.rewrite_statements(&function.body);
        if let Some(error) = resolver.error.take() {
            return Err(error);
        }
        if let Some(function) = flat.functions.get_mut(&name) {
            function.body = body;
        }
    }
    Ok(())
}

fn rewrite_model(flat: &mut flat::Model, model: &ModelScope, resolver: &mut Resolver<'_, '_, '_>) {
    for variable in flat.variables.values_mut() {
        if let Some(binding) = variable.binding.as_mut() {
            *binding = resolver.rewrite_expression(binding);
        }
    }
    let restated: Vec<usize> = flat
        .equations
        .iter_mut()
        .enumerate()
        .filter_map(|(index, equation)| resolver.rewrite_equation(model, equation).then_some(index))
        .collect();
    drop_restated_families(&mut flat.structured_equations, &restated);
    let restated: Vec<usize> = flat
        .initial_equations
        .iter_mut()
        .enumerate()
        .filter_map(|(index, equation)| resolver.rewrite_equation(model, equation).then_some(index))
        .collect();
    drop_restated_families(&mut flat.initial_structured_equations, &restated);
    for algorithm in flat
        .algorithms
        .iter_mut()
        .chain(flat.initial_algorithms.iter_mut())
    {
        algorithm.statements = resolver.rewrite_statements(&algorithm.statements);
    }
    for assertion in flat
        .assert_equations
        .iter_mut()
        .chain(flat.initial_assert_equations.iter_mut())
    {
        assertion.condition = resolver.rewrite_expression(&assertion.condition);
    }
}

/// The scalar operand type of a Real or Integer Flat variable, from the
/// canonical root of its effective type (`SI.Voltage` is Real).
fn numeric_variable_type(flat: &flat::Model, variable: &flat::Variable) -> Option<OperandType> {
    let canonical = flat
        .effective_types
        .get(&variable.type_id)
        .map_or(variable.type_id, rumoca_core::EffectiveType::canonical_type);
    let types = &flat.predefined_types;
    if canonical == types.integer {
        Some(OperandType::Numeric { integer: true })
    } else if canonical == types.real {
        Some(OperandType::Numeric { integer: false })
    } else {
        None
    }
}

/// The operand types of the names one expression scope reads.
trait Scope {
    fn name_type(&self, catalog: &mut OperatorCatalog<'_>, name: &str) -> OperandType;
    /// The declared record class and the element coordinates of a
    /// one-dimensional record array `name` whose elements Flat declares.
    fn record_array(&self, _name: &str) -> Option<(DefId, &[rumoca_core::ComponentReference])> {
        None
    }
    /// Whether the scope declares `name`; a `for` index is not declared by it.
    fn declares(&self, name: &str) -> bool;
    /// The scalar type and rank of a Real or Integer variable `name` of any
    /// rank, so an element `phi[k]` of a numeric array is a numeric operand.
    fn numeric_variable(
        &self,
        catalog: &OperatorCatalog<'_>,
        name: &str,
    ) -> Option<(OperandType, usize)>;
}

struct ModelScope {
    records: rustc_hash::FxHashMap<String, DefId>,
    record_arrays: rustc_hash::FxHashMap<String, (DefId, Vec<rumoca_core::ComponentReference>)>,
    /// Every declared variable's numeric scalar type (`None` when it is not
    /// Real or Integer) and rank.
    variables: rustc_hash::FxHashMap<String, (Option<OperandType>, usize)>,
    record_types: rustc_hash::FxHashMap<DefId, Vec<(String, DefId)>>,
}

impl ModelScope {
    fn from_model(flat: &flat::Model) -> Self {
        Self {
            record_arrays: record_arrays(flat),
            records: flat
                .record_instances
                .iter()
                .filter(|(_, record)| record.dims.is_empty())
                .map(|(name, record)| (name.as_str().to_string(), record.type_def_id))
                .collect(),
            variables: flat
                .variables
                .iter()
                .map(|(name, variable)| {
                    (
                        name.as_str().to_string(),
                        (numeric_variable_type(flat, variable), variable.dims.len()),
                    )
                })
                .collect(),
            record_types: flat
                .record_types
                .iter()
                .map(|(def_id, record)| {
                    (
                        *def_id,
                        record
                            .fields
                            .iter()
                            .map(|field| (field.name.clone(), field.def_id))
                            .collect(),
                    )
                })
                .collect(),
        }
    }
}

impl ModelScope {
    /// The field names and definitions of record class `def_id`.
    fn record_fields(&self, def_id: DefId) -> Option<Vec<(String, DefId)>> {
        self.record_types.get(&def_id).cloned()
    }

    /// The declared record class of the scalar record variable `name`.
    fn record_def(&self, name: &str) -> Option<DefId> {
        self.records.get(name).copied()
    }
}

impl Scope for ModelScope {
    fn declares(&self, name: &str) -> bool {
        self.variables.contains_key(name)
            || self.records.contains_key(name)
            || self.record_arrays.contains_key(name)
    }

    fn name_type(&self, catalog: &mut OperatorCatalog<'_>, name: &str) -> OperandType {
        if let Some(owner) = self.records.get(name).and_then(|def| catalog.owner(*def)) {
            return OperandType::Record(owner);
        }
        if let Some(owner) = self
            .record_arrays
            .get(name)
            .and_then(|(def, _)| catalog.owner(*def))
        {
            return OperandType::RecordVector(owner);
        }
        match self.variables.get(name) {
            Some((Some(scalar), 0)) => *scalar,
            _ => OperandType::Other,
        }
    }

    fn numeric_variable(
        &self,
        _catalog: &OperatorCatalog<'_>,
        name: &str,
    ) -> Option<(OperandType, usize)> {
        let (scalar, rank) = self.variables.get(name)?;
        Some(((*scalar)?, *rank))
    }

    fn record_array(&self, name: &str) -> Option<(DefId, &[rumoca_core::ComponentReference])> {
        self.record_arrays
            .get(name)
            .map(|(def, elements)| (*def, elements.as_slice()))
    }
}

struct FunctionScope {
    /// Each parameter's record class, scalar type (from its canonical type
    /// identity), and rank.
    params: rustc_hash::FxHashMap<String, (Option<DefId>, OperandType, usize)>,
}

impl FunctionScope {
    fn from_function(function: &rumoca_core::Function, types: &flat::PredefinedTypeIds) -> Self {
        let scalar = |param: &rumoca_core::FunctionParam| {
            let canonical = param.effective_type.canonical_type();
            if canonical == types.integer {
                OperandType::Numeric { integer: true }
            } else if canonical == types.boolean || canonical == types.string {
                OperandType::Other
            } else {
                OperandType::Numeric { integer: false }
            }
        };
        Self {
            params: function
                .inputs
                .iter()
                .chain(&function.outputs)
                .chain(&function.locals)
                .map(|param| {
                    (
                        param.name.clone(),
                        (param.type_def_id, scalar(param), param.shape_expr.len()),
                    )
                })
                .collect(),
        }
    }
}

impl Scope for FunctionScope {
    fn declares(&self, name: &str) -> bool {
        self.params.contains_key(name)
    }

    fn name_type(&self, catalog: &mut OperatorCatalog<'_>, name: &str) -> OperandType {
        let Some((type_def_id, scalar, rank)) = self.params.get(name) else {
            return OperandType::Other;
        };
        if let Some(owner) = type_def_id.and_then(|def| catalog.owner(def)) {
            return match rank {
                0 => OperandType::Record(owner),
                1 => OperandType::RecordVector(owner),
                _ => OperandType::Other,
            };
        }
        if *rank != 0 {
            return OperandType::Other;
        }
        *scalar
    }

    fn numeric_variable(
        &self,
        catalog: &OperatorCatalog<'_>,
        name: &str,
    ) -> Option<(OperandType, usize)> {
        let (type_def_id, scalar, rank) = self.params.get(name)?;
        let record = type_def_id
            .and_then(|def| catalog.classes.get(def))
            .is_some_and(|class| class.class_type == rumoca_core::ClassType::Record);
        (!record && matches!(scalar, OperandType::Numeric { .. })).then_some((*scalar, *rank))
    }
}

struct Resolver<'catalog, 'tree, 'scope> {
    catalog: &'catalog mut OperatorCatalog<'tree>,
    scope: &'scope dyn Scope,
    error: Option<FlattenError>,
}

impl Resolver<'_, '_, '_> {
    /// Rewrite one equation; a record-valued equation no record owner reads
    /// directly is stated field by field.
    /// Returns whether the equation was restated field by field.
    fn rewrite_equation(&mut self, model: &ModelScope, equation: &mut flat::Equation) -> bool {
        let Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            span,
        } = &equation.residual
        else {
            equation.residual = self.rewrite_expression(&equation.residual);
            return false;
        };
        let lhs = self.rewrite_expression(lhs);
        let rhs = self.rewrite_expression(rhs);
        let span = *span;
        let (left, right) = (self.operand_type(&lhs), self.operand_type(&rhs));
        let pairs = match (left, right) {
            (OperandType::Record(owner), _) | (_, OperandType::Record(owner))
                if !self.owned_record_equation(model, &lhs, &rhs) =>
            {
                Some((owner, vec![(lhs.clone(), rhs.clone())]))
            }
            (OperandType::RecordVector(owner), _) | (_, OperandType::RecordVector(owner)) => self
                .vector_equation_pairs(&lhs, &rhs, span)
                .map(|pairs| (owner, pairs)),
            _ => None,
        };
        let Some((fields, pairs)) =
            pairs.and_then(|(owner, pairs)| Some((self.owner_fields(model, owner)?, pairs)))
        else {
            equation.residual = subtract(lhs, rhs, span);
            return false;
        };
        let elements = pairs
            .iter()
            .flat_map(|(lhs, rhs)| {
                fields.iter().map(move |(field, def_id)| {
                    subtract(
                        field_access(lhs, field, *def_id, span),
                        field_access(rhs, field, *def_id, span),
                        span,
                    )
                })
            })
            .collect::<Vec<_>>();
        equation.scalar_count = elements.len();
        equation.residual = Expression::Array {
            elements,
            kind: rumoca_core::ArrayConstructor::Array,
            span,
        };
        true
    }

    fn owner_fields(&self, model: &ModelScope, owner: DefId) -> Option<Vec<(String, DefId)>> {
        if let Some(fields) = model.record_fields(owner) {
            return Some(fields);
        }
        let class = self.catalog.classes.get(owner)?;
        Some(
            class
                .components
                .values()
                .filter_map(|component| Some((component.name.clone(), component.def_id?)))
                .collect(),
        )
    }

    /// The type of `name` read with `subscripts`: a declared element
    /// (`v[1]` of a record vector Flat declares element by element), else an
    /// element selected by scalar subscripts (`c1[i]` of a record vector,
    /// `phi[k]` of a numeric array), else the declared type of the name.
    fn reference_type(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) -> OperandType {
        if let Some(element) = literal_element_name(name.as_str(), subscripts) {
            let known = self.scope.name_type(self.catalog, &element);
            if known != OperandType::Other {
                return known;
            }
        }
        if subscripts.is_empty() {
            return self.name_type(name, name.as_str());
        }
        if !subscripts
            .iter()
            .all(|subscript| self.scalar_subscript(subscript))
        {
            return OperandType::Other;
        }
        if let Some((scalar, rank)) = self.scope.numeric_variable(self.catalog, name.as_str()) {
            return if rank == subscripts.len() {
                scalar
            } else {
                OperandType::Other
            };
        }
        match self.name_type(name, name.as_str()) {
            OperandType::RecordVector(owner) if subscripts.len() == 1 => OperandType::Record(owner),
            _ => OperandType::Other,
        }
    }

    /// Whether `subscript` selects one index: a literal, a loop index (which
    /// the scope does not declare), or an Integer-valued expression.
    fn scalar_subscript(&mut self, subscript: &rumoca_core::Subscript) -> bool {
        match subscript {
            rumoca_core::Subscript::Index { .. } => true,
            rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::VarRef {
                    name: index,
                    subscripts,
                    ..
                } if subscripts.is_empty() && !self.scope.declares(index.as_str()) => true,
                other => matches!(self.operand_type(other), OperandType::Numeric { .. }),
            },
            rumoca_core::Subscript::Colon { .. } => false,
        }
    }

    fn operand_type(&mut self, expression: &Expression) -> OperandType {
        match expression {
            Expression::VarRef {
                name, subscripts, ..
            } => self.reference_type(name, subscripts),
            Expression::Index {
                base, subscripts, ..
            } => match base.as_ref() {
                Expression::VarRef {
                    name,
                    subscripts: base_subscripts,
                    ..
                } if base_subscripts.is_empty() => self.reference_type(name, subscripts),
                _ => OperandType::Other,
            },
            Expression::Literal {
                value: rumoca_core::Literal::Integer(_),
                ..
            } => OperandType::Numeric { integer: true },
            Expression::Literal {
                value: rumoca_core::Literal::Real(_),
                ..
            } => OperandType::Numeric { integer: false },
            Expression::FunctionCall { name, .. } => self.call_type(name),
            Expression::Unary {
                op: OpUnary::Minus | OpUnary::Plus,
                rhs,
                ..
            } => self.operand_type(rhs),
            Expression::Binary { op, lhs, rhs, .. } if !op.is_relational() => {
                match (self.operand_type(lhs), self.operand_type(rhs)) {
                    (
                        OperandType::Numeric { integer: left },
                        OperandType::Numeric { integer: right },
                    ) => OperandType::Numeric {
                        integer: left && right && !matches!(op, OpBinary::Div),
                    },
                    _ => OperandType::Other,
                }
            }
            Expression::Array {
                elements,
                kind: rumoca_core::ArrayConstructor::Array,
                ..
            } => match elements.first().map(|element| self.operand_type(element)) {
                Some(OperandType::Record(owner)) => OperandType::RecordVector(owner),
                _ => OperandType::Other,
            },
            Expression::If { branches, .. } => branches
                .first()
                .map_or(OperandType::Other, |(_, value)| self.operand_type(value)),
            Expression::BuiltinCall { function, .. } if scalar_real_builtin(*function) => {
                OperandType::Numeric { integer: false }
            }
            _ => OperandType::Other,
        }
    }

    /// The scope's type for `element`, else the declared type of the
    /// declaration the reference names (a package constant this phase has not
    /// injected yet, such as `Modelica.ComplexMath.j`).
    fn name_type(&mut self, reference: &rumoca_core::Reference, element: &str) -> OperandType {
        match self.scope.name_type(self.catalog, element) {
            OperandType::Other => reference
                .component_ref()
                .map_or(OperandType::Other, |component| {
                    self.catalog.reference_type(component)
                }),
            known => known,
        }
    }

    fn call_type(&mut self, name: &rumoca_core::Reference) -> OperandType {
        let Some(def_id) = name
            .component_ref()
            .map(|reference| reference.target_def_id())
        else {
            return OperandType::Other;
        };
        if let Some(owner) = self.catalog.owner(def_id) {
            return OperandType::Record(owner);
        }
        let Some(class) = self.catalog.classes.get(def_id) else {
            return OperandType::Other;
        };
        let output = class
            .components
            .values()
            .find(|component| matches!(component.causality, rumoca_core::Causality::Output(_)));
        match output {
            Some(component) if component.shape_expr.is_empty() && component.shape.is_empty() => {
                self.catalog.component_type(component)
            }
            _ => OperandType::Other,
        }
    }

    fn resolve_binary(
        &mut self,
        op: &OpBinary,
        lhs: Expression,
        rhs: Expression,
        span: Span,
    ) -> Expression {
        let Some(operator) = binary_operator_name(op) else {
            return binary(op, lhs, rhs, span);
        };
        let left = self.operand_type(&lhs);
        let right = self.operand_type(&rhs);
        let owners = record_owners(left, right);
        if owners.is_empty() {
            return binary(op, lhs, rhs, span);
        }
        match self.unique_match(&owners, operator, &[left, right], span) {
            Ok(Some(function)) => return self.call(function.def_id, vec![lhs, rhs], span),
            Ok(None) => {}
            Err(error) => {
                self.error.get_or_insert(error);
                return binary(op, lhs, rhs, span);
            }
        }
        // MLS §14.5: convert a numeric operand through the other operand's
        // record constructor, then match again.
        let (lhs, left) = self.promote(lhs, left, right, span);
        let (rhs, right) = self.promote(rhs, right, left, span);
        match self.unique_match(&owners, operator, &[left, right], span) {
            Ok(Some(function)) => self.call(function.def_id, vec![lhs, rhs], span),
            Ok(None) => self
                .elementwise_binary(op, (&lhs, left), (&rhs, right), span)
                .unwrap_or_else(|| binary(op, lhs, rhs, span)),
            Err(error) => {
                self.error.get_or_insert(error);
                binary(op, lhs, rhs, span)
            }
        }
    }

    fn promote(
        &mut self,
        operand: Expression,
        own: OperandType,
        other: OperandType,
        span: Span,
    ) -> (Expression, OperandType) {
        let (OperandType::Numeric { .. }, OperandType::Record(owner)) = (own, other) else {
            return (operand, own);
        };
        match self.unique_match(&[owner], "'constructor'", &[own], span) {
            Ok(Some(function)) => (
                self.call(function.def_id, vec![operand], span),
                OperandType::Record(owner),
            ),
            _ => (operand, own),
        }
    }

    fn resolve_unary(&mut self, op: &OpUnary, rhs: Expression, span: Span) -> Expression {
        let operand = self.operand_type(&rhs);
        if let (OperandType::RecordVector(_), OpUnary::Minus) = (operand, op)
            && let Some(negated) = self.elementwise_negate(&rhs, span)
        {
            return negated;
        }
        let OperandType::Record(owner) = operand else {
            return unary(op, rhs, span);
        };
        let name = match op {
            OpUnary::Minus => "'-'",
            OpUnary::Plus => "'+'",
            _ => return unary(op, rhs, span),
        };
        match self.unique_match(&[owner], name, &[operand], span) {
            Ok(Some(function)) => self.call(function.def_id, vec![rhs], span),
            Ok(None) if matches!(op, OpUnary::Plus) => rhs,
            Ok(None) => unary(op, rhs, span),
            Err(error) => {
                self.error.get_or_insert(error);
                unary(op, rhs, span)
            }
        }
    }

    /// The single function of `operator`, across `owners`, whose inputs
    /// accept `operands` exactly (MLS §14.2).
    fn unique_match(
        &mut self,
        owners: &[DefId],
        operator: &str,
        operands: &[OperandType],
        span: Span,
    ) -> Result<Option<OperatorFunction>, FlattenError> {
        let mut matches: Vec<OperatorFunction> = owners
            .iter()
            .flat_map(|owner| self.catalog.functions(*owner, operator))
            .filter(|function| accepts(function, operands))
            .collect();
        matches.dedup_by_key(|function| function.def_id);
        match matches.len() {
            0 => Ok(None),
            1 => Ok(matches.pop()),
            _ => Err(FlattenError::invalid_function_call_args(
                operator,
                "more than one operator function matches these operands (MLS §14.2)",
                span,
            )),
        }
    }

    /// A call of operator function `def_id`, named by its qualified path so
    /// function collection resolves it like a source-written call.
    fn call(&mut self, def_id: DefId, args: Vec<Expression>, span: Span) -> Expression {
        let classes = self.catalog.classes;
        let reference = crate::functions::class_path_reference(classes, def_id, span)
            .unwrap_or_else(|| {
                self.error.get_or_insert(FlattenError::internal(format!(
                    "operator function {def_id:?} has no complete class path"
                )));
                rumoca_core::Reference::new(String::new())
            });
        // Flat declares a model's record vector only element by element
        // (`v[1]`, `v[2]`), so a whole-vector argument is passed as the array
        // of its elements.
        let args = args
            .into_iter()
            .map(|arg| match &arg {
                Expression::VarRef {
                    name, subscripts, ..
                } if subscripts.is_empty() && self.scope.record_array(name.as_str()).is_some() => {
                    self.record_elements(&arg, span)
                        .map_or(arg, |elements| Expression::Array {
                            elements,
                            kind: rumoca_core::ArrayConstructor::Array,
                            span,
                        })
                }
                _ => arg,
            })
            .collect();
        Expression::FunctionCall {
            name: reference,
            args,
            is_constructor: false,
            span,
        }
    }
}

impl ExpressionRewriter for Resolver<'_, '_, '_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        match self.walk_expression(expression) {
            Expression::Binary { op, lhs, rhs, span } => self.resolve_binary(&op, *lhs, *rhs, span),
            Expression::Unary { op, rhs, span } => self.resolve_unary(&op, *rhs, span),
            Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sum,
                args,
                span,
            } => match args.as_slice() {
                [argument] => self
                    .record_sum(argument, span)
                    .unwrap_or(Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Sum,
                        args,
                        span,
                    }),
                _ => Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sum,
                    args,
                    span,
                },
            },
            Expression::FunctionCall {
                name,
                args,
                is_constructor: true,
                span,
            } => self
                .resolve_constructor(&name, &args, span)
                .unwrap_or(Expression::FunctionCall {
                    name,
                    args,
                    is_constructor: true,
                    span,
                }),
            other => other,
        }
    }
}

impl Resolver<'_, '_, '_> {
    /// MLS §14.3: `C(0)` of an operator record whose implicit record
    /// constructor cannot take the arguments calls the `'constructor'`
    /// function that accepts them (`Complex.'constructor'.fromReal`).
    fn resolve_constructor(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        span: Span,
    ) -> Option<Expression> {
        if args.iter().any(is_named_argument) {
            return None;
        }
        let record = name.component_ref()?.target_def_id();
        let owner = self.catalog.owner(record)?;
        let required_fields = self
            .catalog
            .classes
            .get(record)?
            .components
            .values()
            .filter(|field| field.binding.is_none() && !field.has_explicit_binding)
            .count();
        if args.len() >= required_fields {
            return None;
        }
        let operands: Vec<OperandType> = args.iter().map(|arg| self.operand_type(arg)).collect();
        match self.unique_match(&[owner], "'constructor'", &operands, span) {
            Ok(Some(function)) => Some(self.call(function.def_id, args.to_vec(), span)),
            Ok(None) => None,
            Err(error) => {
                self.error.get_or_insert(error);
                None
            }
        }
    }
}

fn is_named_argument(arg: &Expression) -> bool {
    matches!(
        arg,
        Expression::FunctionCall { name, is_constructor: true, .. }
            if name.as_str().starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    )
}

impl StatementRewriter for Resolver<'_, '_, '_> {}

/// A record equation the DAE record owner reads directly: a record
/// coordinate equal to a record call or another record coordinate.
impl Resolver<'_, '_, '_> {
    /// A record coordinate equal to a record call or another record
    /// coordinate of the same declared record class, which the DAE record
    /// owner reads directly.
    fn owned_record_equation(
        &mut self,
        model: &ModelScope,
        lhs: &Expression,
        rhs: &Expression,
    ) -> bool {
        let owned_shape = matches!(
            (lhs, rhs),
            (
                Expression::VarRef { subscripts, .. },
                Expression::FunctionCall { .. } | Expression::VarRef { .. }
            ) if subscripts.is_empty()
        );
        owned_shape
            && match (self.record_def(model, lhs), self.record_def(model, rhs)) {
                (Some(left), Some(right)) => left == right,
                _ => false,
            }
    }

    /// The declared record class of a record-valued expression.
    fn record_def(&mut self, model: &ModelScope, expression: &Expression) -> Option<DefId> {
        match expression {
            Expression::VarRef { name, .. } => model.record_def(name.as_str()).or_else(|| {
                let def = name.component_ref()?.target_def_id();
                self.catalog.declared_record(def)
            }),
            Expression::FunctionCall { name, .. } => {
                let def = name.component_ref()?.target_def_id();
                let class = self.catalog.classes.get(def)?;
                if class.class_type == rumoca_core::ClassType::Record {
                    return Some(def);
                }
                class
                    .components
                    .values()
                    .find(|component| {
                        matches!(component.causality, rumoca_core::Causality::Output(_))
                    })?
                    .type_def_id
            }
            _ => None,
        }
    }
}

/// A structured family over an equation restated field by field no longer
/// describes that equation's scalar views; the restated equation is lowered
/// from its scalar rows alone.
fn drop_restated_families(families: &mut Vec<flat::StructuredEquationFamily>, restated: &[usize]) {
    if restated.is_empty() {
        return;
    }
    families.retain(|family| {
        let points = family.domain.scalar_count().unwrap_or(usize::MAX);
        let end = family
            .first_equation_index
            .saturating_add(points.saturating_mul(family.equations_per_point));
        !restated
            .iter()
            .any(|index| (family.first_equation_index..end).contains(index))
    });
}

/// The flat name `v[1,2]` of an element read with literal subscripts, or the
/// name itself when there are none.
fn literal_element_name(name: &str, subscripts: &[rumoca_core::Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.to_string());
    }
    let indices = subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => Some(value.to_string()),
            rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(value),
                    ..
                } => Some(value.to_string()),
                _ => None,
            },
            rumoca_core::Subscript::Colon { .. } => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some(format!("{name}[{}]", indices.join(",")))
}

fn accepts(function: &OperatorFunction, operands: &[OperandType]) -> bool {
    function.inputs.len() >= operands.len()
        && function.required <= operands.len()
        && function
            .inputs
            .iter()
            .zip(operands)
            .all(|(input, operand)| input_accepts(*input, *operand))
}

fn input_accepts(input: OperandType, operand: OperandType) -> bool {
    match (input, operand) {
        (OperandType::Record(expected), OperandType::Record(actual))
        | (OperandType::RecordVector(expected), OperandType::RecordVector(actual)) => {
            expected == actual
        }
        (OperandType::Numeric { integer: expected }, OperandType::Numeric { integer: actual }) => {
            !expected || actual
        }
        _ => false,
    }
}

fn record_owners(left: OperandType, right: OperandType) -> Vec<DefId> {
    let mut owners = Vec::new();
    for operand in [left, right] {
        if let OperandType::Record(owner) | OperandType::RecordVector(owner) = operand
            && !owners.contains(&owner)
        {
            owners.push(owner);
        }
    }
    owners
}

fn binary_operator_name(op: &OpBinary) -> Option<&'static str> {
    Some(match op {
        OpBinary::Add => "'+'",
        OpBinary::Sub => "'-'",
        OpBinary::Mul => "'*'",
        OpBinary::Div => "'/'",
        OpBinary::Exp => "'^'",
        OpBinary::Eq => "'=='",
        OpBinary::Neq => "'<>'",
        OpBinary::Lt => "'<'",
        OpBinary::Le => "'<='",
        OpBinary::Gt => "'>'",
        OpBinary::Ge => "'>='",
        OpBinary::And => "'and'",
        OpBinary::Or => "'or'",
        _ => return None,
    })
}

fn scalar_real_builtin(function: rumoca_core::BuiltinFunction) -> bool {
    use rumoca_core::BuiltinFunction as B;
    matches!(
        function,
        B::Abs
            | B::Sqrt
            | B::Sin
            | B::Cos
            | B::Tan
            | B::Asin
            | B::Acos
            | B::Atan
            | B::Atan2
            | B::Sinh
            | B::Cosh
            | B::Tanh
            | B::Exp
            | B::Log
            | B::Log10
    )
}

fn binary(op: &OpBinary, lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: op.clone(),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn unary(op: &OpUnary, rhs: Expression, span: Span) -> Expression {
    Expression::Unary {
        op: op.clone(),
        rhs: Box::new(rhs),
        span,
    }
}

fn subtract(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    binary(&OpBinary::Sub, lhs, rhs, span)
}

fn field_access(base: &Expression, field: &str, def_id: DefId, span: Span) -> Expression {
    Expression::FieldAccess {
        base: Box::new(base.clone()),
        field: field.to_string(),
        field_def_id: def_id,
        span,
    }
}
