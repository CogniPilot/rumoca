//! Guard classification for MLS §3.6.5 conditional folding in attribute and
//! binding values.

#[cfg(test)]
mod tests;

use super::*;

/// Whether `condition` reads a tunable parameter coordinate's value.
///
/// A Modelica `constant` and a structural quantity (`size`/`ndims`, an
/// enumeration extent) fold to a literal and own no runtime coordinate, so only
/// a tunable `parameter` appears here as [`Coordinate::Parameter`]. A guard over
/// one's value must not be folded in an attribute or binding value, where the
/// parameter can change after translation.
///
/// A parameter that appears only as the array operand of `size`/`ndims`
/// contributes its shape, not its value, and a parameter's shape is fixed at
/// translation. Such a guard (`size(offset, 1) == 1`) is structural: it proves
/// the same Boolean however the parameter is later recalibrated, so it still
/// folds. The array operand of `size`/`ndims` is therefore not scanned; a
/// dimension index or any other position that reads the value still is.
pub(super) fn guard_reads_tunable_parameter<'dae>(
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    condition: &Expression,
) -> bool {
    let parameter =
        |name: &VarName| matches!(coordinates.get(name), Some(Coordinate::Parameter(_)));
    reads_value_where(condition, &parameter)
}

/// Whether any guard or value of a conditional calls a user function.
///
/// Shape discovery prunes the calls of a statically dead arm, so a call that
/// discovery removed carries no call-shape certificate. Preserving such an arm
/// would try to build that uncertified call, so a conditional with a user-
/// function call in any arm keeps folding rather than being preserved.
pub(in crate::construction) fn conditional_calls_a_user_function(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
) -> bool {
    // Incidence mode walks every builtin argument, so a call inside the array
    // operand of `size` is found too.
    let mut scan = ArmScan::incidence(&|_| false);
    for (condition, value) in branches {
        rumoca_core::ExpressionVisitor::visit_expression(&mut scan, condition);
        rumoca_core::ExpressionVisitor::visit_expression(&mut scan, value);
    }
    rumoca_core::ExpressionVisitor::visit_expression(&mut scan, else_branch);
    scan.calls_function
}

/// Whether an attribute or binding conditional must fold at translation.
///
/// It folds when an arm calls a user function (see
/// [`conditional_calls_a_user_function`]) or when this scope does not prove
/// that every arm has the same shape. A run-time MLS §3.6.5 conditional needs
/// type-compatible arms (MLS §6.7), which for arrays means equal sizes, so a
/// conditional whose arms are not proven equal in shape selects structure: the
/// parameters its guard reads are structural parameters (MLS §4.5), fixed at
/// translation, and cannot be preserved as a run-time branch.
pub(in crate::construction) fn attribute_conditional_folds(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    values: &ShapeEnvironment,
) -> bool {
    if conditional_calls_a_user_function(branches, else_branch) {
        return true;
    }
    let Some(expected) = call_free_expression_shape(else_branch, values) else {
        return true;
    };
    !branches.iter().all(|(_, value)| {
        call_free_expression_shape(value, values).is_some_and(|shape| shape == expected)
    })
}

/// [`retains_parameter_guard`] over the DAE coordinates of an equation: a
/// parameter coordinate outside `evaluable` is tunable, and a state,
/// algebraic, or discrete coordinate is an unknown.
pub(super) fn retains_equation_guard<'dae>(
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    evaluable: &std::collections::HashSet<VarName>,
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
) -> bool {
    let tunable = |name: &VarName| {
        matches!(coordinates.get(name), Some(Coordinate::Parameter(_))) && !evaluable.contains(name)
    };
    let unknown = |name: &VarName| {
        matches!(
            coordinates.get(name),
            Some(
                Coordinate::State(_)
                    | Coordinate::Algebraic(_)
                    | Coordinate::DiscreteReal(_)
                    | Coordinate::DiscreteValue(_)
            )
        )
    };
    retains_parameter_guard(
        branches,
        else_branch,
        &GuardClasses {
            tunable_parameter: &tunable,
            unknown: &unknown,
        },
    )
}

/// How a conditional's names are classified for SPEC_0040 DAE-C22.
pub(in crate::construction) struct GuardClasses<'a> {
    /// A parameter that is not fixed at translation (not evaluable).
    pub(in crate::construction) tunable_parameter: &'a dyn Fn(&VarName) -> bool,
    /// An unknown of the equation system: a state, algebraic, or discrete
    /// variable.
    pub(in crate::construction) unknown: &'a dyn Fn(&VarName) -> bool,
}

/// Whether a conditional whose guard reads a tunable parameter is kept as a
/// run-time branch (SPEC_0040 DAE-C22, MLS 3.7 §8.3.4): its arms read the same
/// unknowns in the same positions (derivative, `pre`, and subscripts
/// included), so the branch changes no equation structure. Shape discovery
/// certifies the calls of every arm of such a conditional.
pub(in crate::construction) fn retains_parameter_guard(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    classes: &GuardClasses<'_>,
) -> bool {
    if !branches
        .iter()
        .any(|(condition, _)| reads_value_where(condition, classes.tunable_parameter))
    {
        return false;
    }
    let incidence = |arm: &Expression| {
        let mut scan = ArmScan::incidence(classes.unknown);
        rumoca_core::ExpressionVisitor::visit_expression(&mut scan, arm);
        scan.reads
    };
    let first = incidence(else_branch);
    branches
        .iter()
        .all(|(_, value)| same_incidence(&incidence(value), &first))
}

/// Source locations do not change equation incidence. The canonical Flat
/// expression comparator retains variable and subscript semantics while
/// excluding those locations; enclosing operators remain part of each read.
struct IncidenceRead {
    operators: Vec<rumoca_core::BuiltinFunction>,
    reference: Expression,
}

impl IncidenceRead {
    fn same(&self, other: &Self) -> bool {
        self.operators == other.operators
            && self
                .reference
                .semantically_eq_ignoring_spans(&other.reference)
    }
}

fn same_incidence(lhs: &[IncidenceRead], rhs: &[IncidenceRead]) -> bool {
    lhs.len() == rhs.len()
        && lhs
            .iter()
            .all(|read| rhs.iter().any(|other| read.same(other)))
}

/// Whether `expression` reads, by value, a name `select` accepts; the array
/// operand of `size`/`ndims` contributes its shape and is not a value read.
fn reads_value_where(expression: &Expression, select: &dyn Fn(&VarName) -> bool) -> bool {
    let mut scan = ArmScan::values(select);
    rumoca_core::ExpressionVisitor::visit_expression(&mut scan, expression);
    scan.found
}

/// One walk over a guard or an arm, shared by every classification here. In
/// value mode it records whether a name `select` accepts is read by value
/// (the array operand of `size`/`ndims` contributes its shape and is
/// skipped); in incidence mode it records each name `select` accepts with its
/// subscripts and the chain of builtin operators (`der`, `pre`, ...)
/// enclosing it. Either mode notes a function call.
struct ArmScan<'a> {
    select: &'a dyn Fn(&VarName) -> bool,
    /// The enclosing builtin operators; `None` in value mode.
    operators: Option<Vec<rumoca_core::BuiltinFunction>>,
    found: bool,
    calls_function: bool,
    reads: Vec<IncidenceRead>,
}

impl<'a> ArmScan<'a> {
    fn values(select: &'a dyn Fn(&VarName) -> bool) -> Self {
        Self {
            select,
            operators: None,
            found: false,
            calls_function: false,
            reads: Vec::new(),
        }
    }

    fn incidence(select: &'a dyn Fn(&VarName) -> bool) -> Self {
        Self {
            operators: Some(Vec::new()),
            ..Self::values(select)
        }
    }

    /// Records the authored reference itself, so the incidence keeps its source span.
    fn record_incidence(&mut self, reference: &Expression) {
        let Some(operators) = &self.operators else {
            return;
        };
        let read = IncidenceRead {
            operators: operators.clone(),
            reference: reference.clone(),
        };
        if !self.reads.iter().any(|other| read.same(other)) {
            self.reads.push(read);
        }
    }
}

impl rumoca_core::ExpressionVisitor for ArmScan<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if let Expression::VarRef { name, .. } = expr
            && (self.select)(name.var_name())
        {
            self.found = true;
            self.record_incidence(expr);
        }
        self.walk_expression(expr);
    }

    fn visit_builtin_call(&mut self, function: &rumoca_core::BuiltinFunction, args: &[Expression]) {
        let Some(operators) = &mut self.operators else {
            let structural = matches!(
                function,
                rumoca_core::BuiltinFunction::Size | rumoca_core::BuiltinFunction::Ndims
            );
            for argument in args.iter().skip(usize::from(structural)) {
                self.visit_expression(argument);
            }
            return;
        };
        operators.push(*function);
        for argument in args {
            self.visit_expression(argument);
        }
        if let Some(operators) = &mut self.operators {
            operators.pop();
        }
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
    ) {
        self.calls_function = true;
        self.walk_function_call(name, args, is_constructor);
    }
}

/// [`retains_parameter_guard`] over the Flat variabilities, before DAE
/// coordinates exist: a parameter outside `evaluable` is tunable, and a
/// variable that is neither a parameter nor a constant is an unknown.
pub(in crate::construction) fn retains_flat_guard(
    flat: &flat::Model,
    evaluable: &std::collections::HashSet<VarName>,
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
) -> bool {
    let variability = |name: &VarName| {
        flat.variables
            .get(name)
            .map(|variable| &variable.variability)
    };
    let tunable = |name: &VarName| {
        !evaluable.contains(name) && matches!(variability(name), Some(Variability::Parameter(_)))
    };
    let unknown = |name: &VarName| flat_unknown(flat, name);
    retains_parameter_guard(
        branches,
        else_branch,
        &GuardClasses {
            tunable_parameter: &tunable,
            unknown: &unknown,
        },
    )
}

/// Whether `name` reads an unknown of the equation system: a leaf coordinate
/// that is neither a parameter, a constant, nor an external input. A record
/// aggregate reads every leaf field. A reference without a leaf layout reads
/// no unknown here; record-equation analysis reports its error.
fn flat_unknown(flat: &flat::Model, name: &VarName) -> bool {
    let Ok(leaves) = super::super::analysis::reference_leaf_coordinates(flat, name) else {
        return false;
    };
    leaves.iter().any(|leaf| {
        flat.variables.get(leaf).is_some_and(|variable| {
            // Use the same resolved top-level ownership as role planning.
            // Invalid ownership stays conservative here; role planning emits
            // its original error before constructing a DAE coordinate.
            let external =
                super::super::analysis::is_external_input(flat, leaf, variable).unwrap_or(false);
            !external
                && !matches!(
                    variable.variability,
                    Variability::Parameter(_) | Variability::Constant(_)
                )
        })
    })
}
