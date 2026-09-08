use super::AlgorithmCodeArithmeticProfile;

/// Exact declaration identities authenticated while the sole retained
/// topology traversal assigns this occurrence's statement and child locators.
pub(crate) struct ValidatedRealMatrixMultiplyOccurrence {
    #[cfg(test)]
    pub(crate) target: crate::validate::DeclarationLoc,
    #[cfg(test)]
    pub(crate) source: crate::validate::DeclarationLoc,
}

pub(crate) fn validate_real_matrix_multiply_occurrence(
    outer: &crate::ast::ForLoop,
    occurrence: &crate::ast::RealMatrixMultiplyOccurrenceContract,
    declarations: &[(
        crate::validate::DeclarationLoc,
        &crate::ast::VariableDeclaration,
    )],
    arithmetic: AlgorithmCodeArithmeticProfile,
) -> Result<ValidatedRealMatrixMultiplyOccurrence, String> {
    let (target_element, source_element, target, source) =
        validate_occurrence_frame(outer, occurrence, declarations, arithmetic)?;
    validate_occurrence_legalization(
        outer,
        occurrence,
        target_element,
        source_element,
        arithmetic,
    )?;
    #[cfg(not(test))]
    let _ = (target, source);
    Ok(ValidatedRealMatrixMultiplyOccurrence {
        #[cfg(test)]
        target,
        #[cfg(test)]
        source,
    })
}

fn validate_occurrence_frame(
    outer: &crate::ast::ForLoop,
    occurrence: &crate::ast::RealMatrixMultiplyOccurrenceContract,
    declarations: &[(
        crate::validate::DeclarationLoc,
        &crate::ast::VariableDeclaration,
    )],
    arithmetic: AlgorithmCodeArithmeticProfile,
) -> Result<
    (
        crate::ast::Reference,
        crate::ast::Reference,
        crate::validate::DeclarationLoc,
        crate::validate::DeclarationLoc,
    ),
    String,
> {
    let column = outer
        .iterator
        .as_ref()
        .ok_or_else(|| "output loop has no binder".to_owned())?;
    if occurrence.format() != arithmetic.source_real() {
        return Err(
            "occurrence Real format disagrees with the package arithmetic profile".to_owned(),
        );
    }
    if occurrence.extent() == 0
        && arithmetic.real_matrix_multiply()
            == rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct
    {
        return Err("first-product inner domain is empty".to_owned());
    }
    require_loop_domain(outer, 1, occurrence.count(), "output")?;
    let target_element = append_subscript(occurrence.target_run(), column)
        .ok_or_else(|| "target run is not a local contiguous reference".to_owned())?;
    let source_element = append_subscript(occurrence.source_run(), column)
        .ok_or_else(|| "source run is not a local contiguous reference".to_owned())?;
    let target = require_real_run(
        occurrence.target_run(),
        occurrence.count(),
        declarations,
        "target",
    )?;
    let source = require_real_run(
        occurrence.source_run(),
        occurrence.count(),
        declarations,
        "source",
    )?;
    if reference_root(occurrence.target_run()) == reference_root(occurrence.source_run()) {
        return Err("target and source runs may alias".to_owned());
    }
    if !inert_arithmetic(occurrence.scale()) || mentions(occurrence.scale(), column.lexeme()) {
        return Err("scale is not inert output-coordinate-independent arithmetic".to_owned());
    }
    if mentions(occurrence.scale(), reference_root(occurrence.target_run())) {
        return Err("scale may read the target being accumulated".to_owned());
    }
    Ok((target_element, source_element, target, source))
}

fn validate_occurrence_legalization(
    outer: &crate::ast::ForLoop,
    occurrence: &crate::ast::RealMatrixMultiplyOccurrenceContract,
    target_element: crate::ast::Reference,
    source_element: crate::ast::Reference,
    arithmetic: AlgorithmCodeArithmeticProfile,
) -> Result<(), String> {
    use crate::ast::{BinaryOp, Expression, RealMatrixMultiplySeed, Statement};

    let product = Expression::binary(
        BinaryOp::Mul,
        occurrence.scale().clone(),
        Expression::Ref(source_element),
    );
    let (seed_statement, remainder_statement) = match outer.body.as_slice() {
        [seed, remainder] => (seed, remainder),
        _ => return Err("legalization is not exactly seed plus remainder loop".to_owned()),
    };
    let accumulated = Expression::binary(
        BinaryOp::Add,
        Expression::Ref(target_element.clone()),
        product.clone(),
    );
    let Statement::Assignment {
        target: seeded_target,
        value: seeded_value,
    } = &seed_statement.node
    else {
        return Err("legalization seed is not an assignment".to_owned());
    };
    if seeded_target != &target_element {
        return Err("legalization seed writes a different target".to_owned());
    }
    let Statement::For(remainder) = &remainder_statement.node else {
        return Err("legalization remainder is not a loop".to_owned());
    };
    if remainder.iterator.as_ref() != Some(occurrence.iterator()) {
        return Err("legalization remainder uses a different binder".to_owned());
    }
    let expected_start = match (arithmetic.real_matrix_multiply(), occurrence.seed()) {
        (
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            RealMatrixMultiplySeed::PositiveZero,
        ) => {
            if !matches!(seeded_value, Expression::Real(value) if value.to_bits() == 0.0_f64.to_bits())
            {
                return Err("positive-zero seed is not canonical +0.0".to_owned());
            }
            1
        }
        (
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
            RealMatrixMultiplySeed::FirstProduct { value },
        ) => {
            let expected_seed = instantiate_expression(product, occurrence.iterator(), 1);
            if value != &expected_seed || seeded_value != value {
                return Err(
                    "first-product seed is not the exact product at coordinate one".to_owned(),
                );
            }
            2
        }
        _ => {
            return Err("occurrence seed disagrees with the package arithmetic profile".to_owned());
        }
    };
    require_loop_domain(remainder, expected_start, occurrence.extent(), "remainder")?;
    let [remainder_body] = remainder.body.as_slice() else {
        return Err("remainder loop is not exactly one accumulation".to_owned());
    };
    let Statement::Assignment { target, value } = &remainder_body.node else {
        return Err("remainder body is not an assignment".to_owned());
    };
    if target != &target_element || value != &accumulated {
        return Err("remainder is not the exact ascending separate multiply/add".to_owned());
    }
    Ok(())
}

fn require_loop_domain(
    loop_: &crate::ast::ForLoop,
    start: i64,
    stop: u32,
    label: &str,
) -> Result<(), String> {
    if loop_.step.is_some()
        || loop_.start != crate::ast::Expression::Integer(start)
        || loop_.stop != crate::ast::Expression::Integer(i64::from(stop))
    {
        return Err(format!(
            "{label} loop is not the exact ascending {start}..{stop} domain"
        ));
    }
    Ok(())
}

fn append_subscript(
    reference: &crate::ast::Reference,
    iterator: &crate::ast::Name,
) -> Option<crate::ast::Reference> {
    let crate::ast::Reference::Local(mut part) = reference.clone() else {
        return None;
    };
    part.subscripts
        .push(crate::ast::Expression::Ref(crate::ast::Reference::local(
            iterator.clone(),
        )));
    Some(crate::ast::Reference::Local(part))
}

fn require_real_run(
    reference: &crate::ast::Reference,
    count: u32,
    declarations: &[(
        crate::validate::DeclarationLoc,
        &crate::ast::VariableDeclaration,
    )],
    label: &str,
) -> Result<crate::validate::DeclarationLoc, String> {
    let crate::ast::Reference::Local(part) = reference else {
        return Err(format!("{label} run is not local"));
    };
    let (locator, declaration) = declarations
        .iter()
        .copied()
        .find(|(_, declaration)| declaration.name.lexeme() == part.name.lexeme())
        .ok_or_else(|| format!("{label} run has no declaration in its owner"))?;
    if declaration.ty != crate::ast::TypeRef::Primitive(crate::ast::ScalarType::Real) {
        return Err(format!("{label} run is not Real"));
    }
    if part.subscripts.len().checked_add(1) != Some(declaration.dimensions.len()) {
        return Err(format!(
            "{label} reference does not leave exactly one row dimension"
        ));
    }
    let Some(crate::ast::Dimension::Expr(crate::ast::Expression::Integer(extent))) =
        declaration.dimensions.last()
    else {
        return Err(format!("{label} row extent is not a literal"));
    };
    if *extent != i64::from(count) {
        return Err(format!("{label} row extent disagrees with output count"));
    }
    Ok(locator)
}

fn reference_root(reference: &crate::ast::Reference) -> &str {
    match reference {
        crate::ast::Reference::Local(part) => part.name.lexeme(),
        crate::ast::Reference::State(parts) => parts.first().map_or("", |part| part.name.lexeme()),
    }
}

fn inert_arithmetic(expression: &crate::ast::Expression) -> bool {
    use crate::ast::{BinaryOp, Expression};
    match expression {
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => true,
        Expression::Ref(reference) | Expression::Neg(reference) => {
            reference_expressions(reference).all(inert_arithmetic)
        }
        Expression::Paren(value) => inert_arithmetic(value),
        Expression::Binary { op, lhs, rhs } => {
            matches!(
                op,
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Pow
            ) && inert_arithmetic(lhs)
                && inert_arithmetic(rhs)
        }
        Expression::Size { .. }
        | Expression::Call(_)
        | Expression::Not(_)
        | Expression::If(_)
        | Expression::Array(_) => false,
    }
}

fn mentions(expression: &crate::ast::Expression, name: &str) -> bool {
    use crate::ast::Expression;
    match expression {
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => false,
        Expression::Ref(reference) | Expression::Neg(reference) => {
            reference_root(reference) == name
                || reference_expressions(reference).any(|value| mentions(value, name))
        }
        Expression::Size { array, dimension } => {
            reference_root(array) == name
                || reference_expressions(array).any(|value| mentions(value, name))
                || mentions(dimension, name)
        }
        Expression::Call(call) => call.arguments.iter().any(|value| mentions(value, name)),
        Expression::Paren(value) | Expression::Not(value) => mentions(value, name),
        Expression::If(value) => {
            value
                .branches
                .iter()
                .any(|(condition, branch)| mentions(condition, name) || mentions(branch, name))
                || mentions(&value.else_value, name)
        }
        Expression::Array(values) => values.iter().any(|value| mentions(value, name)),
        Expression::Binary { lhs, rhs, .. } => mentions(lhs, name) || mentions(rhs, name),
    }
}

fn reference_expressions(
    reference: &crate::ast::Reference,
) -> impl Iterator<Item = &crate::ast::Expression> {
    let parts = match reference {
        crate::ast::Reference::Local(part) => std::slice::from_ref(part),
        crate::ast::Reference::State(parts) => parts.as_slice(),
    };
    parts.iter().flat_map(|part| &part.subscripts)
}

fn instantiate_expression(
    mut expression: crate::ast::Expression,
    iterator: &crate::ast::Name,
    coordinate: i64,
) -> crate::ast::Expression {
    instantiate_expression_in_place(&mut expression, iterator, coordinate);
    expression
}

fn instantiate_expression_in_place(
    expression: &mut crate::ast::Expression,
    iterator: &crate::ast::Name,
    coordinate: i64,
) {
    use crate::ast::Expression;
    if matches!(expression,
        Expression::Ref(crate::ast::Reference::Local(part))
            if part.name == *iterator && part.subscripts.is_empty())
    {
        *expression = Expression::Integer(coordinate);
        return;
    }
    match expression {
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {}
        Expression::Ref(reference) | Expression::Neg(reference) => {
            instantiate_reference(reference, iterator, coordinate);
        }
        Expression::Size { array, dimension } => {
            instantiate_reference(array, iterator, coordinate);
            instantiate_expression_in_place(dimension, iterator, coordinate);
        }
        Expression::Call(call) => {
            for argument in &mut call.arguments {
                instantiate_expression_in_place(argument, iterator, coordinate);
            }
        }
        Expression::Paren(value) | Expression::Not(value) => {
            instantiate_expression_in_place(value, iterator, coordinate);
        }
        Expression::If(value) => {
            for (condition, branch) in &mut value.branches {
                instantiate_expression_in_place(condition, iterator, coordinate);
                instantiate_expression_in_place(branch, iterator, coordinate);
            }
            instantiate_expression_in_place(&mut value.else_value, iterator, coordinate);
        }
        Expression::Array(values) => {
            for value in values {
                instantiate_expression_in_place(value, iterator, coordinate);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            instantiate_expression_in_place(lhs, iterator, coordinate);
            instantiate_expression_in_place(rhs, iterator, coordinate);
        }
    }
}

fn instantiate_reference(
    reference: &mut crate::ast::Reference,
    iterator: &crate::ast::Name,
    coordinate: i64,
) {
    let parts = match reference {
        crate::ast::Reference::Local(part) => std::slice::from_mut(part),
        crate::ast::Reference::State(parts) => parts,
    };
    for part in parts {
        for subscript in &mut part.subscripts {
            instantiate_expression_in_place(subscript, iterator, coordinate);
        }
    }
}
