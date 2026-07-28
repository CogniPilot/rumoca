//! Derived scalar views of compact AST connection families.

use std::borrow::Cow;

use rumoca_ir_ast as ast;

/// Borrowing scalar compatibility view over a class instance's connections.
///
/// Compact families stay authoritative in Instance IR (SPEC_0032 §1). This
/// iterator yields the scalar view a consumer boundary still needs without
/// materializing it: family-free connections are borrowed, and each domain
/// point of a family is produced on demand in the same ordinal order as
/// [`scalar_connection_members`].
pub struct ScalarConnectionView<'a> {
    source: &'a [ast::InstanceConnection],
    position: usize,
    ordinal: usize,
    count: usize,
}

/// Iterate the scalar compatibility view of a connection list.
pub fn scalar_connection_view(connections: &[ast::InstanceConnection]) -> ScalarConnectionView<'_> {
    ScalarConnectionView {
        source: connections,
        position: 0,
        ordinal: 0,
        count: 0,
    }
}

impl ScalarConnectionView<'_> {
    /// Cache the domain cardinality when the cursor enters a new family.
    fn begin_family(&mut self, family: &ast::InstanceConnectionFamily) -> Result<(), String> {
        self.count = family
            .domain
            .scalar_count()
            .map_err(|error| format!("invalid connection family domain: {error}"))?;
        Ok(())
    }

    /// Move the cursor to the next source connection.
    fn advance_connection(&mut self) {
        self.position += 1;
        self.ordinal = 0;
    }
}

impl<'a> Iterator for ScalarConnectionView<'a> {
    type Item = Result<Cow<'a, ast::InstanceConnection>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let source = self.source;
        loop {
            let connection = source.get(self.position)?;
            let Some(family) = &connection.family else {
                self.advance_connection();
                return Some(Ok(Cow::Borrowed(connection)));
            };
            if self.ordinal == 0
                && let Err(message) = self.begin_family(family)
            {
                self.advance_connection();
                return Some(Err(message));
            }
            if self.ordinal >= self.count {
                self.advance_connection();
                continue;
            }
            let ordinal = self.ordinal;
            self.ordinal += 1;
            return Some(family_member(connection, family, ordinal).map(Cow::Owned));
        }
    }
}

fn family_member(
    connection: &ast::InstanceConnection,
    family: &ast::InstanceConnectionFamily,
    ordinal: usize,
) -> Result<ast::InstanceConnection, String> {
    let tuple = family
        .domain
        .index_tuple_at(ordinal)
        .map_err(|error| format!("invalid connection family domain: {error}"))?
        .ok_or_else(|| format!("connection family is missing domain ordinal {ordinal}"))?;
    Ok(ast::InstanceConnection {
        a: evaluate_connection_endpoint(&family.a, &tuple)?,
        b: evaluate_connection_endpoint(&family.b, &tuple)?,
        connector_type: connection.connector_type,
        span: connection.span,
        scope: connection.scope.clone(),
        family: None,
    })
}

/// Materialize the scalar compatibility view of an instance connection.
///
/// Compact families remain authoritative in Instance IR. Callers use this
/// derived view only at a consumer boundary that still requires an owned
/// scalar connection list; prefer [`scalar_connection_view`] otherwise.
pub fn scalar_connection_members(
    connection: &ast::InstanceConnection,
) -> Result<Vec<ast::InstanceConnection>, String> {
    let single = std::slice::from_ref(connection);
    let mut members = Vec::new();
    if let Some(family) = &connection.family {
        let count = family
            .domain
            .scalar_count()
            .map_err(|error| format!("invalid connection family domain: {error}"))?;
        members
            .try_reserve_exact(count)
            .map_err(|_| "connection family scalar view exceeds host memory limits".to_string())?;
    }
    for member in scalar_connection_view(single) {
        members.push(member?.into_owned());
    }
    Ok(members)
}

/// Evaluate one compact connection endpoint for a domain index tuple.
pub fn evaluate_connection_endpoint(
    endpoint: &ast::InstanceConnectionEndpoint,
    binders: &[i64],
) -> Result<ast::QualifiedName, String> {
    let mut result = ast::QualifiedName::new();
    for (name, subscripts) in &endpoint.parts {
        let mut values = Vec::new();
        values.try_reserve_exact(subscripts.len()).map_err(|_| {
            "connection endpoint subscript count exceeds host memory limits".to_string()
        })?;
        for subscript in subscripts {
            values.push(evaluate_affine_form(subscript, binders)?);
        }
        result.push(name.clone(), values);
    }
    Ok(result)
}

fn evaluate_affine_form(form: &rumoca_core::AffineForm, binders: &[i64]) -> Result<i64, String> {
    if form.coeffs.len() != binders.len() {
        return Err(format!(
            "connection endpoint affine rank {} does not match domain rank {}",
            form.coeffs.len(),
            binders.len()
        ));
    }
    let value = form
        .coeffs
        .iter()
        .zip(binders)
        .try_fold(i128::from(form.constant), |value, (coefficient, binder)| {
            value.checked_add(i128::from(*coefficient) * i128::from(*binder))
        })
        .ok_or_else(|| "connection endpoint affine subscript overflows i128".to_string())?;
    i64::try_from(value).map_err(|_| "connection endpoint affine subscript exceeds i64".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{AffineForm, BytePos, Span, StructuredIndexBinder, StructuredIndexDomain};

    fn span() -> Span {
        Span::new(
            rumoca_core::SourceId::from_source_name("connection_view_tests"),
            BytePos(0),
            BytePos(1),
        )
    }

    fn binder(id: usize, upper: i64) -> StructuredIndexBinder {
        StructuredIndexBinder {
            id,
            display_name: format!("i{id}"),
            lower: 1,
            upper,
            step: 1,
        }
    }

    fn endpoint(
        name: &str,
        member: &str,
        binder_index: usize,
        rank: usize,
    ) -> ast::InstanceConnectionEndpoint {
        ast::InstanceConnectionEndpoint {
            parts: vec![
                (
                    name.to_string(),
                    vec![AffineForm::unit_binder(binder_index, rank)],
                ),
                (member.to_string(), Vec::new()),
            ],
        }
    }

    fn rank_two_family_connection() -> ast::InstanceConnection {
        ast::InstanceConnection {
            a: ast::QualifiedName::from_ident("placeholder_a"),
            b: ast::QualifiedName::from_ident("placeholder_b"),
            connector_type: None,
            span: span(),
            scope: "root".to_string(),
            family: Some(ast::InstanceConnectionFamily {
                domain: StructuredIndexDomain {
                    binders: vec![binder(0, 2), binder(1, 3)],
                },
                a: endpoint("a", "p", 0, 2),
                b: endpoint("b", "p", 1, 2),
            }),
        }
    }

    fn family_free_connection() -> ast::InstanceConnection {
        ast::InstanceConnection {
            a: ast::QualifiedName::from_ident("x"),
            b: ast::QualifiedName::from_ident("y"),
            connector_type: None,
            span: span(),
            scope: String::new(),
            family: None,
        }
    }

    /// The scalar view of `rank_two_family_connection`, written out literally.
    ///
    /// `scalar_connection_members` is implemented on top of
    /// `scalar_connection_view`, so comparing the two proves nothing. SPEC_0032
    /// §2 fixes this order: binder declaration order, lexicographic, innermost
    /// binder varying fastest — endpoint `a` is subscripted by binder 0 and
    /// endpoint `b` by binder 1.
    fn expected_rank_two_members() -> Vec<(String, String)> {
        [
            ("a[1].p", "b[1].p"),
            ("a[1].p", "b[2].p"),
            ("a[1].p", "b[3].p"),
            ("a[2].p", "b[1].p"),
            ("a[2].p", "b[2].p"),
            ("a[2].p", "b[3].p"),
        ]
        .into_iter()
        .map(|(left, right)| (left.to_string(), right.to_string()))
        .collect()
    }

    fn rendered(members: &[ast::InstanceConnection]) -> Vec<(String, String)> {
        members
            .iter()
            .map(|member| (member.a.to_flat_string(), member.b.to_flat_string()))
            .collect()
    }

    #[test]
    fn scalar_connection_view_yields_the_documented_domain_order() {
        let connection = rank_two_family_connection();
        let viewed: Vec<ast::InstanceConnection> =
            scalar_connection_view(std::slice::from_ref(&connection))
                .map(|member| member.expect("view member").into_owned())
                .collect();
        assert_eq!(rendered(&viewed), expected_rank_two_members());
        // Derived members are scalar: they carry no family of their own.
        assert!(viewed.iter().all(|member| member.family.is_none()));
        assert!(viewed.iter().all(|member| member.scope == "root"));
    }

    #[test]
    fn scalar_connection_members_yields_the_documented_domain_order() {
        let connection = rank_two_family_connection();
        let members = scalar_connection_members(&connection).expect("scalar members");
        assert_eq!(rendered(&members), expected_rank_two_members());
        assert!(members.iter().all(|member| member.family.is_none()));
    }

    #[test]
    fn scalar_connection_members_passes_family_free_connections_through() {
        let connection = family_free_connection();
        let members = scalar_connection_members(&connection).expect("scalar members");
        assert_eq!(rendered(&members), vec![("x".to_string(), "y".to_string())]);
    }

    #[test]
    fn scalar_connection_view_borrows_family_free_connections() {
        let connections = vec![family_free_connection(), rank_two_family_connection()];
        let mut view = scalar_connection_view(&connections);
        let first = view.next().expect("first member").expect("no error");
        assert!(matches!(first, Cow::Borrowed(_)));
        let second = view.next().expect("second member").expect("no error");
        assert!(matches!(second, Cow::Owned(_)));
        // 1 borrowed + 6 owned family members.
        assert_eq!(view.count() + 2, 7);
    }

    #[test]
    fn scalar_connection_view_spans_multiple_connections_in_order() {
        let connections = vec![rank_two_family_connection(), family_free_connection()];
        let rendered: Vec<String> = scalar_connection_view(&connections)
            .map(|member| member.expect("no error").a.to_flat_string())
            .collect();
        assert_eq!(rendered.len(), 7);
        assert_eq!(rendered[0], "a[1].p");
        assert_eq!(rendered[6], "x");
    }
}
