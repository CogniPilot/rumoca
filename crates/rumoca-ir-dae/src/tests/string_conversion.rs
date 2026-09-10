use super::*;

const STRING_DECLARATION: rumoca_core::DefId = rumoca_core::DefId(41);

#[test]
fn string_conversion_round_trips_exact_identity_operands_and_provenance() {
    let source = TestSource::new(
        "String(x, minimumLength = width, leftJustified = left, significantDigits = digits); \
         String(i, format = format);",
    );
    let options_at = source.source(
        "String(x, minimumLength = width, leftJustified = left, significantDigits = digits)",
        0,
    );
    let explicit_at = source.source("String(i, format = format)", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.register_predefined_string(STRING_DECLARATION)?;
        dae.expressions(|expressions| {
            let value = expressions.at(options_at).literal(DaeLiteral::Real(12.5))?;
            let width = expressions.at(options_at).literal(DaeLiteral::Integer(8))?;
            let left = expressions
                .at(options_at)
                .literal(DaeLiteral::Boolean(false))?;
            let digits = expressions.at(options_at).literal(DaeLiteral::Integer(3))?;
            expressions.at(options_at).string_conversion(
                STRING_DECLARATION,
                value,
                StringConversionFormatInput::Options {
                    minimum_length: Some(width),
                    left_justified: Some(left),
                    significant_digits: Some(digits),
                },
            )?;

            let integer = expressions
                .at(explicit_at)
                .literal(DaeLiteral::Integer(12))?;
            let format = expressions
                .at(explicit_at)
                .literal(DaeLiteral::String("04d".to_owned()))?;
            expressions.at(explicit_at).string_conversion(
                STRING_DECLARATION,
                integer,
                StringConversionFormatInput::Format { value: format },
            )?;
            Ok(())
        })
    })
    .expect("well-typed String conversions construct");

    assert_string_conversion_round_trip(&dae);
    let json = serde_json::to_string(&dae).expect("checked DAE serializes");
    let decoded: Dae = serde_json::from_str(&json).expect("wire replays checked constructors");
    assert_string_conversion_round_trip(&decoded);
    assert_eq!(
        serde_json::to_string(&decoded).expect("decoded DAE serializes"),
        json
    );
}

fn assert_string_conversion_round_trip(dae: &Dae) {
    dae.inspect(|view| {
        assert_eq!(
            view.predefined_string_declaration(),
            Some(STRING_DECLARATION)
        );
        let conversions = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                matches!(
                    expression.operation(),
                    ExpressionOperation::StringConversion { .. }
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(conversions.len(), 2);
        assert_eq!(
            view.source_text(conversions[0].provenance()),
            Some(
                "String(x, minimumLength = width, leftJustified = left, \
                 significantDigits = digits)"
            )
        );
        assert!(matches!(
            conversions[0].operation(),
            ExpressionOperation::StringConversion {
                declaration: STRING_DECLARATION,
                format: StringConversionFormatView::Options {
                    minimum_length: Some(_),
                    left_justified: Some(_),
                    significant_digits: Some(_),
                },
                ..
            }
        ));
        assert_eq!(
            view.source_text(conversions[1].provenance()),
            Some("String(i, format = format)")
        );
        assert!(matches!(
            conversions[1].operation(),
            ExpressionOperation::StringConversion {
                declaration: STRING_DECLARATION,
                format: StringConversionFormatView::Format { .. },
                ..
            }
        ));
    });
}

#[test]
fn string_conversion_requires_registered_exact_declaration() {
    let source = TestSource::new("String(1)");
    let provenance = source.source("String(1)", 0);
    let error = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let value = expressions.at(provenance).literal(DaeLiteral::Integer(1))?;
            expressions.at(provenance).string_conversion(
                STRING_DECLARATION,
                value,
                StringConversionFormatInput::Options {
                    minimum_length: None,
                    left_justified: None,
                    significant_digits: None,
                },
            )?;
            Ok(())
        })
    })
    .expect_err("a conversion cannot establish its own predefined identity");

    assert_eq!(
        error,
        DaeConstructionError::MissingPredefinedString {
            span: provenance.span()
        }
    );
}

#[test]
fn enumeration_cannot_masquerade_as_integer_string_conversion() {
    let source = TestSource::new("String(color)");
    let provenance = source.source("String(color)", 0);
    let error = Dae::construct(source.map, |dae| {
        dae.register_predefined_string(STRING_DECLARATION)?;
        dae.expressions(|expressions| {
            let value = expressions.at(provenance).enumeration_literal(2)?;
            expressions.at(provenance).string_conversion(
                STRING_DECLARATION,
                value,
                StringConversionFormatInput::Options {
                    minimum_length: None,
                    left_justified: None,
                    significant_digits: None,
                },
            )?;
            Ok(())
        })
    })
    .expect_err("an enumeration needs owner-aware name formatting, not integer formatting");

    assert_eq!(
        error,
        DaeConstructionError::InvalidStringConversionSource {
            found: ScalarType::Enumeration,
            span: provenance.span(),
        }
    );
}
