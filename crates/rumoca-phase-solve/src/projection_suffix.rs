#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct OutputProjectionSuffix {
    pub(crate) output_name: String,
    pub(crate) output_field: Option<String>,
    pub(crate) indices: Vec<usize>,
}

pub(crate) fn parse_output_projection_suffix(suffix: &str) -> Option<OutputProjectionSuffix> {
    if suffix.is_empty() {
        return None;
    }

    let (output_with_field, indices) = match rumoca_core::parse_scalar_name(suffix) {
        Some(scalar) => {
            let indices = scalar
                .indices
                .into_iter()
                .map(|index| usize::try_from(index).ok().filter(|index| *index > 0))
                .collect::<Option<Vec<_>>>()?;
            (scalar.base, indices)
        }
        None if rumoca_core::split_trailing_subscript_suffix(suffix).is_some() => return None,
        None if rumoca_core::component_path_base_name(suffix).is_none() => return None,
        None => (suffix, Vec::new()),
    };

    if let Some((output_name, field)) = crate::path_utils::root_split(output_with_field) {
        if output_name.is_empty() || field.is_empty() {
            return None;
        }
        if indices.is_empty() && rumoca_core::split_trailing_subscript_suffix(output_name).is_some()
        {
            return None;
        }
        return Some(OutputProjectionSuffix {
            output_name: output_name.to_string(),
            output_field: Some(field.to_string()),
            indices,
        });
    }

    Some(OutputProjectionSuffix {
        output_name: output_with_field.to_string(),
        output_field: None,
        indices,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_output_projection_suffixes_with_structured_scalar_names() {
        assert_eq!(
            parse_output_projection_suffix("out").expect("plain output"),
            OutputProjectionSuffix {
                output_name: "out".to_string(),
                output_field: None,
                indices: vec![],
            }
        );
        assert_eq!(
            parse_output_projection_suffix("out.re[2, 3]").expect("fielded output"),
            OutputProjectionSuffix {
                output_name: "out".to_string(),
                output_field: Some("re".to_string()),
                indices: vec![2, 3],
            }
        );
        assert_eq!(
            parse_output_projection_suffix("out[index.with.dot].field[1]").expect("balanced base"),
            OutputProjectionSuffix {
                output_name: "out[index.with.dot]".to_string(),
                output_field: Some("field".to_string()),
                indices: vec![1],
            }
        );

        assert!(parse_output_projection_suffix("").is_none());
        assert!(parse_output_projection_suffix(".re").is_none());
        assert!(parse_output_projection_suffix("out.").is_none());
        assert!(parse_output_projection_suffix("out[0]").is_none());
        assert!(parse_output_projection_suffix("out[-1]").is_none());
        assert!(parse_output_projection_suffix("out[1").is_none());
        assert!(parse_output_projection_suffix("out[1].re").is_none());
    }
}
