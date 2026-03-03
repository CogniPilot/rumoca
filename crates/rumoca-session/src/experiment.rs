use rumoca_ir_ast as ast;

fn component_ref_last_ident(comp_ref: &ast::ComponentReference) -> Option<&str> {
    comp_ref.parts.last().map(|part| part.ident.text.as_ref())
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ExperimentSettings {
    pub(crate) start_time: Option<f64>,
    pub(crate) stop_time: Option<f64>,
    pub(crate) tolerance: Option<f64>,
    pub(crate) interval: Option<f64>,
    pub(crate) solver: Option<String>,
}

impl ExperimentSettings {
    fn merge(&mut self, other: Self) {
        if other.start_time.is_some() {
            self.start_time = other.start_time;
        }
        if other.stop_time.is_some() {
            self.stop_time = other.stop_time;
        }
        if other.tolerance.is_some() {
            self.tolerance = other.tolerance;
        }
        if other.interval.is_some() {
            self.interval = other.interval;
        }
        if other.solver.is_some() {
            self.solver = other.solver;
        }
    }
}

fn extract_numeric_literal(expr: &ast::Expression) -> Option<f64> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal | ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.parse::<f64>().ok(),
        ast::Expression::Unary {
            op: ast::OpUnary::Minus(_),
            rhs,
        } => extract_numeric_literal(rhs).map(|v| -v),
        ast::Expression::Unary {
            op: ast::OpUnary::Plus(_),
            rhs,
        } => extract_numeric_literal(rhs),
        ast::Expression::Parenthesized { inner } => extract_numeric_literal(inner),
        _ => None,
    }
}

fn extract_string_literal(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
        } => {
            let raw = token.text.as_ref().trim();
            let unquoted = raw.trim_matches('"').trim();
            if unquoted.is_empty() {
                None
            } else {
                Some(unquoted.to_string())
            }
        }
        ast::Expression::ComponentReference(comp_ref) => {
            let value = comp_ref
                .parts
                .iter()
                .map(|part| part.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            if value.is_empty() { None } else { Some(value) }
        }
        ast::Expression::Parenthesized { inner } => extract_string_literal(inner),
        _ => None,
    }
}

fn apply_om_simulation_flags_solver(value: &ast::Expression, settings: &mut ExperimentSettings) {
    let modifications = match value {
        ast::Expression::ClassModification { modifications, .. } => modifications.as_slice(),
        ast::Expression::FunctionCall { args, .. } => args.as_slice(),
        ast::Expression::Parenthesized { inner } => {
            apply_om_simulation_flags_solver(inner, settings);
            return;
        }
        _ => return,
    };

    for entry in modifications {
        match entry {
            ast::Expression::NamedArgument { name, value } => {
                let key = name.text.as_ref();
                if key.eq_ignore_ascii_case("s") || key.eq_ignore_ascii_case("solver") {
                    settings.solver = extract_string_literal(value);
                }
            }
            ast::Expression::Modification { target, value } => {
                if let Some(key) = component_ref_last_ident(target)
                    && (key.eq_ignore_ascii_case("s") || key.eq_ignore_ascii_case("solver"))
                {
                    settings.solver = extract_string_literal(value);
                }
            }
            _ => {}
        }
    }
}

fn apply_experiment_entry(key: &str, value: &ast::Expression, settings: &mut ExperimentSettings) {
    if key.eq_ignore_ascii_case("StartTime") {
        settings.start_time = extract_numeric_literal(value);
        return;
    }
    if key.eq_ignore_ascii_case("StopTime") {
        settings.stop_time = extract_numeric_literal(value);
        return;
    }
    if key.eq_ignore_ascii_case("Tolerance") {
        settings.tolerance = extract_numeric_literal(value);
        return;
    }
    if key.eq_ignore_ascii_case("Interval") {
        settings.interval = extract_numeric_literal(value);
        return;
    }

    if key.eq_ignore_ascii_case("Algorithm")
        || key.eq_ignore_ascii_case("Solver")
        || key.eq_ignore_ascii_case("__Dymola_Algorithm")
    {
        settings.solver = extract_string_literal(value);
        return;
    }

    if key.eq_ignore_ascii_case("__OpenModelica_simulationFlags") {
        apply_om_simulation_flags_solver(value, settings);
    }
}

fn extract_experiment_settings_from_modifications(
    modifications: &[ast::Expression],
) -> ExperimentSettings {
    let mut settings = ExperimentSettings::default();
    for expr in modifications {
        match expr {
            ast::Expression::NamedArgument { name, value } => {
                apply_experiment_entry(name.text.as_ref(), value, &mut settings);
            }
            ast::Expression::Modification { target, value } => {
                if let Some(key) = component_ref_last_ident(target) {
                    apply_experiment_entry(key, value, &mut settings);
                }
            }
            ast::Expression::FunctionCall { comp, .. }
                if component_ref_last_ident(comp).is_some_and(|key| {
                    key.eq_ignore_ascii_case("__OpenModelica_simulationFlags")
                }) =>
            {
                apply_om_simulation_flags_solver(expr, &mut settings);
            }
            ast::Expression::ClassModification { target, .. }
                if component_ref_last_ident(target).is_some_and(|key| {
                    key.eq_ignore_ascii_case("__OpenModelica_simulationFlags")
                }) =>
            {
                apply_om_simulation_flags_solver(expr, &mut settings);
            }
            _ => {}
        }
    }
    settings
}

fn extract_experiment_settings_from_annotation_expr(
    expr: &ast::Expression,
) -> Option<ExperimentSettings> {
    match expr {
        ast::Expression::ClassModification {
            target,
            modifications,
        } if component_ref_last_ident(target) == Some("experiment") => Some(
            extract_experiment_settings_from_modifications(modifications),
        ),
        ast::Expression::FunctionCall { comp, args }
            if component_ref_last_ident(comp) == Some("experiment") =>
        {
            Some(extract_experiment_settings_from_modifications(args))
        }
        ast::Expression::NamedArgument { name, value } if name.text.as_ref() == "experiment" => {
            extract_experiment_settings_from_annotation_expr(value)
        }
        ast::Expression::Modification { target, value }
            if component_ref_last_ident(target) == Some("experiment") =>
        {
            extract_experiment_settings_from_annotation_expr(value)
        }
        _ => None,
    }
}

fn sanitize_experiment_settings(mut settings: ExperimentSettings) -> ExperimentSettings {
    settings.start_time = settings.start_time.filter(|value| value.is_finite());
    settings.stop_time = settings
        .stop_time
        .filter(|value| value.is_finite() && *value >= 0.0);
    settings.tolerance = settings
        .tolerance
        .filter(|value| value.is_finite() && *value > 0.0);
    settings.interval = settings
        .interval
        .filter(|value| value.is_finite() && *value > 0.0);
    settings.solver = settings
        .solver
        .map(|value| value.trim().trim_matches('"').trim().to_string())
        .filter(|value| !value.is_empty());
    if let (Some(start), Some(stop)) = (settings.start_time, settings.stop_time)
        && stop < start
    {
        settings.stop_time = None;
    }
    settings
}

pub(crate) fn experiment_settings_for_model(
    tree: &ast::ClassTree,
    model_name: &str,
) -> ExperimentSettings {
    let Some(class) = tree.get_class_by_qualified_name(model_name) else {
        return ExperimentSettings::default();
    };
    let mut settings = ExperimentSettings::default();
    for expr in &class.annotation {
        if let Some(extracted) = extract_experiment_settings_from_annotation_expr(expr) {
            settings.merge(extracted);
        }
    }
    sanitize_experiment_settings(settings)
}
