//! The differentiable view of a parsed Modelica function.
//!
//! Synthesis reads a function through this view only: an ordered port list
//! with a declared type, a declared shape, and a role, plus the algorithm
//! statements. Anything the view cannot state exactly is refused rather than
//! guessed, so a synthesized derivative never rests on an assumed shape.

use rumoca_core::{Causality, ClassType, Variability};
use rumoca_ir_ast as ast;

use crate::refusal::{Refusable, Refusal, Rule, Site};
use crate::scope::FunctionScope;

/// Suffix that names the tangent companion of a declared variable.
pub(crate) const TANGENT_SUFFIX: &str = "_ad";

/// What a port contributes to differentiation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ValueKind {
    /// `Real`: carries a tangent.
    Differentiable,
    /// `Integer`, `Boolean`, `String`: constant under differentiation.
    Constant,
}

/// Where a port sits in the function signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PortRole {
    /// Declared `input`.
    Input,
    /// Declared `output`.
    Output,
    /// Neither: a body-local variable.
    Local,
}

/// One declared dimension of a port.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Dimension {
    /// A dimension the declaration states as an expression.
    Stated(String),
    /// A `:` dimension, sized by the actual argument.
    Deferred,
}

/// A declared variable of a function, as differentiation sees it.
#[derive(Debug, Clone)]
pub(crate) struct Port {
    /// Declared name.
    pub(crate) name: String,
    /// Declared type name, verbatim.
    pub(crate) type_text: String,
    /// Declared variability prefix, verbatim and space-terminated; empty when
    /// the declaration states none.
    pub(crate) variability_text: String,
    /// Declared dimensions, in order.
    pub(crate) dims: Vec<Dimension>,
    /// Signature role.
    pub(crate) role: PortRole,
    /// Differentiability of the declaration.
    pub(crate) kind: ValueKind,
    /// The declaration's binding expression, when it states one.
    pub(crate) binding: Option<ast::Expression>,
}

impl Port {
    /// Declared rank.
    pub(crate) fn rank(&self) -> usize {
        self.dims.len()
    }

    /// The primal's declaration text, without a causality prefix.
    ///
    /// A binding is part of the declaration and computes the value the body
    /// reads, so a redeclaration that drops it declares a different variable.
    pub(crate) fn primal_declaration(&self) -> String {
        format!(
            "{}{}{}{}{}",
            self.variability_text,
            self.type_text,
            self.name,
            self.dimension_text(),
            self.binding_text()
        )
    }

    /// The tangent companion's declaration text.
    ///
    /// A companion is assigned by the generated body, so it takes neither the
    /// primal's variability prefix, which would forbid the assignment, nor its
    /// binding, which is the primal's value and not the tangent's.
    pub(crate) fn tangent_declaration(&self) -> String {
        format!(
            "{}{}{}",
            self.type_text,
            self.tangent_name(),
            self.dimension_text()
        )
    }

    /// The ` = binding` half of a declaration, empty when there is none.
    fn binding_text(&self) -> String {
        match &self.binding {
            Some(expression) => format!(" = {expression}"),
            None => String::new(),
        }
    }

    /// The bracketed dimension list, empty for a scalar.
    pub(crate) fn dimension_text(&self) -> String {
        if self.dims.is_empty() {
            return String::new();
        }
        let dims: Vec<&str> = self
            .dims
            .iter()
            .map(|dim| match dim {
                Dimension::Stated(text) => text.as_str(),
                Dimension::Deferred => ":",
            })
            .collect();
        format!("[{}]", dims.join(", "))
    }

    /// The tangent companion's name.
    pub(crate) fn tangent_name(&self) -> String {
        format!("{}{TANGENT_SUFFIX}", self.name)
    }
}

/// A parsed function, viewed as a differentiation subject.
#[derive(Debug, Clone)]
pub(crate) struct FunctionModel {
    /// Declared function name.
    pub(crate) name: String,
    /// Ports in declaration order.
    pub(crate) ports: Vec<Port>,
    /// Body statements, in the order the algorithm sections state them.
    pub(crate) statements: Vec<ast::Statement>,
}

impl FunctionModel {
    /// Ports in a given role, in declaration order.
    pub(crate) fn ports_with(&self, role: PortRole) -> impl Iterator<Item = &Port> {
        self.ports.iter().filter(move |port| port.role == role)
    }

    /// The port a name denotes, if the function declares one.
    pub(crate) fn port(&self, name: &str) -> Option<&Port> {
        self.ports.iter().find(|port| port.name == name)
    }

    /// Read a parsed function class into the differentiable view.
    pub(crate) fn read(
        class: &ast::ClassDef,
        scope: &FunctionScope<'_>,
        owner: &[String],
        site: &Site,
    ) -> Refusable<Self> {
        let name = class.name.text.to_string();
        if class.class_type != ClassType::Function {
            return Err(Refusal::new(
                Rule::CalleeLookup,
                site.clone(),
                format!(
                    "`{name}` is a {} and not a function",
                    class.class_type.as_str()
                ),
            ));
        }
        if class.external.is_some() {
            return Err(Refusal::new(
                Rule::CalleeTangent,
                site.clone(),
                format!("`{name}` is an external function and states no derivative rule"),
            ));
        }
        if !class.equations.is_empty() || !class.initial_equations.is_empty() {
            return Err(Refusal::new(
                Rule::StatementForm,
                site.clone(),
                format!("`{name}` has an equation section"),
            ));
        }
        if !class.initial_algorithms.is_empty() {
            return Err(Refusal::new(
                Rule::StatementForm,
                site.clone(),
                format!("`{name}` has an initial algorithm section"),
            ));
        }
        if !class.extends.is_empty() {
            return Err(Refusal::new(
                Rule::Signature,
                site.clone(),
                format!("`{name}` extends another class"),
            ));
        }
        let ports = read_ports(class, scope, owner, site)?;
        check_tangent_name_collisions(&ports, &name, site)?;
        Ok(Self {
            name,
            ports,
            statements: class.algorithms.iter().flatten().cloned().collect(),
        })
    }
}

fn read_ports(
    class: &ast::ClassDef,
    scope: &FunctionScope<'_>,
    owner: &[String],
    site: &Site,
) -> Refusable<Vec<Port>> {
    let mut ports = Vec::new();
    for component in class.components.values() {
        ports.push(read_port(class, component, scope, owner, site)?);
    }
    Ok(ports)
}

fn read_port(
    class: &ast::ClassDef,
    component: &ast::Component,
    scope: &FunctionScope<'_>,
    owner: &[String],
    site: &Site,
) -> Refusable<Port> {
    let type_text = component.type_name.to_string();
    let [type_part] = component.type_name.name.as_slice() else {
        return Err(Refusal::new(
            Rule::DifferentiableType,
            site.clone(),
            format!(
                "`{}` is declared `{type_text}`; only simple predefined `Real`, `Integer`, \
                 `Boolean`, and `String` port types are admitted",
                component.name
            ),
        ));
    };
    let spelling = type_part.text.as_ref();
    let declared = match spelling {
        "Real" => ValueKind::Differentiable,
        "Integer" | "Boolean" | "String" => ValueKind::Constant,
        _ => {
            return Err(Refusal::new(
                Rule::DifferentiableType,
                site.clone(),
                format!(
                    "`{}` is declared `{type_text}`; only predefined `Real`, `Integer`, \
                     `Boolean`, and `String` port types are admitted",
                    component.name
                ),
            ));
        }
    };
    if let Some(reason) = scope.predefined_type_shadow_reason(class, owner, spelling) {
        return Err(Refusal::new(
            Rule::CalleeLookup,
            site.clone(),
            format!(
                "cannot prove that `{}`'s `{spelling}` denotes the predefined type: {reason}",
                component.name
            ),
        ));
    };
    let role = match component.causality {
        Causality::Input(_) => PortRole::Input,
        Causality::Output(_) => PortRole::Output,
        Causality::Empty => PortRole::Local,
    };
    // A variability prefix that forbids a value from changing forbids it from
    // moving too, whatever its type says, so it carries no tangent and gets no
    // companion. JAC-R9 refuses the declaration whose binding contradicts that.
    let kind = if holds_still(&component.variability) {
        ValueKind::Constant
    } else {
        declared
    };
    Ok(Port {
        name: component.name.clone(),
        type_text: format!("{type_text} "),
        variability_text: variability_text(&component.variability),
        dims: component
            .shape_expr
            .iter()
            .map(|subscript| read_dimension(subscript, component, site))
            .collect::<Refusable<Vec<_>>>()?,
        role,
        kind,
        binding: component.binding.clone(),
    })
}

/// Whether a variability prefix states that a value cannot change.
pub(crate) fn holds_still(variability: &Variability) -> bool {
    matches!(
        variability,
        Variability::Constant(_) | Variability::Parameter(_)
    )
}

/// The keyword a variability prefix is written with, space-terminated.
fn variability_text(variability: &Variability) -> String {
    match variability {
        Variability::Constant(_) => "constant ".to_string(),
        Variability::Parameter(_) => "parameter ".to_string(),
        Variability::Discrete(_) => "discrete ".to_string(),
        // Continuous is the absence of a prefix: Modelica spells no keyword
        // for it, so a declaration that carries it is written plainly.
        Variability::Continuous(_) | Variability::Empty => String::new(),
    }
}

fn read_dimension(
    subscript: &ast::Subscript,
    component: &ast::Component,
    site: &Site,
) -> Refusable<Dimension> {
    if let Some(violation) = ast::declaration_subscript_required_value_violation(subscript) {
        let owner = Site::at(&site.file, Some(&component.location));
        let exact = violation
            .span
            .map(|span| Site::with_span(&site.file, Some(&component.location), span));
        return Err(Refusal::new(
            Rule::ExpressionForm,
            exact.unwrap_or(owner),
            format!(
                "the declared shape of `{}` contains {}",
                component.name,
                violation.kind.description()
            ),
        ));
    }
    match subscript {
        ast::Subscript::Expression(expression) => Ok(Dimension::Stated(expression.to_string())),
        ast::Subscript::Range { .. } => Ok(Dimension::Deferred),
        ast::Subscript::Empty => unreachable!("recovery shape rejected above"),
    }
}

/// The generated tangent name of every port must be free in the function.
fn check_tangent_name_collisions(ports: &[Port], function: &str, site: &Site) -> Refusable<()> {
    for port in ports {
        let tangent = port.tangent_name();
        if ports.iter().any(|other| other.name == tangent) {
            return Err(Refusal::new(
                Rule::NameCollision,
                site.clone(),
                format!(
                    "`{function}` declares `{tangent}`, the tangent companion of `{}`",
                    port.name
                ),
            ));
        }
    }
    Ok(())
}
