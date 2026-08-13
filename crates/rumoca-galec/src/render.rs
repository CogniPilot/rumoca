// render.rs, renders a Galec IR to .alg / .h / .c text via embedded Jinja templates

use crate::galec::{Galec, GalecLiteral};
use minijinja::{Environment, Error};
use serde_json::Value;

const ALG_TEMPLATE: &str = include_str!("templates/galec.alg.jinja");
const H_TEMPLATE: &str = include_str!("templates/model.h.jinja");
const C_TEMPLATE: &str = include_str!("templates/model.c.jinja");
const RS_TEMPLATE: &str = include_str!("templates/model.rs.jinja");

pub fn render(galec: &Galec) -> Result<String, Error> {
    run_template("galec.alg", ALG_TEMPLATE, base_ctx(galec))
}

pub fn render_c_header(galec: &Galec) -> Result<String, Error> {
    run_template("model.h", H_TEMPLATE, c_ctx(galec))
}

pub fn render_c_source(galec: &Galec) -> Result<String, Error> {
    run_template("model.c", C_TEMPLATE, c_ctx(galec))
}

pub fn render_rust(galec: &Galec) -> Result<String, Error> {
    run_template("model.rs", RS_TEMPLATE, rust_ctx(galec))
}

fn run_template(name: &'static str, src: &'static str, ctx: Value) -> Result<String, Error> {
    let mut env = Environment::new();
    env.add_template(name, src)?;
    env.get_template(name)?.render(ctx)
}

fn base_ctx(galec: &Galec) -> Value {
    serde_json::to_value(galec).expect("Galec is always serializable")
}

fn c_ctx(galec: &Galec) -> Value {
    let mut ctx = base_ctx(galec);
    ctx["name_upper"] = galec.name.to_uppercase().into();
    let prev: Vec<String> = crate::manifest::collect_prev_refs(galec)
        .into_iter()
        .map(|(name, _)| name)
        .collect();
    ctx["previous_vars"] = prev.into();
    ctx
}

fn rust_ctx(galec: &Galec) -> Value {
    let mut ctx = c_ctx(galec);
    let integer_vars: Vec<String> = galec
        .variables
        .iter()
        .filter(|v| matches!(v.ty, GalecLiteral::Integer(_)))
        .map(|v| v.name.clone())
        .collect();
    ctx["integer_vars"] = integer_vars.into();
    ctx
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::galec::*;

    fn simple_galec() -> Galec {
        Galec {
            name: "Counter".into(),
            period: 0.001,
            variables: vec![
                GalecVariable {
                    name: "u".into(),
                    role: DataRole::Input,
                    ty: GalecLiteral::Real(0.0),
                    dims: vec![],
                    start: None,
                },
                GalecVariable {
                    name: "y".into(),
                    role: DataRole::Output,
                    ty: GalecLiteral::Real(0.0),
                    dims: vec![],
                    start: Some(GalecExpr::Literal(GalecLiteral::Real(0.0))),
                },
                GalecVariable {
                    name: "count".into(),
                    role: DataRole::State,
                    ty: GalecLiteral::Real(0.0),
                    dims: vec![],
                    start: Some(GalecExpr::Literal(GalecLiteral::Real(0.0))),
                },
            ],
            startup: vec![
                GalecStatement::Assign {
                    lhs: "count".into(),
                    rhs: GalecExpr::Literal(GalecLiteral::Real(0.0)),
                },
                GalecStatement::Assign {
                    lhs: "y".into(),
                    rhs: GalecExpr::Literal(GalecLiteral::Real(0.0)),
                },
            ],
            recalibrate: vec![],
            do_step: vec![
                // unconditional: always increment count
                GalecGroup {
                    condition: None,
                    statements: vec![GalecStatement::Assign {
                        lhs: "count".into(),
                        rhs: GalecExpr::Binary {
                            op: GalecBinaryOp::Add,
                            lhs: Box::new(GalecExpr::PreviousRef {
                                name: "count".into(),
                            }),
                            rhs: Box::new(GalecExpr::Literal(GalecLiteral::Real(1.0))),
                        },
                    }],
                },
                // guarded: when u > 0, scale count by u and forward to y
                GalecGroup {
                    condition: Some(GalecExpr::Binary {
                        op: GalecBinaryOp::Gt,
                        lhs: Box::new(GalecExpr::SelfRef { name: "u".into() }),
                        rhs: Box::new(GalecExpr::Literal(GalecLiteral::Real(0.0))),
                    }),
                    statements: vec![
                        GalecStatement::Assign {
                            lhs: "count".into(),
                            rhs: GalecExpr::Binary {
                                op: GalecBinaryOp::Mul,
                                lhs: Box::new(GalecExpr::SelfRef {
                                    name: "count".into(),
                                }),
                                rhs: Box::new(GalecExpr::SelfRef { name: "u".into() }),
                            },
                        },
                        GalecStatement::Assign {
                            lhs: "y".into(),
                            rhs: GalecExpr::SelfRef {
                                name: "count".into(),
                            },
                        },
                        GalecStatement::Assign {
                            lhs: "y".into(),
                            rhs: GalecExpr::Binary {
                                op: GalecBinaryOp::Add,
                                lhs: Box::new(GalecExpr::SelfRef { name: "y".into() }),
                                rhs: Box::new(GalecExpr::Literal(GalecLiteral::Real(1.0))),
                            },
                        },
                    ],
                },
            ],
        }
    }

    #[test]
    fn print_alg() {
        println!("{}", render(&simple_galec()).unwrap());
    }

    #[test]
    fn print_c() {
        let galec = simple_galec();
        println!("=== Counter.h ===\n{}", render_c_header(&galec).unwrap());
        println!("=== Counter.c ===\n{}", render_c_source(&galec).unwrap());
    }

    #[test]
    fn print_rust() {
        println!(
            "=== Counter.rs ===\n{}",
            render_rust(&simple_galec()).unwrap()
        );
    }

    #[test]
    fn render_produces_block_keyword() {
        let galec = simple_galec();
        let out = render(&galec).unwrap();
        assert!(
            out.contains("block Counter"),
            "output must start with `block Counter`"
        );
        assert!(
            out.contains("end Counter;"),
            "output must end with `end Counter;`"
        );
    }

    #[test]
    fn render_contains_method_sections() {
        let out = render(&simple_galec()).unwrap();
        assert!(out.contains("method Startup"));
        assert!(out.contains("end Startup;"));
        assert!(out.contains("method Recalibrate"));
        assert!(out.contains("end Recalibrate;"));
        assert!(out.contains("method DoStep"));
        assert!(out.contains("end DoStep;"));
    }

    #[test]
    fn render_interface_declarations() {
        let out = render(&simple_galec()).unwrap();
        assert!(out.contains("input Real u;"));
        assert!(out.contains("output Real y;"));
    }

    #[test]
    fn render_state_in_protected() {
        let out = render(&simple_galec()).unwrap();
        // State goes in protected section (after `protected` line)
        let protected_pos = out.find("protected").unwrap();
        let public_pos = out.find("public").unwrap();
        let count_pos = out.find("Real count;").unwrap();
        assert!(
            count_pos > protected_pos && count_pos < public_pos,
            "state variable must appear in protected section"
        );
    }

    #[test]
    fn render_assignment_uses_self_prefix() {
        let out = render(&simple_galec()).unwrap();
        assert!(
            out.contains("self.count :="),
            "assignments must use self. prefix"
        );
    }

    #[test]
    fn render_previous_ref() {
        let out = render(&simple_galec()).unwrap();
        assert!(
            out.contains("previous(self.count)"),
            "pre() must render as previous(self.x)"
        );
    }

    #[test]
    fn render_guarded_group() {
        let galec = Galec {
            name: "Guarded".into(),
            period: 0.01,
            variables: vec![GalecVariable {
                name: "x".into(),
                role: DataRole::State,
                ty: GalecLiteral::Real(0.0),
                dims: vec![],
                start: None,
            }],
            startup: vec![],
            recalibrate: vec![],
            do_step: vec![GalecGroup {
                condition: Some(GalecExpr::SelfRef {
                    name: "enable".into(),
                }),
                statements: vec![GalecStatement::Assign {
                    lhs: "x".into(),
                    rhs: GalecExpr::Literal(GalecLiteral::Real(1.0)),
                }],
            }],
        };
        let out = render(&galec).unwrap();
        assert!(
            out.contains("if self.enable then"),
            "guarded group must produce if statement"
        );
        assert!(out.contains("end if;"));
    }
}
