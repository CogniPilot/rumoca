use rumoca_core::Span;
use rumoca_ir_dae as dae;

pub fn var_ref(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

pub fn var(name: &str) -> dae::Expression {
    var_ref(name)
}

pub fn real(v: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(v))
}

pub fn lit(v: f64) -> dae::Expression {
    real(v)
}

pub fn binop(op: dae::OpBinary, lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    binop(dae::OpBinary::Sub(Default::default()), lhs, rhs)
}

pub fn eq_from(rhs: dae::Expression) -> dae::Equation {
    dae::Equation {
        lhs: None,
        rhs,
        span: Span::DUMMY,
        origin: String::new(),
        scalar_count: 1,
    }
}

pub fn comp_ref(name: &str) -> dae::ComponentReference {
    dae::ComponentReference {
        local: false,
        parts: name
            .split('.')
            .map(|ident| dae::ComponentRefPart {
                ident: ident.to_string(),
                subs: Vec::new(),
            })
            .collect(),
        def_id: None,
    }
}
