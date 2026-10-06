use rumoca_core::{
    Expression, ExpressionRewriter, ExpressionVisitor, GeneratedBooleanLocal,
    GeneratedFunctionLocalKey, GeneratedFunctionLocals, GeneratedLocalError, Span,
};

/// Boolean composition introduced by normalization; source expressions still
/// require the ordinary function-expression typing and definedness checks.
#[derive(Debug, Clone, PartialEq)]
pub enum Guard<'locals> {
    Source(Expression),
    Local(GeneratedBooleanLocal<'locals>),
    Literal(bool),
    Not(Box<Self>),
    And(Box<Self>, Box<Self>),
    /// Later branch predicates are evaluated only if no earlier branch won.
    If {
        condition: Box<Self>,
        if_true: Box<Self>,
        if_false: Box<Self>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct GeneratedDefinition<'locals> {
    local: GeneratedBooleanLocal<'locals>,
    rhs: Guard<'locals>,
}

impl<'locals> GeneratedDefinition<'locals> {
    pub fn construct(
        locals: &GeneratedFunctionLocals<'_, 'locals>,
        target: GeneratedFunctionLocalKey<'locals>,
        rhs: Guard<'locals>,
    ) -> Result<Self, GeneratedLocalError> {
        rhs.check_locals(locals)?;
        Ok(Self {
            local: locals.declaration(target)?,
            rhs,
        })
    }

    pub const fn target(&self) -> GeneratedFunctionLocalKey<'locals> {
        self.local.key()
    }

    pub const fn span(&self) -> Span {
        self.local.provenance()
    }

    pub fn rhs(&self) -> &Guard<'locals> {
        &self.rhs
    }

    /// All dependencies precede the target kill in sequential dataflow.
    pub fn visit<V: GuardVisitor<'locals>>(&self, visitor: &mut V) {
        self.rhs.visit(visitor);
        visitor.visit_definition_target(self.target());
    }

    pub fn rewrite<R: ExpressionRewriter>(&self, rewriter: &mut R) -> Self {
        Self {
            local: self.local,
            rhs: self.rhs.rewrite(rewriter),
        }
    }
}

pub trait GuardVisitor<'locals>: ExpressionVisitor {
    fn visit_generated_read(&mut self, key: GeneratedFunctionLocalKey<'locals>);
    fn visit_definition_target(&mut self, key: GeneratedFunctionLocalKey<'locals>);
}

impl<'locals> Guard<'locals> {
    pub fn check_locals(
        &self,
        locals: &GeneratedFunctionLocals<'_, 'locals>,
    ) -> Result<(), GeneratedLocalError> {
        match self {
            Self::Local(local) => {
                locals.declaration(local.key())?;
            }
            Self::Not(value) => value.check_locals(locals)?,
            Self::And(left, right) => {
                left.check_locals(locals)?;
                right.check_locals(locals)?;
            }
            Self::If {
                condition,
                if_true,
                if_false,
            } => {
                condition.check_locals(locals)?;
                if_true.check_locals(locals)?;
                if_false.check_locals(locals)?;
            }
            Self::Source(_) | Self::Literal(_) => {}
        }
        Ok(())
    }

    pub fn visit<V: GuardVisitor<'locals>>(&self, visitor: &mut V) {
        match self {
            Self::Source(source) => visitor.visit_expression(source),
            Self::Local(local) => visitor.visit_generated_read(local.key()),
            Self::Literal(_) => {}
            Self::Not(value) => value.visit(visitor),
            Self::And(left, right) => {
                left.visit(visitor);
                right.visit(visitor);
            }
            Self::If {
                condition,
                if_true,
                if_false,
            } => {
                condition.visit(visitor);
                if_true.visit(visitor);
                if_false.visit(visitor);
            }
        }
    }

    pub fn rewrite<R: ExpressionRewriter>(&self, rewriter: &mut R) -> Self {
        match self {
            Self::Source(source) => Self::Source(rewriter.rewrite_expression(source)),
            Self::Local(local) => Self::Local(*local),
            Self::Literal(value) => Self::Literal(*value),
            Self::Not(value) => Self::Not(Box::new(value.rewrite(rewriter))),
            Self::And(left, right) => Self::And(
                Box::new(left.rewrite(rewriter)),
                Box::new(right.rewrite(rewriter)),
            ),
            Self::If {
                condition,
                if_true,
                if_false,
            } => Self::If {
                condition: Box::new(condition.rewrite(rewriter)),
                if_true: Box::new(if_true.rewrite(rewriter)),
                if_false: Box::new(if_false.rewrite(rewriter)),
            },
        }
    }
}
