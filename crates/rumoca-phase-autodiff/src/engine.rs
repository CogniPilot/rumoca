//! Forward-mode tangent propagation over function algorithm statements.
//!
//! The engine pairs every admitted statement with the statement that carries
//! its tangent, and every admitted expression with its directional derivative.
//! A tangent is emitted *before* the primal statement it belongs to, so the
//! derivative of `x := x + 1` reads the incoming `x`, which is the value the
//! chain rule is stated at.
//!
//! A structurally vanishing derivative is [`None`], not the text `0`: a term
//! that cannot move never enters the generated body, and a zero that must
//! keep an array shape is written as `0.0*(primal)` where the shape is what
//! makes it well typed.

use rumoca_ir_ast as ast;

use crate::model::{FunctionModel, Port, PortRole, ValueKind};
use crate::refusal::{Refusable, Refusal, Rule, Site};
use crate::scope::FunctionScope;

/// Name suffix of a synthesized tangent function.
pub(crate) const TANGENT_FUNCTION_SUFFIX: &str = "_ad_tangent";

/// Derivative text, or the structural zero.
type Slope = Option<String>;

/// Forward-mode transformer for one function body.
pub(crate) struct TangentBuilder<'a> {
    model: &'a FunctionModel,
    scope: &'a FunctionScope<'a>,
    site: Site,
    indices: Vec<String>,
    requested: Vec<String>,
}

impl<'a> TangentBuilder<'a> {
    /// A transformer for `model`, resolving called functions through `scope`.
    pub(crate) fn new(model: &'a FunctionModel, scope: &'a FunctionScope<'a>, site: Site) -> Self {
        Self {
            model,
            scope,
            site,
            indices: Vec::new(),
            requested: Vec::new(),
        }
    }

    /// Functions whose tangent this body called for, in first-request order.
    pub(crate) fn requested(&self) -> &[String] {
        &self.requested
    }

    /// The tangent statements the function's declaration bindings state.
    ///
    /// `Real t = 2.0*u[1];` is an assignment Modelica performs before the
    /// algorithm runs, so its tangent belongs there too, in declaration order.
    /// A binding whose own tangent vanishes still gets a statement: a
    /// companion the generated body never assigns is read as whatever the
    /// caller's memory holds, which is an all-zero Jacobian on a good day and
    /// an arbitrary one otherwise.
    pub(crate) fn declaration_tangents(&mut self, indent: usize) -> Refusable<Vec<String>> {
        let mut lines = Vec::new();
        for position in 0..self.model.ports.len() {
            let port = self.model.ports[position].clone();
            let Some(binding) = port.binding.clone() else {
                continue;
            };
            self.reject_expression_value(&binding, "declaration binding")?;
            self.check_binding_reach(&port, position, &binding)?;
            if port.kind == ValueKind::Constant {
                self.check_binding_holds_still(&port, &binding)?;
                continue;
            }
            // An input's companion is a formal of the generated function, and
            // the wrapper passes every argument, so an input's binding is a
            // default this expansion never takes.
            if port.role == PortRole::Input {
                continue;
            }
            let slope = self.derivative(&binding)?;
            lines.push(indented(
                indent,
                &format!(
                    "{} := {};",
                    port.tangent_name(),
                    slope.unwrap_or_else(|| zero_like(&binding))
                ),
            ));
        }
        Ok(lines)
    }

    /// A binding may read the ports declared before it, and nothing else.
    ///
    /// Binding tangents are stated in declaration order, so a binding reading a
    /// port declared at or after its own has no position this engine can put
    /// its tangent in.
    fn check_binding_reach(
        &self,
        port: &Port,
        position: usize,
        binding: &ast::Expression,
    ) -> Refusable<()> {
        let mut names = Vec::new();
        reads(binding, &mut names);
        for name in names {
            let Some(other) = self.model.ports.iter().position(|held| held.name == name) else {
                continue;
            };
            if other >= position {
                return Err(Refusal::new(
                    Rule::DeclarationBinding,
                    self.site.clone(),
                    format!(
                        "`{}` is bound by an expression reading `{name}`, which this function \
                         declares no earlier; a binding's tangent is stated in declaration order, \
                         so it can only read what is already declared",
                        port.name
                    ),
                ));
            }
        }
        Ok(())
    }

    /// A declaration that cannot change may not be bound to a value that moves.
    fn check_binding_holds_still(&self, port: &Port, binding: &ast::Expression) -> Refusable<()> {
        if !self.moves(binding) {
            return Ok(());
        }
        Err(Refusal::new(
            Rule::DeclarationBinding,
            self.site.clone(),
            format!(
                "`{}` is declared to hold still but is bound to `{binding}`, which carries a \
                 tangent; no companion is minted for it, so that tangent would be dropped",
                port.name
            ),
        ))
    }

    /// Transform the whole body at `indent` levels of indentation.
    pub(crate) fn body(
        &mut self,
        statements: &[ast::Statement],
        indent: usize,
    ) -> Refusable<Vec<String>> {
        let mut lines = Vec::new();
        for statement in statements {
            if !matches!(statement, ast::Statement::Empty)
                && let Some(violation) = ast::statement_required_value_violation(statement)
            {
                return Err(self.value_refusal(violation, "algorithm statement"));
            }
            self.statement_inner(statement, indent, &mut lines)?;
        }
        Ok(lines)
    }

    fn statement_inner(
        &mut self,
        statement: &ast::Statement,
        indent: usize,
        lines: &mut Vec<String>,
    ) -> Refusable<()> {
        match statement {
            ast::Statement::Assignment { comp, value } => {
                self.assignment(comp, value, indent, lines)
            }
            ast::Statement::For { indices, equations } => {
                self.for_loop(indices, equations, indent, lines)
            }
            ast::Statement::If {
                cond_blocks,
                else_block,
            } => self.conditional(cond_blocks, else_block.as_deref(), indent, lines),
            ast::Statement::Assert {
                condition,
                message,
                level,
            } => {
                lines.push(indented(
                    indent,
                    &assert_text(condition, message, level.as_deref()),
                ));
                Ok(())
            }
            // An assertion states a fact about the primal values and carries no
            // tangent of its own, so it is copied unchanged. The parser gives
            // it this shape when it is written as a call statement.
            ast::Statement::FunctionCall {
                comp,
                args,
                outputs,
            } if outputs.is_empty() && is_assertion(comp) => {
                let rendered: Vec<String> = args.iter().map(ToString::to_string).collect();
                lines.push(indented(
                    indent,
                    &format!("assert({});", rendered.join(", ")),
                ));
                Ok(())
            }
            other => Err(self.refuse_statement(other)),
        }
    }

    fn refuse_statement(&self, statement: &ast::Statement) -> Refusal {
        let described = match statement {
            ast::Statement::Empty => {
                "an empty statement node is parser recovery; an empty algorithm is an empty list"
            }
            ast::Statement::While(_) => "a while loop is data dependent and carries no tangent",
            ast::Statement::When(_) => "a when statement is not admitted in a function body",
            ast::Statement::Break { .. } => "break is not admitted in a differentiated body",
            ast::Statement::Return { .. } => "return is not admitted in a differentiated body",
            ast::Statement::Reinit { .. } => "reinit is not admitted in a function body",
            ast::Statement::FunctionCall { .. } => {
                "a call statement is not admitted; assign the call result instead"
            }
            _ => "this statement form carries no tangent",
        };
        Refusal::new(
            Rule::StatementForm,
            self.site_of(statement.get_location()),
            described,
        )
    }

    fn site_of(&self, location: Option<&rumoca_core::Location>) -> Site {
        match location {
            Some(at) => Site::at(&self.site.file, Some(at)),
            None => self.site.clone(),
        }
    }

    fn assignment(
        &mut self,
        comp: &ast::ComponentReference,
        value: &ast::Expression,
        indent: usize,
        lines: &mut Vec<String>,
    ) -> Refusable<()> {
        let slope = self.derivative_inner(value)?;
        let target = self.assignment_target(comp)?;
        match (target, slope) {
            (Some(tangent), Some(slope)) => {
                lines.push(indented(indent, &format!("{tangent} := {slope};")));
            }
            // A target that cannot move still needs its tangent defined, and
            // the value's own shape is the only shape that is right.
            (Some(tangent), None) => {
                lines.push(indented(
                    indent,
                    &format!("{tangent} := {};", zero_like(value)),
                ));
            }
            (None, Some(_)) => {
                return Err(Refusal::new(
                    Rule::DifferentiableType,
                    self.site_of(comp.get_location()),
                    format!("`{comp}` is not Real but is assigned a value that carries a tangent"),
                ));
            }
            (None, None) => {}
        }
        lines.push(indented(indent, &format!("{comp} := {value};")));
        Ok(())
    }

    /// The tangent left-hand side for an assignment target, or [`None`] when
    /// the target is a constant-typed variable.
    fn assignment_target(&self, comp: &ast::ComponentReference) -> Refusable<Option<String>> {
        let (name, subscripts) = self.single_part(comp)?;
        let Some(port) = self.model.port(name) else {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site_of(comp.get_location()),
                format!("`{name}` is assigned but not declared by the function"),
            ));
        };
        if port.kind == ValueKind::Constant {
            return Ok(None);
        }
        Ok(Some(format!("{}{subscripts}", port.tangent_name())))
    }

    /// The single reference part of `comp`, with its subscript text.
    fn single_part<'r>(&self, comp: &'r ast::ComponentReference) -> Refusable<(&'r str, String)> {
        let [part] = comp.parts.as_slice() else {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site_of(comp.get_location()),
                format!("`{comp}` reaches through a composite value"),
            ));
        };
        Ok((
            part.ident.text.as_ref(),
            subscript_text(part.subs.as_deref()),
        ))
    }

    fn for_loop(
        &mut self,
        indices: &[ast::ForIndex],
        body: &[ast::Statement],
        indent: usize,
        lines: &mut Vec<String>,
    ) -> Refusable<()> {
        for index in indices {
            self.check_loop_range(index)?;
        }
        let header = indices
            .iter()
            .map(|index| format!("{} in {}", index.ident.text, index.range))
            .collect::<Vec<_>>()
            .join(", ");
        lines.push(indented(indent, &format!("for {header} loop")));
        let depth = self.indices.len();
        self.indices
            .extend(indices.iter().map(|index| index.ident.text.to_string()));
        for statement in body {
            self.statement_inner(statement, indent + 1, lines)?;
        }
        self.indices.truncate(depth);
        lines.push(indented(indent, "end for;"));
        Ok(())
    }

    /// A loop index is a structural zero only when its range cannot move.
    ///
    /// `for i in 1:n loop` gives the index values no tangent reaches, so the
    /// body may treat it as a translation-time constant. `for e in x loop`
    /// over a `Real` array gives it one element per iteration, and that
    /// element carries the element's own tangent; treating it as constant
    /// there mints an all-zero Jacobian that looks exactly like a correct one.
    fn check_loop_range(&self, index: &ast::ForIndex) -> Refusable<()> {
        let site = self.site_of(Some(&index.ident.location));
        let name = &index.ident.text;
        let range = &index.range;
        if !matches!(range, ast::Expression::Range { .. }) {
            return Err(Refusal::new(
                Rule::StatementForm,
                site,
                format!(
                    "`for {name} in {range}` iterates a value this engine cannot show is a \
                     range; only a range is admitted, because any other iterable gives the \
                     index a tangent of its own"
                ),
            ));
        }
        if self.moves(range) {
            return Err(Refusal::new(
                Rule::StatementForm,
                site,
                format!(
                    "`for {name} in {range}` has a range that carries a tangent, so the index \
                     carries one too"
                ),
            ));
        }
        Ok(())
    }

    /// Whether any tangent can reach `expression`.
    ///
    /// Conservative in the direction of refusing: an expression this engine
    /// cannot take apart is assumed to move, so a construct admitted on the
    /// strength of a non-moving subexpression really does hold still.
    fn moves(&self, expression: &ast::Expression) -> bool {
        if ast::expression_required_value_violation(expression).is_some() {
            return true;
        }
        self.moves_inner(expression)
    }

    fn moves_inner(&self, expression: &ast::Expression) -> bool {
        match expression {
            ast::Expression::Terminal { .. } => false,
            ast::Expression::Empty { .. } => true,
            ast::Expression::ComponentReference(comp) => self.reference_moves(comp),
            ast::Expression::Parenthesized { inner, .. } => self.moves_inner(inner),
            ast::Expression::Unary { rhs, .. } => self.moves_inner(rhs),
            ast::Expression::Binary { lhs, rhs, .. } => {
                self.moves_inner(lhs) || self.moves_inner(rhs)
            }
            ast::Expression::Range {
                start, step, end, ..
            } => {
                self.moves_inner(start)
                    || step.as_deref().is_some_and(|step| self.moves_inner(step))
                    || self.moves_inner(end)
            }
            ast::Expression::Array { elements, .. } => {
                elements.iter().any(|element| self.moves_inner(element))
            }
            ast::Expression::FunctionCall { comp, args, .. } => {
                let [part] = comp.parts.as_slice() else {
                    return true;
                };
                // `size` and `ndims` read a shape, which no Real perturbation
                // moves, so their arguments do not have to hold still.
                !crate::builtins::is_constant(part.ident.text.as_ref())
                    && args.iter().any(|argument| self.moves_inner(argument))
            }
            _ => true,
        }
    }

    /// Whether a reference names something a tangent reaches.
    fn reference_moves(&self, comp: &ast::ComponentReference) -> bool {
        let [part] = comp.parts.as_slice() else {
            return true;
        };
        let name = part.ident.text.as_ref();
        if self.indices.iter().any(|index| index == name) {
            return false;
        }
        self.model
            .port(name)
            .is_some_and(|port| port.kind == ValueKind::Differentiable)
    }

    /// The declared rank of an expression, when the engine can state it exactly.
    ///
    /// [`None`] means this engine cannot state the rank. Every rule that needs
    /// a rank treats that as a refusal rather than as a guess, because the two
    /// multiplications a rank decides between, `*` and `.*`, are both well
    /// typed for some shape and compute different things.
    fn rank(&self, expression: &ast::Expression) -> Option<usize> {
        use rumoca_core::OpBinary as Op;
        match expression {
            ast::Expression::Terminal { .. } => Some(0),
            ast::Expression::Parenthesized { inner, .. } => self.rank(inner),
            ast::Expression::Unary { rhs, .. } => self.rank(rhs),
            ast::Expression::ComponentReference(comp) => self.reference_rank(comp),
            ast::Expression::Array { elements, .. } => self.array_rank(elements),
            ast::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                let rank = self.rank(else_branch)?;
                branches
                    .iter()
                    .all(|(_, value)| self.rank(value) == Some(rank))
                    .then_some(rank)
            }
            ast::Expression::Binary { op, lhs, rhs, .. } => match op {
                // The power operators pair the way the elementwise operators
                // do; the rest state their own shapes.
                Op::Exp | Op::ExpElem => paired_rank(self.rank(lhs)?, self.rank(rhs)?),
                other => arithmetic_rank(other, self.rank(lhs)?, self.rank(rhs)?),
            },
            ast::Expression::FunctionCall { comp, args, .. } => self.call_rank(comp, args),
            _ => None,
        }
    }

    /// An array literal's rank: one more than the common rank of its elements.
    fn array_rank(&self, elements: &[ast::Expression]) -> Option<usize> {
        let rank = self.rank(elements.first()?)?;
        elements
            .iter()
            .all(|element| self.rank(element) == Some(rank))
            .then_some(rank + 1)
    }

    /// The rank a reference leaves after its subscripts.
    fn reference_rank(&self, comp: &ast::ComponentReference) -> Option<usize> {
        let [part] = comp.parts.as_slice() else {
            return None;
        };
        let name = part.ident.text.as_ref();
        if self.indices.iter().any(|index| index == name) {
            return Some(0);
        }
        let port = self.model.port(name)?;
        let subscripts = part.subs.as_deref().unwrap_or_default();
        // A subscript that is not one index keeps a dimension whose extent this
        // engine does not track.
        for subscript in subscripts {
            match subscript {
                ast::Subscript::Expression(index) if self.rank(index) == Some(0) => {}
                _ => return None,
            }
        }
        port.rank().checked_sub(subscripts.len())
    }

    /// The rank a call's result has, when this engine states one.
    fn call_rank(&self, comp: &ast::ComponentReference, args: &[ast::Expression]) -> Option<usize> {
        let [part] = comp.parts.as_slice() else {
            return None;
        };
        if part.subs.as_deref().is_some_and(|subs| !subs.is_empty()) {
            return None;
        }
        crate::builtins::result_rank(part.ident.text.as_ref(), args, &|argument| {
            self.rank(argument)
        })
    }

    fn conditional(
        &mut self,
        blocks: &[ast::StatementBlock],
        else_block: Option<&[ast::Statement]>,
        indent: usize,
        lines: &mut Vec<String>,
    ) -> Refusable<()> {
        for (position, block) in blocks.iter().enumerate() {
            let keyword = if position == 0 { "if" } else { "elseif" };
            lines.push(indented(indent, &format!("{keyword} {} then", block.cond)));
            for statement in &block.stmts {
                self.statement_inner(statement, indent + 1, lines)?;
            }
        }
        if let Some(statements) = else_block {
            lines.push(indented(indent, "else"));
            for statement in statements {
                self.statement_inner(statement, indent + 1, lines)?;
            }
        }
        lines.push(indented(indent, "end if;"));
        Ok(())
    }

    /// The directional derivative of `expression`, or the structural zero.
    pub(crate) fn derivative(&mut self, expression: &ast::Expression) -> Refusable<Slope> {
        self.reject_expression_value(expression, "differentiated expression")?;
        self.derivative_inner(expression)
    }

    fn reject_expression_value(
        &self,
        expression: &ast::Expression,
        context: &str,
    ) -> Refusable<()> {
        if let Some(violation) = ast::expression_required_value_violation(expression) {
            return Err(self.value_refusal(violation, context));
        }
        Ok(())
    }

    fn value_refusal(&self, violation: ast::RequiredValueViolation, context: &str) -> Refusal {
        let site = violation.span.map_or_else(
            || self.site.clone(),
            |span| Site {
                line: self.site.line,
                column: self.site.column,
                ..Site::with_span(&self.site.file, None, span)
            },
        );
        Refusal::new(
            Rule::ExpressionForm,
            site,
            format!("{context} contains {}", violation.kind.description()),
        )
    }

    fn derivative_inner(&mut self, expression: &ast::Expression) -> Refusable<Slope> {
        match expression {
            ast::Expression::Terminal { .. } => Ok(None),
            ast::Expression::Empty { .. } => Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                "parser-recovery expression cannot carry a tangent",
            )),
            ast::Expression::ComponentReference(comp) => self.reference_derivative(comp),
            ast::Expression::Parenthesized { inner, .. } => self.derivative_inner(inner),
            ast::Expression::Unary { op, rhs, .. } => self.unary_derivative(op, rhs),
            ast::Expression::Binary { op, lhs, rhs, .. } => self.binary_derivative(op, lhs, rhs),
            ast::Expression::Array { elements, .. } => self.array_derivative(elements),
            ast::Expression::If {
                branches,
                else_branch,
                ..
            } => self.if_derivative(branches, else_branch),
            ast::Expression::FunctionCall { comp, args, .. } => self.call_derivative(comp, args),
            other => Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                format!("`{other}` is not an admitted expression form"),
            )),
        }
    }

    fn reference_derivative(&self, comp: &ast::ComponentReference) -> Refusable<Slope> {
        let (name, subscripts) = self.single_part(comp)?;
        if self.indices.iter().any(|index| index == name) {
            return Ok(None);
        }
        match self.model.port(name) {
            Some(port) if port.kind == ValueKind::Differentiable => {
                Ok(Some(format!("{}{subscripts}", port.tangent_name())))
            }
            // A constant-typed declaration, or a name the function does not
            // declare: a package constant or an enumeration literal, both of
            // which are translation-time constants inside a function body.
            Some(_) | None => Ok(None),
        }
    }

    fn unary_derivative(
        &mut self,
        op: &rumoca_core::OpUnary,
        rhs: &ast::Expression,
    ) -> Refusable<Slope> {
        let slope = self.derivative_inner(rhs)?;
        match (op, slope) {
            (_, None) => Ok(None),
            (rumoca_core::OpUnary::Plus, slope) => Ok(slope),
            (rumoca_core::OpUnary::Minus, Some(slope)) => Ok(Some(format!("-({slope})"))),
            (rumoca_core::OpUnary::DotPlus, slope) => Ok(slope),
            (rumoca_core::OpUnary::DotMinus, Some(slope)) => Ok(Some(format!("-({slope})"))),
            (other, Some(_)) => Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                format!("unary `{other:?}` carries no tangent"),
            )),
        }
    }

    fn binary_derivative(
        &mut self,
        op: &rumoca_core::OpBinary,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
    ) -> Refusable<Slope> {
        use rumoca_core::OpBinary as Op;
        self.check_operand_shapes(op, lhs, rhs)?;
        let left = self.derivative_inner(lhs)?;
        let right = self.derivative_inner(rhs)?;
        if left.is_none() && right.is_none() {
            return Ok(None);
        }
        // A sum whose two operands may differ in rank spreads the shorter one
        // over the longer (MLS 10.6.5). Dropping the term that does not move
        // would drop the shape it contributed with it, so the shape is kept as
        // a structural zero unless both ranks are known and equal.
        let pad = self.pads_the_still_operand(lhs, rhs);
        match op {
            Op::Add | Op::AddElem => Ok(Some(sum_text(op_text(op), left, right, lhs, rhs, pad))),
            Op::Sub | Op::SubElem => Ok(Some(difference_text(
                op_text(op),
                left,
                right,
                lhs,
                rhs,
                pad,
            ))),
            Op::Mul | Op::MulElem => Ok(Some(product_text(op_text(op), left, right, lhs, rhs))),
            Op::Div | Op::DivElem => Ok(Some(quotient_text(
                op_text(op),
                product_op_text(op),
                left,
                right,
                lhs,
                rhs,
            ))),
            Op::Exp | Op::ExpElem => self.power_derivative(op, left, right, lhs, rhs),
            other => Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                format!("binary `{other:?}` carries no tangent"),
            )),
        }
    }

    /// JAC-T1 at the operator surface.
    ///
    /// An operator applied at a pair of operand shapes Modelica does not
    /// define it at produces no value, so there is no rule of that shape to
    /// state and nothing for a tangent to be the derivative of. Without this
    /// the additive rules mint a tangent anyway, and the pair is then rejected
    /// by whichever reader meets the generated text first, which is the third
    /// state between "differentiated" and "refused" that this engine's
    /// dichotomy rules out.
    ///
    /// The power operators are left to [`Self::power_derivative`], which
    /// states its own, narrower conditions on both the base and the exponent.
    fn check_operand_shapes(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
    ) -> Refusable<()> {
        use rumoca_core::OpBinary as Op;
        if matches!(op, Op::Exp | Op::ExpElem) {
            return Ok(());
        }
        let (Some(left), Some(right)) = (self.rank(lhs), self.rank(rhs)) else {
            return Ok(());
        };
        if arithmetic_rank(op, left, right).is_some() {
            return Ok(());
        }
        Err(Refusal::new(
            Rule::ExpressionForm,
            self.site.clone(),
            format!(
                "`{lhs} {operator} {rhs}` joins a rank-{left} operand with a rank-{right} one, \
                 which is a pair Modelica does not define `{operator}` at, so its rule has no \
                 shape here",
                operator = op_text(op)
            ),
        ))
    }

    /// Whether an additive rule has to write the operand that holds still.
    ///
    /// It does unless both ranks are known and equal, which is the one case
    /// where the moving term already has the shape the sum produces.
    fn pads_the_still_operand(&self, lhs: &ast::Expression, rhs: &ast::Expression) -> bool {
        let left = self.rank(lhs);
        left.is_none() || left != self.rank(rhs)
    }

    /// The chain rule for a power.
    ///
    /// `d(l^r) = r*l^(r-1)*dl` is the rule for a rank-zero base, and both of
    /// its multiplications are scalar ones. Modelica spells the matrix power
    /// with the same operator (MLS 10.6.6), where the derivative is
    /// `dA*A + A*dA` and not that expression at all, so a `^` whose base this
    /// engine cannot show is rank zero refuses. The elementwise form has the
    /// scalar rule at every entry, so it admits any shape as long as the two
    /// multiplications it writes are elementwise too: written with `*` they
    /// would be Modelica's scalar product (MLS 10.6.3), which collapses a
    /// diagonal Jacobian to a rank-one one while staying well typed.
    fn power_derivative(
        &mut self,
        op: &rumoca_core::OpBinary,
        left: Slope,
        right: Slope,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
    ) -> Refusable<Slope> {
        let power = op_text(op);
        if right.is_some() {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                format!("`{lhs} {power} {rhs}` has an exponent that carries a tangent"),
            ));
        }
        let Some(left) = left else {
            return Ok(None);
        };
        // The rule writes the exponent once as `r - 1`. Modelica has no
        // elementwise difference of an array and a scalar, so a non-scalar
        // exponent has no spelling this rule can take.
        if self.rank(rhs) != Some(0) {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                format!(
                    "`{lhs} {power} {rhs}` has an exponent this engine cannot show is scalar, and \
                     the rule has to write it as `{rhs} - 1`"
                ),
            ));
        }
        let elementwise = matches!(op, rumoca_core::OpBinary::ExpElem);
        if !elementwise && self.rank(lhs) != Some(0) {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                format!(
                    "`{lhs} {power} {rhs}` raises a base this engine cannot show is scalar; the \
                     stated power rule is the scalar one, and a matrix power has a different \
                     derivative"
                ),
            ));
        }
        let times = if elementwise { ".*" } else { "*" };
        Ok(Some(format!(
            "({rhs}) {times} (({lhs}) {power} (({rhs}) - 1)) {times} ({left})"
        )))
    }

    fn array_derivative(&mut self, elements: &[ast::Expression]) -> Refusable<Slope> {
        let mut slopes = Vec::with_capacity(elements.len());
        let mut moving = false;
        for element in elements {
            match self.derivative_inner(element)? {
                Some(slope) => {
                    moving = true;
                    slopes.push(slope);
                }
                None => slopes.push(zero_like(element)),
            }
        }
        if !moving {
            return Ok(None);
        }
        Ok(Some(format!("{{{}}}", slopes.join(", "))))
    }

    fn if_derivative(
        &mut self,
        branches: &[(ast::Expression, ast::Expression)],
        else_branch: &ast::Expression,
    ) -> Refusable<Slope> {
        let mut slopes = Vec::with_capacity(branches.len());
        let mut moving = false;
        for (condition, value) in branches {
            let slope = self.derivative_inner(value)?;
            moving |= slope.is_some();
            slopes.push((condition, slope.unwrap_or_else(|| zero_like(value))));
        }
        let otherwise = self.derivative_inner(else_branch)?;
        moving |= otherwise.is_some();
        if !moving {
            return Ok(None);
        }
        let otherwise = otherwise.unwrap_or_else(|| zero_like(else_branch));
        let mut text = String::new();
        for (position, (condition, slope)) in slopes.iter().enumerate() {
            let keyword = if position == 0 { "if" } else { " elseif" };
            text.push_str(&format!("{keyword} {condition} then {slope}"));
        }
        Ok(Some(format!("({text} else {otherwise})")))
    }

    fn call_derivative(
        &mut self,
        comp: &ast::ComponentReference,
        args: &[ast::Expression],
    ) -> Refusable<Slope> {
        let (name, subscripts) = self.single_part(comp)?;
        if !subscripts.is_empty() {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site_of(comp.get_location()),
                format!("`{comp}` subscripts a call result"),
            ));
        }
        // A declared function whose name is also a builtin is two functions
        // with one name. Which one the primal takes is a name-resolution
        // question, and answering it here would risk differentiating a body
        // the program never runs.
        if crate::builtins::is_builtin(name) && self.scope.find(name).is_some() {
            return Err(Refusal::new(
                Rule::CalleeLookup,
                self.site_of(comp.get_location()),
                format!(
                    "`{name}` is declared in this file and is also a builtin carrying a tangent \
                     rule; which one this call takes is not stated"
                ),
            ));
        }
        if crate::builtins::is_refused(name) {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site_of(comp.get_location()),
                format!("`{name}` is stated non-differentiable"),
            ));
        }
        if crate::builtins::is_constant(name) {
            // JAC-T1 holds over these too. Their tangent is the structural
            // zero, but a zero still has a shape, and a call at operand shapes
            // the builtin is not defined at states no shape for the rule
            // around it to be zero in.
            if crate::builtins::result_rank(name, args, &|argument| self.rank(argument)).is_none() {
                return Err(Refusal::new(
                    Rule::ExpressionForm,
                    self.site_of(comp.get_location()),
                    format!(
                        "`{name}` is called at operand shapes whose result rank this engine \
                         cannot state, so the shape its zero tangent would carry is not stated \
                         either"
                    ),
                ));
            }
            return Ok(None);
        }
        if let Some(shape) = crate::builtins::rule(name) {
            return self.builtin_derivative(name, shape, args);
        }
        self.user_call_derivative(name, args)
    }

    fn user_call_derivative(&mut self, name: &str, args: &[ast::Expression]) -> Refusable<Slope> {
        let site = self.site.clone();
        let Some((class, owner)) = self.scope.find_with_owner(name) else {
            return Err(Refusal::new(
                Rule::CalleeTangent,
                site,
                format!("`{name}` is neither a rule-carrying builtin nor a function in scope"),
            ));
        };
        let callee = FunctionModel::read(class, self.scope, &owner, &site)?;
        let inputs: Vec<_> = callee.ports_with(PortRole::Input).cloned().collect();
        if inputs.len() != args.len() {
            return Err(Refusal::new(
                Rule::CalleeTangent,
                site,
                format!(
                    "`{name}` takes {} inputs but the call passes {}; default and named \
                     arguments are not admitted",
                    inputs.len(),
                    args.len()
                ),
            ));
        }
        let outputs: Vec<_> = callee.ports_with(PortRole::Output).collect();
        let [output] = outputs.as_slice() else {
            return Err(Refusal::new(
                Rule::CalleeTangent,
                site,
                format!("`{name}` has {} outputs; one is required", outputs.len()),
            ));
        };
        if output.kind == ValueKind::Constant {
            return Ok(None);
        }
        let mut tangents = Vec::new();
        let mut moving = false;
        for (port, argument) in inputs.iter().zip(args) {
            let slope = self.derivative_inner(argument)?;
            if port.kind == ValueKind::Constant {
                continue;
            }
            moving |= slope.is_some();
            tangents.push(slope.unwrap_or_else(|| zero_like(argument)));
        }
        if !moving {
            return Ok(None);
        }
        if !self.requested.iter().any(|held| held == name) {
            self.requested.push(name.to_string());
        }
        let primal = args.iter().map(ToString::to_string).collect::<Vec<_>>();
        Ok(Some(format!(
            "{name}{TANGENT_FUNCTION_SUFFIX}({})",
            primal
                .into_iter()
                .chain(tangents)
                .collect::<Vec<_>>()
                .join(", ")
        )))
    }

    /// The derivative of a builtin call whose rule shape is `shape`.
    fn builtin_derivative(
        &mut self,
        name: &str,
        shape: crate::builtins::Rule,
        args: &[ast::Expression],
    ) -> Refusable<Slope> {
        let admitted = match shape {
            crate::builtins::Rule::Unary => args.len() == 1,
            crate::builtins::Rule::Binary => args.len() == 2,
            crate::builtins::Rule::Mapped => args.len() >= 2,
        };
        if !admitted {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                format!("`{name}` is called with {} arguments", args.len()),
            ));
        }
        // JAC-T1 over the builtin surface: a rule is stated at the shapes its
        // result has a rank here, and a call at any other shape refuses at the
        // call site. Without this the rule still mints a tangent, and the shape
        // it is wrong at is decided later, by whichever tool reads the
        // generated text, which is the third state the specification's
        // dichotomy rules out.
        if crate::builtins::result_rank(name, args, &|argument| self.rank(argument)).is_none() {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                format!(
                    "`{name}` is called at operand shapes whose result rank this engine cannot \
                     state, so the shape of its rule is not stated either"
                ),
            ));
        }
        let mut slopes = Vec::with_capacity(args.len());
        for argument in args {
            slopes.push(self.derivative_inner(argument)?);
        }
        if slopes.iter().all(Option::is_none) {
            return Ok(None);
        }
        // A vectorized call maps a scalar function over its argument
        // (MLS 10.6.4). A rule that branches on the sign of its argument has
        // one branch, and a vectorized call would need one per element.
        if crate::builtins::branches_on_its_argument(name)
            && args.first().and_then(|first| self.rank(first)) != Some(0)
        {
            return Err(Refusal::new(
                Rule::ExpressionForm,
                self.site.clone(),
                format!(
                    "`{name}` is called on a value this engine cannot show is scalar; its rule \
                     is the derivative of the branch the value takes, and a vectorized call \
                     takes one branch per element"
                ),
            ));
        }
        crate::builtins::apply(name, args, &slopes)
            .map(Some)
            .ok_or_else(|| {
                Refusal::new(
                    Rule::ExpressionForm,
                    self.site.clone(),
                    format!("`{name}` carries no tangent rule"),
                )
            })
    }
}

/// The structural zero of `expression`, keeping the expression's own shape.
pub(crate) fn zero_like(expression: &ast::Expression) -> String {
    format!("0.0*({expression})")
}

/// Every name `expression` reads, including the ones its subscripts read.
///
/// The walk descends through every expression form, so a name it misses is a
/// form the derivative rules refuse anyway.
fn reads<'a>(expression: &'a ast::Expression, found: &mut Vec<&'a str>) {
    if let ast::Expression::ComponentReference(comp) = expression {
        for part in &comp.parts {
            found.push(part.ident.text.as_ref());
            reads_subscripts(part.subs.iter().flatten(), found);
        }
        return;
    }
    if let ast::Expression::ArrayIndex { subscripts, .. } = expression {
        reads_subscripts(subscripts.iter(), found);
    }
    for child in crate::sites::children(expression) {
        reads(child, found);
    }
}

/// Every name the index expressions of a subscript list read.
fn reads_subscripts<'a>(
    subscripts: impl Iterator<Item = &'a ast::Subscript>,
    found: &mut Vec<&'a str>,
) {
    for subscript in subscripts {
        if let ast::Subscript::Expression(index) = subscript {
            reads(index, found);
        }
    }
}

/// The rank an operator that pairs entries produces.
///
/// A rank-zero operand is spread over the other, and two operands of equal
/// rank pair entry by entry; anything else is a shape this engine does not
/// state.
pub(crate) fn paired_rank(left: usize, right: usize) -> Option<usize> {
    match (left, right) {
        (0, other) | (other, 0) => Some(other),
        (left, right) if left == right => Some(left),
        _ => None,
    }
}

/// The rank an arithmetic operator produces at two operand ranks, or [`None`]
/// where Modelica does not define the operator at that pair of shapes.
///
/// `+` and `-` pair equal dimensions and only those (MLS 10.6.3); their
/// elementwise forms, and `.*` and `./`, also spread a rank-0 operand over the
/// other (MLS 10.6.5); `*` is the matrix, matrix-vector and scalar product;
/// `/` divides by a rank-0 value (MLS 10.6.7). Every other spelling, the
/// relational and logical operators among them, carries no rule here.
pub(crate) fn arithmetic_rank(
    op: &rumoca_core::OpBinary,
    left: usize,
    right: usize,
) -> Option<usize> {
    use rumoca_core::OpBinary as Op;
    match op {
        Op::Add | Op::Sub => (left == right).then_some(left),
        Op::AddElem | Op::SubElem | Op::MulElem | Op::DivElem => paired_rank(left, right),
        Op::Mul => product_rank(left, right),
        Op::Div => (right == 0).then_some(left),
        _ => None,
    }
}

/// The rank Modelica's `*` produces (MLS 10.6.3).
///
/// Two vectors give the scalar product, which is why a tangent written with
/// `*` where the primal was elementwise still type-checks: the rank it loses
/// is absorbed by whatever the product feeds.
fn product_rank(left: usize, right: usize) -> Option<usize> {
    match (left, right) {
        (0, other) | (other, 0) => Some(other),
        (1, 1) => Some(0),
        (1, 2) | (2, 1) => Some(1),
        (2, 2) => Some(2),
        _ => None,
    }
}

fn subscript_text(subscripts: Option<&[ast::Subscript]>) -> String {
    match subscripts {
        None => String::new(),
        Some([]) => String::new(),
        Some(subscripts) => format!(
            "[{}]",
            subscripts
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn assert_text(
    condition: &ast::Expression,
    message: &ast::Expression,
    level: Option<&ast::Expression>,
) -> String {
    match level {
        Some(level) => format!("assert({condition}, {message}, {level});"),
        None => format!("assert({condition}, {message});"),
    }
}

/// Every binary operator this engine states a tangent rule for, spelled as
/// Modelica writes it.
///
/// The list is the engine's own account of its binary surface: an operator
/// absent from it carries no rule and refuses, and one present in it owes the
/// admission table a row at every shape of operands.
pub(crate) const BINARY_OPERATORS: &[(rumoca_core::OpBinary, &str)] = {
    use rumoca_core::OpBinary as Op;
    &[
        (Op::Add, "+"),
        (Op::AddElem, ".+"),
        (Op::Sub, "-"),
        (Op::SubElem, ".-"),
        (Op::Mul, "*"),
        (Op::MulElem, ".*"),
        (Op::Div, "/"),
        (Op::DivElem, "./"),
        (Op::Exp, "^"),
        (Op::ExpElem, ".^"),
    ]
};

/// Every unary operator this engine states a tangent rule for.
pub(crate) const UNARY_OPERATORS: &[(rumoca_core::OpUnary, &str)] = {
    use rumoca_core::OpUnary as Op;
    &[
        (Op::Plus, "+"),
        (Op::Minus, "-"),
        (Op::DotPlus, ".+"),
        (Op::DotMinus, ".-"),
    ]
};

fn op_text(op: &rumoca_core::OpBinary) -> &'static str {
    BINARY_OPERATORS
        .iter()
        .find(|(held, _)| held == op)
        .map_or("?", |(_, text)| *text)
}

/// The sum rule. `pad` writes the operand that holds still as a structural
/// zero, which is what carries its shape into the result.
fn sum_text(
    op: &str,
    left: Slope,
    right: Slope,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    pad: bool,
) -> String {
    match (left, right) {
        (Some(left), Some(right)) => format!("({left}) {op} ({right})"),
        (Some(left), None) if pad => format!("({left}) {op} ({})", zero_like(rhs)),
        (Some(left), None) => left,
        (None, Some(right)) if pad => format!("({}) {op} ({right})", zero_like(lhs)),
        (None, Some(right)) => right,
        (None, None) => format!("0.0*(({lhs}) {op} ({rhs}))"),
    }
}

/// The difference rule, padded the same way as [`sum_text`].
fn difference_text(
    op: &str,
    left: Slope,
    right: Slope,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    pad: bool,
) -> String {
    match (left, right) {
        (Some(left), Some(right)) => format!("({left}) {op} ({right})"),
        (Some(left), None) if pad => format!("({left}) {op} ({})", zero_like(rhs)),
        (Some(left), None) => left,
        (None, Some(right)) if pad => format!("({}) {op} ({right})", zero_like(lhs)),
        (None, Some(right)) => format!("-({right})"),
        (None, None) => format!("0.0*(({lhs}) {op} ({rhs}))"),
    }
}

fn product_text(
    op: &str,
    left: Slope,
    right: Slope,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
) -> String {
    match (left, right) {
        (Some(left), Some(right)) => {
            format!("({left}) {op} ({rhs}) + ({lhs}) {op} ({right})")
        }
        (Some(left), None) => format!("({left}) {op} ({rhs})"),
        (None, Some(right)) => format!("({lhs}) {op} ({right})"),
        (None, None) => format!("0.0*(({lhs}) {op} ({rhs}))"),
    }
}

/// The quotient rule, written so the numerator keeps the value's own shape:
/// `d(l/r) = dl/r - (l*dr)/(r*r)`.
fn quotient_text(
    div: &str,
    mul: &str,
    left: Slope,
    right: Slope,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
) -> String {
    let moved_numerator = right
        .as_ref()
        .map(|right| format!("(({lhs}) {mul} ({right})) {div} (({rhs}) {mul} ({rhs}))"));
    match (left, moved_numerator) {
        (Some(left), Some(moved)) => format!("({left}) {div} ({rhs}) - {moved}"),
        (Some(left), None) => format!("({left}) {div} ({rhs})"),
        (None, Some(moved)) => format!("-{moved}"),
        (None, None) => format!("0.0*(({lhs}) {div} ({rhs}))"),
    }
}

/// The multiplication that pairs with a division operator's shape rule.
fn product_op_text(op: &rumoca_core::OpBinary) -> &'static str {
    match op {
        rumoca_core::OpBinary::DivElem => ".*",
        _ => "*",
    }
}

/// Whether a call statement is the `assert` builtin.
fn is_assertion(comp: &ast::ComponentReference) -> bool {
    matches!(comp.parts.as_slice(), [part] if part.ident.text.as_ref() == "assert")
}

fn indented(depth: usize, text: &str) -> String {
    format!("{}{text}", "  ".repeat(depth + 1))
}
