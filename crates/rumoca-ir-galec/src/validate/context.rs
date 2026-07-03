//! Shared symbol tables, reference/call resolution, and the small type and
//! signal-set lattices used by the six validator analyses.

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{
    Block, BlockMethod, BlockMethodKind, Dimension, FunctionKind, Name, Parameter,
    PredefinedSignal, RefPart, Reference, ScalarType, StateCompartment, Statement, TypeRef,
    UserFunction, VariableDeclaration,
};
use crate::builtins::{Builtin, BuiltinType, find_builtin, find_lifted_base};
use crate::diagnostic::{Location, PathSegment};

/// Lexical spelling of a [`Name`]; consistent-naming checks compare lexical
/// equivalence, so `x` and `'x'` are different names (S-1.3).
pub(super) fn lexeme(name: &Name) -> String {
    match name {
        Name::Ident(id) => id.0.clone(),
        Name::Quoted(content) => format!("'{content}'"),
    }
}

/// Human-readable spelling of a reference for diagnostics (subscripts elided).
pub(super) fn reference_lexeme(reference: &Reference) -> String {
    match reference {
        Reference::Local(part) => lexeme(&part.name),
        Reference::State(parts) => {
            let mut text = "self".to_string();
            for part in parts {
                text.push('.');
                text.push_str(&lexeme(&part.name));
            }
            text
        }
    }
}

/// The parts of a reference, uniformly for local and state references.
pub(super) fn reference_parts(reference: &Reference) -> &[RefPart] {
    match reference {
        Reference::Local(part) => std::slice::from_ref(part),
        Reference::State(parts) => parts,
    }
}

/// Classification of a block-level state entity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum EntityKind {
    Input,
    Output,
    TunableParameter,
    DependentParameter,
    Constant,
    State,
}

/// One block-level state entity (interface or protected).
pub(super) struct EntityInfo<'a> {
    pub kind: EntityKind,
    pub decl: &'a VariableDeclaration,
}

/// Immutable symbol tables for one block, shared by all analyses. Duplicate
/// declarations keep the FIRST occurrence (duplicates are reported by the
/// name analysis, not silently merged).
pub(super) struct BlockContext<'a> {
    pub block: &'a Block,
    pub entities: BTreeMap<String, EntityInfo<'a>>,
    pub compartments: BTreeMap<String, &'a StateCompartment>,
    pub functions: BTreeMap<String, &'a UserFunction>,
    pub signals: SignalTable,
}

impl<'a> BlockContext<'a> {
    pub(super) fn new(block: &'a Block) -> Self {
        let mut entities = BTreeMap::new();
        for var in &block.interface {
            let kind = match var.kind {
                crate::ast::InterfaceKind::Input => EntityKind::Input,
                crate::ast::InterfaceKind::Output => EntityKind::Output,
                crate::ast::InterfaceKind::TunableParameter => EntityKind::TunableParameter,
            };
            let info = EntityInfo {
                kind,
                decl: &var.decl,
            };
            entities.entry(lexeme(&var.decl.name)).or_insert(info);
        }
        for entity in &block.protected {
            let kind = match entity.kind {
                crate::ast::ProtectedKind::DependentParameter => EntityKind::DependentParameter,
                crate::ast::ProtectedKind::Constant => EntityKind::Constant,
                crate::ast::ProtectedKind::State => EntityKind::State,
            };
            let info = EntityInfo {
                kind,
                decl: &entity.decl,
            };
            entities.entry(lexeme(&entity.decl.name)).or_insert(info);
        }
        let mut compartments = BTreeMap::new();
        for compartment in &block.compartments {
            compartments
                .entry(lexeme(&compartment.name))
                .or_insert(compartment);
        }
        let mut functions = BTreeMap::new();
        for function in block
            .protected_functions
            .iter()
            .chain(&block.public_functions)
        {
            functions.entry(lexeme(&function.name)).or_insert(function);
        }
        Self {
            block,
            entities,
            compartments,
            functions,
            signals: SignalTable::new(block),
        }
    }

    /// All executable bodies in declaration order: the three block-interface
    /// methods, then protected functions, then public functions.
    pub(super) fn bodies(&self) -> Vec<BodyView<'a>> {
        let method = |kind: BlockMethodKind, m: &'a BlockMethod| BodyView {
            name: kind.name().to_string(),
            segment: PathSegment::Method(kind),
            kind: FunctionKind::Stateful,
            method: Some(kind),
            parameters: &[],
            locals: &m.locals,
            statements: &m.statements,
            user: None,
        };
        let user = |f: &'a UserFunction| BodyView {
            name: lexeme(&f.name),
            segment: PathSegment::Function(lexeme(&f.name)),
            kind: f.kind,
            method: None,
            parameters: &f.parameters,
            locals: &f.locals,
            statements: &f.statements,
            user: Some(f),
        };
        let mut bodies = vec![
            method(BlockMethodKind::Startup, &self.block.startup),
            method(BlockMethodKind::Recalibrate, &self.block.recalibrate),
            method(BlockMethodKind::DoStep, &self.block.do_step),
        ];
        bodies.extend(self.block.protected_functions.iter().map(user));
        bodies.extend(self.block.public_functions.iter().map(user));
        bodies
    }
}

/// Uniform view over the three block-interface methods and user functions.
pub(super) struct BodyView<'a> {
    pub name: String,
    pub segment: PathSegment,
    pub kind: FunctionKind,
    pub method: Option<BlockMethodKind>,
    pub parameters: &'a [Parameter],
    pub locals: &'a [VariableDeclaration],
    pub statements: &'a [Statement],
    pub user: Option<&'a UserFunction>,
}

/// What a single-part local reference resolved to.
enum LocalBinding<'a> {
    Parameter(&'a Parameter),
    Local(&'a VariableDeclaration),
    Iterator,
}

/// Lexical scope of one body: parameters, locals, and (dynamically pushed)
/// loop iterators. Iterators shadow parameters/locals per lookup order.
pub(super) struct FunctionScope<'a> {
    parameters: BTreeMap<String, &'a Parameter>,
    locals: BTreeMap<String, &'a VariableDeclaration>,
    iterators: Vec<String>,
}

impl<'a> FunctionScope<'a> {
    pub(super) fn new(body: &BodyView<'a>) -> Self {
        let mut parameters = BTreeMap::new();
        for parameter in body.parameters {
            parameters
                .entry(lexeme(&parameter.decl.name))
                .or_insert(parameter);
        }
        let mut locals = BTreeMap::new();
        for local in body.locals {
            locals.entry(lexeme(&local.name)).or_insert(local);
        }
        Self {
            parameters,
            locals,
            iterators: Vec::new(),
        }
    }

    /// Enter a for-loop scope. Anonymous iterators push an unnameable marker
    /// so that push/pop always pair.
    pub(super) fn push_iterator(&mut self, iterator: Option<&Name>) {
        self.iterators.push(iterator.map_or(String::new(), lexeme));
    }

    pub(super) fn pop_iterator(&mut self) {
        self.iterators.pop();
    }

    pub(super) fn is_iterator(&self, name: &str) -> bool {
        !name.is_empty() && self.iterators.iter().any(|i| i == name)
    }

    fn lookup(&self, name: &str) -> Option<LocalBinding<'a>> {
        if self.is_iterator(name) {
            return Some(LocalBinding::Iterator);
        }
        if let Some(parameter) = self.parameters.get(name) {
            return Some(LocalBinding::Parameter(parameter));
        }
        self.locals.get(name).copied().map(LocalBinding::Local)
    }
}

/// Final target of a resolved reference.
pub(super) enum Resolved<'a> {
    /// A block-level state variable (possibly nested in compartments);
    /// `root_kind` is the classification of the outermost entity.
    Entity {
        decl: &'a VariableDeclaration,
        root_kind: EntityKind,
    },
    /// A compartment-typed state component (valueless at runtime).
    Component {
        decl: &'a VariableDeclaration,
    },
    Parameter(&'a Parameter),
    Local(&'a VariableDeclaration),
    Iterator,
}

/// Declared dimensions of each reference part, for subscript-arity and
/// literal-bounds checks (empty for loop iterators, which are scalars).
pub(super) struct PartDims<'a> {
    pub part: &'a RefPart,
    pub dimensions: &'a [Dimension],
}

/// A fully resolved reference: the final target plus per-part dims.
pub(super) struct ResolvedRef<'a> {
    pub target: Resolved<'a>,
    pub parts: Vec<PartDims<'a>>,
}

/// Resolve a reference against the block context and function scope. The
/// error carries the lexeme of the name that failed to resolve; callers
/// decide whether to report it (exactly one analysis — type — reports).
pub(super) fn resolve<'a>(
    ctx: &BlockContext<'a>,
    scope: &FunctionScope<'a>,
    reference: &'a Reference,
) -> Result<ResolvedRef<'a>, String> {
    match reference {
        Reference::Local(part) => resolve_local(scope, part),
        Reference::State(parts) => resolve_state(ctx, parts),
    }
}

fn resolve_local<'a>(
    scope: &FunctionScope<'a>,
    part: &'a RefPart,
) -> Result<ResolvedRef<'a>, String> {
    let name = lexeme(&part.name);
    let Some(binding) = scope.lookup(&name) else {
        return Err(name);
    };
    let (target, dimensions): (_, &[Dimension]) = match binding {
        LocalBinding::Iterator => (Resolved::Iterator, &[]),
        LocalBinding::Parameter(parameter) => {
            (Resolved::Parameter(parameter), &parameter.decl.dimensions)
        }
        LocalBinding::Local(decl) => (Resolved::Local(decl), &decl.dimensions),
    };
    Ok(ResolvedRef {
        target,
        parts: vec![PartDims { part, dimensions }],
    })
}

fn resolve_state<'a>(
    ctx: &BlockContext<'a>,
    parts: &'a [RefPart],
) -> Result<ResolvedRef<'a>, String> {
    let Some((first, rest)) = parts.split_first() else {
        return Err("self".to_string());
    };
    let first_name = lexeme(&first.name);
    let Some(info) = ctx.entities.get(&first_name) else {
        return Err(format!("self.{first_name}"));
    };
    let root_kind = info.kind;
    let mut decl = info.decl;
    let mut bindings = vec![PartDims {
        part: first,
        dimensions: &decl.dimensions,
    }];
    for part in rest {
        let TypeRef::Compartment(compartment_name) = &decl.ty else {
            return Err(format!(
                "{}.{}",
                reference_prefix(&bindings),
                lexeme(&part.name)
            ));
        };
        let member = ctx
            .compartments
            .get(&lexeme(compartment_name))
            .and_then(|c| {
                c.entities
                    .iter()
                    .find(|e| lexeme(&e.decl.name) == lexeme(&part.name))
            });
        let Some(member) = member else {
            return Err(format!(
                "{}.{}",
                reference_prefix(&bindings),
                lexeme(&part.name)
            ));
        };
        decl = &member.decl;
        bindings.push(PartDims {
            part,
            dimensions: &decl.dimensions,
        });
    }
    let target = match &decl.ty {
        TypeRef::Compartment(_) => Resolved::Component { decl },
        TypeRef::Primitive(_) => Resolved::Entity { decl, root_kind },
    };
    Ok(ResolvedRef {
        target,
        parts: bindings,
    })
}

fn reference_prefix(bindings: &[PartDims<'_>]) -> String {
    let mut text = "self".to_string();
    for binding in bindings {
        text.push('.');
        text.push_str(&lexeme(&binding.part.name));
    }
    text
}

/// Rank-aware scalar type used by the type analysis. Sizes are not tracked
/// (element type + rank only); `Unknown` suppresses cascade errors after a
/// resolution failure that has already been reported.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Ty {
    Scalar(ScalarType),
    Array(ScalarType, usize),
    Unknown,
}

impl Ty {
    pub(super) fn of_decl(decl: &VariableDeclaration) -> Self {
        match &decl.ty {
            TypeRef::Primitive(scalar) if decl.dimensions.is_empty() => Self::Scalar(*scalar),
            TypeRef::Primitive(scalar) => Self::Array(*scalar, decl.dimensions.len()),
            TypeRef::Compartment(_) => Self::Unknown,
        }
    }

    pub(super) fn of_builtin(ty: BuiltinType) -> Self {
        match ty {
            BuiltinType::Boolean => Self::Scalar(ScalarType::Boolean),
            BuiltinType::Integer => Self::Scalar(ScalarType::Integer),
            BuiltinType::Real => Self::Scalar(ScalarType::Real),
            BuiltinType::IntegerVector => Self::Array(ScalarType::Integer, 1),
            BuiltinType::RealVector => Self::Array(ScalarType::Real, 1),
            BuiltinType::RealMatrix => Self::Array(ScalarType::Real, 2),
            BuiltinType::RealArray3 => Self::Array(ScalarType::Real, 3),
        }
    }

    pub(super) fn describe(self) -> String {
        match self {
            Self::Scalar(scalar) => scalar.keyword().to_string(),
            Self::Array(scalar, rank) => {
                format!("{}[{}]", scalar.keyword(), vec![":"; rank].join(", "))
            }
            Self::Unknown => "<unknown>".to_string(),
        }
    }

    pub(super) const fn is_known(self) -> bool {
        !matches!(self, Self::Unknown)
    }

    /// Element scalar type: the type itself for scalars, the element type
    /// for arrays, `None` for unknown.
    pub(super) const fn element(self) -> Option<ScalarType> {
        match self {
            Self::Scalar(scalar) | Self::Array(scalar, _) => Some(scalar),
            Self::Unknown => None,
        }
    }

    /// Rank: 0 for scalars, the dimension count for arrays, `None` for
    /// unknown.
    pub(super) const fn rank(self) -> Option<usize> {
        match self {
            Self::Scalar(_) => Some(0),
            Self::Array(_, rank) => Some(rank),
            Self::Unknown => None,
        }
    }
}

/// A resolved call target.
pub(super) enum Callee<'a> {
    User(&'a UserFunction),
    Builtin(&'static Builtin),
    /// A `C_builtin4` lifted variant: `<base>1D` / `<base>2D`.
    Lifted {
        base: &'static Builtin,
        rank: usize,
    },
}

impl Callee<'_> {
    pub(super) fn display_name(&self) -> String {
        match self {
            Self::User(function) => lexeme(&function.name),
            Self::Builtin(builtin) => builtin.name.to_string(),
            Self::Lifted { base, rank } => format!("{}{}D", base.name, rank),
        }
    }

    pub(super) fn inputs(&self) -> Vec<Ty> {
        match self {
            Self::User(function) => function
                .parameters
                .iter()
                .filter(|p| p.direction == crate::ast::Direction::Input)
                .map(|p| Ty::of_decl(&p.decl))
                .collect(),
            Self::Builtin(builtin) => builtin
                .inputs
                .iter()
                .map(|p| Ty::of_builtin(p.ty))
                .collect(),
            Self::Lifted { base, rank } => base
                .inputs
                .iter()
                .map(|p| lift(Ty::of_builtin(p.ty), *rank))
                .collect(),
        }
    }

    pub(super) fn outputs(&self) -> Vec<Ty> {
        match self {
            Self::User(function) => function
                .parameters
                .iter()
                .filter(|p| p.direction == crate::ast::Direction::Output)
                .map(|p| Ty::of_decl(&p.decl))
                .collect(),
            Self::Builtin(builtin) => builtin
                .outputs
                .iter()
                .map(|p| Ty::of_builtin(p.ty))
                .collect(),
            Self::Lifted { base, rank } => base
                .outputs
                .iter()
                .map(|p| lift(Ty::of_builtin(p.ty), *rank))
                .collect(),
        }
    }

    pub(super) const fn is_stateful(&self) -> bool {
        match self {
            Self::User(function) => matches!(function.kind, FunctionKind::Stateful),
            Self::Builtin(_) | Self::Lifted { .. } => false,
        }
    }

    /// The callee's declared signal-set (§3.2.5 §1.5: the signal-set of a
    /// call is the referred function's DECLARED signal-set; each function's
    /// own declared-equals-computed check makes this sound).
    pub(super) fn signal_set(&self, table: &SignalTable) -> SignalSet {
        let mut set = SignalSet::default();
        match self {
            Self::User(function) => {
                let bits = function.signals.iter().filter_map(|id| table.bit(&id.0));
                for bit in bits {
                    set.insert(bit);
                }
            }
            Self::Builtin(builtin) | Self::Lifted { base: builtin, .. } => {
                for signal in builtin.signals {
                    set.insert(SignalTable::predefined_bit(*signal));
                }
            }
        }
        set
    }
}

fn lift(ty: Ty, rank: usize) -> Ty {
    match ty {
        Ty::Scalar(scalar) => Ty::Array(scalar, rank),
        other => other,
    }
}

/// Resolve a call target: user functions first (a legal program has no
/// user/builtin collisions — the name analysis enforces that), then the
/// builtin catalog including lifted variants. Builtins are only reachable
/// through plain identifiers (quoted names are lexically different, S-1.3).
pub(super) fn resolve_call<'a>(ctx: &BlockContext<'a>, name: &Name) -> Option<Callee<'a>> {
    if let Some(function) = ctx.functions.get(&lexeme(name)) {
        return Some(Callee::User(function));
    }
    let Name::Ident(id) = name else { return None };
    if let Some(builtin) = find_builtin(&id.0) {
        return Some(Callee::Builtin(builtin));
    }
    if let Some(base) = find_lifted_base(&id.0) {
        let rank = if id.0.ends_with("1D") { 1 } else { 2 };
        return Some(Callee::Lifted { base, rank });
    }
    None
}

/// Interns block signal names: bits 0–5 are the predefined signals in their
/// normative encoding order; user signals follow in declaration order.
pub(super) struct SignalTable {
    names: Vec<String>,
    user_count: usize,
}

impl SignalTable {
    fn new(block: &Block) -> Self {
        let mut names: Vec<String> = PredefinedSignal::ALL
            .iter()
            .map(|s| s.name().to_string())
            .collect();
        let mut user_count = 0;
        for signal in &block.error_signals {
            if !names.iter().any(|n| n == &signal.0) {
                names.push(signal.0.clone());
                user_count += 1;
            }
        }
        Self { names, user_count }
    }

    pub(super) fn bit(&self, name: &str) -> Option<u32> {
        self.names
            .iter()
            .position(|n| n == name)
            .map(|i| u32::try_from(i).expect("signal table is small"))
    }

    pub(super) fn name(&self, bit: u32) -> &str {
        &self.names[bit as usize]
    }

    pub(super) const fn predefined_bit(signal: PredefinedSignal) -> u32 {
        signal.bit() as u32
    }

    pub(super) const fn user_count(&self) -> usize {
        self.user_count
    }
}

/// An ordered set of signal bits (deterministic iteration for stable
/// diagnostics regardless of how many user signals a block declares).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(super) struct SignalSet(BTreeSet<u32>);

impl SignalSet {
    pub(super) fn insert(&mut self, bit: u32) {
        self.0.insert(bit);
    }

    pub(super) fn union_with(&mut self, other: &Self) {
        self.0.extend(&other.0);
    }

    pub(super) fn remove_all(&mut self, other: &Self) {
        for bit in &other.0 {
            self.0.remove(bit);
        }
    }

    pub(super) fn difference(&self, other: &Self) -> Self {
        Self(self.0.difference(&other.0).copied().collect())
    }

    pub(super) fn contains(&self, bit: u32) -> bool {
        self.0.contains(&bit)
    }

    pub(super) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = u32> + '_ {
        self.0.iter().copied()
    }
}

/// Structural path cursor shared by the statement walkers.
pub(super) struct Cursor {
    segments: Vec<PathSegment>,
}

impl Cursor {
    /// Cursor rooted at `block <name> / <body segment>`.
    pub(super) fn for_body(ctx: &BlockContext<'_>, body: &BodyView<'_>) -> Self {
        Self {
            segments: vec![
                PathSegment::Block(lexeme(&ctx.block.name)),
                body.segment.clone(),
            ],
        }
    }

    /// Cursor rooted at `block <name>` only.
    pub(super) fn for_block(ctx: &BlockContext<'_>) -> Self {
        Self {
            segments: vec![PathSegment::Block(lexeme(&ctx.block.name))],
        }
    }

    pub(super) fn push(&mut self, segment: PathSegment) {
        self.segments.push(segment);
    }

    pub(super) fn pop(&mut self) {
        self.segments.pop();
    }

    pub(super) fn here(&self) -> Location {
        Location::at(self.segments.clone())
    }
}
