//! SOLVE-C62: one recursive call SCC lowers to one recursive owner group.
//!
//! Every call occurrence inside an SCC member body whose callee is an SCC
//! member becomes one group member. The group is issued before any owner whose
//! body enters it, so a call from outside the SCC is an ordinary owner that
//! names an already issued member.

use super::registration::{FunctionInterface, OwnerBody, assertion_layout};
use super::*;

/// Issue the recursive owner group of `function`'s call SCC, once, when that
/// SCC is recursive. A function outside every recursive SCC issues nothing.
pub(super) fn register_group_of<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    function: dae::FunctionId<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
    identities: &mut CallRegistration<'dae>,
    active: &mut Vec<dae::FunctionId<'dae>>,
) -> Result<(), solve::SolveProgramConstructionError> {
    if !identities.examined_functions.insert(function) {
        return Ok(());
    }
    let Some(members) = recursive_component(view, function)? else {
        return Ok(());
    };
    identities
        .examined_functions
        .extend(members.iter().copied());
    register_group(table, view, &members, arithmetic, identities, active)
}

/// The members of `function`'s call SCC, in function order, when it contains
/// a call cycle.
fn recursive_component<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionId<'dae>,
) -> Result<Option<Vec<dae::FunctionId<'dae>>>, solve::SolveProgramConstructionError> {
    let mut nodes = vec![function];
    let mut index = HashMap::from([(function, 0usize)]);
    let mut edges: Vec<Vec<usize>> = vec![Vec::new()];
    let mut next = 0;
    while next < nodes.len() {
        for (_, callee) in function_calls(view, nodes[next])? {
            let target = *index.entry(callee).or_insert_with(|| {
                nodes.push(callee);
                edges.push(Vec::new());
                nodes.len() - 1
            });
            edges[next].push(target);
        }
        next += 1;
    }
    let components = rumoca_core::dependency_first_sccs(&edges)
        .map_err(|_| solve::SolveProgramConstructionError::WireMismatch)?;
    let Some(component) = components
        .into_iter()
        .find(|component| component.members.contains(&0))
        .filter(|component| component.recursive)
    else {
        return Ok(None);
    };
    let mut members = component
        .members
        .iter()
        .map(|&member| nodes[member])
        .collect::<Vec<_>>();
    members.sort_by_key(|member| member.index());
    Ok(Some(members))
}

/// Each distinct call owner a Modelica function body names, with its callee.
fn function_calls<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionId<'dae>,
) -> Result<Vec<(dae::ExprId<'dae>, dae::FunctionId<'dae>)>, solve::SolveProgramConstructionError> {
    let function = view
        .function(function)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if function.is_external() {
        return Ok(Vec::new());
    }
    let assertions = assertion_conditions(view, function)?;
    nested_calls(view, function, &assertions)
        .into_iter()
        .map(|call| Ok((call, callee(view, call)?)))
        .collect()
}

fn callee<'dae>(
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
) -> Result<dae::FunctionId<'dae>, solve::SolveProgramConstructionError> {
    let node = view
        .expression(call)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    match node.operation() {
        dae::ExpressionOperation::Call { function, .. } => Ok(function),
        _ => Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: node.provenance().span(),
        }),
    }
}

fn call_span<'dae>(
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
) -> Result<rumoca_core::Span, solve::SolveProgramConstructionError> {
    Ok(view
        .expression(call)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
        .provenance()
        .span())
}

/// One member function of the SCC: its body's nested calls and typed interface.
struct MemberFunction<'dae> {
    function: dae::FunctionView<'dae>,
    calls: Vec<dae::ExprId<'dae>>,
    interface: FunctionInterface<'dae>,
}

/// The SCC's member functions and, in issue order, each call occurrence
/// that becomes a group member together with its callee.
struct GroupPlan<'dae> {
    functions: HashMap<dae::FunctionId<'dae>, MemberFunction<'dae>>,
    sites: Vec<(dae::ExprId<'dae>, dae::FunctionId<'dae>)>,
}

impl<'dae> GroupPlan<'dae> {
    /// Plan the group and issue every owner its member bodies call outside
    /// the SCC; those cannot reach the SCC, so they precede the group.
    fn issue(
        table: &mut solve::SolvePureCallTableBuilder,
        view: dae::DaeView<'dae>,
        members: &[dae::FunctionId<'dae>],
        arithmetic: solve::SolveArithmeticProfile,
        identities: &mut CallRegistration<'dae>,
        active: &mut Vec<dae::FunctionId<'dae>>,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let mut plan = Self {
            functions: HashMap::new(),
            sites: Vec::new(),
        };
        let mut bodies = Vec::with_capacity(members.len());
        for &member in members {
            let function = view
                .function(member)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
            if let Some(assertion) = assertion_conditions(view, function)?.first() {
                return Err(solve::SolveProgramConstructionError::RecursiveAssertion {
                    provenance: assertion.provenance.span(),
                });
            }
            bodies.push((member, function, nested_calls(view, function, &[])));
        }
        let calls = bodies
            .iter()
            .flat_map(|(_, _, calls)| calls.iter().copied())
            .collect::<Vec<_>>();
        for call in calls {
            let target = callee(view, call)?;
            if members.contains(&target) {
                plan.add_site(call, target);
                continue;
            }
            let registered =
                register_call(table, view, call, None, arithmetic, identities, active)?;
            if !registered.callee.assertion_slots.is_empty() {
                return Err(solve::SolveProgramConstructionError::RecursiveAssertion {
                    provenance: call_span(view, call)?,
                });
            }
        }
        for (member, function, calls) in bodies {
            let interface = FunctionInterface::lower(view, function, arithmetic)?;
            plan.functions.insert(
                member,
                MemberFunction {
                    function,
                    calls,
                    interface,
                },
            );
        }
        Ok(plan)
    }

    fn add_site(&mut self, call: dae::ExprId<'dae>, target: dae::FunctionId<'dae>) {
        if !self.sites.iter().any(|&(site, _)| site == call) {
            self.sites.push((call, target));
        }
    }

    /// The owner interface each nested call of `member` names, given the
    /// reserved member ids.
    fn callees(
        &self,
        view: dae::DaeView<'dae>,
        registered: &HashMap<dae::ExprId<'dae>, RegisteredCall<'dae>>,
        ids: &[solve::SolvePureCallOwnerId],
        member: &MemberFunction<'dae>,
    ) -> Result<
        HashMap<dae::ExprId<'dae>, CalleeInterface<'dae>>,
        solve::SolveProgramConstructionError,
    > {
        let mut callees = HashMap::new();
        for &call in &member.calls {
            let interface = match self.sites.iter().position(|&(site, _)| site == call) {
                Some(ordinal) => group_callee(
                    ids[ordinal],
                    &self.functions[&self.sites[ordinal].1].interface,
                ),
                None => registered
                    .get(&call)
                    .map(|registered| registered.callee.clone())
                    .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner {
                        provenance: call_span(view, call)?,
                    })?,
            };
            callees.insert(call, interface);
        }
        Ok(callees)
    }
}

fn register_group<'dae>(
    table: &mut solve::SolvePureCallTableBuilder,
    view: dae::DaeView<'dae>,
    members: &[dae::FunctionId<'dae>],
    arithmetic: solve::SolveArithmeticProfile,
    identities: &mut CallRegistration<'dae>,
    active: &mut Vec<dae::FunctionId<'dae>>,
) -> Result<(), solve::SolveProgramConstructionError> {
    let plan = GroupPlan::issue(table, view, members, arithmetic, identities, active)?;
    let mut group_members = Vec::with_capacity(plan.sites.len());
    for &(call, target) in &plan.sites {
        let span = call_span(view, call)?;
        let interface = &plan.functions[&target].interface;
        group_members.push(solve::SolveRecursiveMember::new(
            identities.issue(span)?,
            interface.inputs.clone(),
            interface
                .results
                .iter()
                .cloned()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            span,
        ));
    }
    let registered = &identities.calls;
    let ids =
        table.add_recursive_group(group_members, |ordinal, ids, builder, inputs, outputs| {
            let (call, target) = plan.sites[ordinal];
            let member = &plan.functions[&target];
            let callees = plan.callees(view, registered, ids, member)?;
            let layout = assertion_layout(
                view,
                &[],
                &member.calls,
                &callees,
                member.interface.result_leaf_count(),
                arithmetic,
            )?;
            let body = OwnerBody {
                function: member.function,
                interface: &member.interface,
                callees,
                assertion_count: 0,
                layout,
            };
            body.lower(view, builder, inputs, outputs, call_span(view, call)?)
        })?;
    for (&(call, target), owner) in plan.sites.iter().zip(ids) {
        let span = call_span(view, call)?;
        let site = table
            .call_site(owner)
            .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance: span })?;
        let callee = group_callee(owner, &plan.functions[&target].interface);
        identities
            .calls
            .insert(call, RegisteredCall { callee, site });
    }
    Ok(())
}

/// A group member names results only: SOLVE-C62 admits no assertion slot.
fn group_callee<'dae>(
    owner: solve::SolvePureCallOwnerId,
    interface: &FunctionInterface<'dae>,
) -> CalleeInterface<'dae> {
    CalleeInterface {
        owner,
        result_ranges: interface.result_ranges.clone().into_boxed_slice(),
        result_leaf_count: interface.result_leaf_count(),
        assertion_slots: std::sync::Arc::from(Vec::new()),
        assertions: Box::new([]),
        recursive: true,
    }
}
