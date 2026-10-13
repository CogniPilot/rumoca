use super::*;

#[derive(Clone, Copy)]
enum BranchCase {
    OrderedPrefix,
    SiblingRead,
    Abandoned,
    StaleAttachment,
}

fn branch_fixture(case: BranchCase) -> Result<Dae, DaeConstructionError> {
    let mut source_map = SourceMap::new();
    let source = source_map.add("branches.mo", "function branch y := 1; y := 2; end branch;");
    let at = DaeProvenance::source(Span::from_offsets(source, 0, 15)).unwrap();
    Dae::construct(source_map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("branch"), [], [integer], at),
            |dae, reservation| branch_body(dae, reservation, at, case),
        )?;
        Ok(())
    })
}

fn branch_body<'dae>(
    dae: &mut DaeConstruction<'dae>,
    reservation: FunctionReservation<'_, 'dae>,
    at: DaeProvenance,
    case: BranchCase,
) -> Result<(), DaeConstructionError> {
    let target =
        dae.functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
    let untouched = dae.functions(|functions| {
        let integer = functions.value_type(target, at)?;
        functions.local(&reservation, VarName::new("untouched"), integer, at)
    })?;
    let (one, two) = dae.expressions(|expressions| {
        Ok((
            expressions.at(at).literal(DaeLiteral::Integer(1))?,
            expressions.at(at).literal(DaeLiteral::Integer(2))?,
        ))
    })?;
    let parent = dae.functions(|functions| functions.begin(reservation, at))?;
    let (mut child, finish) = dae.functions(|functions| functions.begin_branch(parent, at))?;
    if matches!(case, BranchCase::Abandoned) {
        drop(finish);
        return dae.functions(|functions| functions.define(child, at));
    }
    dae.functions(|functions| {
        functions.assign(&mut child, target, one, at)?;
        functions.assign(&mut child, target, two, at)
    })?;
    let read = dae.functions(|functions| functions.read(&child, target, at))?;
    let (parent, owner) = dae.functions(|functions| functions.finish_branch(finish, child))?;
    dae.functions(|functions| {
        let branch = functions.branch_entry(&owner, at)?;
        assert_eq!(branch.incoming, [FunctionValueState::Unassigned; 2]);
        assert_eq!(
            branch.outgoing,
            [
                FunctionValueState::Assigned(1),
                FunctionValueState::Unassigned
            ]
        );
        assert_eq!(branch.declaration, at);
        assert_eq!(branch.scope, 0);
        assert!(matches!(
            &branch.statements[..],
            [
                FunctionStatementWire::Assignment { definition: 0 },
                FunctionStatementWire::Assignment { definition: 1 }
            ]
        ));
        assert_eq!(
            functions.branch_outgoing(&owner, target, at)?,
            FunctionAssignedState::Assigned(FunctionDefinitionId::from_raw(
                target.function().index(),
                1
            ))
        );
        assert_eq!(
            functions.branch_outgoing(&owner, untouched, at)?,
            FunctionAssignedState::Unassigned
        );
        assert!(matches!(
            functions.current_definition_id(&parent, target, at),
            Err(DaeConstructionError::IncompleteDefinition {
                kind: "function value",
                ..
            })
        ));
        Ok(())
    })?;
    match case {
        BranchCase::OrderedPrefix => dae.functions(|functions| functions.define(parent, at)),
        BranchCase::SiblingRead => {
            let (mut sibling, _finish) =
                dae.functions(|functions| functions.begin_branch(parent, at))?;
            dae.functions(|functions| functions.assign(&mut sibling, target, read, at))
        }
        BranchCase::Abandoned => unreachable!("abandoned branch returns before construction"),
        BranchCase::StaleAttachment => stale_attachment(dae, parent, target, owner, two, at),
    }
}

fn stale_attachment<'dae>(
    dae: &mut DaeConstruction<'dae>,
    mut parent: FunctionBody<'dae>,
    target: FunctionValueId<'dae>,
    branch: FunctionBranchOwner<'dae>,
    value: ExprId<'dae>,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    dae.functions(|functions| functions.assign(&mut parent, target, value, at))?;
    let (child, finish) = dae.functions(|functions| functions.begin_branch(parent, at))?;
    let (mut parent, fallback) =
        dae.functions(|functions| functions.finish_branch(finish, child))?;
    let condition =
        dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Boolean(true)))?;
    dae.functions(|functions| {
        functions.finish_conditional(&mut parent, vec![(condition, branch)], fallback, at)
    })?;
    Ok(())
}

#[test]
fn retained_branch_cannot_attach_after_its_parent_definition_advances() {
    let error = branch_fixture(BranchCase::StaleAttachment).unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::IncompleteDefinition {
            kind: "function branch incoming state",
            ..
        }
    ));
}

#[test]
fn ordered_branch_preserves_overwritten_prefix_and_blocks_unattached_root() {
    let error = branch_fixture(BranchCase::OrderedPrefix).unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::IncompleteDefinition {
            kind: "function branch attachment",
            ..
        }
    ));
}

#[test]
fn sibling_branch_cannot_capture_another_branch_definition() {
    let error = branch_fixture(BranchCase::SiblingRead).unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::InvalidFunctionValueRead {
            expected_definition: None,
            found_definition: 1,
            ..
        }
    ));
}

#[test]
fn abandoned_branch_cannot_finalize_the_surviving_child_as_a_root() {
    let error = branch_fixture(BranchCase::Abandoned).unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::IncompleteDefinition {
            kind: "function branch attachment",
            ..
        }
    ));
}

#[test]
fn branch_finish_rejects_a_genuine_other_function_body() {
    let mut source_map = SourceMap::new();
    let source = source_map.add("foreign.mo", "function a end a; function b end b;");
    let at = DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    let error = Dae::construct(source_map, |dae| {
        let first = FunctionSignature::new(VarName::new("a"), [], [], at);
        let second = FunctionSignature::new(VarName::new("b"), [], [], at);
        dae.recursive_functions(first, [second], |dae, reservations| {
            reject_foreign_finish(dae, reservations, at)
        })?;
        Ok(())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::IncompleteDefinition {
            kind: "function branch body",
            ..
        }
    ));
}

fn reject_foreign_finish<'dae>(
    dae: &mut DaeConstruction<'dae>,
    reservations: Vec<FunctionReservation<'_, 'dae>>,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let mut reservations = reservations.into_iter();
    let parent = dae.functions(|functions| functions.begin(reservations.next().unwrap(), at))?;
    let foreign = dae.functions(|functions| functions.begin(reservations.next().unwrap(), at))?;
    let (_child, finish) = dae.functions(|functions| functions.begin_branch(parent, at))?;
    dae.functions(|functions| functions.finish_branch(finish, foreign))?;
    Ok(())
}
