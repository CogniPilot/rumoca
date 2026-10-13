//! Original fixture assignments expressed through canonical ordered branch owners.

use rumoca_ir_dae as dae;

pub(crate) fn scoped_assignment_body<'dae>(
    functions: &mut dae::Functions<'_, 'dae>,
    mut parent: dae::FunctionBody<'dae>,
    targets: &[dae::FunctionValueId<'dae>],
    conditions: &[dae::ExprId<'dae>],
    branches: &[Vec<dae::ExprId<'dae>>],
    fallback: &[dae::ExprId<'dae>],
    at: dae::DaeProvenance,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    assert_eq!(conditions.len(), branches.len());
    let mut owners = Vec::new();
    for (condition, values) in conditions.iter().zip(branches) {
        assert_eq!(targets.len(), values.len());
        let (mut branch, finish) = functions.begin_branch(parent, at)?;
        let assignments = targets
            .iter()
            .copied()
            .zip(values.iter().copied())
            .collect::<Vec<_>>();
        functions.assign_all(&mut branch, &assignments, at)?;
        let (resumed, branch) = functions.finish_branch(finish, branch)?;
        parent = resumed;
        owners.push((*condition, branch));
    }
    assert_eq!(targets.len(), fallback.len());
    let (mut branch, finish) = functions.begin_branch(parent, at)?;
    let assignments = targets
        .iter()
        .copied()
        .zip(fallback.iter().copied())
        .collect::<Vec<_>>();
    functions.assign_all(&mut branch, &assignments, at)?;
    let (mut parent, branch) = functions.finish_branch(finish, branch)?;
    functions.finish_conditional(&mut parent, owners, branch, at)?;
    Ok(parent)
}

pub(crate) fn scoped_assignment_loop<'dae>(
    functions: &mut dae::Functions<'_, 'dae>,
    mut parent: dae::FunctionLoop<'dae>,
    targets: &[dae::FunctionValueId<'dae>],
    conditions: &[dae::ExprId<'dae>],
    branches: &[Vec<dae::ExprId<'dae>>],
    fallback: &[dae::ExprId<'dae>],
    at: dae::DaeProvenance,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    assert_eq!(conditions.len(), branches.len());
    let mut owners = Vec::new();
    for (condition, values) in conditions.iter().zip(branches) {
        assert_eq!(targets.len(), values.len());
        let (mut branch, finish) = functions.begin_branch_loop(parent, at)?;
        let assignments = targets
            .iter()
            .copied()
            .zip(values.iter().copied())
            .collect::<Vec<_>>();
        functions.assign_all(&mut branch, &assignments, at)?;
        let (resumed, branch) = functions.finish_branch_loop(finish, branch)?;
        parent = resumed;
        owners.push((*condition, branch));
    }
    assert_eq!(targets.len(), fallback.len());
    let (mut branch, finish) = functions.begin_branch_loop(parent, at)?;
    let assignments = targets
        .iter()
        .copied()
        .zip(fallback.iter().copied())
        .collect::<Vec<_>>();
    functions.assign_all(&mut branch, &assignments, at)?;
    let (mut parent, branch) = functions.finish_branch_loop(finish, branch)?;
    functions.finish_conditional_loop(&mut parent, owners, branch, at)?;
    Ok(parent)
}
