//! Branded direct-lifecycle ownership navigation.

use rumoca_ir_galec::package::{
    AlgorithmCodeChildRole, AlgorithmCodeInspection, AlgorithmCodeSubject, AlgorithmCodeSubjectId,
    AlgorithmCodeSubjectParent, DeclarationId, LifecycleMethodSubject, StatementSubject,
};

use super::root::SolveAlgorithmBlockConstructionError;

pub(super) fn direct_method_action_count<'a, 'id>(
    inspection: &AlgorithmCodeInspection<'a, 'id>,
    method: LifecycleMethodSubject<'a, 'id>,
    expected_locals: &[DeclarationId<'id>],
) -> Result<usize, SolveAlgorithmBlockConstructionError> {
    let owner = AlgorithmCodeSubjectId::LifecycleMethod(method.id());
    let mut local_count = 0usize;
    let mut action_count = 0usize;
    for edge in inspection.children(owner) {
        let expected_local = u32::try_from(local_count).map_err(|_| {
            SolveAlgorithmBlockConstructionError::MethodLocalIndexOverflow {
                provenance: method.provenance(),
            }
        })?;
        if let (
            AlgorithmCodeSubjectId::Declaration(found),
            AlgorithmCodeChildRole::MethodLocal(found_index),
        ) = (edge.subject(), edge.role())
        {
            if action_count != 0
                || found_index != expected_local
                || expected_locals.get(local_count).copied() != Some(found)
            {
                return Err(
                    SolveAlgorithmBlockConstructionError::UnsupportedLifecycleSource {
                        method: method.kind().into(),
                        provenance: method.provenance(),
                    },
                );
            }
            local_count += 1;
            continue;
        }
        let expected_action = u32::try_from(action_count).map_err(|_| {
            SolveAlgorithmBlockConstructionError::LifecycleActionCountOverflow {
                method: method.kind().into(),
                provenance: method.provenance(),
            }
        })?;
        if !matches!(edge.subject(), AlgorithmCodeSubjectId::Statement(_))
            || edge.role() != AlgorithmCodeChildRole::MethodAction(expected_action)
        {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedLifecycleSource {
                    method: method.kind().into(),
                    provenance: method.provenance(),
                },
            );
        }
        action_count += 1;
    }
    if local_count != expected_locals.len() {
        return Err(
            SolveAlgorithmBlockConstructionError::UnsupportedLifecycleSource {
                method: method.kind().into(),
                provenance: method.provenance(),
            },
        );
    }
    Ok(action_count)
}

pub(super) fn direct_statement_owner<'a, 'id>(
    inspection: &AlgorithmCodeInspection<'a, 'id>,
    statement: StatementSubject<'a, 'id>,
) -> Result<(LifecycleMethodSubject<'a, 'id>, u32), SolveAlgorithmBlockConstructionError> {
    let AlgorithmCodeSubjectParent::Subject {
        owner: AlgorithmCodeSubjectId::LifecycleMethod(method),
        role: AlgorithmCodeChildRole::MethodAction(action),
    } = inspection.parent(AlgorithmCodeSubjectId::Statement(statement.id()))
    else {
        return Err(
            SolveAlgorithmBlockConstructionError::UnsupportedStatementContext {
                provenance: statement.provenance(),
            },
        );
    };
    let AlgorithmCodeSubject::LifecycleMethod(method) =
        inspection.subject(AlgorithmCodeSubjectId::LifecycleMethod(method))
    else {
        return Err(
            SolveAlgorithmBlockConstructionError::ForeignLifecycleStatement {
                provenance: statement.provenance(),
            },
        );
    };
    Ok((method, action))
}
