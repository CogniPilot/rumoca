use super::*;

/// Cells the call-output family owning `register` has retained.
fn retained(registers: &DependencyRegisters, register: Reg) -> Option<usize> {
    let (_, piece) = registers.pieces.range(..=register).next_back()?;
    let FamilyKind::Call { projected, .. } = &piece.family.kind else {
        return None;
    };
    Some(projected.lock().unwrap().len())
}

fn call(registers: &mut DependencyRegisters, start: Reg, count: usize, index: usize) {
    registers.call_output(
        start,
        count,
        vec![CallInput::Whole(DependencyState::singleton(index))],
    );
}

#[test]
fn a_call_output_cell_is_projected_once_and_answers_the_same() {
    let mut registers = DependencyRegisters::default();
    call(&mut registers, 4, 3, 7);
    assert_eq!(retained(&registers, 4), Some(0));
    for _ in 0..3 {
        assert_eq!(*registers.state(5).unwrap(), DependencyState::singleton(7));
        assert_eq!(retained(&registers, 4), Some(1));
    }
    assert_eq!(*registers.state(6).unwrap(), DependencyState::singleton(7));
    assert_eq!(retained(&registers, 4), Some(2));
    // A range is answered from the family, not cell by cell.
    assert_eq!(
        registers.range(4, 3).unwrap(),
        DependencyState::singleton(7)
    );
    assert_eq!(retained(&registers, 4), Some(2));
    assert!(registers.state(7).is_none());
}

#[test]
fn a_reissued_register_is_projected_from_its_new_family() {
    let mut registers = DependencyRegisters::default();
    call(&mut registers, 4, 2, 7);
    assert_eq!(*registers.state(5).unwrap(), DependencyState::singleton(7));
    // A view captured before the register is reissued keeps the family it
    // read, with the cell that family retained.
    let captured = registers.view(4, 2).unwrap();
    call(&mut registers, 4, 2, 9);
    assert_eq!(retained(&registers, 4), Some(0));
    assert_eq!(*registers.state(5).unwrap(), DependencyState::singleton(9));
    assert_eq!(
        captured.strided(1, 1, 0).unwrap(),
        DependencyState::singleton(7)
    );
}

#[test]
fn only_call_output_families_retain_cells() {
    let mut registers = DependencyRegisters::default();
    registers.load(0, 4, 1, Some(10), None);
    let source = registers.view(0, 4).unwrap();
    registers.transpose(4, source, 2, 2, 1);
    for _ in 0..2 {
        assert_eq!(*registers.state(5).unwrap(), DependencyState::singleton(12));
    }
    assert_eq!(retained(&registers, 0), None);
    assert_eq!(retained(&registers, 5), None);
}
