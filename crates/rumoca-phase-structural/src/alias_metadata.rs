use rumoca_ir_dae::Variable;

pub(crate) fn state_select_rank(state_select: rumoca_core::StateSelect) -> u8 {
    match state_select {
        rumoca_core::StateSelect::Never => 0,
        rumoca_core::StateSelect::Avoid => 1,
        rumoca_core::StateSelect::Default => 2,
        rumoca_core::StateSelect::Prefer => 3,
        rumoca_core::StateSelect::Always => 4,
    }
}

pub(crate) fn inherit_exact_alias_state_selection(target: &mut Variable, donor: &Variable) {
    if state_select_rank(donor.state_select) > state_select_rank(target.state_select) {
        target.state_select = donor.state_select;
    }
}

pub(crate) fn inherit_exact_alias_initial_metadata(target: &mut Variable, donor: &Variable) {
    if target.fixed.is_none() && donor.fixed == Some(true) {
        target.fixed = donor.fixed;
    }
    if target.start.is_none() && donor.start.is_some() {
        target.start = donor.start.clone();
        target.start_span = donor.start_span;
    }
}
