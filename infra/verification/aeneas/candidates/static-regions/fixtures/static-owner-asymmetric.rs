// Asymmetric static registration: a static shared loan created on one side of a
// control-flow merge and not the other.
//
// `static-owner-lifecycle.rs` covers branches and loops over statics, but every
// path there registers a static. These are the cases where one path does not.
//
// The two groups are not interchangeable:
//
//   branch_*  BEHAVIOUR controls. Both arms are tail returns, so this shape does
//             not reach a join; they pin that each arm keeps its own binding.
//   plain_*   JOIN controls. A plain `static u32` in a loop keeps the borrow
//             single-level and does reach `join_ctxs`; an array of references is
//             nested borrows and blocks the loop abstraction for ordinary code
//             too, which is what `loop_one_side` and its analogue record.
//
// The static values come from a const aggregate of shared references read at a
// runtime index, and from a call result, so the borrow is not folded away. Every
// function has an ordinary analogue, so a refusal that is not static-specific
// stays distinguishable. The tests pin the retained binding with `ptr::eq`.

const TABLE: [&u32; 2] = [&11, &29];

fn via_call() -> [&'static u32; 2] {
    TABLE
}

/// Static borrow on the `true` arm only; the `false` arm carries an ordinary
/// reference that was never registered.
pub fn branch_one_side(flag: bool, fallback: &u32, index: usize) -> &u32 {
    if flag {
        TABLE[index]
    } else {
        fallback
    }
}

/// Ordinary analogue: neither arm registers a static.
pub fn branch_one_side_ordinary<'a>(
    flag: bool,
    values: [&'a u32; 2],
    fallback: &'a u32,
    index: usize,
) -> &'a u32 {
    if flag {
        values[index]
    } else {
        fallback
    }
}

/// The same asymmetry with the static reaching the join through a call result.
pub fn branch_call_result(flag: bool, fallback: &u32, index: usize) -> &u32 {
    if flag {
        via_call()[index]
    } else {
        fallback
    }
}

/// Both arms carry a reference, one static and one ordinary, so the join sees
/// two different region kinds meeting.
pub fn mixed_selection(flag: bool, values: [&u32; 2], index: usize) -> &u32 {
    if flag {
        TABLE[index]
    } else {
        values[index]
    }
}

/// Asymmetry under a loop fixed point. With `count == 0` the static is never
/// registered, with `count == 1` once, and with larger counts the conditional
/// takes both arms across iterations.
pub fn loop_one_side(count: usize, fallback: &u32) -> &u32 {
    let mut chosen = fallback;
    let mut iteration = 0;
    while iteration < count {
        if iteration % 2 == 0 {
            chosen = TABLE[iteration % 2];
        }
        iteration += 1;
    }
    chosen
}

/// Ordinary analogue of the loop.
pub fn loop_one_side_ordinary<'a>(
    count: usize,
    values: [&'a u32; 2],
    fallback: &'a u32,
) -> &'a u32 {
    let mut chosen = fallback;
    let mut iteration = 0;
    while iteration < count {
        if iteration % 2 == 0 {
            chosen = values[iteration % 2];
        }
        iteration += 1;
    }
    chosen
}

static PLAIN: u32 = 13;
static PLAIN_OTHER: u32 = 17;

/// Reaches `join_ctxs`. A plain `static u32` keeps the borrow single-level, so
/// the loop abstraction is not blocked by nested borrows the way an array of
/// references is, and the static owner is live across the loop fixed point.
pub fn plain_loop_static(count: usize, fallback: &u32) -> &u32 {
    let mut chosen = fallback;
    let mut iteration = 0;
    while iteration < count {
        if iteration % 2 == 0 {
            chosen = &PLAIN;
        } else {
            chosen = &PLAIN_OTHER;
        }
        iteration += 1;
    }
    chosen
}

/// Ordinary analogue of `plain_loop_static`.
pub fn plain_loop_ordinary<'a>(
    count: usize,
    first: &'a u32,
    second: &'a u32,
    fallback: &'a u32,
) -> &'a u32 {
    let mut chosen = fallback;
    let mut iteration = 0;
    while iteration < count {
        if iteration % 2 == 0 {
            chosen = first;
        } else {
            chosen = second;
        }
        iteration += 1;
    }
    chosen
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn branch_arms_retain_their_own_binding() {
        let fallback = 5u32;
        for (index, entry) in TABLE.into_iter().enumerate() {
            assert!(std::ptr::eq(branch_one_side(true, &fallback, index), entry));
            assert!(std::ptr::eq(
                branch_one_side(false, &fallback, index),
                &fallback
            ));
            assert!(std::ptr::eq(
                branch_call_result(true, &fallback, index),
                TABLE[index]
            ));
            assert!(std::ptr::eq(
                branch_call_result(false, &fallback, index),
                &fallback
            ));
        }
    }

    #[test]
    fn ordinary_analogue_agrees_arm_for_arm() {
        let values = [7u32, 9u32];
        let borrowed = [&values[0], &values[1]];
        let fallback = 5u32;
        for (index, value) in values.iter().enumerate() {
            assert!(std::ptr::eq(
                branch_one_side_ordinary(true, borrowed, &fallback, index),
                value
            ));
            assert!(std::ptr::eq(
                branch_one_side_ordinary(false, borrowed, &fallback, index),
                &fallback
            ));
        }
    }

    #[test]
    fn mixed_selection_picks_the_named_side() {
        let values = [7u32, 9u32];
        let borrowed = [&values[0], &values[1]];
        for index in 0..2 {
            assert!(std::ptr::eq(
                mixed_selection(true, borrowed, index),
                TABLE[index]
            ));
            assert!(std::ptr::eq(
                mixed_selection(false, borrowed, index),
                &values[index]
            ));
        }
    }

    #[test]
    fn loop_covers_zero_one_and_several_iterations() {
        let fallback = 5u32;
        // No iterations: the static is never registered.
        assert!(std::ptr::eq(loop_one_side(0, &fallback), &fallback));
        // One iteration: registered once, on the taken arm.
        assert!(std::ptr::eq(loop_one_side(1, &fallback), TABLE[0]));
        // Two and three: the conditional takes both arms and the last write wins.
        assert!(std::ptr::eq(loop_one_side(2, &fallback), TABLE[0]));
        assert!(std::ptr::eq(loop_one_side(3, &fallback), TABLE[0]));
    }

    #[test]
    fn plain_loop_alternates_and_covers_zero_iterations() {
        let fallback = 5u32;
        assert!(std::ptr::eq(plain_loop_static(0, &fallback), &fallback));
        assert!(std::ptr::eq(plain_loop_static(1, &fallback), &PLAIN));
        assert!(std::ptr::eq(plain_loop_static(2, &fallback), &PLAIN_OTHER));
        assert!(std::ptr::eq(plain_loop_static(3, &fallback), &PLAIN));
        let first = 1u32;
        let second = 2u32;
        assert!(std::ptr::eq(
            plain_loop_ordinary(0, &first, &second, &fallback),
            &fallback
        ));
        assert!(std::ptr::eq(
            plain_loop_ordinary(2, &first, &second, &fallback),
            &second
        ));
    }

    #[test]
    fn loop_ordinary_analogue_matches_iteration_for_iteration() {
        let values = [7u32, 9u32];
        let borrowed = [&values[0], &values[1]];
        let fallback = 5u32;
        assert!(std::ptr::eq(
            loop_one_side_ordinary(0, borrowed, &fallback),
            &fallback
        ));
        for count in 1..4 {
            assert!(std::ptr::eq(
                loop_one_side_ordinary(count, borrowed, &fallback),
                &values[0]
            ));
        }
    }
}
