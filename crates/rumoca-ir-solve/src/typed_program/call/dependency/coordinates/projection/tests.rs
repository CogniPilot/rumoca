use super::*;

fn form(constant: i64, coeffs: &[i64]) -> AffineForm {
    AffineForm {
        constant,
        coeffs: coeffs.to_vec(),
    }
}

/// The axis range by the 128-bit rule alone.
fn exact_axis_range(relation: &Coordinates, point: &[i64], axis: usize) -> Option<Range<i64>> {
    let extent = i64::from(*relation.free_dimensions.get(axis)?);
    if extent == 0 {
        return Some(0..0);
    }
    let mut low = 0i128;
    let mut high = i128::from(extent - 1);
    for guard in &relation.guards {
        let bounds = guard_axis_bounds(
            guard,
            point,
            axis,
            relation.output_rank,
            &relation.free_dimensions,
        )?;
        let Some((minimum, maximum)) = bounds else {
            continue;
        };
        low = low.max(minimum);
        high = high.min(maximum);
        if low > high {
            return Some(0..0);
        }
    }
    Some(i64::try_from(low).ok()?..i64::try_from(high.checked_add(1)?).ok()?)
}

/// Every point of the free box, tested against every guard.
fn scanned_input_elements(
    relation: &Coordinates,
    output_dimensions: &[u32],
    output_element: usize,
    input_dimensions: &[u32],
) -> Option<Vec<usize>> {
    let output = coordinates(output_dimensions, output_element)?;
    let free_count: usize = relation
        .free_dimensions
        .iter()
        .map(|&extent| extent as usize)
        .product();
    let mut elements = Vec::new();
    for index in 0..free_count {
        let mut point = output.clone();
        point.extend(coordinates(&relation.free_dimensions, index)?);
        if relation.guards_match(&point)? {
            elements.push(flatten_subscripts(
                &relation.subscripts,
                &point,
                input_dimensions,
            )?);
        }
    }
    Some(elements)
}

const SMALL: [i64; 5] = [0, 1, -1, 2, -3];
const EDGE: [i64; 6] = [
    i32::MAX as i64,
    i32::MIN as i64,
    i32::MAX as i64 + 1,
    i32::MIN as i64 - 1,
    i64::MAX,
    i64::MIN,
];

fn is_small(value: i64) -> bool {
    SMALL.contains(&value)
}

/// Compares both rules for `guard` at one prefix point; whether the 64-bit
/// rule answered.
fn compare_at(relation: &Coordinates, guard: &Guard, point: &[i64], small_inputs: bool) -> bool {
    let axis = point.len() - 1;
    let free = &relation.free_dimensions;
    let exact = guard_axis_bounds(guard, point, axis, relation.output_rank, free);
    let small = small_guard_axis_bounds(guard, point, axis, relation);
    // A coefficient past 32 bits on the output axis is never multiplied in
    // 64 bits.
    assert!(small.is_none() || i32::try_from(guard.form.coeffs[0]).is_ok());
    let Some(small) = small else {
        assert!(!small_inputs, "{guard:?} at {point:?} is small");
        return false;
    };
    let widened = small.map(|(low, high)| (i128::from(low), i128::from(high)));
    assert_eq!(Some(widened), exact, "{guard:?} at {point:?}");
    true
}

/// Compares both rules for the one guard `form` in `[lower, upper]` over
/// `free` at each sampled prefix point; returns how often the 64-bit rule
/// answered and how often it declined.
fn compare_guard(form: &AffineForm, (lower, upper): (i64, i64), free: [u32; 2]) -> (usize, usize) {
    let relation = Coordinates::access(1, &free, vec![AffineForm::constant(0, 3)]).restricted(
        form.clone(),
        lower,
        upper,
    );
    let small_guard = form.coeffs.iter().all(|&c| is_small(c))
        && is_small(form.constant)
        && [lower, upper].iter().all(|bound| (-4..=4).contains(bound));
    let (mut answered, mut declined) = (0, 0);
    for output in [
        0,
        1,
        7,
        i64::from(u32::MAX) - 1,
        -1,
        i64::from(u32::MAX) + 1,
    ] {
        for point in [vec![output], vec![output, 0], vec![output, 2]] {
            let axis = point.len() - 1;
            assert_eq!(
                relation.axis_range(&point, axis),
                exact_axis_range(&relation, &point, axis),
                "{relation:?} at {point:?}"
            );
            let small_inputs = small_guard && (0..=7).contains(&output);
            let answers = relation
                .guards
                .iter()
                .filter(|guard| compare_at(&relation, guard, &point, small_inputs))
                .count();
            answered += answers;
            declined += relation.guards.len() - answers;
        }
    }
    (answered, declined)
}

/// [`compare_guard`] for one form under every sampled bound pair and box.
fn compare_form(form: &AffineForm) -> (usize, usize) {
    let bounds = [
        (0, 0),
        (-4, 3),
        (2, 1),
        (i64::MIN, i64::MAX),
        (i64::MIN, 0),
        (0, i64::MAX),
        (i64::MAX, i64::MAX),
        (i64::MIN, i64::MIN),
    ];
    let boxes = [[3, 4], [1, 1], [4, 0], [0, 4], [u32::MAX, 2], [2, u32::MAX]];
    let (mut answered, mut declined) = (0, 0);
    for range in bounds {
        for free in boxes {
            let (yes, no) = compare_guard(form, range, free);
            answered += yes;
            declined += no;
        }
    }
    (answered, declined)
}

#[test]
fn the_64_bit_guard_rule_answers_exactly_what_the_128_bit_rule_answers() {
    let coefficients: Vec<i64> = SMALL.into_iter().chain(EDGE).collect();
    let count = coefficients.len();
    let (mut answered, mut declined) = (0, 0);
    for index in 0..count * count * count {
        let coeffs =
            [index, index / count, index / count / count].map(|at| coefficients[at % count]);
        for constant in [0, 1, -3, i64::MAX, i64::MIN] {
            let (yes, no) = compare_form(&form(constant, &coeffs));
            answered += yes;
            declined += no;
        }
    }
    // Both rules are exercised: small relations are answered in 64 bits and
    // near-overflow ones fall back to the 128-bit rule.
    assert!(answered > 100_000, "{answered}");
    assert!(declined > 100_000, "{declined}");
}

#[test]
fn guarded_projection_equals_the_scanned_box() {
    let unit = |axis| AffineForm::unit_binder(axis, 3);
    let relations = [
        // Small: one free axis under a one-axis guard.
        Coordinates::access(1, &[3], vec![form(0, &[3, 1])]).restricted(form(0, &[0, 1]), 1, 2),
        // Guarded reshape: the free pair is tied to the output coordinate.
        Coordinates::access(1, &[4, 3], vec![unit(1), unit(2)]).restricted(
            form(0, &[1, -3, -1]),
            0,
            0,
        ),
        // Strided: every second and every third coordinate.
        Coordinates::access(1, &[5, 3], vec![unit(1), unit(2)])
            .restricted(form(0, &[-1, 2, 0]), 0, 1)
            .restricted(form(1, &[0, 0, 3]), 2, 5),
        // Negative coefficients on both the output and a free axis.
        Coordinates::access(1, &[5, 3], vec![unit(1), unit(2)])
            .restricted(form(4, &[1, -2, 0]), -3, 2)
            .restricted(form(0, &[0, 0, -1]), -2, -1),
        // Empty: contradictory bounds, an infeasible guard, an empty axis.
        Coordinates::access(1, &[5, 3], vec![unit(1), unit(2)]).restricted(
            form(0, &[0, 1, 0]),
            2,
            1,
        ),
        Coordinates::access(1, &[5, 3], vec![unit(1), unit(2)]).restricted(
            form(0, &[0, 2, 2]),
            13,
            20,
        ),
        Coordinates::access(1, &[5, 0], vec![unit(1), unit(2)]).restricted(
            form(0, &[1, 1, 0]),
            0,
            9,
        ),
        // One-sided sentinel bounds.
        Coordinates::access(1, &[5, 3], vec![unit(1), unit(2)])
            .restricted(form(-2, &[1, -1, 0]), 0, i64::MAX)
            .restricted(form(0, &[0, 1, 1]), i64::MIN, 3),
    ];
    for relation in &relations {
        let input = [relation.free_dimensions[0], 3];
        let input = if relation.subscripts.len() == 2 {
            &input[..]
        } else {
            &[40]
        };
        for element in 0..12 {
            assert_eq!(
                relation.input_elements(&[12], element, input),
                scanned_input_elements(relation, &[12], element, input),
                "{relation:?} at {element}"
            );
        }
    }
}

#[test]
fn near_overflow_guards_keep_the_128_bit_answer() {
    // The guard value leaves 64 bits for every output coordinate but the
    // first, so only the 128-bit rule can bound the free axis.
    let relation = Coordinates::access(1, &[3], vec![form(0, &[0, 1])]).restricted(
        form(i64::MAX, &[1, 1]),
        i64::MAX,
        i64::MAX,
    );
    let guard = &relation.guards[0];
    assert_eq!(small_guard_axis_bounds(guard, &[1], 0, &relation), None);
    assert_eq!(
        guard_axis_bounds(guard, &[1], 0, 1, &[3]),
        Some(Some((-1, -1)))
    );
    assert_eq!(relation.input_elements(&[2], 0, &[3]), Some(vec![0]));
    assert_eq!(relation.input_elements(&[2], 1, &[3]), Some(vec![]));
    // A mismatched guard arity is refused by both rules.
    let mut malformed = relation.clone();
    malformed.guards[0].form.coeffs.push(1);
    assert_eq!(
        small_guard_axis_bounds(&malformed.guards[0], &[0], 0, &malformed),
        None
    );
    assert_eq!(malformed.axis_range(&[0], 0), None);
    assert_eq!(malformed.input_elements(&[2], 0, &[3]), None);
}
