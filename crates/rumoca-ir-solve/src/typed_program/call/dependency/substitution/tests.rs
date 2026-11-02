use super::*;
use rumoca_core::{AffineForm, SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("substitution.alg"), 0, 1)
}

/// A small deterministic generator, so a failing case can be replayed.
struct Lcg(u64);

impl Lcg {
    fn next(&mut self, bound: u64) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1);
        (self.0 >> 33) % bound
    }
}

/// A rank-one relation `c + k * i`, sometimes with an existential free axis.
fn relation(random: &mut Lcg) -> Coordinates {
    let constant = random.next(4) as i64;
    let slope = random.next(3) as i64;
    if random.next(3) == 0 {
        Coordinates::access(
            1,
            &[2],
            vec![AffineForm {
                constant,
                coeffs: vec![slope, random.next(2) as i64],
            }],
        )
    } else {
        Coordinates::access(
            1,
            &[],
            vec![AffineForm {
                constant,
                coeffs: vec![slope],
            }],
        )
    }
}

fn dependency(random: &mut Lcg, inputs: usize) -> SolveCallDependency {
    let start = random.next(inputs as u64) as usize;
    let end = (start + 1 + random.next(2) as usize).min(inputs);
    if random.next(5) == 0 {
        SolveCallDependency::whole_run(start, end)
    } else {
        SolveCallDependency::exact_run(start, end, relation(random))
    }
}

/// Compose and insert every relation of every read in turn: the original
/// call substitution.
fn naive(
    reads: &[(usize, Option<Coordinates>)],
    registers: &[Vec<SolveCallDependency>],
) -> Vec<SolveCallDependency> {
    let mut dependencies = Dependencies::default();
    for (register, access) in reads {
        for source in &registers[*register] {
            dependencies.insert(match access {
                Some(access) => source.remap_may_depend(access, span()).unwrap(),
                None => source.widened(),
            });
        }
    }
    dependencies.finish()
}

fn substituted(
    substitution: &mut Substitution,
    reads: &[(usize, Option<Coordinates>)],
    registers: &[Vec<SolveCallDependency>],
) -> Vec<SolveCallDependency> {
    substitution
        .output(
            reads.iter().map(|(register, access)| Read {
                register: *register,
                access: access.as_ref(),
            }),
            registers,
            |source, access| super::super::may_depend_coordinates(source, access, span()),
            || super::super::SolveProgramConstructionError::InvalidCallInterface {
                provenance: span(),
            },
        )
        .unwrap()
}

#[test]
fn shared_and_repeated_reads_give_the_original_union() {
    let mut random = Lcg(7);
    for _ in 0..200 {
        // A few registers drawing on a small pool, so they share relations.
        let pool: Vec<_> = (0..6).map(|_| dependency(&mut random, 5)).collect();
        let registers: Vec<Vec<_>> = (0..4)
            .map(|_| {
                (0..1 + random.next(6))
                    .map(|_| pool[random.next(pool.len() as u64) as usize].clone())
                    .collect()
            })
            .collect();
        let accesses: Vec<_> = (0..3).map(|_| relation(&mut random)).collect();
        let mut substitution = Substitution::default();
        // Several outputs of one call, each with repeated reads.
        for _ in 0..4 {
            let reads: Vec<_> = (0..1 + random.next(8))
                .map(|_| {
                    let access = (random.next(6) != 0)
                        .then(|| accesses[random.next(accesses.len() as u64) as usize].clone());
                    (random.next(registers.len() as u64) as usize, access)
                })
                .collect();
            assert_eq!(
                substituted(&mut substitution, &reads, &registers),
                naive(&reads, &registers)
            );
        }
    }
}

#[test]
fn a_reassigned_register_is_read_again() {
    let access = Coordinates::identity(1);
    let first = vec![SolveCallDependency::exact_run(
        0,
        1,
        Coordinates::identity(1),
    )];
    let second = vec![SolveCallDependency::whole_run(2, 3)];
    let mut registers = vec![first];
    let reads = [(0, Some(access))];
    let mut substitution = Substitution::default();
    assert_eq!(
        substituted(&mut substitution, &reads, &registers),
        naive(&reads, &registers)
    );
    registers[0] = second;
    substitution.assigned(0);
    assert_eq!(
        substituted(&mut substitution, &reads, &registers),
        naive(&reads, &registers)
    );
}

#[test]
fn an_over_budget_composition_widens_as_before() {
    // A relation already at the term budget cannot take another free axis.
    let wide = Coordinates::access(
        1,
        &[2; super::super::coordinates::RELATION_TERM_BUDGET],
        vec![AffineForm {
            constant: 0,
            coeffs: vec![1; super::super::coordinates::RELATION_TERM_BUDGET + 1],
        }],
    );
    let access = Coordinates::access(
        1,
        &[2],
        vec![AffineForm {
            constant: 0,
            coeffs: vec![1, 1],
        }],
    );
    let registers = vec![vec![SolveCallDependency::exact_run(0, 1, wide)]];
    let reads = [(0, Some(access))];
    let result = substituted(&mut Substitution::default(), &reads, &registers);
    assert_eq!(result, naive(&reads, &registers));
    assert!(result.iter().all(SolveCallDependency::is_whole_input));
}
