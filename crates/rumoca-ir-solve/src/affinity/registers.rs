use std::collections::BTreeMap;

use super::Degree;

/// Piecewise-constant degree bounds over register ranges; tensor extents do
/// not allocate one abstract value per coordinate.
#[derive(Default)]
pub(super) struct Registers(BTreeMap<u32, Degree>);

impl Registers {
    fn at(&self, register: u32) -> Degree {
        self.0
            .range(..=register)
            .next_back()
            .map_or(Degree::Nonlinear, |(_, degree)| *degree)
    }

    pub(super) fn read(&self, start: u32, count: usize) -> Option<Degree> {
        if count == 0 {
            return Some(Degree::Independent);
        }
        let end = start.checked_add(u32::try_from(count).ok()?)?;
        Some(
            self.0
                .range(start..end)
                .fold(self.at(start), |degree, (_, next)| degree.max(*next)),
        )
    }

    pub(super) fn read_strided(&self, start: u32, count: usize, stride: usize) -> Option<Degree> {
        if count == 0 {
            return Some(Degree::Independent);
        }
        self.read(start, (count - 1).checked_mul(stride)?.checked_add(1)?)
    }

    pub(super) fn write(&mut self, start: u32, count: usize, degree: Degree) -> Option<()> {
        if count == 0 {
            return Some(());
        }
        let end = start.checked_add(u32::try_from(count).ok()?)?;
        let before = start
            .checked_sub(1)
            .map_or(Degree::Nonlinear, |register| self.at(register));
        let after = self.at(end);
        let overwritten = self
            .0
            .range(start..end)
            .map(|(&index, _)| index)
            .collect::<Vec<_>>();
        for index in overwritten {
            self.0.remove(&index);
        }
        if before != degree {
            self.0.insert(start, degree);
        }
        if degree == after {
            self.0.remove(&end);
        } else {
            self.0.insert(end, after);
        }
        Some(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adjacent_equal_writes_keep_two_boundaries_over_seven_million_registers() {
        let mut registers = Registers::default();
        for chunk in 0..10_000 {
            registers.write(chunk * 700, 700, Degree::Affine).unwrap();
        }
        assert_eq!(registers.read(0, 7_000_000), Some(Degree::Affine));
        assert_eq!(registers.read(7_000_000, 1), Some(Degree::Nonlinear));
        assert_eq!(registers.0.len(), 2);
    }

    #[test]
    fn range_writes_match_dense_semantics_and_retain_only_degree_changes() {
        let mut registers = Registers::default();
        let mut expected = [Degree::Nonlinear; 12];
        for start in 0..12 {
            for end in start..=12 {
                check_degree_writes(&mut registers, &mut expected, start, end);
            }
        }
    }

    fn check_degree_writes(
        registers: &mut Registers,
        expected: &mut [Degree],
        start: usize,
        end: usize,
    ) {
        for degree in [Degree::Independent, Degree::Affine, Degree::Nonlinear] {
            registers.write(start as u32, end - start, degree).unwrap();
            expected[start..end].fill(degree);
            check_dense_reads(registers, expected);
            let mut prior = Degree::Nonlinear;
            let changes = expected
                .iter()
                .copied()
                .chain([Degree::Nonlinear])
                .filter(|&next| {
                    let changed = next != prior;
                    prior = next;
                    changed
                })
                .count();
            assert_eq!(registers.0.len(), changes);
        }
    }

    fn check_dense_reads(registers: &Registers, expected: &[Degree]) {
        for start in 0..expected.len() {
            for end in start..=expected.len() {
                let degree = expected[start..end]
                    .iter()
                    .copied()
                    .max()
                    .unwrap_or(Degree::Independent);
                assert_eq!(registers.read(start as u32, end - start), Some(degree));
            }
        }
        assert_eq!(
            registers.read(expected.len() as u32, 1),
            Some(Degree::Nonlinear)
        );
    }

    #[test]
    fn rejected_writes_keep_boundaries_and_strided_reads_keep_the_hull() {
        let mut registers = Registers::default();
        registers.write(2, 3, Degree::Independent).unwrap();
        registers.write(3, 1, Degree::Nonlinear).unwrap();
        assert_eq!(registers.read_strided(2, 2, 2), Some(Degree::Nonlinear));
        let before = registers.0.clone();
        assert_eq!(registers.write(u32::MAX, 1, Degree::Affine), None);
        assert_eq!(registers.0, before);
        assert_eq!(registers.write(u32::MAX, 0, Degree::Affine), Some(()));
        assert_eq!(registers.0, before);
        assert_eq!(registers.read_strided(0, usize::MAX, 2), None);
    }

    #[test]
    fn overlapping_ranges_preserve_outside_degrees_without_expansion() {
        let mut registers = Registers::default();
        registers.write(0, 1_000_000, Degree::Affine).unwrap();
        assert_eq!(registers.0.len(), 2);
        registers.write(2, 3, Degree::Independent).unwrap();
        assert_eq!(registers.read(1, 1), Some(Degree::Affine));
        assert_eq!(registers.read(2, 3), Some(Degree::Independent));
        assert_eq!(registers.read(5, 1), Some(Degree::Affine));
        assert_eq!(registers.read(999_999, 1), Some(Degree::Affine));
        assert_eq!(registers.read(1_000_000, 1), Some(Degree::Nonlinear));
        assert_eq!(registers.0.len(), 4);
    }
}
