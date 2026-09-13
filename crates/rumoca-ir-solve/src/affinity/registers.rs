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
        let after = self.at(end);
        let overwritten = self
            .0
            .range(start..end)
            .map(|(&index, _)| index)
            .collect::<Vec<_>>();
        for index in overwritten {
            self.0.remove(&index);
        }
        self.0.insert(start, degree);
        self.0.insert(end, after);
        Some(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
