pub trait NeverSource: Copy {
    fn run(self) -> !;
}

pub fn relay_never<S: NeverSource>(source: S) -> ! {
    source.run()
}

pub fn loop_branch<S: NeverSource>(source: S, mut count: u32, take: bool) -> u32 {
    while count > 0 {
        if take {
            source.run();
        }
        count -= 1;
    }
    count
}

pub fn borrowed<S: NeverSource>(source: S, value: &mut u32, take: bool) -> &mut u32 {
    if take { source.run() } else { value }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Copy)]
    struct PanicSource;

    impl NeverSource for PanicSource {
        fn run(self) -> ! {
            panic!("source panic")
        }
    }

    #[test]
    fn loop_ordinary_branch() {
        for count in [0, 1, 4] {
            assert_eq!(loop_branch(PanicSource, count, false), 0);
        }
    }

    #[test]
    fn borrowed_ordinary_branch_preserves_alias() {
        let mut value = 5;
        *borrowed(PanicSource, &mut value, false) = 9;
        assert_eq!(value, 9);
    }

    #[test]
    #[should_panic(expected = "source panic")]
    fn loop_taken_branch_preserves_panic() {
        loop_branch(PanicSource, 1, true);
    }

    #[test]
    #[should_panic(expected = "source panic")]
    fn borrowed_taken_branch_preserves_panic() {
        borrowed(PanicSource, &mut 5, true);
    }
}
