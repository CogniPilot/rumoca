pub trait NeverSource: Copy {
    fn run(self) -> !;
}

pub fn relay<S: NeverSource>(source: S) -> u32 {
    source.run()
}

pub fn branch<S: NeverSource>(source: S, take: bool) -> u32 {
    if take { source.run() } else { 7 }
}

#[cfg(test)]
mod tests {
    use super::{NeverSource, branch, relay};

    #[derive(Clone, Copy)]
    struct PanicSource;

    impl NeverSource for PanicSource {
        fn run(self) -> ! {
            panic!("source panic")
        }
    }

    #[test]
    fn ordinary_branch_does_not_call_source() {
        assert_eq!(branch(PanicSource, false), 7);
    }

    #[test]
    #[should_panic(expected = "source panic")]
    fn relay_preserves_source_panic() {
        relay(PanicSource);
    }

    #[test]
    #[should_panic(expected = "source panic")]
    fn taken_branch_preserves_source_panic() {
        branch(PanicSource, true);
    }
}
