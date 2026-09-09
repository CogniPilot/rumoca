pub struct Cell {
    pub value: u32,
}

impl Cell {
    pub fn value(&self) -> u32 {
        self.value
    }

    pub fn peek(&self) -> u32 {
        self.value
    }
}

pub struct Wrap<T>(pub T);

pub fn make(value: u32) -> Wrap<u32> {
    let constructor = Wrap;
    constructor(value)
}

pub fn read(value: u32) -> u32 {
    Cell {
        value: make(value).0,
    }
    .peek()
}

#[cfg(test)]
mod tests {
    #[test]
    fn constructor_and_colliding_method_preserve_payload() {
        assert_eq!(super::Cell { value: 7 }.value(), 7);
        assert_eq!(super::read(0), 0);
        assert_eq!(super::read(7), 7);
        assert_eq!(super::read(u32::MAX), u32::MAX);
    }
}
