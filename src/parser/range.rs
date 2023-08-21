use std::ops::Add;

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Add for Range {
    type Output = Range;

    fn add(self, other: Range) -> Range {
        Range {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}
