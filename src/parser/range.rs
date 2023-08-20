#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn merge(r1: &Range, r2: &Range) -> Range {
        Range {
            start: r1.start,
            end: r2.end,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}
