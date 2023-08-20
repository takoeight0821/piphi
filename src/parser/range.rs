#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}
