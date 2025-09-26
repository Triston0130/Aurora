//! Span and source mapping utilities for diagnostics.

use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(offset: usize, line: usize, column: usize) -> Self {
        Self {
            offset,
            line,
            column,
        }
    }

    pub fn from_line_col(line: usize, column: usize) -> Self {
        Self {
            offset: 0,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub file: FileId,
    pub start: Position,
    pub end: Position,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn from_range(file: FileId, start: Position, end: Position) -> Self {
        Self {
            file,
            start,
            end,
            line: start.line,
            column: start.column,
        }
    }

    pub fn new(line: usize, column: usize) -> Self {
        Self::point(line, column)
    }

    pub fn single(file: FileId, position: Position) -> Self {
        Self::from_range(file, position, position)
    }

    pub fn point(line: usize, column: usize) -> Self {
        let pos = Position::from_line_col(line, column);
        Self::single(FileId::default(), pos)
    }

    pub fn with_file(mut self, file: FileId) -> Self {
        self.file = file;
        self
    }

    pub fn with_end(mut self, end: Position) -> Self {
        self.end = end;
        self
    }

    pub fn merge(&self, other: Span) -> Span {
        if self.file != other.file {
            return *self;
        }
        let start = if self.start.offset <= other.start.offset {
            self.start
        } else {
            other.start
        };
        let end = if self.end.offset >= other.end.offset {
            self.end
        } else {
            other.end
        };
        Span::from_range(self.file, start, end)
    }

    pub fn is_empty(&self) -> bool {
        self.start.offset == self.end.offset
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start.line == self.end.line && self.start.column == self.end.column {
            write!(f, "{}:{}", self.start.line, self.start.column)
        } else if self.start.line == self.end.line {
            write!(
                f,
                "{}:{}-{}",
                self.start.line, self.start.column, self.end.column
            )
        } else {
            write!(
                f,
                "{}:{}-{}:{}",
                self.start.line, self.start.column, self.end.line, self.end.column
            )
        }
    }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub id: FileId,
    pub name: String,
    pub source: String,
    line_starts: Vec<usize>,
}

impl SourceFile {
    pub fn line_start(&self, line: usize) -> Option<usize> {
        self.line_starts.get(line.saturating_sub(1)).copied()
    }
}

#[derive(Debug, Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self { files: Vec::new() }
    }

    pub fn add_file(&mut self, name: impl Into<String>, source: impl Into<String>) -> FileId {
        let name = name.into();
        let source = source.into();
        let line_starts = compute_line_starts(&source);
        let id = FileId(self.files.len() as u32);
        self.files.push(SourceFile {
            id,
            name,
            source,
            line_starts,
        });
        id
    }

    pub fn file(&self, id: FileId) -> Option<&SourceFile> {
        self.files.get(id.0 as usize)
    }

    pub fn lookup_position(&self, id: FileId, offset: usize) -> Option<Position> {
        let file = self.file(id)?;
        let line_index = match file.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };
        let line_start = *file.line_starts.get(line_index)?;
        Some(Position {
            offset,
            line: line_index + 1,
            column: offset.saturating_sub(line_start) + 1,
        })
    }

    pub fn span_snippet(&self, span: Span) -> Option<&str> {
        let file = self.file(span.file)?;
        file.source.get(span.start.offset..span.end.offset)
    }
}

fn compute_line_starts(source: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for (idx, ch) in source.char_indices() {
        if ch == '\n' {
            starts.push(idx + ch.len_utf8());
        }
    }
    starts
}
