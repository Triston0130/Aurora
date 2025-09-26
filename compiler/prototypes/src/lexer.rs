//! Quick-and-dirty lexer prototype for Aurora tokens.
//! This Rust implementation validates tokenisation rules prior to
//! integrating the logic into the production compiler crates.

use std::str::Chars;

use crate::span::{FileId, Position, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword(&'static str),
    Identifier(String),
    Lifetime(String),
    Integer(String),
    Float(String),
    StringLiteral(String),
    CharLiteral(char),
    Operator(String),
    Delimiter(char),
    Boolean(bool),
    DocComment(DocCommentTokenKind, String),
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DocCommentTokenKind {
    Outer,
    Inner,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
    pub span: Span,
}

pub struct Lexer<'a> {
    input: &'a str,
    iter: Chars<'a>,
    current: Option<char>,
    peeked: Option<char>,
    line: usize,
    column: usize,
    index: usize,
    file_id: FileId,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self::with_file(FileId::default(), input)
    }

    pub fn with_file(file_id: FileId, input: &'a str) -> Self {
        let mut iter = input.chars();
        let current = iter.next();
        let peeked = iter.clone().next();
        Lexer {
            input,
            iter,
            current,
            peeked,
            line: 1,
            column: 1,
            index: 0,
            file_id,
        }
    }

    fn advance(&mut self) {
        if let Some(ch) = self.current {
            self.index += ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        self.current = self.iter.next();
        self.peeked = self.iter.clone().next();
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();
        let start_line = self.line;
        let start_column = self.column;
        let start_position = self.position();

        if self.starts_with("///") {
            let text = self.consume_doc_comment_line(3);
            return self.finish_token(
                TokenKind::DocComment(DocCommentTokenKind::Outer, text.clone()),
                text,
                start_line,
                start_column,
                start_position,
            );
        }

        if self.starts_with("//!") {
            let text = self.consume_doc_comment_line(3);
            return self.finish_token(
                TokenKind::DocComment(DocCommentTokenKind::Inner, text.clone()),
                text,
                start_line,
                start_column,
                start_position,
            );
        }

        match self.current {
            Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => {
                let ident = self.consume_identifier();
                if let Some(keyword) = keyword_if(&ident) {
                    self.finish_token(
                        TokenKind::Keyword(keyword),
                        ident,
                        start_line,
                        start_column,
                        start_position,
                    )
                } else if ident == "true" {
                    self.finish_token(
                        TokenKind::Boolean(true),
                        ident,
                        start_line,
                        start_column,
                        start_position,
                    )
                } else if ident == "false" {
                    self.finish_token(
                        TokenKind::Boolean(false),
                        ident,
                        start_line,
                        start_column,
                        start_position,
                    )
                } else {
                    let kind = TokenKind::Identifier(ident.clone());
                    self.finish_token(kind, ident, start_line, start_column, start_position)
                }
            }
            Some(ch) if ch.is_ascii_digit() => {
                let literal = self.consume_number();
                let kind = if literal.contains('.') {
                    TokenKind::Float(literal.clone())
                } else {
                    TokenKind::Integer(literal.clone())
                };
                self.finish_token(kind, literal, start_line, start_column, start_position)
            }
            Some('"') => {
                let literal = self.consume_string();
                let kind = TokenKind::StringLiteral(literal.clone());
                self.finish_token(kind, literal, start_line, start_column, start_position)
            }
            Some('\'') => match self.peeked {
                Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => {
                    let lifetime = self.consume_lifetime();
                    let text = format!("'{}", lifetime);
                    self.finish_token(
                        TokenKind::Lifetime(text.clone()),
                        text,
                        start_line,
                        start_column,
                        start_position,
                    )
                }
                _ => {
                    let ch = self.consume_char();
                    self.finish_token(
                        TokenKind::CharLiteral(ch),
                        ch.to_string(),
                        start_line,
                        start_column,
                        start_position,
                    )
                }
            },
            Some('.') => {
                if self.peeked == Some('.') {
                    let op = self.consume_operator();
                    let kind = TokenKind::Operator(op.clone());
                    self.finish_token(kind, op, start_line, start_column, start_position)
                } else {
                    self.advance();
                    self.finish_token(
                        TokenKind::Delimiter('.'),
                        ".".to_string(),
                        start_line,
                        start_column,
                        start_position,
                    )
                }
            }
            Some(ch) if is_operator_start(ch) => {
                let op = self.consume_operator();
                let kind = TokenKind::Operator(op.clone());
                self.finish_token(kind, op, start_line, start_column, start_position)
            }
            Some(ch) if is_delimiter(ch) => {
                self.advance();
                self.finish_token(
                    TokenKind::Delimiter(ch),
                    ch.to_string(),
                    start_line,
                    start_column,
                    start_position,
                )
            }
            None => self.finish_token(
                TokenKind::Eof,
                String::new(),
                start_line,
                start_column,
                start_position,
            ),
            Some(other) => {
                self.advance();
                let text = other.to_string();
                let kind = TokenKind::Identifier(text.clone());
                self.finish_token(kind, text, start_line, start_column, start_position)
            }
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            while matches!(self.current, Some(ch) if ch.is_whitespace()) {
                self.advance();
            }
            if self.current == Some('/') && self.peeked == Some('/') {
                if self.starts_with("///") || self.starts_with("//!") {
                    break;
                }
                while self.current.is_some() && self.current != Some('\n') {
                    self.advance();
                }
                continue;
            }
            if self.current == Some('/') && self.peeked == Some('*') {
                self.advance();
                self.advance();
                let mut depth = 1;
                while depth > 0 {
                    match (self.current, self.peeked) {
                        (Some('/'), Some('*')) => {
                            depth += 1;
                            self.advance();
                            self.advance();
                        }
                        (Some('*'), Some('/')) => {
                            depth -= 1;
                            self.advance();
                            self.advance();
                        }
                        (None, _) => break,
                        _ => self.advance(),
                    }
                }
                continue;
            }
            break;
        }
    }

    fn starts_with(&self, pat: &str) -> bool {
        self.input[self.index..].starts_with(pat)
    }

    fn consume_identifier(&mut self) -> String {
        let mut ident = String::new();
        while matches!(self.current, Some(ch) if ch.is_ascii_alphanumeric() || ch == '_') {
            ident.push(self.current.unwrap());
            self.advance();
        }
        ident
    }

    fn consume_lifetime(&mut self) -> String {
        debug_assert_eq!(self.current, Some('\''));
        self.advance();
        let mut name = String::new();
        while matches!(self.current, Some(ch) if ch.is_ascii_alphanumeric() || ch == '_') {
            name.push(self.current.unwrap());
            self.advance();
        }
        name
    }

    fn consume_number(&mut self) -> String {
        let mut literal = String::new();
        while matches!(self.current, Some(ch) if ch.is_ascii_alphanumeric() || ch == '_' || ch == '.')
        {
            literal.push(self.current.unwrap());
            self.advance();
        }
        literal
    }

    fn consume_string(&mut self) -> String {
        self.advance(); // opening quote
        let mut literal = String::new();
        while let Some(ch) = self.current {
            match ch {
                '"' => {
                    self.advance();
                    break;
                }
                '\\' => {
                    self.advance();
                    if let Some(escaped) = self.current {
                        let resolved = match escaped {
                            'n' => '\n',
                            't' => '\t',
                            '"' => '"',
                            '\\' => '\\',
                            'r' => '\r',
                            other => other,
                        };
                        literal.push(resolved);
                        self.advance();
                    }
                }
                other => {
                    literal.push(other);
                    self.advance();
                }
            }
        }
        literal
    }

    fn consume_doc_comment_line(&mut self, prefix_len: usize) -> String {
        for _ in 0..prefix_len {
            self.advance();
        }
        if self.current == Some(' ') {
            self.advance();
        }
        let mut text = String::new();
        while let Some(ch) = self.current {
            if ch == '\r' {
                self.advance();
                if self.current == Some('\n') {
                    self.advance();
                }
                break;
            } else if ch == '\n' {
                self.advance();
                break;
            } else {
                text.push(ch);
                self.advance();
            }
        }
        text.trim_end().to_string()
    }

    fn consume_char(&mut self) -> char {
        self.advance(); // opening '
        let ch = match self.current {
            Some('\\') => {
                self.advance();
                let escaped = self.current.unwrap_or('\0');
                self.advance();
                match escaped {
                    'n' => '\n',
                    't' => '\t',
                    '\'' => '\'',
                    '\\' => '\\',
                    other => other,
                }
            }
            Some(c) => {
                self.advance();
                c
            }
            None => '\0',
        };
        if self.current == Some('\'') {
            self.advance();
        }
        ch
    }

    fn consume_operator(&mut self) -> String {
        const OPERATORS: [&str; 28] = [
            "+=", "-=", "*=", "/=", "%=", "==", "!=", "<=", ">=", "&&", "||", "->", "=>", "::<",
            "::", "..", "...", "+", "-", "*", "/", "%", "=", "!", "@", "|", "&", "..=",
        ];
        for op in OPERATORS {
            if self.remaining().starts_with(op) {
                for _ in 0..op.len() {
                    self.advance();
                }
                return op.to_string();
            }
        }
        let ch = self.current.expect("operator start");
        self.advance();
        ch.to_string()
    }

    fn remaining(&self) -> &str {
        &self.input[self.index..]
    }

    fn position(&self) -> Position {
        Position {
            offset: self.index,
            line: self.line,
            column: self.column,
        }
    }

    fn finish_token(
        &self,
        kind: TokenKind,
        lexeme: String,
        start_line: usize,
        start_column: usize,
        start_position: Position,
    ) -> Token {
        let end_position = self.position();
        Token {
            kind,
            lexeme,
            line: start_line,
            column: start_column,
            span: Span::from_range(self.file_id, start_position, end_position),
        }
    }
}

fn keyword_if(ident: &str) -> Option<&'static str> {
    match ident {
        "fn" => Some("fn"),
        "let" => Some("let"),
        "mut" => Some("mut"),
        "as" => Some("as"),
        "pub" => Some("pub"),
        "mod" => Some("mod"),
        "macro" => Some("macro"),
        "macro_rules" => Some("macro_rules"),
        "use" => Some("use"),
        "in" => Some("in"),
        "struct" => Some("struct"),
        "enum" => Some("enum"),
        "actor" => Some("actor"),
        "async" => Some("async"),
        "await" => Some("await"),
        "effect" => Some("effect"),
        "zone" => Some("zone"),
        "region" => Some("region"),
        "if" => Some("if"),
        "else" => Some("else"),
        "match" => Some("match"),
        "where" => Some("where"),
        "for" => Some("for"),
        "while" => Some("while"),
        "loop" => Some("loop"),
        "return" => Some("return"),
        "break" => Some("break"),
        "continue" => Some("continue"),
        "impl" => Some("impl"),
        "trait" => Some("trait"),
        "spawn" => Some("spawn"),
        "parallel" => Some("parallel"),
        "move" => Some("move"),
        "try" => Some("try"),
        "catch" => Some("catch"),
        "const" => Some("const"),
        "dyn" => Some("dyn"),
        "crate" => Some("crate"),
        "self" => Some("self"),
        "super" => Some("super"),
        _ => None,
    }
}

fn is_operator_start(ch: char) -> bool {
    matches!(
        ch,
        '+' | '-' | '*' | '/' | '%' | '=' | '!' | '<' | '>' | '&' | '|' | ':' | '@' | '$'
    )
}

fn is_delimiter(ch: char) -> bool {
    matches!(ch, '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',')
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect(source: &str) -> Vec<TokenKind> {
        let mut lexer = Lexer::new(source);
        let mut kinds = Vec::new();
        loop {
            let token = lexer.next_token();
            if matches!(token.kind, TokenKind::Eof) {
                break;
            }
            kinds.push(token.kind);
        }
        kinds
    }

    #[test]
    fn lex_simple_function() {
        let kinds = collect("fn main() { let x = 42; }");
        assert!(kinds.iter().any(|t| matches!(t, TokenKind::Keyword("fn"))));
        assert!(kinds
            .iter()
            .any(|t| matches!(t, TokenKind::Identifier(id) if id == "main")));
    }

    #[test]
    fn lex_literals_and_operators() {
        let kinds = collect("let n = 0xFF_u8; let pi = 3.14f32; let ok = true && false;");
        assert!(kinds
            .iter()
            .any(|t| matches!(t, TokenKind::Integer(lit) if lit == "0xFF_u8")));
        assert!(kinds
            .iter()
            .any(|t| matches!(t, TokenKind::Float(lit) if lit == "3.14f32")));
        assert!(
            kinds
                .iter()
                .filter(|t| matches!(t, TokenKind::Boolean(_)))
                .count()
                == 2
        );
        assert!(kinds
            .iter()
            .any(|t| matches!(t, TokenKind::Operator(op) if op == "&&")));
    }

    #[test]
    fn lex_string_and_char_literals() {
        let kinds = collect("let s = \"Hello\\n\"; let c = '\\'';");
        assert!(kinds
            .iter()
            .any(|t| matches!(t, TokenKind::StringLiteral(lit) if lit == "Hello\n")));
        assert!(kinds
            .iter()
            .any(|t| matches!(t, TokenKind::CharLiteral('\''))));
    }

    #[test]
    fn lex_effect_annotation_signature() {
        let kinds = collect("fn read() -> Data ! IOError | NetworkError { }");
        let effect_pipe_count = kinds
            .iter()
            .filter(|t| matches!(t, TokenKind::Operator(op) if op == "|"))
            .count();
        assert!(kinds
            .iter()
            .any(|t| matches!(t, TokenKind::Operator(op) if op == "!")));
        assert_eq!(effect_pipe_count, 1);
        assert!(kinds
            .iter()
            .any(|t| matches!(t, TokenKind::Identifier(id) if id == "IOError")));
        assert!(kinds
            .iter()
            .any(|t| matches!(t, TokenKind::Identifier(id) if id == "NetworkError")));
    }
}
