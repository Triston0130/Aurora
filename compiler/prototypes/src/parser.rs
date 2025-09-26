//! Recursive-descent parser for the Aurora prototype language.

use crate::ast::*;
use crate::diagnostics::{primary_label, Diagnostic};
use crate::lexer::{DocCommentTokenKind, Lexer, Token, TokenKind};
use crate::span::{FileId, SourceMap, Span};
use std::borrow::Cow;

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ParseError {
    #[error("unexpected token {found:?}, expected {expected} at {span}")]
    UnexpectedToken {
        expected: Cow<'static, str>,
        found: Box<TokenKind>,
        span: Span,
    },
    #[error("unexpected end of input, expected {expected} at {span}")]
    UnexpectedEof {
        expected: Cow<'static, str>,
        span: Span,
    },
    #[error("parser error: {message} at {span}")]
    Message {
        message: Cow<'static, str>,
        span: Span,
    },
}

pub fn parse_module(source: &str) -> Result<Module, ParseError> {
    parse_module_with_source("<anonymous>", source).map(|parsed| parsed.module)
}

#[derive(Debug)]
pub struct ParsedModule {
    pub module: Module,
    pub source_map: SourceMap,
    pub file_id: FileId,
}

pub fn parse_module_with_source(
    name: impl Into<String>,
    source: &str,
) -> Result<ParsedModule, ParseError> {
    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file(name, source.to_string());
    let tokens = lex_tokens(file_id, source);
    let mut parser = Parser {
        tokens,
        pos: 0,
        next_node_id: 0,
    };
    let module = parser.parse_module()?;
    Ok(ParsedModule {
        module,
        source_map,
        file_id,
    })
}

impl ParseError {
    pub fn into_diagnostic(self) -> Diagnostic {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                span,
            } => Diagnostic::error(format!("expected {expected}, found {found:?}"))
                .with_label(primary_label(span, "unexpected token")),
            ParseError::UnexpectedEof { expected, span } => {
                Diagnostic::error(format!("unexpected end of input; expected {expected}"))
                    .with_label(primary_label(span, "input ended here"))
            }
            ParseError::Message { message, span } => Diagnostic::error(message.to_string())
                .with_label(primary_label(span, "parse error")),
        }
    }
}

fn lex_tokens(file_id: FileId, source: &str) -> Vec<Token> {
    let mut lexer = Lexer::with_file(file_id, source);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        let eof = matches!(token.kind, TokenKind::Eof);
        tokens.push(token);
        if eof {
            break;
        }
    }
    tokens
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    next_node_id: u32,
}

impl Parser {
    fn parse_module(&mut self) -> Result<Module, ParseError> {
        let module_id = self.allocate_id();
        let mut docs = Vec::new();
        while let TokenKind::DocComment(kind, text) = self.peek_kind().clone() {
            if matches!(kind, DocCommentTokenKind::Inner) {
                let span = self.current_span();
                self.advance();
                docs.push(DocComment {
                    kind: DocCommentKind::Inner,
                    text,
                    span,
                });
                continue;
            }
            break;
        }
        let mut items = Vec::new();
        while !self.is_eof() {
            items.push(self.parse_item()?);
        }
        Ok(Module {
            id: module_id,
            span: Span::default(),
            docs,
            items,
        })
    }

    fn current_span(&self) -> Span {
        self.span_at(self.pos)
    }

    fn span_at(&self, index: usize) -> Span {
        let token = self
            .tokens
            .get(index)
            .unwrap_or_else(|| self.tokens.last().expect("token stream not empty"));
        token.span
    }

    fn span_between(&self, start: usize, end: usize) -> Span {
        let start_span = self
            .tokens
            .get(start)
            .map(|token| token.span)
            .unwrap_or_else(|| self.tokens.last().expect("token stream not empty").span);
        let end_span = self
            .tokens
            .get(end)
            .map(|token| token.span)
            .unwrap_or(start_span);
        Span::from_range(start_span.file, start_span.start, end_span.end)
    }

    fn span_since(&self, start: usize) -> Span {
        if self.pos == 0 {
            return Span::default();
        }
        let end = self.pos.saturating_sub(1);
        self.span_between(start, end)
    }

    fn error_message(&self, message: &'static str) -> ParseError {
        ParseError::Message {
            message: Cow::Borrowed(message),
            span: self.current_span(),
        }
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        let (docs, attributes) = self.parse_attributes()?;
        let visibility = self.parse_visibility()?;

        if self.match_keyword("use")? {
            let span = self.span_at(self.pos - 1);
            let decl = self.parse_use_decl(docs, attributes, visibility, span)?;
            self.expect_symbol(';')?;
            return Ok(Item::Use(decl));
        }

        if self.match_keyword("mod")? {
            let span = self.span_at(self.pos - 1);
            let decl = self.parse_module_decl(docs, attributes, visibility, span)?;
            return Ok(Item::Module(decl));
        }

        if self.match_keyword("macro_rules")? {
            let span = self.span_at(self.pos - 1);
            let decl = self.parse_macro_rules_decl(docs, attributes, visibility, span)?;
            return Ok(Item::Macro(decl));
        }

        if self.match_keyword("macro")? {
            let span = self.span_at(self.pos - 1);
            let decl = self.parse_macro_decl(docs, attributes, visibility, span)?;
            return Ok(Item::Macro(decl));
        }

        if self.match_keyword("zone")? {
            let span = self.span_at(self.pos - 1);
            let decl = self.parse_zone_decl(docs, attributes, visibility, span)?;
            return Ok(Item::Zone(decl));
        }

        let constness = self.match_keyword("const")?;
        let asyncness = self.match_keyword("async")?;
        if self.match_keyword("fn")? {
            let span = self.span_at(self.pos - 1);
            let decl =
                self.parse_function_decl(docs, attributes, visibility, constness, asyncness, span)?;
            return Ok(Item::Function(decl));
        }

        if self.match_keyword("struct")? {
            let span = self.span_at(self.pos - 1);
            let decl = self.parse_struct_decl(docs, attributes, visibility, span)?;
            return Ok(Item::Struct(decl));
        }

        if self.match_keyword("enum")? {
            let span = self.span_at(self.pos - 1);
            let decl = self.parse_enum_decl(docs, attributes, visibility, span)?;
            return Ok(Item::Enum(decl));
        }

        if self.match_keyword("trait")? {
            let span = self.span_at(self.pos - 1);
            let decl = self.parse_trait_decl(docs, attributes, visibility, span)?;
            return Ok(Item::Trait(decl));
        }

        if self.match_keyword("impl")? {
            let span = self.span_at(self.pos - 1);
            let decl = self.parse_impl_decl(docs, attributes, span)?;
            return Ok(Item::Impl(decl));
        }

        if self.match_keyword("extern")? {
            let span = self.span_at(self.pos - 1);
            let block = self.parse_extern_block(docs, attributes, visibility, span)?;
            return Ok(Item::Extern(block));
        }

        Err(self.error_expected("item"))
    }

    fn parse_attributes(&mut self) -> Result<(Vec<DocComment>, Vec<Attribute>), ParseError> {
        let mut docs = Vec::new();
        while let TokenKind::DocComment(kind, text) = self.peek_kind().clone() {
            let span = self.current_span();
            self.advance();
            let doc_kind = match kind {
                DocCommentTokenKind::Outer => DocCommentKind::Outer,
                DocCommentTokenKind::Inner => DocCommentKind::Inner,
            };
            docs.push(DocComment {
                kind: doc_kind,
                text,
                span,
            });
        }

        let mut attrs = Vec::new();
        while self.match_operator("@")? {
            let attr_span = self.span_at(self.pos - 1);
            let name = match self.peek_kind().clone() {
                TokenKind::Identifier(id) => {
                    self.advance();
                    id
                }
                TokenKind::Keyword(kw) => {
                    self.advance();
                    kw.to_string()
                }
                other => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "attribute name".into(),
                        found: Box::new(other),
                        span: self.current_span(),
                    })
                }
            };
            let mut args = Vec::new();
            if self.match_symbol('(')? {
                if !self.check_symbol(')') {
                    loop {
                        args.push(self.parse_attribute_arg()?);
                        if self.match_symbol(',')? {
                            if self.check_symbol(')') {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                self.expect_symbol(')')?;
            }
            attrs.push(Attribute {
                name,
                args,
                span: attr_span,
            });
        }
        Ok((docs, attrs))
    }

    fn parse_attribute_arg(&mut self) -> Result<AttributeArg, ParseError> {
        if let TokenKind::Identifier(name) = self.peek_kind().clone() {
            if matches!(self.peek_next_kind(), Some(TokenKind::Operator(op)) if op == "=") {
                self.advance();
                self.expect_operator("=")?;
                let value = self.parse_expression()?;
                return Ok(AttributeArg::KeyValue { key: name, value });
            }
        }

        let expr = self.parse_expression()?;
        Ok(AttributeArg::Expr(expr))
    }

    fn parse_visibility(&mut self) -> Result<Visibility, ParseError> {
        if !self.match_keyword("pub")? {
            return Ok(Visibility::Private);
        }
        if !self.match_symbol('(')? {
            return Ok(Visibility::Pub);
        }
        if self.match_keyword("crate")? {
            self.expect_symbol(')')?;
            return Ok(Visibility::PubCrate);
        }
        if self.match_keyword("super")? {
            self.expect_symbol(')')?;
            return Ok(Visibility::PubSuper);
        }
        if self.match_keyword("self")? {
            self.expect_symbol(')')?;
            return Ok(Visibility::PubSelf);
        }
        if self.match_keyword("in")? {
            let path = self.parse_path_segments()?;
            self.expect_symbol(')')?;
            let names = path.into_iter().map(|seg| seg.ident).collect();
            return Ok(Visibility::PubIn(names));
        }
        Err(self.error_expected("visibility scope"))
    }

    fn parse_use_decl(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        visibility: Visibility,
        span: Span,
    ) -> Result<UseDecl, ParseError> {
        let leading_colon = self.match_operator("::")?;
        let tree = if self.check_symbol('{') {
            self.advance();
            let items = self.parse_use_group_items()?;
            UseTree::Group {
                leading_colon,
                prefix: Vec::new(),
                items,
            }
        } else {
            self.parse_use_tree_with_prefix(leading_colon, Vec::new())?
        };
        Ok(UseDecl {
            attributes,
            docs,
            visibility,
            tree,
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_zone_decl(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        visibility: Visibility,
        span: Span,
    ) -> Result<ZoneDecl, ParseError> {
        if !matches!(visibility, Visibility::Private) {
            return Err(self.error_message("zone declarations do not support visibility modifiers"));
        }
        let name = self.expect_identifier()?;
        let args = self.parse_zone_args()?;
        let body = self.parse_block()?;
        Ok(ZoneDecl {
            attributes,
            docs,
            name,
            args,
            body,
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_zone_args(&mut self) -> Result<Vec<ZoneArg>, ParseError> {
        if !self.match_symbol('(')? {
            return Ok(Vec::new());
        }
        let mut args = Vec::new();
        if !self.check_symbol(')') {
            loop {
                let key = self.expect_identifier()?;
                self.expect_operator("=")?;
                let value = self.parse_expression()?;
                args.push(ZoneArg { key, value });
                if !self.match_symbol(',')? {
                    break;
                }
            }
        }
        self.expect_symbol(')')?;
        Ok(args)
    }

    fn parse_use_tree_with_prefix(
        &mut self,
        leading_colon: bool,
        mut segments: Vec<PathSegment>,
    ) -> Result<UseTree, ParseError> {
        let ident = self.parse_use_segment_ident()?;
        segments.push(PathSegment {
            ident,
            generics: Vec::new(),
        });

        loop {
            if self.match_operator("::")? {
                if self.match_operator("*")? {
                    return Ok(UseTree::Glob {
                        leading_colon,
                        segments,
                    });
                }
                if self.match_symbol('{')? {
                    let items = self.parse_use_group_items()?;
                    return Ok(UseTree::Group {
                        leading_colon,
                        prefix: segments,
                        items,
                    });
                }
                let ident = self.parse_use_segment_ident()?;
                segments.push(PathSegment {
                    ident,
                    generics: Vec::new(),
                });
                continue;
            }
            break;
        }

        let alias = if self.match_keyword("as")? {
            Some(self.expect_identifier()?)
        } else {
            None
        };
        Ok(UseTree::Path {
            leading_colon,
            segments,
            alias,
        })
    }

    fn parse_use_group_items(&mut self) -> Result<Vec<UseTree>, ParseError> {
        let mut items = Vec::new();
        if !self.check_symbol('}') {
            loop {
                // Intentionally left without logging in production builds.
                let item = if self.match_operator("*")? {
                    UseTree::Glob {
                        leading_colon: false,
                        segments: Vec::new(),
                    }
                } else {
                    let leading = self.match_operator("::")?;
                    self.parse_use_tree_with_prefix(leading, Vec::new())?
                };
                items.push(item);
                if self.match_symbol(',')? {
                    if self.check_symbol('}') {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        self.expect_symbol('}')?;
        Ok(items)
    }

    fn parse_use_segment_ident(&mut self) -> Result<String, ParseError> {
        match self.peek_kind() {
            TokenKind::Identifier(_) => self.expect_identifier(),
            TokenKind::Keyword(kw) if *kw == "crate" || *kw == "self" || *kw == "super" => {
                let kw = if let TokenKind::Keyword(name) = self.peek_kind() {
                    *name
                } else {
                    unreachable!()
                };
                self.advance();
                Ok(kw.to_string())
            }
            other => Err(ParseError::UnexpectedToken {
                expected: "path segment".into(),
                found: Box::new(other.clone()),
                span: self.current_span(),
            }),
        }
    }

    fn parse_module_decl(
        &mut self,
        mut docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        visibility: Visibility,
        span: Span,
    ) -> Result<ModuleDecl, ParseError> {
        let decl_id = self.allocate_id();
        let name = self.expect_identifier()?;
        if self.match_symbol(';')? {
            return Ok(ModuleDecl {
                attributes,
                docs,
                visibility,
                name,
                body: None,
                span,
                id: decl_id,
            });
        }
        self.expect_symbol('{')?;
        while let TokenKind::DocComment(kind, text) = self.peek_kind().clone() {
            if matches!(kind, DocCommentTokenKind::Inner) {
                let doc_span = self.current_span();
                self.advance();
                docs.push(DocComment {
                    kind: DocCommentKind::Inner,
                    text,
                    span: doc_span,
                });
                continue;
            }
            break;
        }
        let mut items = Vec::new();
        while !self.check_symbol('}') {
            items.push(self.parse_item()?);
        }
        self.expect_symbol('}')?;
        let body = Module {
            id: self.allocate_id(),
            span,
            docs: docs.clone(),
            items,
        };
        Ok(ModuleDecl {
            attributes,
            docs,
            visibility,
            name,
            body: Some(body),
            span,
            id: decl_id,
        })
    }

    fn parse_macro_decl(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        visibility: Visibility,
        keyword_span: Span,
    ) -> Result<MacroDecl, ParseError> {
        let macro_start = self.pos.saturating_sub(1);
        let name = self.expect_identifier()?;
        let pattern = if self.match_symbol('(')? {
            let mut params = Vec::new();
            if !self.check_symbol(')') {
                loop {
                    let name = self.expect_identifier()?;
                    params.push(MacroPattern::Fragment {
                        name,
                        kind: MacroFragmentKind::Expr,
                        repeat: MacroRepeatKind::None,
                    });
                    if self.match_symbol(',')? {
                        if self.check_symbol(')') {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            self.expect_symbol(')')?;
            params
        } else {
            Vec::new()
        };
        let body = self.parse_block()?;
        let rule_span = self.span_between(macro_start, self.pos.saturating_sub(1));
        let span = self.span_since(macro_start).merge(keyword_span);
        Ok(MacroDecl {
            attributes,
            docs,
            visibility,
            name,
            rules: vec![MacroRule {
                pattern,
                body,
                span: rule_span,
            }],
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_macro_rules_decl(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        visibility: Visibility,
        keyword_span: Span,
    ) -> Result<MacroDecl, ParseError> {
        self.expect_operator("!")?;
        let macro_start = self.pos.saturating_sub(1);
        let name = self.expect_identifier()?;
        self.expect_symbol('{')?;

        let mut rules = Vec::new();
        while !self.check_symbol('}') {
            let rule_start = self.pos;
            self.expect_symbol('(')?;
            let pattern = self.parse_macro_pattern_sequence(')')?;
            self.expect_symbol(')')?;
            self.expect_operator("=>")?;
            let body = self.parse_block()?;
            let span = self.span_between(rule_start, self.pos.saturating_sub(1));
            rules.push(MacroRule {
                pattern,
                body,
                span,
            });
            let _ = self.match_symbol(';')?;
        }
        self.expect_symbol('}')?;

        if rules.is_empty() {
            return Err(self.error_message("macro_rules! must contain at least one rule"));
        }

        let span = self.span_since(macro_start).merge(keyword_span);
        Ok(MacroDecl {
            attributes,
            docs,
            visibility,
            name,
            rules,
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_macro_pattern_sequence(
        &mut self,
        terminator: char,
    ) -> Result<Vec<MacroPattern>, ParseError> {
        let mut patterns = Vec::new();
        while !self.check_symbol(terminator) {
            patterns.push(self.parse_macro_pattern()?);
            if self.match_symbol(',')? {
                if self.check_symbol(terminator) {
                    break;
                }
                continue;
            }
            if self.match_symbol(';')? {
                if self.check_symbol(terminator) {
                    break;
                }
                continue;
            }
        }
        Ok(patterns)
    }

    fn parse_macro_pattern(&mut self) -> Result<MacroPattern, ParseError> {
        self.expect_operator("$")?;
        if self.match_symbol('(')? {
            let elements = self.parse_macro_pattern_sequence(')')?;
            self.expect_symbol(')')?;
            let separator = if self.match_symbol(',')? {
                Some(",".into())
            } else if self.match_symbol(';')? {
                Some(";".into())
            } else {
                None
            };
            let repeat = self.parse_macro_repeat_kind()?;
            Ok(MacroPattern::Group {
                elements,
                separator,
                repeat,
            })
        } else {
            let name = self.expect_identifier()?;
            let kind = if self.match_operator(":")? {
                let spec = match self.peek_kind() {
                    TokenKind::Identifier(id) => {
                        let s = id.clone();
                        self.advance();
                        s
                    }
                    TokenKind::Keyword(kw) => {
                        let s = kw.to_string();
                        self.advance();
                        s
                    }
                    _ => return Err(self.error_expected("fragment specifier")),
                };
                match spec.as_str() {
                    "expr" => MacroFragmentKind::Expr,
                    "ident" => MacroFragmentKind::Ident,
                    "block" => MacroFragmentKind::Block,
                    _ => {
                        return Err(self.error_message("unsupported macro fragment kind"));
                    }
                }
            } else {
                MacroFragmentKind::Expr
            };
            let repeat = self.parse_macro_repeat_kind()?;
            Ok(MacroPattern::Fragment { name, kind, repeat })
        }
    }

    fn parse_macro_repeat_kind(&mut self) -> Result<MacroRepeatKind, ParseError> {
        if self.match_operator("?")? {
            Ok(MacroRepeatKind::Optional)
        } else if self.match_operator("*")? {
            Ok(MacroRepeatKind::ZeroOrMore)
        } else if self.match_operator("+")? {
            Ok(MacroRepeatKind::OneOrMore)
        } else {
            Ok(MacroRepeatKind::None)
        }
    }

    fn parse_function_decl(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        visibility: Visibility,
        constness: bool,
        asyncness: bool,
        span: Span,
    ) -> Result<FunctionDecl, ParseError> {
        let name = self.expect_identifier()?;
        let signature = self.parse_function_signature(name, constness, asyncness)?;
        let body = self.parse_block()?;
        Ok(FunctionDecl {
            attributes,
            docs,
            visibility,
            signature,
            body,
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_function_signature(
        &mut self,
        name: String,
        constness: bool,
        asyncness: bool,
    ) -> Result<FunctionSig, ParseError> {
        let generics = self.parse_generic_params()?;
        self.expect_symbol('(')?;
        let mut params = Vec::new();
        if !self.check_symbol(')') {
            loop {
                let param_name = self.expect_identifier()?;
                self.expect_operator(":")?;
                let ty = self.parse_type()?;
                params.push(Param {
                    name: param_name,
                    ty,
                });
                if self.match_symbol(',')? {
                    if self.check_symbol(')') {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        self.expect_symbol(')')?;
        let return_type = if self.match_operator("->")? {
            Some(self.parse_type()?)
        } else {
            None
        };
        let effect = if self.match_operator("!")? {
            Some(self.parse_effect_row()?)
        } else {
            None
        };
        let where_clause = if self.match_keyword("where")? {
            self.parse_where_clause()?
        } else {
            Vec::new()
        };
        Ok(FunctionSig {
            constness,
            asyncness,
            name,
            generics,
            params,
            return_type,
            effect,
            where_clause,
        })
    }

    fn parse_effect_row(&mut self) -> Result<EffectExpr, ParseError> {
        if !self.check_operator("<") {
            let identifier = self.expect_identifier()?;
            return Ok(EffectExpr {
                labels: Vec::new(),
                tail: Some(identifier),
            });
        }

        self.expect_operator("<")?;
        let mut labels = Vec::new();
        let mut tail = None;
        if self.match_operator(">")? {
            return Ok(EffectExpr { labels, tail });
        }

        loop {
            let name = self.expect_identifier()?;
            let args = self.parse_effect_label_args()?;
            labels.push(EffectLabel { name, args });

            if self.match_symbol(',')? {
                if self.check_operator(">") {
                    break;
                }
                continue;
            }

            if self.match_operator("|")? {
                tail = Some(self.expect_identifier()?);
                break;
            }

            if self.check_operator(">") {
                break;
            }

            return Err(ParseError::UnexpectedToken {
                expected: "effect row separator".into(),
                found: Box::new(self.peek_kind().clone()),
                span: self.current_span(),
            });
        }

        self.expect_operator(">")?;
        Ok(EffectExpr { labels, tail })
    }

    fn parse_effect_label_args(&mut self) -> Result<Vec<Expr>, ParseError> {
        if !self.match_symbol('(')? {
            return Ok(Vec::new());
        }
        let mut args = Vec::new();
        if !self.check_symbol(')') {
            loop {
                args.push(self.parse_expression()?);
                if self.match_symbol(',')? {
                    if self.check_symbol(')') {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        self.expect_symbol(')')?;
        Ok(args)
    }

    fn parse_struct_decl(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        visibility: Visibility,
        span: Span,
    ) -> Result<StructDecl, ParseError> {
        let name = self.expect_identifier()?;
        let generics = self.parse_generic_params()?;
        let kind = if self.match_symbol('{')? {
            let mut fields = Vec::new();
            if !self.check_symbol('}') {
                loop {
                    let field_span = self.current_span();
                    let (field_docs, field_attrs) = self.parse_attributes()?;
                    let field_vis = self.parse_visibility()?;
                    let field_name = self.expect_identifier()?;
                    self.expect_operator(":")?;
                    let ty = self.parse_type()?;
                    fields.push(StructField {
                        attributes: field_attrs,
                        docs: field_docs,
                        visibility: field_vis,
                        name: Some(field_name),
                        ty,
                        span: field_span,
                        id: self.allocate_id(),
                    });
                    if self.match_symbol(',')? {
                        if self.check_symbol('}') {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            self.expect_symbol('}')?;
            StructKind::Record(fields)
        } else if self.match_symbol('(')? {
            let mut fields = Vec::new();
            if !self.check_symbol(')') {
                loop {
                    let field_span = self.current_span();
                    let (field_docs, field_attrs) = self.parse_attributes()?;
                    let field_vis = self.parse_visibility()?;
                    let ty = self.parse_type()?;
                    fields.push(StructField {
                        attributes: field_attrs,
                        docs: field_docs,
                        visibility: field_vis,
                        name: None,
                        ty,
                        span: field_span,
                        id: self.allocate_id(),
                    });
                    if self.match_symbol(',')? {
                        if self.check_symbol(')') {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            self.expect_symbol(')')?;
            StructKind::Tuple(fields)
        } else {
            self.expect_symbol(';')?;
            StructKind::Unit
        };
        let where_clause = if self.match_keyword("where")? {
            self.parse_where_clause()?
        } else {
            Vec::new()
        };
        Ok(StructDecl {
            attributes,
            docs,
            visibility,
            name,
            generics,
            kind,
            where_clause,
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_enum_decl(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        visibility: Visibility,
        span: Span,
    ) -> Result<EnumDecl, ParseError> {
        let name = self.expect_identifier()?;
        let generics = self.parse_generic_params()?;
        self.expect_symbol('{')?;
        let mut variants = Vec::new();
        while !self.check_symbol('}') {
            let (variant_docs, variant_attrs) = self.parse_attributes()?;
            let variant_name = self.expect_identifier()?;
            let variant_span = self.span_at(self.pos - 1);
            let kind = if self.match_symbol('{')? {
                let mut fields = Vec::new();
                if !self.check_symbol('}') {
                    loop {
                        let field_span = self.current_span();
                        let (field_docs, field_attrs) = self.parse_attributes()?;
                        let field_vis = self.parse_visibility()?;
                        let field_name = self.expect_identifier()?;
                        self.expect_operator(":")?;
                        let ty = self.parse_type()?;
                        fields.push(StructField {
                            attributes: field_attrs,
                            docs: field_docs,
                            visibility: field_vis,
                            name: Some(field_name),
                            ty,
                            span: field_span,
                            id: self.allocate_id(),
                        });
                        if self.match_symbol(',')? {
                            if self.check_symbol('}') {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                self.expect_symbol('}')?;
                EnumVariantKind::Struct(fields)
            } else if self.match_symbol('(')? {
                let mut fields = Vec::new();
                if !self.check_symbol(')') {
                    loop {
                        let field_span = self.current_span();
                        let (field_docs, attrs) = self.parse_attributes()?;
                        let vis = self.parse_visibility()?;
                        let ty = self.parse_type()?;
                        fields.push(StructField {
                            attributes: attrs,
                            docs: field_docs,
                            visibility: vis,
                            name: None,
                            ty,
                            span: field_span,
                            id: self.allocate_id(),
                        });
                        if self.match_symbol(',')? {
                            if self.check_symbol(')') {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                self.expect_symbol(')')?;
                EnumVariantKind::Tuple(fields)
            } else {
                EnumVariantKind::Unit
            };
            if self.match_symbol(',')? {
                // continue
            }
            variants.push(EnumVariant {
                attributes: variant_attrs,
                docs: variant_docs,
                name: variant_name,
                kind,
                span: variant_span,
                id: self.allocate_id(),
            });
        }
        self.expect_symbol('}')?;
        let where_clause = if self.match_keyword("where")? {
            self.parse_where_clause()?
        } else {
            Vec::new()
        };
        Ok(EnumDecl {
            attributes,
            docs,
            visibility,
            name,
            generics,
            variants,
            where_clause,
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_trait_decl(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        visibility: Visibility,
        span: Span,
    ) -> Result<TraitDecl, ParseError> {
        let name = self.expect_identifier()?;
        let generics = self.parse_generic_params()?;
        let mut super_traits = Vec::new();
        if self.match_operator(":")? {
            super_traits.push(self.parse_path_type()?);
            while self.match_operator("+")? {
                super_traits.push(self.parse_path_type()?);
            }
        }
        self.expect_symbol('{')?;
        let mut items = Vec::new();
        while !self.check_symbol('}') {
            let (item_docs, item_attrs) = self.parse_attributes()?;
            if self.match_keyword("fn")? {
                let item_span = self.span_at(self.pos - 1);
                let name = self.expect_identifier()?;
                let sig = self.parse_function_signature(name, false, false)?;
                let default = if self.check_symbol('{') {
                    Some(Box::new(self.parse_block()?))
                } else {
                    self.expect_symbol(';')?;
                    None
                };
                items.push(TraitItem::Function {
                    signature: Box::new(sig),
                    default,
                    attributes: item_attrs,
                    docs: item_docs,
                    span: item_span,
                    id: self.allocate_id(),
                });
                continue;
            }
            if self.match_keyword("type")? {
                let item_span = self.span_at(self.pos - 1);
                let assoc = self.expect_identifier()?;
                self.expect_symbol(';')?;
                items.push(TraitItem::AssociatedType {
                    name: assoc,
                    attributes: item_attrs,
                    docs: item_docs,
                    span: item_span,
                    id: self.allocate_id(),
                });
                continue;
            }
            if self.match_keyword("effect")? {
                let item_span = self.span_at(self.pos - 1);
                let name = self.expect_identifier()?;
                self.expect_operator("=")?;
                let effect = self.parse_effect_row()?;
                self.expect_symbol(';')?;
                items.push(TraitItem::AssociatedEffect {
                    name,
                    effect,
                    attributes: item_attrs,
                    docs: item_docs,
                    span: item_span,
                    id: self.allocate_id(),
                });
                continue;
            }
            return Err(self.error_expected("trait item"));
        }
        self.expect_symbol('}')?;
        Ok(TraitDecl {
            attributes,
            docs,
            visibility,
            name,
            generics,
            super_traits,
            items,
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_impl_decl(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        span: Span,
    ) -> Result<ImplDecl, ParseError> {
        let generics = self.parse_generic_params()?;
        let checkpoint = self.checkpoint();
        let mut trait_ref = None;
        let for_type;

        if let Some(candidate_trait) = self.try_parse_path_type() {
            if self.match_keyword("for")? {
                trait_ref = Some(candidate_trait);
                for_type = self.parse_type()?;
            } else {
                self.rewind(checkpoint);
                for_type = self.parse_type()?;
            }
        } else {
            self.rewind(checkpoint);
            for_type = self.parse_type()?;
        }

        let where_clause = if self.match_keyword("where")? {
            self.parse_where_clause()?
        } else {
            Vec::new()
        };
        self.expect_symbol('{')?;
        let mut items = Vec::new();
        while !self.check_symbol('}') {
            let (item_docs, item_attrs) = self.parse_attributes()?;
            if self.match_keyword("fn")? {
                let item_span = self.span_at(self.pos - 1);
                let name = self.expect_identifier()?;
                let signature = self.parse_function_signature(name, false, false)?;
                let body = self.parse_block()?;
                items.push(ImplItem::Function {
                    signature: Box::new(signature),
                    body: Box::new(body),
                    attributes: item_attrs,
                    docs: item_docs,
                    span: item_span,
                    id: self.allocate_id(),
                });
                continue;
            }
            if self.match_keyword("type")? {
                let item_span = self.span_at(self.pos - 1);
                let name = self.expect_identifier()?;
                self.expect_operator("=")?;
                let ty = self.parse_type()?;
                self.expect_symbol(';')?;
                items.push(ImplItem::AssociatedType {
                    name,
                    ty,
                    attributes: item_attrs,
                    docs: item_docs,
                    span: item_span,
                    id: self.allocate_id(),
                });
                continue;
            }
            if self.match_keyword("effect")? {
                let item_span = self.span_at(self.pos - 1);
                let name = self.expect_identifier()?;
                self.expect_operator("=")?;
                let effect = self.parse_effect_row()?;
                self.expect_symbol(';')?;
                items.push(ImplItem::AssociatedEffect {
                    name,
                    effect,
                    attributes: item_attrs,
                    docs: item_docs,
                    span: item_span,
                    id: self.allocate_id(),
                });
                continue;
            }
            return Err(self.error_expected("impl item"));
        }
        self.expect_symbol('}')?;
        Ok(ImplDecl {
            attributes,
            docs,
            generics,
            trait_ref,
            for_type,
            where_clause,
            items,
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_extern_block(
        &mut self,
        docs: Vec<DocComment>,
        attributes: Vec<Attribute>,
        _visibility: Visibility,
        span: Span,
    ) -> Result<ExternBlock, ParseError> {
        let abi = if self.check_string_literal() {
            let lit = match self.peek_kind().clone() {
                TokenKind::StringLiteral(s) => s,
                _ => unreachable!(),
            };
            self.advance();
            Some(lit)
        } else {
            None
        };
        self.expect_symbol('{')?;
        let mut items = Vec::new();
        while !self.check_symbol('}') {
            let (item_docs, item_attrs) = self.parse_attributes()?;
            if self.match_keyword("fn")? {
                let item_span = self.span_at(self.pos - 1);
                let name = self.expect_identifier()?;
                let sig = self.parse_function_signature(name, false, false)?;
                self.expect_symbol(';')?;
                items.push(ExternItem::Function {
                    signature: sig,
                    attributes: item_attrs,
                    docs: item_docs,
                    span: item_span,
                    id: self.allocate_id(),
                });
                continue;
            }
            if self.match_keyword("static")? {
                let item_span = self.span_at(self.pos - 1);
                let mutable = self.match_keyword("mut")?;
                let name = self.expect_identifier()?;
                self.expect_operator(":")?;
                let ty = self.parse_type()?;
                self.expect_symbol(';')?;
                items.push(ExternItem::Static {
                    name,
                    ty,
                    mutable,
                    attributes: item_attrs,
                    docs: item_docs,
                    span: item_span,
                    id: self.allocate_id(),
                });
                continue;
            }
            return Err(self.error_expected("extern item"));
        }
        self.expect_symbol('}')?;
        Ok(ExternBlock {
            attributes,
            docs,
            abi,
            items,
            span,
            id: self.allocate_id(),
        })
    }

    fn parse_generic_params(&mut self) -> Result<GenericParams, ParseError> {
        if !self.match_operator("<")? {
            return Ok(GenericParams::default());
        }
        let mut params = Vec::new();
        if !self.check_operator(">") {
            loop {
                if let TokenKind::Lifetime(name) = self.peek_kind().clone() {
                    self.advance();
                    let mut bounds = Vec::new();
                    if self.match_operator(":")? {
                        loop {
                            match self.peek_kind().clone() {
                                TokenKind::Lifetime(bound) => {
                                    self.advance();
                                    bounds.push(bound);
                                }
                                _ => return Err(self.error_expected("lifetime bound")),
                            }
                            if !self.match_operator("+")? {
                                break;
                            }
                        }
                    }
                    params.push(GenericParam {
                        name,
                        kind: GenericParamKind::Lifetime { bounds },
                    });
                } else if self.match_keyword("const")? {
                    let name = self.expect_identifier()?;
                    self.expect_operator(":")?;
                    let ty = self.parse_type()?;
                    params.push(GenericParam {
                        name,
                        kind: GenericParamKind::Const { ty: Box::new(ty) },
                    });
                } else {
                    let name = self.expect_identifier()?;
                    let mut bounds = Vec::new();
                    if self.match_operator(":")? {
                        bounds.push(self.parse_path_type()?);
                        while self.match_operator("+")? {
                            bounds.push(self.parse_path_type()?);
                        }
                    }
                    params.push(GenericParam {
                        name,
                        kind: GenericParamKind::Type { bounds },
                    });
                }
                if self.match_symbol(',')? {
                    if self.check_operator(">") {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        self.expect_operator(">")?;
        Ok(GenericParams { params })
    }

    fn parse_where_clause(&mut self) -> Result<Vec<WherePredicate>, ParseError> {
        let mut predicates = Vec::new();
        loop {
            let path = self.parse_path_type()?;
            if self.match_operator("=")? {
                let ty = self.parse_type()?;
                predicates.push(WherePredicate::Equality { path, ty });
            } else {
                let mut bounds = Vec::new();
                self.expect_operator(":")?;
                bounds.push(self.parse_path_type()?);
                while self.match_operator("+")? {
                    bounds.push(self.parse_path_type()?);
                }
                predicates.push(WherePredicate::Bound {
                    target: TypeExpr::Path(path),
                    bounds,
                });
            }
            if !self.match_symbol(',')? {
                break;
            }
        }
        Ok(predicates)
    }

    fn is_item_start(&self) -> bool {
        match self.peek_kind() {
            TokenKind::Operator(op) if op == "@" => true,
            TokenKind::Keyword(kw) if *kw == "pub" => true,
            TokenKind::Keyword(kw)
                if matches!(
                    *kw,
                    "use"
                        | "mod"
                        | "macro"
                        | "macro_rules"
                        | "struct"
                        | "enum"
                        | "trait"
                        | "impl"
                        | "extern"
                        | "fn"
                ) =>
            {
                true
            }
            TokenKind::Keyword(kw) if *kw == "async" => matches!(
                self.peek_next_kind(),
                Some(TokenKind::Keyword(next)) if *next == "fn"
            ),
            _ => false,
        }
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let open_index = self.pos;
        self.expect_symbol('{')?;
        let open_span = self.span_at(open_index);
        let mut statements = Vec::new();
        let mut tail = None;
        while !self.check_symbol('}') {
            if self.is_item_start() {
                let item = self.parse_item()?;
                let item_span = item_span(&item);
                statements.push(Statement::Item(item, item_span));
                continue;
            }
            if self.match_keyword("let")? {
                let let_start = self.pos - 1;
                let pattern = self.parse_pattern()?;
                let ty = if self.match_operator(":")? {
                    Some(self.parse_type()?)
                } else {
                    None
                };
                let value = if self.match_operator("=")? {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.expect_symbol(';')?;
                let end_index = self.pos - 1;
                let span = self.span_between(let_start, end_index);
                statements.push(Statement::Let {
                    pattern,
                    ty,
                    value,
                    span,
                });
                continue;
            }
            if self.match_keyword("return")? {
                let start_index = self.pos - 1;
                if self.check_symbol(';') {
                    self.advance();
                    let span = self.span_between(start_index, self.pos - 1);
                    statements.push(Statement::Return { value: None, span });
                } else {
                    let expr = self.parse_expression()?;
                    self.expect_symbol(';')?;
                    let span = self.span_between(start_index, self.pos - 1);
                    statements.push(Statement::Return {
                        value: Some(expr),
                        span,
                    });
                }
                continue;
            }
            let expr = self.parse_expression()?;
            if self.match_symbol(';')? {
                let end_span = self.span_at(self.pos - 1);
                let span = Span::from_range(expr.span.file, expr.span.start, end_span.end);
                statements.push(Statement::Expr(expr, span));
            } else {
                tail = Some(Box::new(expr));
                break;
            }
        }
        self.expect_symbol('}')?;
        let close_span = self.span_at(self.pos - 1);
        let span = open_span.merge(close_span);
        Ok(Block {
            statements,
            tail,
            span,
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        self.parse_pattern_or()
    }

    fn parse_pattern_or(&mut self) -> Result<Pattern, ParseError> {
        let mut patterns = vec![self.parse_pattern_binding()?];
        while self.match_operator("|")? {
            patterns.push(self.parse_pattern_binding()?);
        }
        if patterns.len() == 1 {
            Ok(patterns.pop().unwrap())
        } else {
            Ok(Pattern::Or(patterns))
        }
    }

    fn parse_pattern_binding(&mut self) -> Result<Pattern, ParseError> {
        let is_move = self.match_keyword("move")?;
        let mutable = self.match_keyword("mut")?;

        if mutable {
            if !matches!(self.peek_kind(), TokenKind::Identifier(_)) {
                return Err(self.error_expected("identifier after `mut`"));
            }
            let name = self.expect_identifier()?;
            if self.match_operator("@")? {
                let subpattern = self.parse_pattern_binding()?;
                let binding = Pattern::Binding {
                    name,
                    mutable: true,
                    is_move,
                    subpattern: Box::new(subpattern),
                };
                return Ok(binding);
            }
            let base = Pattern::Ident(name.clone());
            let binding = Pattern::Binding {
                name,
                mutable: true,
                is_move,
                subpattern: Box::new(base),
            };
            return Ok(binding);
        }

        if matches!(self.peek_kind(), TokenKind::Identifier(_)) {
            if let Some(TokenKind::Operator(op)) = self.peek_next_kind() {
                if op == "@" {
                    let name = self.expect_identifier()?;
                    self.expect_operator("@")?;
                    let subpattern = self.parse_pattern_binding()?;
                    let binding = Pattern::Binding {
                        name,
                        mutable: false,
                        is_move,
                        subpattern: Box::new(subpattern),
                    };
                    return Ok(binding);
                }
            }
        }

        let mut pattern = self.parse_pattern_prefix()?;
        if is_move {
            pattern = Pattern::Move(Box::new(pattern));
        }
        Ok(pattern)
    }

    fn parse_pattern_prefix(&mut self) -> Result<Pattern, ParseError> {
        if self.match_operator("&")? {
            let mutable = self.match_keyword("mut")?;
            let inner = self.parse_pattern_prefix()?;
            return Ok(Pattern::Reference {
                mutable,
                inner: Box::new(inner),
            });
        }
        self.parse_pattern_primary()
    }

    fn parse_pattern_primary(&mut self) -> Result<Pattern, ParseError> {
        match self.peek_kind().clone() {
            TokenKind::Identifier(id) if id == "_" => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            TokenKind::Identifier(_) => {
                let path = self.parse_path_expr()?;
                if self.match_symbol('{')? {
                    let mut fields = Vec::new();
                    let mut rest = false;
                    if !self.check_symbol('}') {
                        loop {
                            if self.match_operator("..")? {
                                rest = true;
                                break;
                            }
                            let name = self.expect_identifier()?;
                            let pattern = if self.match_operator(":")? {
                                Some(self.parse_pattern()?)
                            } else {
                                None
                            };
                            fields.push(StructPatternField { name, pattern });
                            if self.match_symbol(',')? {
                                if self.check_symbol('}') {
                                    break;
                                }
                                continue;
                            }
                            break;
                        }
                    }
                    self.expect_symbol('}')?;
                    Ok(Pattern::Struct { path, fields, rest })
                } else if self.match_symbol('(')? {
                    let mut elems = Vec::new();
                    if !self.check_symbol(')') {
                        loop {
                            elems.push(self.parse_pattern()?);
                            if self.match_symbol(',')? {
                                if self.check_symbol(')') {
                                    break;
                                }
                                continue;
                            }
                            break;
                        }
                    }
                    self.expect_symbol(')')?;
                    Ok(Pattern::Enum {
                        path,
                        inner: if elems.is_empty() {
                            None
                        } else if elems.len() == 1 {
                            Some(Box::new(elems.remove(0)))
                        } else {
                            Some(Box::new(Pattern::Tuple(elems)))
                        },
                    })
                } else if path.segments.len() == 1 {
                    Ok(Pattern::Ident(path.segments[0].ident.clone()))
                } else {
                    Ok(Pattern::Enum { path, inner: None })
                }
            }
            TokenKind::Delimiter('(') => {
                self.advance();
                let mut elems = Vec::new();
                if !self.check_symbol(')') {
                    loop {
                        elems.push(self.parse_pattern()?);
                        if self.match_symbol(',')? {
                            if self.check_symbol(')') {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                self.expect_symbol(')')?;
                Ok(Pattern::Tuple(elems))
            }
            TokenKind::Delimiter('[') => {
                self.advance();
                let mut prefix = Vec::new();
                let mut suffix = Vec::new();
                let mut rest_pattern = None;
                let mut seen_rest = false;
                while !self.check_symbol(']') {
                    if self.match_operator("..")? {
                        if rest_pattern.is_some() {
                            let span = self.span_at(self.pos.saturating_sub(1));
                            return Err(ParseError::UnexpectedToken {
                                expected: "single slice rest".into(),
                                found: Box::new(TokenKind::Operator("..".into())),
                                span,
                            });
                        }
                        if self.check_symbol(']') || self.check_symbol(',') {
                            rest_pattern = Some(Box::new(Pattern::Wildcard));
                        } else {
                            rest_pattern = Some(Box::new(self.parse_pattern()?));
                        }
                        seen_rest = true;
                    } else {
                        let pat = self.parse_pattern()?;
                        if seen_rest {
                            suffix.push(pat);
                        } else {
                            prefix.push(pat);
                        }
                    }
                    if self.match_symbol(',')? {
                        continue;
                    } else {
                        break;
                    }
                }
                self.expect_symbol(']')?;
                Ok(Pattern::Slice {
                    prefix,
                    rest: rest_pattern,
                    suffix,
                })
            }
            TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::StringLiteral(_)
            | TokenKind::Boolean(_) => {
                let lit = self.parse_literal()?;
                Ok(Pattern::Literal(lit))
            }
            other => Err(ParseError::UnexpectedToken {
                expected: "pattern".into(),
                found: Box::new(other),
                span: self.current_span(),
            }),
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_logic_or()?;
        if let Some(op) = self.match_assignment_op()? {
            let value = self.parse_assignment()?;
            expr = self.make_assignment(expr, op, value);
        }
        Ok(expr)
    }

    fn parse_logic_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_logic_and()?;
        while self.match_operator("||")? {
            let right = self.parse_logic_and()?;
            expr = self.make_binary(BinaryOp::Or, expr, right);
        }
        Ok(expr)
    }

    fn parse_logic_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;
        while self.match_operator("&&")? {
            let right = self.parse_equality()?;
            expr = self.make_binary(BinaryOp::And, expr, right);
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_comparison()?;
        loop {
            if self.match_operator("==")? {
                let right = self.parse_comparison()?;
                expr = self.make_binary(BinaryOp::Eq, expr, right);
            } else if self.match_operator("!=")? {
                let right = self.parse_comparison()?;
                expr = self.make_binary(BinaryOp::NotEq, expr, right);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_term()?;
        loop {
            if self.match_operator("<")? {
                let right = self.parse_term()?;
                expr = self.make_binary(BinaryOp::Lt, expr, right);
            } else if self.match_operator(">")? {
                let right = self.parse_term()?;
                expr = self.make_binary(BinaryOp::Gt, expr, right);
            } else if self.match_operator("<=")? {
                let right = self.parse_term()?;
                expr = self.make_binary(BinaryOp::Le, expr, right);
            } else if self.match_operator(">=")? {
                let right = self.parse_term()?;
                expr = self.make_binary(BinaryOp::Ge, expr, right);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;
        loop {
            if self.match_operator("+")? {
                let right = self.parse_factor()?;
                expr = self.make_binary(BinaryOp::Add, expr, right);
            } else if self.match_operator("-")? {
                let right = self.parse_factor()?;
                expr = self.make_binary(BinaryOp::Sub, expr, right);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        loop {
            if self.match_operator("*")? {
                let right = self.parse_unary()?;
                expr = self.make_binary(BinaryOp::Mul, expr, right);
            } else if self.match_operator("/")? {
                let right = self.parse_unary()?;
                expr = self.make_binary(BinaryOp::Div, expr, right);
            } else if self.match_operator("%")? {
                let right = self.parse_unary()?;
                expr = self.make_binary(BinaryOp::Rem, expr, right);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        let start = self.pos;
        if matches!(self.peek_kind(), TokenKind::Identifier(name) if name == "handle")
            && matches!(self.peek_next_kind(), Some(TokenKind::Delimiter('{')))
        {
            self.advance();
            let block = self.parse_block()?;
            let body_expr = Expr::dummy(ExprKind::Block(block));
            match self.peek_kind() {
                TokenKind::Identifier(name) if name == "with" => self.advance(),
                TokenKind::Keyword(kw) if *kw == "with" => self.advance(),
                _ => return Err(self.error_expected("with")),
            }
            self.expect_symbol('{')?;
            let mut handlers = Vec::new();
            if !self.check_symbol('}') {
                loop {
                    let label = self.expect_identifier()?;
                    let span = self.span_at(self.pos - 1);
                    self.expect_operator("=>")?;
                    let block = self.parse_block()?;
                    handlers.push(EffectHandler {
                        label,
                        body: block,
                        span,
                    });
                    if self.match_symbol(',')? {
                        if self.check_symbol('}') {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            self.expect_symbol('}')?;
            let mut expr = Expr::dummy(ExprKind::Handle {
                body: Box::new(body_expr),
                handlers,
            });
            expr.span = self.span_since(start);
            return Ok(expr);
        }
        if self.match_keyword("await")? {
            let expr = self.parse_unary()?;
            let mut result = Expr::dummy(ExprKind::Await(Box::new(expr)));
            result.span = self.span_since(start);
            return Ok(result);
        }
        if self.match_keyword("spawn")? {
            let expr = self.parse_unary()?;
            let mut result = Expr::dummy(ExprKind::Spawn(Box::new(expr)));
            result.span = self.span_since(start);
            return Ok(result);
        }
        if self.match_operator("-")? {
            let expr = self.parse_unary()?;
            let mut result = Expr::dummy(ExprKind::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(expr),
            });
            result.span = self.span_since(start);
            return Ok(result);
        }
        if self.match_operator("!")? {
            let expr = self.parse_unary()?;
            let mut result = Expr::dummy(ExprKind::Unary {
                op: UnaryOp::Not,
                expr: Box::new(expr),
            });
            result.span = self.span_since(start);
            return Ok(result);
        }
        if self.match_operator("&")? {
            let mutable = self.match_keyword("mut")?;
            let expr = self.parse_unary()?;
            let mut result = Expr::dummy(ExprKind::Unary {
                op: if mutable {
                    UnaryOp::RefMut
                } else {
                    UnaryOp::Ref
                },
                expr: Box::new(expr),
            });
            result.span = self.span_since(start);
            return Ok(result);
        }
        if self.match_operator("*")? {
            let expr = self.parse_unary()?;
            let mut result = Expr::dummy(ExprKind::Unary {
                op: UnaryOp::Deref,
                expr: Box::new(expr),
            });
            result.span = self.span_since(start);
            return Ok(result);
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.match_symbol('(')? {
                let mut args = Vec::new();
                if !self.check_symbol(')') {
                    loop {
                        args.push(self.parse_expression()?);
                        if self.match_symbol(',')? {
                            if self.check_symbol(')') {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                self.expect_symbol(')')?;
                let close_span = self.span_at(self.pos - 1);
                let span = Span::from_range(expr.span.file, expr.span.start, close_span.end);
                expr = Expr::new(
                    ExprKind::Call {
                        callee: Box::new(expr),
                        args,
                    },
                    span,
                );
                continue;
            }
            if self.match_symbol('.')? {
                if self.match_keyword("await")? {
                    let span = Span::from_range(
                        expr.span.file,
                        expr.span.start,
                        self.span_at(self.pos - 1).end,
                    );
                    expr = Expr::new(ExprKind::Await(Box::new(expr)), span);
                    continue;
                }
                let field = self.expect_identifier()?;
                let field_span = self.span_at(self.pos - 1);
                let mut turbofish = Vec::new();
                if self.match_operator("::<")? {
                    turbofish = self.parse_type_list()?;
                    self.expect_operator(">")?;
                }
                if self.match_symbol('(')? {
                    let mut args = Vec::new();
                    if !self.check_symbol(')') {
                        loop {
                            args.push(self.parse_expression()?);
                            if self.match_symbol(',')? {
                                if self.check_symbol(')') {
                                    break;
                                }
                                continue;
                            }
                            break;
                        }
                    }
                    self.expect_symbol(')')?;
                    let close_span = self.span_at(self.pos - 1);
                    let span = Span::from_range(expr.span.file, expr.span.start, close_span.end);
                    expr = Expr::new(
                        ExprKind::MethodCall {
                            receiver: Box::new(expr),
                            method: field,
                            turbofish,
                            args,
                        },
                        span,
                    );
                } else {
                    let span = Span::from_range(expr.span.file, expr.span.start, field_span.end);
                    expr = Expr::new(
                        ExprKind::Field {
                            base: Box::new(expr),
                            field,
                        },
                        span,
                    );
                }
                continue;
            }
            if self.match_symbol('[')? {
                let index = self.parse_expression()?;
                self.expect_symbol(']')?;
                let close_span = self.span_at(self.pos - 1);
                let span = Span::from_range(expr.span.file, expr.span.start, close_span.end);
                expr = Expr::new(
                    ExprKind::Index {
                        base: Box::new(expr),
                        index: Box::new(index),
                    },
                    span,
                );
                continue;
            }
            break;
        }
        if self.match_keyword("await")? {
            let span = Span::from_range(
                expr.span.file,
                expr.span.start,
                self.span_at(self.pos - 1).end,
            );
            expr = Expr::new(ExprKind::Await(Box::new(expr)), span);
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let start = self.pos;
        if self.peek_macro_invocation() {
            let name = match self.peek_kind().clone() {
                TokenKind::Identifier(name) => name,
                _ => unreachable!(),
            };
            self.advance();
            self.expect_operator("!")?;
            let mut args = Vec::new();
            if self.match_symbol('(')? {
                if !self.check_symbol(')') {
                    loop {
                        args.push(self.parse_expression()?);
                        if self.match_symbol(',')? {
                            if self.check_symbol(')') {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                self.expect_symbol(')')?;
            } else if self.match_symbol('{')? {
                let block = self.parse_block()?;
                args.push(Expr::dummy(ExprKind::Block(block)));
            } else {
                return Err(ParseError::Message {
                    message: Cow::Borrowed("expected macro arguments"),
                    span: self.current_span(),
                });
            }
            let expr = Expr::dummy(ExprKind::MacroCall { name, args });
            return Ok(self.attach_span(start, expr));
        }

        let expr = match self.peek_kind().clone() {
            TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::StringLiteral(_)
            | TokenKind::Boolean(_)
            | TokenKind::CharLiteral(_) => {
                let literal = self.parse_literal()?;
                Expr::dummy(ExprKind::Literal(literal))
            }
            TokenKind::Identifier(_)
            | TokenKind::Keyword("self")
            | TokenKind::Keyword("super")
            | TokenKind::Keyword("crate") => {
                let path = self.parse_path_expr()?;
                if self.check_symbol('{') {
                    let struct_expr = self.parse_struct_expr(path)?;
                    Expr::dummy(ExprKind::Struct(struct_expr))
                } else {
                    Expr::dummy(ExprKind::Path(path))
                }
            }
            TokenKind::Operator(op) if op == "::" => {
                let path = self.parse_path_expr()?;
                if self.check_symbol('{') {
                    let struct_expr = self.parse_struct_expr(path)?;
                    Expr::dummy(ExprKind::Struct(struct_expr))
                } else {
                    Expr::dummy(ExprKind::Path(path))
                }
            }
            TokenKind::Keyword("actor") => {
                self.advance();
                let path = self.parse_path_expr()?;
                let literal = self.parse_actor_literal(path)?;
                Expr::dummy(ExprKind::Actor(literal))
            }
            TokenKind::Keyword("zone") => {
                self.advance();
                let name = self.expect_identifier()?;
                let args = self.parse_zone_args()?;
                let body = self.parse_block()?;
                Expr::dummy(ExprKind::Zone { name, args, body })
            }
            TokenKind::Delimiter('(') => {
                self.advance();
                if self.check_symbol(')') {
                    self.advance();
                    Expr::dummy(ExprKind::Literal(Literal::Unit))
                } else {
                    let first = self.parse_expression()?;
                    if self.match_symbol(',')? {
                        let mut elems = vec![first];
                        loop {
                            elems.push(self.parse_expression()?);
                            if self.match_symbol(',')? {
                                if self.check_symbol(')') {
                                    break;
                                }
                                continue;
                            }
                            break;
                        }
                        self.expect_symbol(')')?;
                        Expr::dummy(ExprKind::Tuple(elems))
                    } else {
                        self.expect_symbol(')')?;
                        first
                    }
                }
            }
            TokenKind::Delimiter('[') => {
                self.advance();
                let mut elems = Vec::new();
                if !self.check_symbol(']') {
                    loop {
                        elems.push(self.parse_expression()?);
                        if self.match_symbol(',')? {
                            if self.check_symbol(']') {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                self.expect_symbol(']')?;
                Expr::dummy(ExprKind::Array(elems))
            }
            TokenKind::Keyword("if") => {
                self.advance();
                let condition = self.parse_expression()?;
                let then_branch = self.parse_block()?;
                let else_branch = if self.match_keyword("else")? {
                    Some(Box::new(if self.check_keyword("if") {
                        self.parse_expression()?
                    } else {
                        Expr::dummy(ExprKind::Block(self.parse_block()?))
                    }))
                } else {
                    None
                };
                Expr::dummy(ExprKind::If {
                    condition: Box::new(condition),
                    then_branch,
                    else_branch,
                })
            }
            TokenKind::Keyword("match") => {
                self.advance();
                let scrutinee = self.parse_expression()?;
                self.expect_symbol('{')?;
                let mut arms = Vec::new();
                while !self.check_symbol('}') {
                    let pattern = self.parse_pattern()?;
                    let guard = if self.match_keyword("if")? {
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };
                    self.expect_operator("=>")?;
                    let body = if self.check_symbol('{') {
                        Expr::dummy(ExprKind::Block(self.parse_block()?))
                    } else {
                        self.parse_expression()?
                    };
                    if self.match_symbol(',')? {
                        // optional comma
                    }
                    arms.push(MatchArm {
                        pattern,
                        guard,
                        body,
                    });
                }
                self.expect_symbol('}')?;
                Expr::dummy(ExprKind::Match {
                    scrutinee: Box::new(scrutinee),
                    arms,
                })
            }
            TokenKind::Keyword("loop") => {
                self.advance();
                let body = self.parse_block()?;
                Expr::dummy(ExprKind::Loop(body))
            }
            TokenKind::Keyword("while") => {
                self.advance();
                let condition = self.parse_expression()?;
                let body = self.parse_block()?;
                Expr::dummy(ExprKind::While {
                    condition: Box::new(condition),
                    body,
                })
            }
            TokenKind::Keyword("for") => {
                self.advance();
                let pattern = self.parse_pattern()?;
                self.expect_keyword("in")?;
                let iterator = self.parse_expression()?;
                let body = self.parse_block()?;
                Expr::dummy(ExprKind::For {
                    pattern,
                    iterator: Box::new(iterator),
                    body,
                })
            }
            TokenKind::Keyword("async") => {
                self.advance();
                let block = self.parse_block()?;
                Expr::dummy(ExprKind::Async(block))
            }
            TokenKind::Keyword("break") => {
                self.advance();
                let value = if self.check_symbol(';') || self.check_symbol('}') {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };
                Expr::dummy(ExprKind::Break(value))
            }
            TokenKind::Keyword("continue") => {
                self.advance();
                Expr::dummy(ExprKind::Continue)
            }
            TokenKind::Keyword("move") => {
                self.advance();
                let body = if self.check_symbol('{') {
                    self.parse_block()?
                } else {
                    let expr = self.parse_expression()?;
                    Block {
                        statements: Vec::new(),
                        tail: Some(Box::new(expr)),
                        span: Span::default(),
                    }
                };
                Expr::dummy(ExprKind::Closure {
                    params: Vec::new(),
                    body,
                })
            }
            TokenKind::Operator(op) if op == "|" => {
                self.advance();
                let mut params = Vec::new();
                while !self.check_operator("|") {
                    params.push(self.expect_identifier()?);
                    if self.match_symbol(',')? {
                        continue;
                    }
                }
                self.expect_operator("|")?;
                let body = if self.check_symbol('{') {
                    self.parse_block()?
                } else {
                    let expr = self.parse_expression()?;
                    Block {
                        statements: Vec::new(),
                        tail: Some(Box::new(expr)),
                        span: Span::default(),
                    }
                };
                Expr::dummy(ExprKind::Closure { params, body })
            }
            other => {
                return Err(ParseError::UnexpectedToken {
                    expected: "expression".into(),
                    found: Box::new(other),
                    span: self.current_span(),
                })
            }
        };

        Ok(self.attach_span(start, expr))
    }

    fn parse_struct_expr(&mut self, path: PathExpr) -> Result<StructExpr, ParseError> {
        self.expect_symbol('{')?;
        let mut fields = Vec::new();
        let mut shorthand = false;
        let mut spread = None;
        if !self.check_symbol('}') {
            loop {
                if self.match_operator("..")? {
                    let expr = self.parse_expression()?;
                    spread = Some(Box::new(expr));
                    break;
                }
                let name = self.expect_identifier()?;
                if self.match_operator(":")? {
                    let expr = self.parse_expression()?;
                    fields.push(StructExprField {
                        name,
                        expr: Some(expr),
                    });
                } else {
                    shorthand = true;
                    fields.push(StructExprField { name, expr: None });
                }
                if self.match_symbol(',')? {
                    if self.check_symbol('}') {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        self.expect_symbol('}')?;
        Ok(StructExpr {
            path,
            fields,
            shorthand,
            spread,
        })
    }

    fn peek_macro_invocation(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Identifier(_))
            && matches!(
                self.peek_next_kind(),
                Some(TokenKind::Operator(op)) if op == "!"
            )
    }

    fn parse_actor_literal(&mut self, path: PathExpr) -> Result<ActorLiteral, ParseError> {
        self.expect_symbol('{')?;
        let mut fields = Vec::new();
        if !self.check_symbol('}') {
            loop {
                let name = self.expect_identifier()?;
                self.expect_operator(":")?;
                let value = self.parse_expression()?;
                fields.push(ActorField { name, value });
                if self.match_symbol(',')? {
                    if self.check_symbol('}') {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        self.expect_symbol('}')?;
        Ok(ActorLiteral { path, fields })
    }

    fn match_assignment_op(&mut self) -> Result<Option<AssignmentOp>, ParseError> {
        if self.match_operator("=")? {
            return Ok(Some(AssignmentOp::Assign));
        }
        if self.match_operator("+=")? {
            return Ok(Some(AssignmentOp::AddAssign));
        }
        if self.match_operator("-=")? {
            return Ok(Some(AssignmentOp::SubAssign));
        }
        if self.match_operator("*=")? {
            return Ok(Some(AssignmentOp::MulAssign));
        }
        if self.match_operator("/=")? {
            return Ok(Some(AssignmentOp::DivAssign));
        }
        if self.match_operator("%=")? {
            return Ok(Some(AssignmentOp::RemAssign));
        }
        Ok(None)
    }

    fn parse_literal(&mut self) -> Result<Literal, ParseError> {
        match self.peek_kind().clone() {
            TokenKind::Integer(lit) => {
                self.advance();
                Ok(Literal::Integer(lit))
            }
            TokenKind::Float(lit) => {
                self.advance();
                Ok(Literal::Float(lit))
            }
            TokenKind::StringLiteral(lit) => {
                self.advance();
                Ok(Literal::String(lit))
            }
            TokenKind::CharLiteral(ch) => {
                self.advance();
                Ok(Literal::Char(ch))
            }
            TokenKind::Boolean(val) => {
                self.advance();
                Ok(Literal::Boolean(val))
            }
            other => Err(ParseError::UnexpectedToken {
                expected: "literal".into(),
                found: Box::new(other),
                span: self.current_span(),
            }),
        }
    }

    fn parse_type(&mut self) -> Result<TypeExpr, ParseError> {
        if self.match_operator("*")? {
            let mutable = if self.match_keyword("const")? {
                false
            } else if self.match_keyword("mut")? {
                true
            } else {
                return Err(self.error_expected("`const` or `mut` after `*`"));
            };
            let inner = self.parse_type()?;
            return Ok(TypeExpr::RawPointer {
                mutable,
                inner: Box::new(inner),
            });
        }
        if self.match_keyword("dyn")? {
            let path = self.parse_path_type()?;
            return Ok(TypeExpr::TraitObject(path));
        }
        if self.match_keyword("fn")? {
            self.expect_symbol('(')?;
            let mut params = Vec::new();
            if !self.check_symbol(')') {
                loop {
                    params.push(self.parse_type()?);
                    if self.match_symbol(',')? {
                        if self.check_symbol(')') {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            self.expect_symbol(')')?;
            let effect = if self.match_operator("!")? {
                Some(self.parse_effect_row()?)
            } else {
                None
            };
            self.expect_operator("->")?;
            let ret = self.parse_type()?;
            return Ok(TypeExpr::Function {
                params,
                effect,
                ret: Box::new(ret),
            });
        }
        if self.match_operator("&")? {
            let lifetime = match self.peek_kind().clone() {
                TokenKind::Lifetime(name) => {
                    self.advance();
                    Some(name)
                }
                _ => None,
            };
            let mutable = self.match_keyword("mut")?;
            let inner = self.parse_type()?;
            return Ok(TypeExpr::Reference {
                lifetime,
                mutable,
                inner: Box::new(inner),
            });
        }
        if self.match_symbol('[')? {
            let element = self.parse_type()?;
            let ty = if self.match_symbol(';')? {
                let len = self.parse_expression()?;
                self.expect_symbol(']')?;
                TypeExpr::Array {
                    element: Box::new(element),
                    length: Box::new(Some(len)),
                }
            } else {
                self.expect_symbol(']')?;
                TypeExpr::Slice(Box::new(element))
            };
            return Ok(ty);
        }
        if self.match_symbol('(')? {
            if self.check_symbol(')') {
                self.advance();
                return Ok(TypeExpr::Unit);
            }
            let mut elems = Vec::new();
            elems.push(self.parse_type()?);
            while self.match_symbol(',')? {
                if self.check_symbol(')') {
                    break;
                }
                elems.push(self.parse_type()?);
            }
            self.expect_symbol(')')?;
            if elems.len() == 1 {
                return Ok(elems.pop().unwrap());
            }
            return Ok(TypeExpr::Tuple(elems));
        }
        if self.match_operator("!")? {
            return Ok(TypeExpr::Never);
        }
        if let TokenKind::Lifetime(name) = self.peek_kind().clone() {
            self.advance();
            return Ok(TypeExpr::Lifetime(name));
        }
        let path = self.parse_path_type()?;
        Ok(TypeExpr::Path(path))
    }

    fn parse_type_list(&mut self) -> Result<Vec<TypeExpr>, ParseError> {
        let mut list = Vec::new();
        loop {
            list.push(self.parse_type()?);
            if self.match_symbol(',')? {
                if self.check_operator(">") {
                    break;
                }
                continue;
            }
            break;
        }
        Ok(list)
    }

    fn parse_path_expr(&mut self) -> Result<PathExpr, ParseError> {
        let start = self.pos;
        let leading_colon = self.match_operator("::")?;
        let segments = self.parse_path_segments()?;
        let span = self.span_since(start);
        Ok(PathExpr {
            leading_colon,
            segments,
            span,
        })
    }

    fn parse_path_type(&mut self) -> Result<PathType, ParseError> {
        let leading_colon = self.match_operator("::")?;
        let segments = self.parse_path_segments()?;
        Ok(PathType {
            leading_colon,
            segments,
        })
    }

    fn parse_path_segments(&mut self) -> Result<Vec<PathSegment>, ParseError> {
        let mut segments = Vec::new();
        loop {
            let ident = self.expect_identifier()?;
            let generics = if self.match_operator("<")? {
                let list = self.parse_type_list()?;
                self.expect_operator(">")?;
                list
            } else {
                Vec::new()
            };
            segments.push(PathSegment { ident, generics });
            if !self.match_operator("::")? {
                break;
            }
        }
        Ok(segments)
    }

    fn expect_keyword(&mut self, keyword: &str) -> Result<(), ParseError> {
        match self.peek_kind() {
            TokenKind::Keyword(kw) if *kw == keyword => {
                self.advance();
                Ok(())
            }
            TokenKind::Eof => Err(ParseError::UnexpectedEof {
                expected: Cow::Owned(format!("keyword `{}`", keyword)),
                span: self.current_span(),
            }),
            other => Err(ParseError::UnexpectedToken {
                expected: Cow::Owned(format!("keyword `{}`", keyword)),
                found: Box::new(other.clone()),
                span: self.current_span(),
            }),
        }
    }

    fn match_keyword(&mut self, keyword: &str) -> Result<bool, ParseError> {
        if self.check_keyword(keyword) {
            self.advance();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn check_keyword(&self, keyword: &str) -> bool {
        matches!(self.peek_kind(), TokenKind::Keyword(kw) if *kw == keyword)
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.peek_kind().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(name)
            }
            other => Err(ParseError::UnexpectedToken {
                expected: "identifier".into(),
                found: Box::new(other),
                span: self.current_span(),
            }),
        }
    }

    fn expect_operator(&mut self, op: &str) -> Result<(), ParseError> {
        match self.peek_kind() {
            TokenKind::Operator(found) if found == op => {
                self.advance();
                Ok(())
            }
            TokenKind::Eof => Err(ParseError::UnexpectedEof {
                expected: Cow::Owned(format!("operator `{}`", op)),
                span: self.current_span(),
            }),
            other => Err(ParseError::UnexpectedToken {
                expected: Cow::Owned(format!("operator `{}`", op)),
                found: Box::new(other.clone()),
                span: self.current_span(),
            }),
        }
    }

    fn match_operator(&mut self, op: &str) -> Result<bool, ParseError> {
        if matches!(self.peek_kind(), TokenKind::Operator(found) if found == op) {
            self.advance();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn check_operator(&self, op: &str) -> bool {
        matches!(self.peek_kind(), TokenKind::Operator(found) if found == op)
    }

    fn expect_symbol(&mut self, sym: char) -> Result<(), ParseError> {
        match self.peek_kind() {
            TokenKind::Delimiter(c) if *c == sym => {
                self.advance();
                Ok(())
            }
            TokenKind::Eof => Err(ParseError::UnexpectedEof {
                expected: Cow::Owned(format!("`{}`", sym)),
                span: self.current_span(),
            }),
            other => Err(ParseError::UnexpectedToken {
                expected: Cow::Owned(format!("`{}`", sym)),
                found: Box::new(other.clone()),
                span: self.current_span(),
            }),
        }
    }

    fn match_symbol(&mut self, sym: char) -> Result<bool, ParseError> {
        if matches!(self.peek_kind(), TokenKind::Delimiter(c) if *c == sym) {
            self.advance();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn check_symbol(&self, sym: char) -> bool {
        matches!(self.peek_kind(), TokenKind::Delimiter(c) if *c == sym)
    }

    fn check_string_literal(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::StringLiteral(_))
    }

    fn peek_kind(&self) -> &TokenKind {
        &self.tokens[self.pos].kind
    }

    fn peek_next_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.pos + 1).map(|token| &token.kind)
    }

    fn checkpoint(&self) -> usize {
        self.pos
    }

    fn rewind(&mut self, pos: usize) {
        self.pos = pos;
    }

    fn try_parse_path_type(&mut self) -> Option<PathType> {
        let checkpoint = self.checkpoint();
        match self.parse_path_type() {
            Ok(path) => Some(path),
            Err(_) => {
                self.rewind(checkpoint);
                None
            }
        }
    }

    fn attach_span(&self, start: usize, mut expr: Expr) -> Expr {
        let end_index = self.pos.saturating_sub(1);
        let span = if self.pos == start {
            self.span_at(start)
        } else {
            self.span_between(start, end_index)
        };
        expr.span = span;
        expr
    }

    fn make_assignment(&self, left: Expr, op: AssignmentOp, right: Expr) -> Expr {
        let span = left.span.merge(right.span);
        Expr::new(
            ExprKind::Assignment {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span,
        )
    }

    fn make_binary(&self, op: BinaryOp, left: Expr, right: Expr) -> Expr {
        let span = left.span.merge(right.span);
        Expr::new(
            ExprKind::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
            span,
        )
    }

    fn advance(&mut self) {
        if !self.is_eof() {
            self.pos += 1;
        }
    }

    fn allocate_id(&mut self) -> crate::ast::NodeId {
        let id = crate::ast::NodeId::new(self.next_node_id);
        self.next_node_id += 1;
        id
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Eof)
    }

    fn error_expected(&self, expected: &'static str) -> ParseError {
        let found = self.peek_kind().clone();
        let span = self.current_span();
        match found {
            TokenKind::Eof => ParseError::UnexpectedEof {
                expected: Cow::Borrowed(expected),
                span,
            },
            _ => ParseError::UnexpectedToken {
                expected: Cow::Borrowed(expected),
                found: Box::new(found),
                span,
            },
        }
    }
}

fn item_span(item: &Item) -> Span {
    match item {
        Item::Use(use_decl) => use_decl.span,
        Item::Module(module_decl) => module_decl.span,
        Item::Macro(decl) => decl.span,
        Item::Function(func) => func.span,
        Item::Struct(decl) => decl.span,
        Item::Enum(decl) => decl.span,
        Item::Trait(decl) => decl.span,
        Item::Impl(decl) => decl.span,
        Item::Extern(block) => block.span,
        Item::Zone(zone) => zone.span,
    }
}

#[cfg(test)]
mod tests {
    use super::parse_module;

    #[test]
    fn parses_slice_binding_pattern() {
        let source = "fn f(values: Vec<Int32>) { let [head, ..tail] = values; }";
        let module = parse_module(source);
        assert!(module.is_ok(), "parser error: {module:?}");
    }
}
