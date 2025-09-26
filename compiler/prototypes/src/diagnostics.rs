//! Shared diagnostic data structures and helpers.

use crate::span::{SourceMap, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
    Help,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    pub style: LabelStyle,
    pub span: Span,
    pub message: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Suggestion {
    pub message: String,
    pub span: Option<Span>,
    pub replacement: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
    pub suggestions: Vec<Suggestion>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self::new(Severity::Error, message)
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, message)
    }

    pub fn note(message: impl Into<String>) -> Self {
        Self::new(Severity::Note, message)
    }

    pub fn help(message: impl Into<String>) -> Self {
        Self::new(Severity::Help, message)
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_suggestion(mut self, suggestion: Suggestion) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    fn new(severity: Severity, message: impl Into<String>) -> Self {
        Self {
            severity,
            code: None,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
            suggestions: Vec::new(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Renderer<'a> {
    source_map: Option<&'a SourceMap>,
}

impl<'a> Renderer<'a> {
    pub fn new() -> Self {
        Self { source_map: None }
    }

    pub fn with_source_map(source_map: &'a SourceMap) -> Self {
        Self {
            source_map: Some(source_map),
        }
    }

    pub fn render(&self, diagnostic: &Diagnostic) -> String {
        let mut lines = Vec::new();
        lines.push(match diagnostic.severity {
            Severity::Error => format!("error: {}", diagnostic.message),
            Severity::Warning => format!("warning: {}", diagnostic.message),
            Severity::Note => format!("note: {}", diagnostic.message),
            Severity::Help => format!("help: {}", diagnostic.message),
        });

        for label in &diagnostic.labels {
            let kind = match label.style {
                LabelStyle::Primary => "=",
                LabelStyle::Secondary => "-",
            };
            match &label.message {
                Some(msg) => lines.push(format!("  {} {} at {}", kind, msg, label.span)),
                None => lines.push(format!("  {} at {}", kind, label.span)),
            }
            if let Some(source_map) = self.source_map {
                if let Some(file) = source_map.file(label.span.file) {
                    let snippet = source_map
                        .span_snippet(label.span)
                        .unwrap_or("")
                        .trim_end_matches(['\n', '\r']);
                    lines.push(format!("    --> {}:{}", file.name, label.span));
                    if !snippet.is_empty() {
                        lines.push(format!("    | {}", snippet));
                    }
                }
            }
        }

        for note in &diagnostic.notes {
            lines.push(format!("  note: {}", note));
        }

        for suggestion in &diagnostic.suggestions {
            match (&suggestion.span, &suggestion.replacement) {
                (Some(span), Some(replacement)) => lines.push(format!(
                    "  help: {} (replace at {} with `{}`)",
                    suggestion.message, span, replacement
                )),
                (Some(span), None) => {
                    lines.push(format!("  help: {} (at {})", suggestion.message, span))
                }
                (None, Some(replacement)) => lines.push(format!(
                    "  help: {} (replace with `{}`)",
                    suggestion.message, replacement
                )),
                (None, None) => lines.push(format!("  help: {}", suggestion.message)),
            }
        }

        lines.join("\n")
    }
}

impl<'a> Renderer<'a> {
    pub fn render_all(&self, diagnostics: &[Diagnostic]) -> String {
        diagnostics
            .iter()
            .map(|diagnostic| self.render(diagnostic))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

pub fn render_all(diagnostics: &[Diagnostic]) -> String {
    Renderer::new().render_all(diagnostics)
}

pub fn render_all_with_sources(source_map: &SourceMap, diagnostics: &[Diagnostic]) -> String {
    Renderer::with_source_map(source_map).render_all(diagnostics)
}

pub fn primary_label(span: Span, message: impl Into<String>) -> Label {
    Label {
        style: LabelStyle::Primary,
        span,
        message: Some(message.into()),
    }
}

pub fn secondary_label(span: Span, message: impl Into<String>) -> Label {
    Label {
        style: LabelStyle::Secondary,
        span,
        message: Some(message.into()),
    }
}

pub fn unlabeled_primary(span: Span) -> Label {
    Label {
        style: LabelStyle::Primary,
        span,
        message: None,
    }
}

pub fn unlabeled_secondary(span: Span) -> Label {
    Label {
        style: LabelStyle::Secondary,
        span,
        message: None,
    }
}

pub fn suggestion(
    message: impl Into<String>,
    span: Option<Span>,
    replacement: Option<String>,
) -> Suggestion {
    Suggestion {
        message: message.into(),
        span,
        replacement,
    }
}
