use anyhow::Result;
use clap::Parser;
use regex::Regex;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Parser)]
#[command(author, version, about = "Aurora linter prototype", long_about = None)]
struct Cli {
    /// Input file or directory
    #[arg(value_hint = clap::ValueHint::AnyPath, default_value = ".")]
    path: PathBuf,

    /// Emit JSON report instead of human-readable output
    #[arg(long)]
    json: bool,
}

#[derive(Debug, serde::Serialize)]
struct LintDiagnostic {
    path: PathBuf,
    line: usize,
    kind: String,
    message: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut diagnostics = Vec::new();
    lint_path(&cli.path, &mut diagnostics)?;

    if cli.json {
        println!("{}", serde_json::to_string_pretty(&diagnostics)?);
    } else {
        for diag in &diagnostics {
            println!(
                "{}:{}: {} - {}",
                diag.path.display(),
                diag.line,
                diag.kind,
                diag.message
            );
        }
        if diagnostics.is_empty() {
            println!("no lint issues found");
        }
    }

    Ok(())
}

fn lint_path(path: &Path, diagnostics: &mut Vec<LintDiagnostic>) -> Result<()> {
    if path.is_dir() {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let child = entry.path();
            lint_path(&child, diagnostics)?;
        }
    } else if is_source_file(path) {
        lint_file(path, diagnostics)?;
    }
    Ok(())
}

fn is_source_file(path: &Path) -> bool {
    matches!(
        path.extension().and_then(|ext| ext.to_str()),
        Some("aur" | "rs")
    )
}

fn lint_file(path: &Path, diagnostics: &mut Vec<LintDiagnostic>) -> Result<()> {
    let content = fs::read_to_string(path)?;
    let trailing_ws = Regex::new(r"\s+$").unwrap();
    let todo_pattern = Regex::new(r"TODO").unwrap();

    for (idx, line) in content.lines().enumerate() {
        if trailing_ws.is_match(line) {
            diagnostics.push(LintDiagnostic {
                path: path.to_path_buf(),
                line: idx + 1,
                kind: "style".into(),
                message: "trailing whitespace".into(),
            });
        }
        if todo_pattern.is_match(line) {
            diagnostics.push(LintDiagnostic {
                path: path.to_path_buf(),
                line: idx + 1,
                kind: "todo".into(),
                message: "TODO marker found".into(),
            });
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn lints_trailing_whitespace() {
        let dir = tempdir().unwrap();
        let file = dir.path().join("example.aur");
        fs::write(&file, "let x = 1;  \n").unwrap();
        let mut diagnostics = Vec::new();
        lint_file(&file, &mut diagnostics).unwrap();
        assert!(diagnostics.iter().any(|diag| diag.kind == "style"));
    }
}
