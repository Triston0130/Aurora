use anyhow::Context;
use clap::Parser;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Parser)]
#[command(author, version, about = "Aurora formatter prototype", long_about = None)]
struct Cli {
    /// Files or directories to format
    #[arg(value_hint = clap::ValueHint::AnyPath)]
    paths: Vec<PathBuf>,

    /// Preview changes without writing to disk
    #[arg(long)]
    check: bool,

    /// Emit diff between original and formatted content
    #[arg(long)]
    diff: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let paths: Vec<PathBuf> = if cli.paths.is_empty() {
        vec![PathBuf::from(".")]
    } else {
        cli.paths
    };

    let mut failures = Vec::new();
    for path in paths {
        walk(&path, cli.check, cli.diff, &mut failures)?;
    }

    if failures.is_empty() {
        Ok(())
    } else {
        anyhow::bail!("formatting failed for {} file(s)", failures.len())
    }
}

fn walk(path: &Path, check: bool, diff: bool, failures: &mut Vec<PathBuf>) -> anyhow::Result<()> {
    if path.is_dir() {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let child = entry.path();
            if child.is_dir() {
                walk(&child, check, diff, failures)?;
            } else if is_source_file(&child) {
                format_file(&child, check, diff, failures)?;
            }
        }
    } else if is_source_file(path) {
        format_file(path, check, diff, failures)?;
    }
    Ok(())
}

fn is_source_file(path: &Path) -> bool {
    matches!(
        path.extension().and_then(|ext| ext.to_str()),
        Some("aur" | "rs")
    )
}

fn format_file(
    path: &Path,
    check: bool,
    diff: bool,
    failures: &mut Vec<PathBuf>,
) -> anyhow::Result<()> {
    let original =
        fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    let formatted = format_stub(&original);

    if original == formatted {
        return Ok(());
    }

    if diff {
        print_diff(path, &original, &formatted);
    }

    if check {
        failures.push(path.to_path_buf());
    } else {
        fs::write(path, formatted).with_context(|| format!("writing {}", path.display()))?;
        println!("formatted {}", path.display());
    }

    Ok(())
}

fn format_stub(input: &str) -> String {
    let mut lines: Vec<&str> = input.lines().collect();
    lines.sort();
    lines.join("\n") + "\n"
}

fn print_diff(path: &Path, original: &str, formatted: &str) {
    println!("--- {}", path.display());
    println!("+++ {} (formatted)", path.display());
    for diff in diffy::create_patch(original, formatted).to_string().lines() {
        println!("{diff}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn formatter_reorders_lines() {
        let dir = tempdir().unwrap();
        let file = dir.path().join("example.aur");
        fs::write(&file, "b\na\n").unwrap();
        format_file(&file, false, false, &mut Vec::new()).unwrap();
        assert_eq!(fs::read_to_string(&file).unwrap(), "a\nb\n");
    }
}
