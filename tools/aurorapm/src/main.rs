use clap::{Parser, Subcommand};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;
use std::process::exit;

#[derive(Debug, Parser)]
#[command(author, version, about = "Aurora package manager prototype", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Initialise a new Aurora workspace manifest
    Init {
        /// Directory to initialise (defaults to current directory)
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    /// Add a dependency to an existing Aurora manifest
    Add {
        /// Package name
        name: String,
        /// Git URL or local path
        #[arg(short, long)]
        source: Option<String>,
        /// Path to manifest (aurora.toml)
        #[arg(short, long, default_value = "aurora.toml")]
        manifest: PathBuf,
    },
}

#[derive(Debug, Serialize, Deserialize, Default)]
struct Manifest {
    #[serde(default)]
    package: Package,
    #[serde(default)]
    dependencies: toml::value::Table,
}

#[derive(Debug, Serialize, Deserialize, Default)]
struct Package {
    #[serde(default = "default_name")]
    name: String,
    #[serde(default = "default_version")]
    version: String,
}

fn default_name() -> String {
    "aurora-app".into()
}

fn default_version() -> String {
    "0.1.0".into()
}

fn main() {
    let cli = Cli::parse();
    if let Err(err) = run(cli) {
        eprintln!("error: {err}");
        exit(1);
    }
}

fn run(cli: Cli) -> anyhow::Result<()> {
    match cli.command {
        Command::Init { path } => init_manifest(path),
        Command::Add {
            name,
            source,
            manifest,
        } => add_dependency(name, source, manifest),
    }
}

fn init_manifest(path: PathBuf) -> anyhow::Result<()> {
    let mut dir = path;
    if dir.is_file() {
        anyhow::bail!("init path must be a directory");
    }
    fs::create_dir_all(&dir)?;
    dir.push("aurora.toml");
    if dir.exists() {
        anyhow::bail!("aurora.toml already exists");
    }
    let manifest = Manifest::default();
    let contents = toml::to_string_pretty(&manifest)?;
    fs::write(&dir, contents)?;
    println!("created {}", dir.display());
    Ok(())
}

fn add_dependency(
    name: String,
    source: Option<String>,
    manifest_path: PathBuf,
) -> anyhow::Result<()> {
    if !manifest_path.exists() {
        anyhow::bail!("manifest not found: {}", manifest_path.display());
    }
    let mut manifest: Manifest = toml::from_str(&fs::read_to_string(&manifest_path)?)?;
    let dependency_value = match source {
        Some(src) => toml::Value::String(src),
        None => toml::Value::String("*".into()),
    };
    manifest.dependencies.insert(name.clone(), dependency_value);
    let contents = toml::to_string_pretty(&manifest)?;
    fs::write(&manifest_path, contents)?;
    println!("added dependency '{name}' to {}", manifest_path.display());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn init_creates_manifest() {
        let tmp = tempdir().unwrap();
        init_manifest(tmp.path().into()).unwrap();
        assert!(tmp.path().join("aurora.toml").exists());
    }

    #[test]
    fn add_dependency_updates_manifest() {
        let tmp = tempdir().unwrap();
        let manifest_path = tmp.path().join("aurora.toml");
        fs::File::create(&manifest_path)
            .unwrap()
            .write_all(b"[package]\nname='example'\nversion='0.1.0'\n")
            .unwrap();

        add_dependency("aurora-runtime".into(), None, manifest_path.clone()).unwrap();

        let manifest: Manifest =
            toml::from_str(&fs::read_to_string(&manifest_path).unwrap()).unwrap();
        assert!(manifest.dependencies.contains_key("aurora-runtime"));
    }
}
