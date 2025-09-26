use anyhow::{Context, Result};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

#[derive(serde::Deserialize)]
struct EstimateSummary {
    #[serde(rename = "point_estimate")]
    point_estimate: f64,
}

#[derive(serde::Deserialize)]
struct EstimatesFile {
    mean: EstimateSummary,
    median: EstimateSummary,
}

struct BenchResult {
    id: String,
    mean_ns: f64,
    median_ns: f64,
    throughput: Option<String>,
}

fn find_estimates(root: &Path) -> Result<Vec<BenchResult>> {
    if !root.exists() {
        return Ok(Vec::new());
    }
    let mut results = Vec::new();

    for entry in WalkDir::new(root).into_iter().filter_map(|e| e.ok()) {
        if entry.file_name() != "estimates.json" {
            continue;
        }
        let path = entry.path();
        if path.parent().and_then(|p| p.file_name()) != Some(std::ffi::OsStr::new("base")) {
            continue; // only use `base` measurements
        }
        let bench_dir = path
            .parent()
            .and_then(|p| p.parent())
            .context("unexpected directory layout for estimates.json")?;
        let estimates_contents = match fs::read_to_string(path) {
            Ok(content) => content,
            Err(_) => continue,
        };
        let estimates: EstimatesFile = match serde_json::from_str(&estimates_contents) {
            Ok(value) => value,
            Err(err) => {
                eprintln!("failed to parse estimates {:?}: {}", path, err);
                continue;
            }
        };

        let throughput = fs::read_to_string(bench_dir.join("benchmark.json"))
            .ok()
            .and_then(|contents| serde_json::from_str::<serde_json::Value>(&contents).ok())
            .and_then(|value| value.get("throughput").cloned())
            .and_then(|tp| {
                tp.get("Elements")
                    .and_then(|v| v.as_u64())
                    .map(|elems| format!("{} elem", elems))
                    .or_else(|| {
                        tp.get("Bytes")
                            .and_then(|v| v.as_u64())
                            .map(|bytes| format!("{} bytes", bytes))
                    })
            });

        let id = bench_dir
            .strip_prefix(root)
            .unwrap_or(bench_dir)
            .display()
            .to_string();

        results.push(BenchResult {
            id,
            mean_ns: estimates.mean.point_estimate,
            median_ns: estimates.median.point_estimate,
            throughput,
        });
    }

    results.sort_by(|a, b| a.id.cmp(&b.id));
    Ok(results)
}

fn format_markdown(results: &[BenchResult], commit: Option<&str>) -> String {
    let mut lines = Vec::new();
    if let Some(hash) = commit {
        lines.push(format!("- Commit: `{}`", hash));
        lines.push(String::new());
    }
    lines.push("| Benchmark | Mean (ms) | Median (ms) | Throughput |".to_string());
    lines.push("| --- | --- | --- | --- |".to_string());
    for bench in results {
        let mean_ms = bench.mean_ns / 1_000_000.0;
        let median_ms = bench.median_ns / 1_000_000.0;
        let thr = bench.throughput.clone().unwrap_or_else(|| "--".into());
        lines.push(format!(
            "| {} | {:.3} | {:.3} | {} |",
            bench.id, mean_ms, median_ms, thr
        ));
    }
    lines.join("\n")
}

fn main() -> Result<()> {
    let mut args = env::args().skip(1);
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let default_dir = manifest_dir.join("../../target/criterion");
    let mut criterion_dir = default_dir.canonicalize().unwrap_or(default_dir.clone());
    let mut output: Option<PathBuf> = None;
    let mut commit: Option<String> = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--criterion-dir" => {
                let value = args.next().context("missing value for --criterion-dir")?;
                let path = PathBuf::from(value);
                criterion_dir = path.canonicalize().unwrap_or(path);
            }
            "--output" => {
                let value = args.next().context("missing value for --output")?;
                output = Some(PathBuf::from(value));
            }
            "--commit" => {
                let value = args.next().context("missing value for --commit")?;
                commit = Some(value);
            }
            other => {
                return Err(anyhow::anyhow!("unrecognised argument: {}", other));
            }
        }
    }

    if !criterion_dir.exists() {
        println!(
            "No Criterion summaries found in {}",
            criterion_dir.display()
        );
        return Ok(());
    }

    let summaries = find_estimates(&criterion_dir)?;
    if summaries.is_empty() {
        println!(
            "No Criterion summaries found in {}",
            criterion_dir.display()
        );
        return Ok(());
    }

    let markdown = format_markdown(&summaries, commit.as_deref());

    if let Some(path) = output {
        fs::write(&path, markdown).with_context(|| format!("writing {:?}", path))?;
    } else {
        println!("{}", markdown);
    }

    Ok(())
}
