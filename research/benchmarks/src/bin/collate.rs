use anyhow::{Context, Result};
use serde::Deserialize;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

#[derive(Deserialize)]
struct EstimateSummary {
    #[serde(rename = "point_estimate")]
    point_estimate: f64,
}

#[derive(Deserialize)]
struct EstimatesFile {
    mean: EstimateSummary,
    median: EstimateSummary,
}

#[derive(Deserialize)]
struct BenchmarkMeta {
    throughput: Option<Throughput>,
    #[serde(rename = "title")]
    title: String,
}

#[derive(Deserialize)]
struct Throughput {
    #[serde(rename = "Elements")]
    elements: Option<u64>,
    #[serde(rename = "Bytes")]
    bytes: Option<u64>,
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
            Err(_) => continue,
        };

        let meta_path = bench_dir.join("benchmark.json");
        let meta_contents = match fs::read_to_string(&meta_path) {
            Ok(content) => content,
            Err(_) => continue,
        };
        let meta: BenchmarkMeta = match serde_json::from_str(&meta_contents) {
            Ok(value) => value,
            Err(_) => continue,
        };

        let throughput = meta.throughput.map(|tp| match (tp.elements, tp.bytes) {
            (Some(elems), _) => format!("{} elem", elems),
            (_, Some(bytes)) => format!("{} bytes", bytes),
            _ => "--".to_string(),
        });

        results.push(BenchResult {
            id: meta.title,
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
    let mut criterion_dir = PathBuf::from("target/criterion");
    let mut output: Option<PathBuf> = None;
    let mut commit: Option<String> = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--criterion-dir" => {
                let value = args.next().context("missing value for --criterion-dir")?;
                criterion_dir = PathBuf::from(value);
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
