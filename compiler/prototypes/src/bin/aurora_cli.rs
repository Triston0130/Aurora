use std::env;
use std::fs;
use std::process;

use aurora_compiler_prototypes::backend::llvm::{emit_module, BackendOptions};
use aurora_compiler_prototypes::diagnostics::render_all;
use aurora_compiler_prototypes::driver::{compile_module, render_diagnostics};

fn main() {
    let args = env::args().skip(1);
    let mut path: Option<String> = None;
    let mut emit_flags: Vec<String> = Vec::new();

    for arg in args {
        if let Some(value) = arg.strip_prefix("--emit=") {
            emit_flags.extend(value.split(',').map(|s| s.to_string()));
        } else if path.is_none() {
            path = Some(arg);
        } else {
            eprintln!("usage: aurora-cli <source file> [--emit=llvm[,ctfe]]");
            process::exit(1);
        }
    }

    let path = match path {
        Some(p) => p,
        None => {
            eprintln!("usage: aurora-cli <source file> [--emit=llvm[,ctfe]]");
            process::exit(1);
        }
    };

    let source = match fs::read_to_string(&path) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("error: could not read `{}`: {}", path, err);
            process::exit(1);
        }
    };

    match compile_module(&path, &source) {
        Ok(result) => {
            let rendered = render_diagnostics(&result);
            if rendered.is_empty() {
                println!("no diagnostics emitted");
            } else {
                println!("{}", rendered);
            }

            if emit_flags.iter().any(|flag| flag == "llvm") && result.diagnostics.is_empty() {
                let artifacts = emit_module(
                    &result.mir,
                    &BackendOptions::default(),
                    &result.ctfe_results,
                );
                println!("\n; === CPU LLVM IR ===\n{}", artifacts.cpu_module);
                for gpu in artifacts.gpu_modules {
                    println!(
                        "\n; === GPU module for {} (zone {}) ===\n{}",
                        gpu.function, gpu.zone, gpu.ir
                    );
                }
                if !artifacts.realtime_metadata.is_empty() {
                    println!("\n; === Realtime Metadata ===");
                    for meta in artifacts.realtime_metadata {
                        match meta.deadline_ms {
                            Some(deadline) => println!(
                                "; function {} (zone {}) deadline {} ms",
                                meta.function, meta.zone, deadline
                            ),
                            None => println!(
                                "; function {} (zone {}) realtime scheduled",
                                meta.function, meta.zone
                            ),
                        }
                    }
                }
            }

            if emit_flags.iter().any(|flag| flag == "ctfe") && result.diagnostics.is_empty() {
                if result.ctfe_results.is_empty() {
                    println!("\n; === CTFE Results ===\n; no const evaluations executed");
                } else {
                    println!("\n; === CTFE Results ===");
                    for (name, value) in result.ctfe_results.iter() {
                        println!("; const fn {} => {:?}", name, value);
                    }
                }
            }
        }
        Err(diags) => {
            let rendered = render_all(&diags);
            eprintln!("{}", rendered);
            process::exit(1);
        }
    }
}
