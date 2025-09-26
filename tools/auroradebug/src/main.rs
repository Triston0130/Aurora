use anyhow::{bail, Context, Result};
use clap::{Parser, Subcommand};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(author, version, about = "Aurora debugger adapter prototype", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Serve a stdin/stdout Debug Adapter Protocol loop
    Serve {
        /// Optional newline-delimited JSON script file to replay
        #[arg(long, value_hint = clap::ValueHint::AnyPath)]
        script: Option<PathBuf>,
    },
    /// Print supported capabilities without starting the adapter
    Probe,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Command::Serve { script } => serve(script),
        Command::Probe => {
            println!(
                "auroradebug: supports initialize, launch, setBreakpoints, continue, disconnect"
            );
            Ok(())
        }
    }
}

fn serve(script: Option<PathBuf>) -> Result<()> {
    let mut reader: Box<dyn BufRead> = match script {
        Some(path) => Box::new(BufReader::new(
            File::open(&path).with_context(|| format!("opening script {}", path.display()))?,
        )),
        None => Box::new(BufReader::new(io::stdin())),
    };

    let mut line = String::new();
    loop {
        line.clear();
        let bytes = reader.read_line(&mut line)?;
        if bytes == 0 {
            break;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        match handle_request(trimmed) {
            Ok(response) => println!("{response}"),
            Err(err) => {
                eprintln!("error: {err}");
            }
        }
    }

    Ok(())
}

#[derive(Debug, Deserialize)]
struct DapRequest {
    #[serde(rename = "type")]
    kind: String,
    seq: u64,
    command: String,
    #[serde(default)]
    arguments: Value,
}

#[derive(Debug, Serialize)]
struct DapResponse {
    #[serde(rename = "type")]
    kind: &'static str,
    seq: u64,
    request_seq: u64,
    success: bool,
    command: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    body: Option<Value>,
}

fn handle_request(payload: &str) -> Result<String> {
    let request: DapRequest = serde_json::from_str(payload).context("parsing DAP request")?;
    if request.kind.as_str() != "request" {
        bail!("unsupported message type: {}", request.kind);
    }

    let (success, body, message) = match request.command.as_str() {
        "initialize" => (
            true,
            Some(json!({
                "capabilities": {
                    "supportsConfigurationDoneRequest": true,
                    "supportsRestartRequest": true,
                    "supportsTerminateRequest": true,
                }
            })),
            None,
        ),
        "launch" => (
            true,
            Some(json!({
                "debuggeeLaunched": true,
            })),
            None,
        ),
        "setBreakpoints" => {
            let breakpoints = request
                .arguments
                .get("breakpoints")
                .and_then(|value| value.as_array())
                .map(|arr| {
                    arr.iter()
                        .map(|entry| {
                            json!({
                                "verified": true,
                                "line": entry.get("line").and_then(Value::as_u64).unwrap_or(0),
                            })
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            (true, Some(json!({ "breakpoints": breakpoints })), None)
        }
        "continue" => (true, Some(json!({ "allThreadsContinued": true })), None),
        "disconnect" => (true, None, None),
        command => (
            false,
            None,
            Some(format!("command '{command}' not implemented")),
        ),
    };

    let response = DapResponse {
        kind: "response",
        seq: request.seq,
        request_seq: request.seq,
        success,
        command: request.command,
        message,
        body,
    };

    serde_json::to_string(&response).context("serialising response")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initialize_reports_capabilities() {
        let response =
            handle_request("{\"type\":\"request\",\"seq\":1,\"command\":\"initialize\"}").unwrap();
        let json: Value = serde_json::from_str(&response).unwrap();
        assert_eq!(json["success"], Value::Bool(true));
        assert_eq!(json["command"], Value::String("initialize".into()));
        assert!(json["body"]["capabilities"].is_object());
    }

    #[test]
    fn unknown_command_fails_cleanly() {
        let response =
            handle_request("{\"type\":\"request\",\"seq\":7,\"command\":\"stepIn\"}").unwrap();
        let json: Value = serde_json::from_str(&response).unwrap();
        assert_eq!(json["success"], Value::Bool(false));
        assert_eq!(json["request_seq"], Value::from(7));
        assert!(json["message"]
            .as_str()
            .unwrap()
            .contains("not implemented"));
    }

    #[test]
    fn breakpoint_payload_is_reflected() {
        let payload = "{\"type\":\"request\",\"seq\":3,\"command\":\"setBreakpoints\",\"arguments\":{\"breakpoints\":[{\"line\":42},{\"line\":7}]}}";
        let response = handle_request(payload).unwrap();
        let json: Value = serde_json::from_str(&response).unwrap();
        assert_eq!(json["success"], Value::Bool(true));
        let breakpoints = json["body"]["breakpoints"].as_array().unwrap();
        assert_eq!(breakpoints.len(), 2);
        assert_eq!(breakpoints[0]["line"], Value::from(42));
        assert_eq!(breakpoints[1]["line"], Value::from(7));
    }
}
