use anyhow::Result;
use clap::Parser;
use serde::{Deserialize, Serialize};
use std::io::{self, BufRead};

#[derive(Debug, Parser)]
#[command(author, version, about = "Aurora language server prototype", long_about = None)]
struct Cli {
    /// Run a single LSP request in JSON (mainly for testing)
    #[arg(long)]
    request: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct LspRequest {
    jsonrpc: String,
    method: String,
    #[serde(default)]
    id: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize)]
struct LspResponse {
    jsonrpc: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    if let Some(request) = cli.request {
        let response = handle_message(&request)?;
        println!("{response}");
        return Ok(());
    }

    for line in io::stdin().lock().lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let response = handle_message(&line)?;
        println!("{response}");
    }

    Ok(())
}

fn handle_message(message: &str) -> Result<String> {
    let request: LspRequest = serde_json::from_str(message)?;
    let response = match request.method.as_str() {
        "initialize" => LspResponse {
            jsonrpc: "2.0".into(),
            id: Some(request.id),
            result: Some(serde_json::json!({
                "capabilities": {
                    "textDocumentSync": 1,
                    "hoverProvider": true,
                    "completionProvider": { "resolveProvider": false }
                }
            })),
            error: None,
        },
        "shutdown" => LspResponse {
            jsonrpc: "2.0".into(),
            id: Some(request.id),
            result: Some(serde_json::Value::Null),
            error: None,
        },
        "textDocument/hover" => LspResponse {
            jsonrpc: "2.0".into(),
            id: Some(request.id),
            result: Some(serde_json::json!({
                "contents": {
                    "kind": "plaintext",
                    "value": "Aurora hover prototype"
                }
            })),
            error: None,
        },
        method => LspResponse {
            jsonrpc: "2.0".into(),
            id: Some(request.id),
            result: None,
            error: Some(format!("method '{method}' not implemented")),
        },
    };

    Ok(serde_json::to_string(&response)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initialize_request() {
        let response: serde_json::Value = serde_json::from_str(
            &handle_message("{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"id\":1}").unwrap(),
        )
        .unwrap();
        assert_eq!(response["result"]["capabilities"]["hoverProvider"], true);
    }
}
