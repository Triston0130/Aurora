use anyhow::Result;
use regex::Regex;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use serde_json::json;

fn main() -> Result<()> {
    let mut editor = DefaultEditor::new()?;
    let mut history_path = dirs::cache_dir().unwrap_or_else(std::env::temp_dir);
    history_path.push("aurorarepl_history");
    let _ = editor.load_history(&history_path);

    println!("Aurora REPL prototype. Type :quit to exit.");
    loop {
        match editor.readline("aurora> ") {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed == ":quit" {
                    break;
                }
                editor.add_history_entry(trimmed)?;
                println!("{}", evaluate(trimmed));
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => return Err(err.into()),
        }
    }

    let _ = editor.save_history(&history_path);
    Ok(())
}

fn evaluate(input: &str) -> String {
    if input.is_empty() {
        return String::new();
    }
    if let Some(caps) = Regex::new(r"^(let)\s+(?<name>[a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(?<value>.+)$")
        .unwrap()
        .captures(input)
    {
        return json!({
            "kind": "binding",
            "name": caps["name"],
            "value": caps["value"],
        })
        .to_string();
    }

    json!({ "kind": "expr", "value": input }).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_let_binding() {
        let response = evaluate("let x = 42");
        let value: serde_json::Value = serde_json::from_str(&response).unwrap();
        assert_eq!(value["kind"], "binding");
        assert_eq!(value["name"], "x");
    }
}
