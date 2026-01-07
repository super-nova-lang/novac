use serde_json::{Value, json};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Run through the pipeline for a test file and collect outputs
fn run_pipeline(test_file: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let test_path = PathBuf::from("tests").join(test_file);
    if !test_path.exists() {
        return Err(format!("Test file not found: {}", test_path.display()).into());
    }

    let source = fs::read_to_string(&test_path)?;
    let mut outputs = serde_json::json!({
        "file": test_file,
        "steps": {}
    });

    // Step 1: Tokenize
    let tokens = {
        let mut lexer = lexer::Lexer::new(&test_path.display().to_string(), &source);
        let tokens = lexer.tokenize();
        tokens
            .iter()
            .map(|t| format!("{:?}", t))
            .collect::<Vec<_>>()
    };
    outputs["steps"]["tokenize"] =
        Value::Array(tokens.iter().map(|t| Value::String(t.clone())).collect());

    // Step 2: Parse
    let nodes = {
        let mut lexer = lexer::Lexer::new(&test_path.display().to_string(), &source);
        let tokens = lexer.tokenize();
        match parser::parse(tokens) {
            Ok(nodes) => nodes,
            Err(e) => {
                outputs["steps"]["parse"] = json!({ "error": format!("{:?}", e) });
                return Ok(outputs);
            }
        }
    };

    let parse_output = nodes
        .iter()
        .map(|n| format!("{:#?}", n))
        .collect::<Vec<_>>();
    outputs["steps"]["parse"] = Value::Array(
        parse_output
            .iter()
            .map(|p| Value::String(p.clone()))
            .collect(),
    );

    // Step 3: Analyze
    let (analysis_results, warnings, _) = analysis::analyze(nodes.clone());
    let mut sorted_results: Vec<_> = analysis_results
        .iter()
        .map(|r| format!("{:?}", r))
        .collect();
    sorted_results.sort();
    let mut sorted_warnings: Vec<_> = warnings.iter().map(|w| format!("{:?}", w)).collect();
    sorted_warnings.sort();
    outputs["steps"]["analyze"] = json!({
        "results": sorted_results,
        "warnings": sorted_warnings,
    });

    // Step 4: Codegen
    let codegen_result = codegen::target_amd64_linux::gen_target(&test_path.display().to_string(), &nodes);
    match codegen_result {
        Ok(ir) => {
            outputs["steps"]["codegen"] = json!({
                "exit_code": 0,
                "output": ir.lines().take(50).collect::<Vec<_>>(),
            });
        }
        Err(e) => {
            outputs["steps"]["codegen"] = json!({
                "exit_code": 1,
                "error": format!("{:?}", e),
            });
        }
    }

    // Step 5: Run (via novac run command)
    let run_output = Command::new("cargo")
        .args(&["run", "--", "run", test_path.to_str().unwrap()])
        .output();
    match run_output {
        Ok(output) => {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            // Extract only the novac output, not cargo build messages
            let stderr_lines: Vec<&str> = stderr
                .lines()
                .filter(|l| !l.contains("Blocking") && !l.contains("Finished") && !l.contains("Running `target"))
                .collect();
            outputs["steps"]["run"] = json!({
                "exit_code": output.status.code().unwrap_or(-1),
                "stdout": stdout,
                "stderr": stderr_lines.join("\n"),
            });
        }
        Err(e) => {
            outputs["steps"]["run"] = json!({
                "exit_code": -1,
                "error": format!("{:?}", e),
            });
        }
    }

    Ok(outputs)
}

/// Get the expected output file path for a test
fn get_expected_path(test_file: &str) -> PathBuf {
    let name = Path::new(test_file)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(test_file);
    PathBuf::from("tests").join(format!("{}.expected.json", name))
}

/// Run a single test file
fn run_test(test_file: &str) -> Result<(), Box<dyn std::error::Error>> {
    let expected_path = get_expected_path(test_file);

    // Run the pipeline
    let actual = run_pipeline(test_file)?;

    // Load or create expected output
    let expected = if expected_path.exists() {
        serde_json::from_str::<Value>(&fs::read_to_string(&expected_path)?)?
    } else {
        // Create expected file on first run
        let json_string = serde_json::to_string_pretty(&actual)?;
        fs::write(&expected_path, &json_string)?;
        eprintln!("Created expected output: {}", expected_path.display());
        actual.clone()
    };

    // Compare
    if actual != expected {
        eprintln!(
            "Test failed: {}\nExpected:\n{}\n\nActual:\n{}",
            test_file,
            serde_json::to_string_pretty(&expected)?,
            serde_json::to_string_pretty(&actual)?
        );
        return Err("Output mismatch".into());
    }

    Ok(())
}

#[test]
fn test_c_style_for_loop() {
    run_test("test_c_style_for_loop.nova").expect("Test failed");
}

#[test]
fn test_compound_assign() {
    run_test("test_compound_assign.nova").expect("Test failed");
}

#[test]
fn test_enum_payload() {
    run_test("test_enum_payload.nova").expect("Test failed");
}

#[test]
fn test_error() {
    run_test("test_error.nova").expect("Test failed");
}

#[test]
fn test_for_loop() {
    run_test("test_for_loop.nova").expect("Test failed");
}

#[test]
fn test_generic_advanced() {
    run_test("test_generic_advanced.nova").expect("Test failed");
}

#[test]
fn test_generic_box() {
    run_test("test_generic_box.nova").expect("Test failed");
}

#[test]
fn test_generics() {
    run_test("test_generics.nova").expect("Test failed");
}

#[test]
fn test_many_params() {
    run_test("test_many_params.nova").expect("Test failed");
}

#[test]
fn test_match() {
    run_test("test_match.nova").expect("Test failed");
}

#[test]
fn test_named_params() {
    run_test("test_named_params.nova").expect("Test failed");
}

#[test]
fn test_power() {
    run_test("test_power.nova").expect("Test failed");
}

#[test]
fn test_reflection() {
    run_test("test_reflection.nova").expect("Test failed");
}

#[test]
fn test_simple_for_loop() {
    run_test("test_simple_for_loop.nova").expect("Test failed");
}

#[test]
fn test_variadic() {
    run_test("test_variadic.nova").expect("Test failed");
}

#[test]
fn test_while() {
    run_test("test_while.nova").expect("Test failed");
}
