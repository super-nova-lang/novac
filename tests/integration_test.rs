use serde_json::{Value, json};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use novac::{analysis, codegen, lexer, parser};

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

    // Step 4: Codegen - test all targets
    let mut codegen_outputs = serde_json::Map::new();

    // AMD64 Linux
    let codegen_result =
        codegen::target_amd64_linux::gen_target(&test_path.display().to_string(), &nodes);
    match codegen_result {
        Ok(ir) => {
            codegen_outputs.insert(
                "amd64_linux".to_string(),
                json!({
                    "exit_code": 0,
                    "output": ir.lines().take(50).collect::<Vec<_>>(),
                }),
            );
        }
        Err(e) => {
            codegen_outputs.insert(
                "amd64_linux".to_string(),
                json!({
                    "exit_code": 1,
                    "error": format!("{:?}", e),
                }),
            );
        }
    }

    // AMD64 Windows
    let codegen_result =
        codegen::target_amd64_windows::gen_target(&test_path.display().to_string(), &nodes);
    match codegen_result {
        Ok(ir) => {
            codegen_outputs.insert(
                "amd64_windows".to_string(),
                json!({
                    "exit_code": 0,
                    "output": ir.lines().take(50).collect::<Vec<_>>(),
                }),
            );
        }
        Err(e) => {
            codegen_outputs.insert(
                "amd64_windows".to_string(),
                json!({
                    "exit_code": 1,
                    "error": format!("{:?}", e),
                }),
            );
        }
    }

    // ARM64 Linux
    let codegen_result =
        codegen::target_arm64_linux::gen_target(&test_path.display().to_string(), &nodes);
    match codegen_result {
        Ok(ir) => {
            codegen_outputs.insert(
                "arm64_linux".to_string(),
                json!({
                    "exit_code": 0,
                    "output": ir.lines().take(50).collect::<Vec<_>>(),
                }),
            );
        }
        Err(e) => {
            codegen_outputs.insert(
                "arm64_linux".to_string(),
                json!({
                    "exit_code": 1,
                    "error": format!("{:?}", e),
                }),
            );
        }
    }

    // ARM64 Windows
    let codegen_result =
        codegen::target_arm64_windows::gen_target(&test_path.display().to_string(), &nodes);
    match codegen_result {
        Ok(ir) => {
            codegen_outputs.insert(
                "arm64_windows".to_string(),
                json!({
                    "exit_code": 0,
                    "output": ir.lines().take(50).collect::<Vec<_>>(),
                }),
            );
        }
        Err(e) => {
            codegen_outputs.insert(
                "arm64_windows".to_string(),
                json!({
                    "exit_code": 1,
                    "error": format!("{:?}", e),
                }),
            );
        }
    }

    outputs["steps"]["codegen"] = Value::Object(codegen_outputs);

    // Step 5: Run (via novac run command)
    let run_output = Command::new("cargo")
        .args(&["run", "--", "run", test_path.to_str().unwrap()])
        .output();
    match run_output {
        Ok(output) => {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            // Extract only the novac output, not cargo build messages
            // Also normalize temporary file paths to make tests deterministic
            let stderr_lines: Vec<String> = stderr
                .lines()
                .filter(|l| {
                    !l.contains("Blocking")
                        && !l.contains("Finished")
                        && !l.contains("Running `target")
                })
                .map(|l| {
                    // Normalize temporary file paths like /tmp/test_generic_box-89e507.o
                    if l.contains("/tmp/") {
                        let re = regex::Regex::new(r"/tmp/[^-]+-[0-9a-f]+\.o").unwrap();
                        re.replace_all(l, "/tmp/<temp>.o").to_string()
                    } else {
                        l.to_string()
                    }
                })
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

/// Macro to generate test functions for all .nova files in the tests directory
macro_rules! generate_tests {
    ($($name:ident => $file:expr),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                run_test($file).expect("Test failed");
            }
        )*
    };
}

// Auto-generate tests for all .nova test files
generate_tests! {
    test_c_style_for_loop => "test_c_style_for_loop.nova",
    test_codegen_minimal => "test_codegen_minimal.nova",
    test_codegen_simple => "test_codegen_simple.nova",
    test_compound_assign => "test_compound_assign.nova",
    test_enum_payload => "test_enum_payload.nova",
    test_error => "test_error.nova",
    test_for_loop => "test_for_loop.nova",
    test_generic_advanced => "test_generic_advanced.nova",
    test_generic_box => "test_generic_box.nova",
    test_generics => "test_generics.nova",
    test_many_params => "test_many_params.nova",
    test_match => "test_match.nova",
    test_named_params => "test_named_params.nova",
    test_power => "test_power.nova",
    test_reflection => "test_reflection.nova",
    test_simple_for_loop => "test_simple_for_loop.nova",
    test_variadic => "test_variadic.nova",
    test_while => "test_while.nova",
}
