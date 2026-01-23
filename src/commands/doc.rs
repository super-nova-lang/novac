use std::fs;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};
use tracing::info;

use novac::analyzer;
use novac::analyzer::TypeContext;
use novac::parser;
use novac::parser::ast::Function;

const CSS: &str = include_str!("../../assets/style.css");
const JS: &str = include_str!("../../assets/script.js");

pub fn execute(filepath: PathBuf) -> Result<()> {
    // Read source file
    let file_contents = fs::read_to_string(&filepath)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", filepath.display()))?;

    // Parse
    let parser = parser::Parser::new(&file_contents);
    let program = parser.parse()?;

    // Analyze (type checking and inference)
    let annotated_program =
        analyzer::analyze(program, &file_contents).wrap_err("Type analysis failed")?;

    // Get base filename without extension
    let base_name = filepath
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| miette::miette!("Invalid filename: {}", filepath.display()))?;

    // Create build/docs directory
    let docs_dir = PathBuf::from("build/docs");
    fs::create_dir_all(&docs_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create directory: {}", docs_dir.display()))?;

    // Generate HTML documentation
    let html = generate_html(&annotated_program, base_name, &file_contents);
    let html_path = docs_dir.join("index.html");
    fs::write(&html_path, html)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write HTML file: {}", html_path.display()))?;
    info!("Generated HTML documentation: {}", html_path.display());

    // Write CSS file

    let css_path = docs_dir.join("style.css");
    fs::write(&css_path, CSS)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write CSS file: {}", css_path.display()))?;
    info!("Generated CSS file: {}", css_path.display());

    // Write JS fileconst JS: &str = include_str!("../../assets/script.js");
    let js_path = docs_dir.join("script.js");
    fs::write(&js_path, JS)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write JS file: {}", js_path.display()))?;
    info!("Generated JS file: {}", js_path.display());

    Ok(())
}

fn escape_html(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}

/// Analyze a method function to get its parameter types
fn analyze_method<'de>(
    func: &Function<'de>,
    program: &analyzer::AnnotatedProgram<'de>,
    source: &'de str,
    _struct_name: &str,
) -> Result<analyzer::AnnotatedFunction<'de>, miette::Error> {
    let mut analyzer_instance = analyzer::Analyzer::new(source);
    let mut ctx = TypeContext::new();

    // Add all type declarations to the context
    for item in &program.items {
        if let analyzer::AnnotatedTopLevelItem::TypeDecl(type_decl) = item {
            ctx.add_type(type_decl.name.clone(), type_decl.clone());
        }
    }

    // Self type is available in method context via the struct_name

    // Analyze the method function
    analyzer_instance.analyze_function(func, &mut ctx)
}

fn generate_html<'de>(
    program: &analyzer::AnnotatedProgram<'de>,
    title: &str,
    source: &'de str,
) -> String {
    let mut html = String::new();

    html.push_str("<!DOCTYPE html>\n");
    html.push_str("<html lang=\"en\">\n");
    html.push_str("<head>\n");
    html.push_str("    <meta charset=\"UTF-8\">\n");
    html.push_str(
        "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n",
    );
    html.push_str(&format!(
        "    <title>{} - Documentation</title>\n",
        escape_html(title)
    ));
    html.push_str("    <link rel=\"stylesheet\" href=\"style.css\">\n");
    html.push_str("</head>\n");
    html.push_str("<body>\n");
    html.push_str("    <div class=\"container\">\n");
    html.push_str(&format!("        <h1>{}</h1>\n", escape_html(title)));

    // Collect items by type
    let mut functions = Vec::new();
    let mut types = Vec::new();
    let mut variables = Vec::new();

    for item in &program.items {
        match item {
            analyzer::AnnotatedTopLevelItem::Function(func) => {
                functions.push(func);
            }
            analyzer::AnnotatedTopLevelItem::TypeDecl(ty) => {
                types.push(ty);
            }
            analyzer::AnnotatedTopLevelItem::VariableDecl(var) => {
                variables.push(var);
            }
        }
    }

    // Generate table of contents
    if !functions.is_empty() || !types.is_empty() || !variables.is_empty() {
        html.push_str("        <nav class=\"toc\">\n");
        html.push_str("            <h2>Table of Contents</h2>\n");
        html.push_str("            <ul>\n");

        if !functions.is_empty() {
            html.push_str("                <li><a href=\"#functions\">Functions</a></li>\n");
        }
        if !types.is_empty() {
            html.push_str("                <li><a href=\"#types\">Types</a></li>\n");
        }
        if !variables.is_empty() {
            html.push_str("                <li><a href=\"#variables\">Variables</a></li>\n");
        }

        html.push_str("            </ul>\n");
        html.push_str("        </nav>\n");
    }

    // Generate functions section
    if !functions.is_empty() {
        html.push_str("        <section id=\"functions\">\n");
        html.push_str("            <h2>Functions</h2>\n");
        for func in &functions {
            html.push_str("            <div class=\"item\">\n");
            html.push_str("                <div class=\"signature\">");
            html.push_str("<span class=\"keyword\">let </span>");
            html.push_str("<span class=\"item-name\" id=\"fn-");
            html.push_str(&escape_html(&func.name));
            html.push_str("\">");
            html.push_str(&escape_html(&func.name));
            html.push_str("</span>");

            // Generics
            if !func.generics.is_empty() {
                html.push_str("<span class=\"keyword\">&lt;</span>");
                for (i, generic) in func.generics.iter().enumerate() {
                    if i > 0 {
                        html.push_str("<span class=\"separator\">, </span>");
                    }
                    html.push_str("<span class=\"generic\">");
                    html.push_str(&escape_html(generic));
                    html.push_str("</span>");
                }
                html.push_str("<span class=\"keyword\">&gt;</span>");
            }

            // Parameters
            html.push_str("<span class=\"keyword\"> :: </span>");
            if func.params.is_empty() {
                html.push_str("<span class=\"params\">()</span>");
            } else {
                html.push_str("<span class=\"params\">");
                for (i, param) in func.params.iter().enumerate() {
                    if i > 0 {
                        html.push_str("<span class=\"separator\">, </span>");
                    }
                    html.push_str("<span class=\"param\">");
                    html.push_str("<span class=\"param-name\">");
                    html.push_str(&escape_html(&param.name));
                    html.push_str("</span>");
                    html.push_str("<span class=\"keyword\">: </span>");
                    html.push_str("<span class=\"type\">");
                    html.push_str(&escape_html(&param.type_annotation.to_string()));
                    html.push_str("</span>");
                    html.push_str("</span>");
                }
                html.push_str("</span>");
            }

            // Return type
            html.push_str("<span class=\"keyword\"> -&gt; </span>");
            html.push_str("<span class=\"return-type\">");
            html.push_str(&escape_html(&func.return_type.to_string()));
            html.push_str("</span>");
            html.push_str("</div>\n");
            html.push_str("            </div>\n");
        }
        html.push_str("        </section>\n");
    }

    // Generate types section
    if !types.is_empty() {
        html.push_str("        <section id=\"types\">\n");
        html.push_str("            <h2>Types</h2>\n");
        for ty in &types {
            html.push_str("            <div class=\"item\">\n");
            html.push_str("                <h3 class=\"item-name\" id=\"type-");
            html.push_str(&escape_html(&ty.name));
            html.push_str("\">");
            html.push_str(&escape_html(&ty.name));
            html.push_str("</h3>\n");

            match &ty.decl {
                novac::parser::ast::TypeDeclKind::Struct(s) => {
                    html.push_str("                <div class=\"type-kind\">struct</div>\n");
                    html.push_str("                <div class=\"type-definition\">\n");
                    html.push_str("                    <span class=\"keyword\">struct {</span>\n");
                    for (i, field) in s.fields.iter().enumerate() {
                        if i > 0 {
                            html.push_str(
                                "                    <span class=\"separator\">, </span>\n",
                            );
                        }
                        html.push_str("                    <div class=\"field\">\n");
                        html.push_str("                        <span class=\"field-name\">");
                        html.push_str(&escape_html(&field.name));
                        html.push_str("</span>\n");
                        html.push_str(
                            "                        <span class=\"keyword\">: </span>\n",
                        );
                        html.push_str("                        <span class=\"type\">");
                        html.push_str(&escape_html(&field.type_.to_string()));
                        html.push_str("</span>\n");
                        html.push_str("                    </div>\n");
                    }
                    html.push_str("                    <span class=\"keyword\">}</span>\n");

                    // Methods
                    if let Some(impl_block) = &s.impl_block {
                        html.push_str(
                            "                    <span class=\"keyword\"> with {</span>\n",
                        );
                        for func in impl_block {
                            // Analyze the method to get inferred parameter types
                            let analyzed_method = analyze_method(
                                func,
                                program,
                                source,
                                ty.name.as_ref(),
                            )
                            .unwrap_or_else(|_| {
                                // If analysis fails, create an annotated function
                                // with all parameters, using explicit types or defaulting to i32
                                use novac::analyzer::default_numeric_type;
                                analyzer::AnnotatedFunction {
                                    name: func.name.clone(),
                                    generics: func.generics.clone(),
                                    params: func
                                        .params
                                        .iter()
                                        .map(|p| {
                                            analyzer::AnnotatedFunctionParam {
                                                name: p.name.clone(),
                                                type_annotation: p
                                                    .type_annotation
                                                    .clone()
                                                    .unwrap_or_else(|| {
                                                        // Clone the static type to match lifetime
                                                        match default_numeric_type() {
                                                            parser::ast::Type::Primitive(prim) => {
                                                                parser::ast::Type::Primitive(prim)
                                                            }
                                                            _ => parser::ast::Type::Primitive(
                                                                parser::ast::PrimitiveType::I32,
                                                            ),
                                                        }
                                                    }),
                                            }
                                        })
                                        .collect(),
                                    return_type: func.return_type.as_ref().cloned().unwrap_or(
                                        parser::ast::Type::Primitive(
                                            parser::ast::PrimitiveType::Nil,
                                        ),
                                    ),
                                    body: analyzer::AnnotatedExprList::Single(Box::new(
                                        analyzer::AnnotatedExpr::new(
                                            parser::ast::Expr::Literal(parser::ast::Literal::Nil),
                                            parser::ast::Type::Primitive(
                                                parser::ast::PrimitiveType::Nil,
                                            ),
                                        ),
                                    )),
                                }
                            });

                            html.push_str("                        <div class=\"method\">\n");
                            html.push_str(
                                "                            <span class=\"keyword\">let </span>\n",
                            );
                            html.push_str(
                                "                            <span class=\"method-name\">",
                            );
                            html.push_str(&escape_html(&func.name));
                            html.push_str("</span>\n");

                            // Show method signature with types
                            html.push_str(
                                "                            <div class=\"method-signature\">\n",
                            );
                            html.push_str("                                <span class=\"keyword\">::</span>\n");
                            if analyzed_method.params.is_empty() {
                                html.push_str("                                <span class=\"params\">()</span>\n");
                            } else {
                                html.push_str(
                                    "                                <span class=\"params\">",
                                );
                                for (i, param) in analyzed_method.params.iter().enumerate() {
                                    if i > 0 {
                                        html.push_str("<span class=\"separator\">, </span>");
                                    }
                                    html.push_str("<span class=\"param\">");
                                    html.push_str("<span class=\"param-name\">");
                                    html.push_str(&escape_html(&param.name));
                                    html.push_str("</span>");
                                    html.push_str("<span class=\"keyword\">: </span>");
                                    html.push_str("<span class=\"type\">");
                                    html.push_str(&escape_html(&param.type_annotation.to_string()));
                                    html.push_str("</span>");
                                    html.push_str("</span>");
                                }
                                html.push_str("</span>\n");
                            }

                            // Return type
                            html.push_str("                                <span class=\"keyword\"> -&gt; </span>\n");
                            html.push_str(
                                "                                <span class=\"return-type\">",
                            );
                            html.push_str(&escape_html(&analyzed_method.return_type.to_string()));
                            html.push_str("</span>\n");
                            html.push_str("                            </div>\n");
                            html.push_str("                        </div>\n");
                        }
                        html.push_str("                    <span class=\"keyword\">}</span>\n");
                    }
                    html.push_str("                </div>\n");
                }
                novac::parser::ast::TypeDeclKind::Enum(e) => {
                    html.push_str("                <div class=\"type-kind\">enum</div>\n");
                    html.push_str("                <div class=\"type-definition\">\n");
                    html.push_str("                    <span class=\"keyword\">enum {</span>\n");
                    for (i, variant) in e.variants.iter().enumerate() {
                        if i > 0 {
                            html.push_str(
                                "                    <span class=\"separator\">, </span>\n",
                            );
                        }
                        html.push_str("                    <div class=\"variant\">\n");
                        html.push_str("                        <span class=\"variant-name\">");
                        html.push_str(&escape_html(&variant.name));
                        html.push_str("</span>\n");
                        if let Some(variant_type) = &variant.type_ {
                            html.push_str(
                                "                        <span class=\"keyword\">: </span>\n",
                            );
                            html.push_str("                        <span class=\"type\">");
                            html.push_str(&escape_html(&variant_type.to_string()));
                            html.push_str("</span>\n");
                        }
                        html.push_str("                    </div>\n");
                    }
                    html.push_str("                    <span class=\"keyword\">}</span>\n");
                    html.push_str("                </div>\n");
                }
            }
            html.push_str("            </div>\n");
        }
        html.push_str("        </section>\n");
    }

    // Generate variables section
    if !variables.is_empty() {
        html.push_str("        <section id=\"variables\">\n");
        html.push_str("            <h2>Variables</h2>\n");
        for var in &variables {
            html.push_str("            <div class=\"item\">\n");
            html.push_str("                <h3 class=\"item-name\" id=\"var-");
            html.push_str(&escape_html(&var.name));
            html.push_str("\">");
            html.push_str(&escape_html(&var.name));
            html.push_str("</h3>\n");
            html.push_str("                <div class=\"type\">");
            html.push_str(&escape_html(&var.type_annotation.to_string()));
            html.push_str("</div>\n");
            html.push_str("            </div>\n");
        }
        html.push_str("        </section>\n");
    }

    html.push_str("    </div>\n");
    html.push_str("    <script src=\"script.js\"></script>\n");
    html.push_str("</body>\n");
    html.push_str("</html>\n");

    html
}
