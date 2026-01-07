use crate::parser::nodes::Node;
use anyhow::Result;

use super::common::Context;

/// Emits AMD64 Windows assembly from the AST.
pub struct Emitter<'ctx> {
    ctx: &'ctx Context,
    header: Vec<String>,
    text: Vec<String>,
    rodata: Vec<String>,
    data: Vec<String>,
    bss: Vec<String>,
}

impl<'ctx> Emitter<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        Self {
            ctx,
            header: Vec::new(),
            text: Vec::new(),
            rodata: Vec::new(),
            data: Vec::new(),
            bss: Vec::new(),
        }
    }

    pub fn emit(&self, ast: &[Node]) -> Result<String> {
        let mut emitter = Emitter::new(self.ctx);

        emitter.preamble(ast);
        emitter.entry_stub();

        Ok(emitter.render())
    }

    fn preamble(&mut self, ast: &[Node]) {
        self.header
            .push(format!("# module: {}", self.ctx.module_name));
        self.header.push(format!("# nodes: {}", ast.len()));
        self.header
            .push(format!("# mangler: {}", self.ctx.mangler().module_tag()));
        self.header.push(String::from("# amd64-windows backend"));
        self.header.push(String::new());
        self.header.push(String::from(".globl main"));
    }

    fn entry_stub(&mut self) {
        self.text.push(String::from("main:"));
        self.text.push(String::from(
            "    sub     rsp, 40         # shadow space + alignment",
        ));
        self.text
            .push(String::from("    xor     ecx, ecx        # exit code 0"));
        self.text
            .push(String::from("    call    ExitProcess     # Windows API"));
    }

    fn render(&self) -> String {
        let mut out = Vec::new();
        out.extend(self.header.iter().cloned());

        out.push(String::new());
        out.push(String::from(".text"));
        if self.text.is_empty() {
            out.push(String::from("# <no text>"));
        } else {
            out.extend(self.text.iter().cloned());
        }

        out.push(String::new());
        out.push(String::from(".rdata"));
        if self.rodata.is_empty() {
            out.push(String::from("# <no rodata>"));
        } else {
            out.extend(self.rodata.iter().cloned());
        }

        out.push(String::new());
        out.push(String::from(".data"));
        if self.data.is_empty() {
            out.push(String::from("# <no data>"));
        } else {
            out.extend(self.data.iter().cloned());
        }

        out.push(String::new());
        out.push(String::from(".bss"));
        if self.bss.is_empty() {
            out.push(String::from("# <no bss>"));
        } else {
            out.extend(self.bss.iter().cloned());
        }

        out.join("\n")
    }
}

/// AMD64 Windows backend entrypoint.
pub fn gen_target(module_name: &str, ast: &[Node]) -> Result<String> {
    let ctx = Context::new(module_name);
    let emitter = Emitter::new(&ctx);
    emitter.emit(ast)
}
