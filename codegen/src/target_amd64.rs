use anyhow::Result;
use parser::nodes::{DeclParam, Node, Type};

/// Mangling utility to keep symbols unique per module and signature.
pub struct Mangler {
    module_tag: String,
}

impl Mangler {
    pub fn new(module_name: &str) -> Self {
        Self {
            module_tag: Self::sanitize_module_name(module_name),
        }
    }

    pub fn module_tag(&self) -> &str {
        &self.module_tag
    }

    pub fn mangle_function_symbol(
        &self,
        name: &str,
        params: &[DeclParam],
        ret: &Option<Type>,
    ) -> String {
        let param_sig = if params.is_empty() {
            "v".to_string()
        } else {
            params
                .iter()
                .map(|p| self.encode_param(p))
                .collect::<Vec<_>>()
                .join("_")
        };

        let ret_sig = match ret {
            Some(t) => self.encode_type(t),
            _ => "v".to_string(),
        };

        format!(
            "_N{}__{}__P{}__R{}",
            self.module_tag,
            self.sanitize_ident(name),
            param_sig,
            ret_sig
        )
    }

    fn encode_param(&self, param: &DeclParam) -> String {
        match param {
            DeclParam::Typed(_, t) => format!("T{}", self.encode_type(t)),
            DeclParam::Untyped(_) => "U".to_string(),
            DeclParam::OptionalTyped(_, t, _) => format!("O{}", self.encode_type(t)),
            DeclParam::OptionalUntyped(_, _) => "O".to_string(),
            DeclParam::Variadic(_, Some(t)) => format!("V{}", self.encode_type(t)),
            DeclParam::Variadic(_, _) => "VZ".to_string(),
        }
    }

    fn encode_type(&self, typ: &Type) -> String {
        match typ {
            Type::User(name) => format!("U{}", self.sanitize_ident(name)),
            Type::Generic(name, args) => {
                let inner = args
                    .iter()
                    .map(|t| self.encode_type(t))
                    .collect::<Vec<_>>()
                    .join("$");
                format!("G{}${}", self.sanitize_ident(name), inner)
            }
            Type::TypeVar(name) => format!("V{}", self.sanitize_ident(name)),
            Type::Builtin(name) => format!("B{}", self.sanitize_ident(name)),
            Type::UnitTyp => "Z".to_string(),
            Type::ListTyp(inner) => format!("L{}", self.encode_type(inner)),
        }
    }

    fn sanitize_module_name(module_name: &str) -> String {
        let mut out: String = module_name
            .chars()
            .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
            .collect();
        if out.is_empty() {
            out.push('m');
        }
        out
    }

    fn sanitize_ident(&self, ident: &str) -> String {
        if ident.is_empty() {
            return "anon".to_string();
        }

        ident
            .chars()
            .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
            .collect()
    }
}

/// Target-level context for AMD64 code generation.
pub struct Context {
    module_name: String,
    mangler: Mangler,
}

impl Context {
    pub fn new(module_name: &str) -> Self {
        Self {
            module_name: module_name.to_string(),
            mangler: Mangler::new(module_name),
        }
    }

    pub fn mangler(&self) -> &Mangler {
        &self.mangler
    }
}

/// Emits AMD64 code from the AST.
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
        self.header.push(String::from("# handrolled amd64 backend"));
        self.header.push(String::new());
        self.header.push(String::from(".global _start"));
    }

    fn entry_stub(&mut self) {
        self.text.push(String::from("_start:"));
        self.text
            .push(String::from("    xor     %rdi, %rdi    # exit code 0"));
        self.text
            .push(String::from("    mov     $60, %rax     # sys_exit"));
        self.text.push(String::from("    syscall"));
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
        out.push(String::from(".rodata"));
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

/// AMD64 backend entrypoint.
pub fn gen_target(module_name: &str, ast: &[Node]) -> Result<String> {
    let ctx = Context::new(module_name);
    let emitter = Emitter::new(&ctx);
    emitter.emit(ast)
}
