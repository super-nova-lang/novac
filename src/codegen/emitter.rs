pub type Section = String;

#[derive(Debug)]
pub struct Emitter {
    indent: usize,
    dat: Section,
    bss: Section,
    txt: Section,
}

impl Emitter {
    pub fn emit(self) -> String {
        let mut s = String::new();
        // NASM syntax - no format directive needed, assembler will determine from -f flag
        s.push_str("section .text\n");
        s.push_str(&self.txt);
        s.push_str("section .bss\n");
        s.push_str(&self.bss);
        s.push_str("section .data\n");
        s.push_str(&self.dat);
        s
    }

    // Indentation helpers
    #[allow(dead_code)]
    pub fn push_indent(&mut self) {
        self.indent = self.indent.saturating_add(2);
    }

    #[allow(dead_code)]
    pub fn pop_indent(&mut self) {
        if self.indent >= 2 {
            self.indent -= 2;
        }
    }

    #[allow(dead_code)]
    pub fn with_indent<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.push_indent();
        let r = f(self);
        self.pop_indent();
        r
    }

    // Comments and raw output
    #[allow(dead_code)]
    pub fn write_comment(&mut self, s: &str) {
        for _ in 0..self.indent {
            self.txt.push(' ');
        }
        self.txt.push_str("; ");
        self.txt.push_str(s);
        self.txt.push('\n');
    }

    /// Write a raw line into the text section without any indentation.
    #[allow(dead_code)]
    pub fn write_raw(&mut self, s: &str) {
        self.txt.push_str(s);
        self.txt.push('\n');
    }

    // Data helpers
    pub fn emit_data_string(&mut self, label: &str, value: &str) {
        // NASM syntax: use db for null-terminated strings
        // NASM doesn't interpret \n in double quotes, so we need to handle newlines specially
        self.write_dat_label(label);

        // Split by newlines and emit each part with a newline byte (10)
        let parts: Vec<&str> = value.split('\n').collect();
        if parts.len() == 1 {
            // No newlines, just emit the string normally
            let esc = escape_asciz(value);
            self.write_dat(&format!("db \"{}\", 0", esc));
        } else {
            // Has newlines - emit parts separated by 10 (newline byte)
            let mut parts_iter = parts.iter();
            if let Some(first) = parts_iter.next() {
                let esc = escape_asciz(first);
                self.write_dat(&format!("db \"{}\"", esc));
            }
            for part in parts_iter {
                self.write_dat("db 10"); // Newline byte
                if !part.is_empty() {
                    let esc = escape_asciz(part);
                    self.write_dat(&format!("db \"{}\"", esc));
                }
            }
            self.write_dat("db 0"); // Null terminator
        }
    }

    #[allow(dead_code)]
    pub fn reserve_bss(&mut self, label: &str, size: usize) {
        self.write_bss_label(label);
        self.write_bss(&format!("resb {}", size));
    }

    // Existing helpers (kept for backward compatibility)
    pub fn write_dat(&mut self, s: &str) {
        for _ in 0..self.indent {
            self.dat.push(' ');
        }
        self.dat.push_str(s);
        self.dat.push('\n');
    }

    pub fn write_dat_label(&mut self, s: &str) {
        self.dat.push_str(s);
        self.dat.push_str(":\n");
    }

    #[allow(dead_code)]
    pub fn write_bss(&mut self, s: &str) {
        for _ in 0..self.indent {
            self.bss.push(' ');
        }
        self.bss.push_str(s);
        self.bss.push('\n');
    }

    #[allow(dead_code)]
    pub fn write_bss_label(&mut self, s: &str) {
        self.bss.push_str(s);
        self.bss.push_str(":\n");
    }

    pub fn write_txt(&mut self, s: &str) {
        for _ in 0..self.indent {
            self.txt.push(' ');
        }
        self.txt.push_str(s);
        self.txt.push('\n');
    }

    pub fn write_txt_label(&mut self, s: &str) {
        self.txt.push_str(s);
        self.txt.push_str(":\n");
    }
}

fn escape_asciz(s: &str) -> String {
    // Escape backslashes and quotes, but NOT newlines (handled separately in emit_data_string)
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

impl Default for Emitter {
    fn default() -> Self {
        Self {
            indent: 2,
            dat: String::default(),
            bss: String::default(),
            txt: String::default(),
        }
    }
}
