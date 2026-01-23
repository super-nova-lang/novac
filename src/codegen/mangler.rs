use crate::parser::ast::{PrimitiveType, Type};

#[derive(Debug)]
pub struct Mangler {
    prefix: String,
}

impl Mangler {
    pub fn new(prefix: String) -> Self {
        Self { prefix }
    }

    /// Simple mangling (backward compatible)
    pub fn mangle(&self, symbol: &str) -> String {
        self.mangle_with_type(symbol, None)
    }

    /// Mangle a function with full signature
    pub fn mangle_function(
        &self,
        name: &str,
        params: &[Type],
        return_type: &Type,
        generics: Option<&[&str]>,
    ) -> String {
        let encoded_name = self.encode_name(name);
        let mut result = format!("{}_f{}", self.prefix, encoded_name);

        // Add generic parameters if present
        if let Some(generic_params) = generics {
            if !generic_params.is_empty() {
                result.push('_');
                for (i, generic_param) in generic_params.iter().enumerate() {
                    if i > 0 {
                        result.push('_');
                    }
                    result.push_str(&self.encode_name(generic_param));
                }
            }
        }

        // Add parameter types
        for param_ty in params {
            result.push('_');
            result.push_str(&self.encode_type(param_ty));
        }

        // Add return type
        result.push('_');
        result.push_str(&self.encode_type(return_type));

        result
    }

    /// Mangle a variable with type
    pub fn mangle_variable(&self, name: &str, ty: &Type) -> String {
        let encoded_name = self.encode_name(name);
        let encoded_type = self.encode_type(ty);
        format!("{}_v{}_{}", self.prefix, encoded_name, encoded_type)
    }

    /// Mangle a type name
    #[allow(dead_code)]
    pub fn mangle_type(&self, name: &str) -> String {
        let encoded_name = self.encode_name(name);
        format!("{}_t{}", self.prefix, encoded_name)
    }

    /// Mangle with optional type information
    pub fn mangle_with_type(&self, name: &str, ty: Option<&Type>) -> String {
        let encoded_name = self.encode_name(name);
        if let Some(ty) = ty {
            let encoded_type = self.encode_type(ty);
            format!("{}_v{}_{}", self.prefix, encoded_name, encoded_type)
        } else {
            // Fallback: just prefix + name (backward compatible)
            format!("{}{}", self.prefix, encoded_name)
        }
    }

    /// Encode a type to a compact string representation
    pub fn encode_type(&self, ty: &Type) -> String {
        match ty {
            Type::Primitive(p) => self.encode_primitive_type(*p),
            Type::Named(name) => {
                // Check if named type is actually a primitive (e.g., "i32" written as Type::Named)
                if let Some(prim) = Self::primitive_from_name(name) {
                    self.encode_primitive_type(prim)
                } else {
                    let encoded_name = self.encode_name(name);
                    format!("T{}", encoded_name)
                }
            }
            Type::Generic { base, args } => {
                let base_str = self.encode_type(base);
                let mut result = format!("G{}", base_str);
                for arg in args {
                    result.push('_');
                    result.push_str(&self.encode_type(arg));
                }
                result
            }
            Type::Function { params } => {
                let mut result = String::from("F");
                for param in params {
                    result.push_str(&self.encode_type(param));
                    result.push('_');
                }
                // Remove trailing underscore
                if result.ends_with('_') {
                    result.pop();
                }
                result
            }
            Type::Tuple(types) => {
                let mut result = String::from("U");
                for ty in types {
                    result.push_str(&self.encode_type(ty));
                    result.push('_');
                }
                // Remove trailing underscore
                if result.ends_with('_') {
                    result.pop();
                }
                result
            }
            Type::AnonymousStruct(fields) => {
                let mut result = String::from("S");
                for field in fields {
                    result.push_str(&self.encode_name(&field.name));
                    result.push('_');
                    result.push_str(&self.encode_type(&field.type_));
                    result.push('_');
                }
                // Remove trailing underscore
                if result.ends_with('_') {
                    result.pop();
                }
                result
            }
        }
    }

    /// Encode a primitive type
    fn encode_primitive_type(&self, ty: PrimitiveType) -> String {
        match ty {
            PrimitiveType::I8 => "i8".to_string(),
            PrimitiveType::I16 => "i16".to_string(),
            PrimitiveType::I32 => "i32".to_string(),
            PrimitiveType::I64 => "i64".to_string(),
            PrimitiveType::Isize => "is".to_string(),
            PrimitiveType::U8 => "u8".to_string(),
            PrimitiveType::U16 => "u16".to_string(),
            PrimitiveType::U32 => "u32".to_string(),
            PrimitiveType::U64 => "u64".to_string(),
            PrimitiveType::Usize => "us".to_string(),
            PrimitiveType::Str => "s".to_string(),
            PrimitiveType::Char => "c".to_string(),
            PrimitiveType::Bool => "b".to_string(),
            PrimitiveType::Nil => "n".to_string(),
            PrimitiveType::List => "List".to_string(),
        }
    }

    /// Encode a symbol name, handling special characters and reserved words
    pub fn encode_name(&self, name: &str) -> String {
        // Check if it's a reserved word
        if Self::is_reserved_word(name) {
            return format!("${}", name);
        }

        let mut result = String::new();
        for ch in name.chars() {
            if Self::needs_encoding(ch) {
                result.push_str(&Self::hex_encode(ch));
            } else {
                result.push(ch);
            }
        }
        result
    }

    /// Check if a name is a reserved word (assembly register names, etc.)
    fn is_reserved_word(name: &str) -> bool {
        matches!(
            name,
            "rax" | "rbx" | "rcx" | "rdx" | "rsi" | "rdi" | "rbp" | "rsp" | "r8" | "r9"
                | "r10" | "r11" | "r12" | "r13" | "r14" | "r15" | "eax" | "ebx" | "ecx"
                | "edx" | "esi" | "edi" | "ebp" | "esp" | "al" | "bl" | "cl" | "dl"
                | "ah" | "bh" | "ch" | "dh" | "ax" | "bx" | "cx" | "dx" | "si" | "di"
                | "bp" | "sp" | "if" | "else" | "return" | "let" | "match" | "struct"
                | "enum" | "with" | "and" | "or" | "true" | "false" | "nil"
        )
    }

    /// Check if a character needs encoding
    fn needs_encoding(ch: char) -> bool {
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => false,
            _ => true,
        }
    }

    /// Encode a character as hex (e.g., '.' -> "$2E")
    fn hex_encode(ch: char) -> String {
        let code = ch as u32;
        format!("${:02X}", code)
    }

    /// Check if a named type is actually a primitive type
    fn primitive_from_name(name: &str) -> Option<PrimitiveType> {
        match name {
            "i8" => Some(PrimitiveType::I8),
            "i16" => Some(PrimitiveType::I16),
            "i32" => Some(PrimitiveType::I32),
            "i64" => Some(PrimitiveType::I64),
            "isize" => Some(PrimitiveType::Isize),
            "u8" => Some(PrimitiveType::U8),
            "u16" => Some(PrimitiveType::U16),
            "u32" => Some(PrimitiveType::U32),
            "u64" => Some(PrimitiveType::U64),
            "usize" => Some(PrimitiveType::Usize),
            "str" => Some(PrimitiveType::Str),
            "char" => Some(PrimitiveType::Char),
            "bool" => Some(PrimitiveType::Bool),
            "nil" => Some(PrimitiveType::Nil),
            "list" => Some(PrimitiveType::List),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{PrimitiveType, Type};

    #[test]
    fn test_simple_mangling() {
        let m = Mangler::new("main".to_string());
        assert_eq!(m.mangle("add"), "mainadd");
    }

    #[test]
    fn test_name_encoding() {
        let m = Mangler::new("main".to_string());
        assert_eq!(m.encode_name("add"), "add");
        assert_eq!(m.encode_name("my-var"), "my$2Dvar");
        assert_eq!(m.encode_name("my.var"), "my$2Evar");
        assert_eq!(m.encode_name("rax"), "$rax"); // Reserved word
    }

    #[test]
    fn test_primitive_type_encoding() {
        let m = Mangler::new("main".to_string());
        assert_eq!(m.encode_type(&Type::Primitive(PrimitiveType::I32)), "i32");
        assert_eq!(m.encode_type(&Type::Primitive(PrimitiveType::Str)), "s");
        assert_eq!(m.encode_type(&Type::Primitive(PrimitiveType::Bool)), "b");
    }

    #[test]
    fn test_function_mangling() {
        let m = Mangler::new("main".to_string());
        let params = vec![
            Type::Primitive(PrimitiveType::I32),
            Type::Primitive(PrimitiveType::I32),
        ];
        let return_type = Type::Primitive(PrimitiveType::I32);
        let mangled = m.mangle_function("add", &params, &return_type, None);
        assert_eq!(mangled, "main_fadd_i32_i32_i32");
    }

    #[test]
    fn test_variable_mangling() {
        let m = Mangler::new("main".to_string());
        let ty = Type::Primitive(PrimitiveType::I32);
        let mangled = m.mangle_variable("x", &ty);
        assert_eq!(mangled, "main_vx_i32");
    }

    #[test]
    fn test_named_type_encoding() {
        let m = Mangler::new("main".to_string());
        let ty = Type::Named("Point".into());
        assert_eq!(m.encode_type(&ty), "TPoint");
    }

    #[test]
    fn test_generic_type_encoding() {
        let m = Mangler::new("main".to_string());
        let ty = Type::Generic {
            base: Box::new(Type::Named("List".into())),
            args: vec![Type::Primitive(PrimitiveType::I32)],
        };
        assert_eq!(m.encode_type(&ty), "GTList_i32");
    }

    #[test]
    fn test_special_characters() {
        let m = Mangler::new("main".to_string());
        assert_eq!(m.encode_name("my-var"), "my$2Dvar");
        assert_eq!(m.encode_name("my.var"), "my$2Evar");
        assert_eq!(m.encode_name("my+var"), "my$2Bvar");
        assert_eq!(m.encode_name("my*var"), "my$2Avar");
    }

    #[test]
    fn test_reserved_words() {
        let m = Mangler::new("main".to_string());
        assert_eq!(m.encode_name("rax"), "$rax");
        assert_eq!(m.encode_name("if"), "$if");
        assert_eq!(m.encode_name("return"), "$return");
    }

    #[test]
    fn test_function_with_generics() {
        let m = Mangler::new("main".to_string());
        let params = vec![Type::Primitive(PrimitiveType::I32)];
        let return_type = Type::Primitive(PrimitiveType::I32);
        let generics = Some(vec!["T"]);
        let mangled = m.mangle_function("map", &params, &return_type, generics.as_deref());
        assert_eq!(mangled, "main_fmap_T_i32_i32");
    }

    #[test]
    fn test_tuple_type_encoding() {
        let m = Mangler::new("main".to_string());
        let ty = Type::Tuple(vec![
            Type::Primitive(PrimitiveType::I32),
            Type::Primitive(PrimitiveType::Str),
        ]);
        assert_eq!(m.encode_type(&ty), "Ui32_s");
    }

    #[test]
    fn test_function_type_encoding() {
        let m = Mangler::new("main".to_string());
        let ty = Type::Function {
            params: vec![
                Type::Primitive(PrimitiveType::I32),
                Type::Primitive(PrimitiveType::I32),
            ],
        };
        assert_eq!(m.encode_type(&ty), "Fi32_i32");
    }
}
