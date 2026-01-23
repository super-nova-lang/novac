use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::*;
use miette::Result;

/// Create a string constant in the module
pub fn create_string_constant<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    value: &str,
) -> PointerValue<'ctx> {
    let string_type = context.i8_type().array_type((value.len() + 1) as u32);
    let string_value = context.const_string(value.as_bytes(), false);
    let global = module.add_global(string_type, None, "str");
    global.set_initializer(&string_value);
    global.set_constant(true);
    global.as_pointer_value()
}

/// Mangle a function name
pub fn mangle_function_name(name: &str) -> String {
    // Simple name mangling - replace special characters
    name.replace("::", "_")
        .replace(":", "_")
        .replace(".", "_")
        .replace("-", "_")
}

/// Mangle a method name (type::method)
pub fn mangle_method_name(type_name: &str, method_name: &str) -> String {
    format!("{}_{}", mangle_function_name(type_name), mangle_function_name(method_name))
}

/// Get a unique name for a local variable
pub fn unique_var_name(base: &str, counter: &mut u32) -> String {
    let name = format!("{}_{}", base, counter);
    *counter += 1;
    name
}

/// Helper to get a pointer value from a basic type enum
pub fn get_pointer_type<'ctx>(
    ty: BasicTypeEnum<'ctx>,
    address_space: AddressSpace,
) -> PointerType<'ctx> {
    match ty {
        BasicTypeEnum::ArrayType(t) => t.ptr_type(address_space),
        BasicTypeEnum::FloatType(t) => t.ptr_type(address_space),
        BasicTypeEnum::IntType(t) => t.ptr_type(address_space),
        BasicTypeEnum::PointerType(t) => t.ptr_type(address_space),
        BasicTypeEnum::StructType(t) => t.ptr_type(address_space),
        BasicTypeEnum::VectorType(t) => t.ptr_type(address_space),
    }
}

/// Convert BuilderError to miette::Error
pub fn builder_error_to_miette(err: inkwell::builder::BuilderError) -> miette::Error {
    miette::miette!("LLVM builder error: {}", err)
}
