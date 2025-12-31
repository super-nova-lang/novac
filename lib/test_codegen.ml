let test_codegen_file (filename) =
  let name, content = List.find (fun (n, _) -> n = filename) Nova_tests.all in
  Format.printf "File: %s\n" name;
  Codegen.reset_module ();
  (* Register stdlib modules for testing *)
  Hashtbl.add Codegen.opened_modules "std_c" "std_c";
  Hashtbl.add Codegen.opened_modules "std_math" "std_math";
  Hashtbl.add Codegen.opened_modules "std_io" "std_io";
  let tokens = Lexer.lex (filename ^ ".nova") content in
  let nodes = Parser.parse (Parser.create tokens) in
  List.iter Codegen.codegen nodes;
  Codegen.finish_module ();
  Llvm.dump_module !Codegen.the_module
;;

[@@@ocamlformat "disable"]
let%expect_test "codegen_basic_functions" = 
  test_codegen_file "basic_functions";
  [%expect {|
    ; ModuleID = 'Nova'
    source_filename = "Nova"

    @str = private unnamed_addr constant [16 x i8] c"add(6, 7) = %d\0A\00", align 1
    @str.1 = private unnamed_addr constant [16 x i8] c"mul(1, 1) = %d\0A\00", align 1
    @str.2 = private unnamed_addr constant [29 x i8] c"my_complex_fn(2, 1, 5) = %d\0A\00", align 1

    define i32 @add(i32 %a, i32 %b) {
    entry:
      %addtmp = add i32 %a, %b
      ret i32 %addtmp
    }

    define i32 @sub(i32 %a, i32 %b) {
    entry:
      %subtmp = sub i32 %a, %b
      ret i32 %subtmp
    }

    define i32 @mul(i32 %a, i32 %b) {
    entry:
      %multmp = mul i32 %a, %b
      ret i32 %multmp
    }

    define i32 @my_complex_fn(i32 %a, i32 %b, i32 %c) {
    entry:
      %multmp = mul i32 %b, 2
      %addtmp = add i32 %a, %multmp
      %divtmp = sdiv i32 %b, %addtmp
      %subtmp = sub i32 %addtmp, 55
      %xortmp = xor i32 %c, %subtmp
      %subtmp1 = sub i32 %xortmp, %divtmp
      ret i32 %subtmp1
    }

    define i32 @main() {
    entry:
      %0 = call i32 @add(i32 6, i32 7)
      %1 = call i32 @std_c_printf(ptr @str, i32 %0)
      %2 = call i32 @mul(i32 1, i32 1)
      %3 = call i32 @std_c_printf(ptr @str.1, i32 %2)
      %4 = call i32 @my_complex_fn(i32 2, i32 1, i32 5)
      %5 = call i32 @std_c_printf(ptr @str.2, i32 %4)
      ret i32 0
      ret i32 0
    }

    declare i32 @std_c_printf(ptr %0, i32 %1)
    ; ModuleID = 'Nova'
    source_filename = "Nova"

    %Job = type { i32, ptr, i32 }
    %Person = type { ptr, i8, ptr }
    %some_struct = type {}
    %some_struct.0 = type {}

    @str = private unnamed_addr constant [11 x i8] c"programmer\00", align 1
    @str.1 = private unnamed_addr constant [10 x i8] c"sales rep\00", align 1
    @fmt = private unnamed_addr constant [36 x i8] c"Hello, my name is %s and I am a %s\0A\00", align 1
    @str.2 = private unnamed_addr constant [7 x i8] c"Ashton\00", align 1
    @str.3 = private unnamed_addr constant [5 x i8] c"nova\00", align 1
    @fmt.4 = private unnamed_addr constant [17 x i8] c"Hourly rate: %d\0A\00", align 1
    @str.5 = private unnamed_addr constant [10 x i8] c"Joe Shmoe\00", align 1

    define ptr @Job_programmer(i32 %0, i32 %1) {
    entry:
      %enum = call ptr @malloc(i64 ptrtoint (ptr getelementptr (%Job, ptr null, i32 1) to i64))
      %tag = getelementptr inbounds %Job, ptr %enum, i32 0, i32 0
      store i32 0, ptr %tag, align 4
      %salary = getelementptr inbounds %Job, ptr %enum, i32 0, i32 2
      store i32 %0, ptr %salary, align 4
      %data = call ptr @malloc(i64 ptrtoint (ptr getelementptr ({ ptr }, ptr null, i32 1) to i64))
      %language = getelementptr inbounds { ptr }, ptr %data, i32 0, i32 0
      store i32 %1, ptr %language, align 4
      %data_ptr = getelementptr inbounds %Job, ptr %enum, i32 0, i32 1
      store ptr %data, ptr %data_ptr, align 8
      ret ptr %enum
    }

    declare ptr @malloc(i64 %0)

    define ptr @Job_other(i32 %0, i32 %1, ptr %2) {
    entry:
      %enum = call ptr @malloc(i64 ptrtoint (ptr getelementptr (%Job, ptr null, i32 1) to i64))
      %tag = getelementptr inbounds %Job, ptr %enum, i32 0, i32 0
      store i32 1, ptr %tag, align 4
      %salary = getelementptr inbounds %Job, ptr %enum, i32 0, i32 2
      store i32 %0, ptr %salary, align 4
      %data = call ptr @malloc(i64 ptrtoint (ptr getelementptr ({ ptr }, ptr null, i32 1) to i64))
      %data1 = getelementptr inbounds { ptr }, ptr %data, i32 0, i32 0
      store ptr %2, ptr %data1, align 8
      %data_ptr = getelementptr inbounds %Job, ptr %enum, i32 0, i32 1
      store ptr %data, ptr %data_ptr, align 8
      ret ptr %enum
    }

    define ptr @Job_sales_rep(i32 %0, i32 %1) {
    entry:
      %enum = call ptr @malloc(i64 ptrtoint (ptr getelementptr (%Job, ptr null, i32 1) to i64))
      %tag = getelementptr inbounds %Job, ptr %enum, i32 0, i32 0
      store i32 2, ptr %tag, align 4
      %salary = getelementptr inbounds %Job, ptr %enum, i32 0, i32 2
      store i32 %0, ptr %salary, align 4
      %data_ptr = getelementptr inbounds %Job, ptr %enum, i32 0, i32 1
      store ptr null, ptr %data_ptr, align 8
      ret ptr %enum
    }

    define i32 @Job_salary(ptr %self) {
    entry:
      ret i32 0
    }

    define i32 @Job_hourly(ptr %self, i32 %hours) {
    entry:
      %ptr = getelementptr inbounds %Job, ptr %self, i32 0, i32 2
      %val = load i32, ptr %ptr, align 4
      %divtmp = sdiv i32 %val, %hours
      ret i32 %divtmp
    }

    define ptr @Job_show(ptr %self) {
    entry:
      %tag = getelementptr inbounds %Job, ptr %self, i32 0, i32 0
      %tag1 = load i32, ptr %tag, align 4
      %match_res = alloca ptr, align 8
      %tag_eq = icmp eq i32 %tag1, 0
      br i1 %tag_eq, label %arm_0, label %next_0

    match_end:                                        ; preds = %next_2, %arm_2, %arm_1, %arm_0
      %res = load ptr, ptr %match_res, align 8
      ret ptr %res

    arm_0:                                            ; preds = %entry
      store ptr @str, ptr %match_res, align 8
      br label %match_end

    next_0:                                           ; preds = %entry
      %tag_eq2 = icmp eq i32 %tag1, 1
      br i1 %tag_eq2, label %arm_1, label %next_1

    arm_1:                                            ; preds = %next_0
      %data_ptr_ptr = getelementptr inbounds %Job, ptr %self, i32 0, i32 1
      %data_ptr = load ptr, ptr %data_ptr_ptr, align 8
      store ptr %data_ptr, ptr %match_res, align 8
      br label %match_end

    next_1:                                           ; preds = %next_0
      %tag_eq3 = icmp eq i32 %tag1, 2
      br i1 %tag_eq3, label %arm_2, label %next_2

    arm_2:                                            ; preds = %next_1
      store ptr @str.1, ptr %match_res, align 8
      br label %match_end

    next_2:                                           ; preds = %next_1
      br label %match_end
    }

    define ptr @Person(ptr %name, i8 %age, ptr %job) {
    entry:
      %struct = call ptr @malloc(i64 ptrtoint (ptr getelementptr (%Person, ptr null, i32 1) to i64))
      %name1 = getelementptr inbounds %Person, ptr %struct, i32 0, i32 0
      store ptr %name, ptr %name1, align 8
      %age2 = getelementptr inbounds %Person, ptr %struct, i32 0, i32 1
      store i8 %age, ptr %age2, align 1
      %job3 = getelementptr inbounds %Person, ptr %struct, i32 0, i32 2
      store ptr %job, ptr %job3, align 8
      ret ptr %struct
    }

    define ptr @Person_introduce(ptr %self) {
    entry:
      %ptr = getelementptr inbounds %Person, ptr %self, i32 0, i32 0
      %val = load ptr, ptr %ptr, align 8
      %ptr1 = getelementptr inbounds %Person, ptr %self, i32 0, i32 2
      %val2 = load ptr, ptr %ptr1, align 8
      %0 = call ptr @Job_show(ptr %val2)
      %calltmp = call i32 (ptr, ...) @printf(ptr @fmt, ptr %val, ptr %0)
      %retcast = inttoptr i32 %calltmp to ptr
      ret ptr %retcast
    }

    declare i32 @printf(ptr %0, ...)

    define i32 @my_printf(i32 %fmt, i32 %args) {
    entry:
      ret i32 0
    }

    define i32 @my_derive(i32 %tt) {
    entry:
      ret i32 0
    }

    define ptr @some_struct() {
    entry:
      %struct = call ptr @malloc(i64 ptrtoint (ptr getelementptr (%some_struct, ptr null, i32 1) to i64))
      ret ptr %struct

    entry1:                                           ; No predecessors!
      %struct2 = call ptr @malloc(i64 ptrtoint (ptr getelementptr (%some_struct.0, ptr null, i32 1) to i64))
      ret ptr %struct2
    }

    define i32 @my_derive_with_params(i32 %tt, i32 %a, i32 %b) {
    entry:
      ret i32 0
    }

    define i32 @main() {
    entry:
      %0 = call ptr @Job_programmer(i32 1000, i32 ptrtoint (ptr @str.3 to i32))
      %1 = call ptr @Person(ptr @str.2, i8 19, ptr %0)
      %2 = call ptr @Person_introduce(ptr %1)
      %ptr = getelementptr inbounds %Person, ptr %1, i32 0, i32 2
      %val = load ptr, ptr %ptr, align 8
      %3 = call i32 @Job_hourly(ptr %val, i32 40)
      %calltmp = call i32 (ptr, ...) @printf(ptr @fmt.4, i32 %3)
      %4 = call ptr @Job_sales_rep(i32 0, i32 0)
      %5 = call ptr @Person(ptr @str.5, i8 37, ptr %4)
      %6 = call ptr @Person_introduce(ptr %5)
      ret i32 0
      ret i32 0
    }
    ; ModuleID = 'Nova'
    source_filename = "Nova"

    @str = private unnamed_addr constant [12 x i8] c"5 * 3 = %d\0A\00", align 1
    @str.1 = private unnamed_addr constant [12 x i8] c"2 + 3 = %d\0A\00", align 1

    define i32 @mul(i32 %a, i32 %b) {
    entry:
      %multmp = mul i32 %a, %b
      ret i32 %multmp
    }

    define i32 @add(i32 %a, i32 %b) {
    entry:
      %addtmp = add i32 %a, %b
      ret i32 %addtmp
    }

    define i32 @main() {
    entry:
      %0 = call i32 @mul(i32 5, i32 3)
      %1 = call i32 @std_c_printf(ptr @str, i32 %0)
      %2 = call i32 @add(i32 2, i32 3)
      %3 = call i32 @std_c_printf(ptr @str.1, i32 %2)
      ret i32 0
      ret i32 0
    }

    declare i32 @std_c_printf(ptr %0, i32 %1)
    ; ModuleID = 'Nova'
    source_filename = "Nova"

    @str = private unnamed_addr constant [17 x i8] c"Hello world! %d\0A\00", align 1

    define i32 @main() {
    entry:
      %0 = call i32 @std_c_printf(ptr @str, i32 32)
      ret i32 0
      ret i32 0
    }

    declare i32 @std_c_printf(ptr %0, i32 %1)
    ; ModuleID = 'Nova'
    source_filename = "Nova"

    @str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
    @str.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

    define i32 @Math_sub(i32 %a, i32 %b) {
    entry:
      %subtmp = sub i32 %a, %b
      ret i32 %subtmp
    }

    define i32 @Math_add(i32 %a, i32 %b) {
    entry:
      %addtmp = add i32 %a, %b
      ret i32 %addtmp
    }

    define i32 @main() {
    entry:
      %0 = call i32 @Math_add(i32 10, i32 5)
      %1 = call i32 @Math_sub(i32 10, i32 5)
      %2 = call i32 @std_c_printf(ptr @str, i32 %0)
      %3 = call i32 @std_c_printf(ptr @str.1, i32 %1)
      %addtmp = add i32 %0, %1
      ret i32 %addtmp
      ret i32 0
    }

    declare i32 @std_c_printf(ptr %0, i32 %1)
    ; ModuleID = 'Nova'
    source_filename = "Nova"

    @str = private unnamed_addr constant [12 x i8] c"Result: %d\0A\00", align 1

    define i32 @my_func() {
    entry:
      ret i32 123
    }

    define i32 @main() {
    entry:
      %0 = call i32 @my_func()
      %1 = call i32 @std_c_printf(ptr @str, i32 %0)
      ret i32 0
      ret i32 0
    }

    declare i32 @std_c_printf(ptr %0, i32 %1)
    ; ModuleID = 'Nova'
    source_filename = "Nova"

    @str = private unnamed_addr constant [17 x i8] c"Square of 5: %d\0A\00", align 1

    define i32 @main() {
    entry:
      %0 = call i32 @std_math_square(i32 5)
      %1 = call i32 @std_c_printf(ptr @str, i32 %0)
      ret i32 %0
      ret i32 0
    }

    declare i32 @std_math_square(i32 %0)

    declare i32 @std_c_printf(ptr %0, i32 %1)
    ; ModuleID = 'Nova'
    source_filename = "Nova"

    @str = private unnamed_addr constant [28 x i8] c"Tagged function result: %d\0A\00", align 1

    define i32 @my_func() {
    entry:
      ret i32 42
    }

    define i32 @main() {
    entry:
      %0 = call i32 @my_func()
      %1 = call i32 @std_c_printf(ptr @str, i32 %0)
      ret i32 0
      ret i32 0
    }

    declare i32 @std_c_printf(ptr %0, i32 %1)
    File: basic_functions
    File: complex_types
    File: currying_functions
    File: ffi_example
    File: module_test
    File: open_statements
    File: stdlib_test
    File: tags_and_macros
    |}]
