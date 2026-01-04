; ModuleID = 'examples/showcase.nova'
source_filename = "examples/showcase.nova"

@str = private unnamed_addr constant [12 x i8] c"Salary: %d\0A\00", align 1

define void @main() {
entry:
  %call = call i32 (ptr, ...) @greet()
  %call1 = call i32 (ptr, ...) @printf(ptr @str, i32 0)
  ret void
}

declare i32 @greet(ptr, ...)

declare i32 @printf(ptr, ...)
