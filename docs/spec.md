```rust
// Functions
const add : fn(a: i32, b: i32) -> i32 = { a + b };
const sub : fn(a: i32, b: i32) -> i32 = { return a - b; };

// Structs
[impl :: std.fmt.Stringify]
const Person : struct = {
    name: str,
    age:  u8,

    const new : fn(name: str, age: u8) -> Self = {.{
        .name = name,
        .age  = age,
    }};

    const greet : fn(&self, greeting: str) -> () = {
        println!("{}, {}!", greeting, self.name);
    }; 
};

[impl :: std.fmt.Stringify]
[impl :: EmployeeContract]
const Employee : struct = {
    embed Person, // struct embedding

    const login : fn(&self, login: str, passwd: str) -> bool = {
        return match self.auth {
            .Guest => true,
            else => |auth| auth.login == login && auth.passwd == passwd
        };
    };
};

// Enums
[impl :: std.fmt.Stringify]
const Auth : enum = {
    Admin, Manager, Employee, Guest
    => { 
        priv login:  str,
        priv passwd: str,
    }
};

// Contract
const EmployeeContract : contract = {
    nda: bool,
    auth: Auth,

    const login : fn(&self, login: str, passwd: str) -> bool;
};
```
```
