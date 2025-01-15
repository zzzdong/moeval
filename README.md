moeval
======

A toy interpreter.

# example

```rust
use moeval::{Interpreter, Evaluator};

fn main() {
    let mut env = Environment::new();

    let script = r#"
    fn fib(n) {
        if n <= 0 {
            return 0;
        }
        if n <= 2 {
            return 1;
        }

        return fib(n-1) - fib(n-2);
    }

    let sum = 0;
    for i in 0..=10 {
        sum += fib(i);
    }

    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap();

    println!("ret: {:?}", ret); 
    // should output: 
    // ret: Some(143)
}


```

# syntax

## primitive type

### undefine

### boolean

`true`, `false`

### integer

$-2^{63}$ ~ $2^{63}$

### float

float number, f64.

### string

string quote by `"`

## control flow

### for

`for` to iterate.

```rust
for i in 0..=10 {}
```

### if 

`if` to justment.

```rust
if condition {} else {}
```

### break

`break` to exist loop.

### continue

`continue` to finish one iterate.
