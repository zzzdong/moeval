use moeval::{Environment, Evaluator, ValueRef};

#[tokio::main]
async fn main() {
    let mut env = Environment::new();
    let mut eval = Evaluator::new();

    env.define_function("println", println);

    let script = r#"
    let sum = 0;
    for i in 0..=10 {
        sum += i;
    }
    println("hello, world");

    return sum;
    "#;

    let retval = eval.eval(script, &env);

    println!("ret: {:?}", retval);
}

fn println(args: &[ValueRef]) {
    let s = args
        .iter()
        .map(|v| format!("{v}"))
        .collect::<Vec<String>>()
        .join("");

    println!("{}", s);
}