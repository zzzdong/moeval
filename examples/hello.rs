use moeval::{Environment, Evaluator, Promise, Value, ValueRef};

#[tokio::main]
async fn main() {
    let mut env = Environment::new();

    env.define_function("println", println);
    env.define_function("http_get", http_get);

    let script = r#"
    println("hello, world");

    let sum = 0;
    for i in 0..=10 {
        sum += i;
    }
    
    let resp = http_get("https://bing.com").await;

    return resp;
    "#;

    let retval = Evaluator::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval.get());
}

fn println(args: &[ValueRef]) {
    let s = args
        .iter()
        .map(|v| format!("{v}"))
        .collect::<Vec<String>>()
        .join("");

    println!("{}", s);
}

fn http_get(url: String) -> Promise {
    Promise::new(Box::pin(async move {
        println!("url: {url:?}");

        let resp = reqwest::get(url).await.unwrap();
        let body = resp.text().await.unwrap();

        Value::new(body)
    }))
}
