use moexpr::{self, Environment, Evaluator, Promise, RuntimeError, Value};

#[tokio::main]
async fn main() {
    let mut env = Environment::new();
    let mut eval = Evaluator::new();

    env.define_function("http_request", http_request);

    let script = r#"
    let resp = http_request("https://www.baidu.com").await;
    return resp;
    "#;

    let retval = eval.eval(script, &env).unwrap();

    println!("ret: {:?}", retval);
}

fn http_request(url: String) -> Result<Promise, RuntimeError> {
    Ok(Promise::new(Box::pin(async { Value::new(0) })))
}

fn http_request2(url: String) -> Result<Promise, RuntimeError> {
    Ok(Promise::new(Box::pin(async move {
        println!("request: {}", url);

        let resp = reqwest::get(url).await.unwrap();

        // println!("resp: {}", resp.text().await.unwrap());

        Value::new(resp.text().await.unwrap())
    })))
}
