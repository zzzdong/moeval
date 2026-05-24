mod utils;
use evalit::{Environment, Error, Interpreter, RuntimeError};
use utils::init_logger;

#[test]
fn test_try_catch_basic() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    let result = 0;
    try {
        result = 1;
        throw 42;
        result = 2;
    } catch e {
        result = result + e;
    }
    return result;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    println!("ret: {:?}", retval);
    assert_eq!(retval, 43);
}

#[test]
fn test_try_catch_no_throw() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    let result = 0;
    try {
        result = 1;
    } catch e {
        result = 2;
    }
    return result;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    println!("ret: {:?}", retval);
    assert_eq!(retval, 1);
}

#[test]
fn test_try_catch_caught_value() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    try {
        throw "error occurred";
    } catch e {
        return e;
    }
    return "should not reach";
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    println!("ret: {:?}", retval);
    assert_eq!(retval, "error occurred");
}

#[test]
fn test_throw_no_catch() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    throw 99;
    return 0;
    "#;
    let result = Interpreter::eval(script, env);
    assert!(result.is_err());
    match result.unwrap_err() {
        Error::Runtime(RuntimeError::UnhandledException { .. }) => {}
        e => panic!("expected UnhandledException, got: {:?}", e),
    }
}

#[test]
fn test_try_catch_nested() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    let result = "";
    try {
        try {
            throw "inner";
        } catch inner_e {
            result = result + inner_e;
        }
        throw "outer";
    } catch outer_e {
        result = result + outer_e;
    }
    return result;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    println!("ret: {:?}", retval);
    assert_eq!(retval, "innerouter");
}

#[test]
fn test_try_catch_wildcard() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    try {
        throw 100;
    } catch _ {
        return 200;
    }
    return 0;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    println!("ret: {:?}", retval);
    assert_eq!(retval, 200);
}