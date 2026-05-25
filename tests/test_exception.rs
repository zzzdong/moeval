mod utils;
use evalit::{Environment, Interpreter, RuntimeError};
use utils::init_logger;

#[test]
fn test_nested_try_catch() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    try {
        try {
            throw "inner";
        } catch (_) {
            return 10;
        }
        return -1;
    } catch (_) {
        return -2;
    }
    return 0;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 10);
}

#[test]
fn test_try_no_throw() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    try {
        return 42;
    } catch (_) {
        return -1;
    }
    return 0;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 42);
}

#[test]
fn test_try_catch_basic() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    try {
        throw "error happened";
    } catch (_) {
        return 99;
    }
    return -1;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 99);
}

#[test]
fn test_try_catch_normal_path() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    let x = 1;
    try {
        let y = x + 1;
        return y;
    } catch (_) {
        return -1;
    }
    return 0;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 2);
}

#[test]
fn test_try_catch_with_local_vars() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    let result = "default";
    try {
        result = "try_body";
        throw "err";
    } catch (_) {
        result = "caught";
    }
    return result;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, "caught");
}

#[test]
fn test_throw_unhandled() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    throw "unhandled error";
    return 0;
    "#;
    let result = Interpreter::eval(script, env);
    assert!(result.is_err());
    match result {
        Err(evalit::Error::Runtime(RuntimeError::UnhandledException)) => {}
        _ => panic!("Expected UnhandledException, got {:?}", result),
    }
}

#[test]
fn test_try_catch_cross_function_unwind() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    fn inner() {
        throw "error from inner";
    }

    fn middle() {
        inner();
        return -1;
    }

    fn outer() {
        try {
            let x = middle();
            return x;
        } catch (_) {
            return 99;
        }
        return -2;
    }

    return outer();
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 99);
}

#[test]
fn test_try_catch_deep_unwind() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    fn level3() {
        throw "deep";
    }

    fn level2() {
        level3();
        return -1;
    }

    fn level1() {
        try {
            let x = level2();
            return x;
        } catch (_) {
            return 42;
        }
        return -2;
    }

    return level1();
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 42);
}

#[test]
fn test_try_catch_unwind_no_handler_in_middle() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    fn inner() {
        throw "err";
    }

    fn middle() {
        let x = inner();
        return x;
    }

    try {
        let y = middle();
        return y;
    } catch (_) {
        return 88;
    }
    return 0;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 88);
}

#[test]
fn test_try_catch_in_function() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    fn test_fn() {
        try {
            throw "err";
        } catch (_) {
            return 42;
        }
        return -1;
    }
    return test_fn();
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 42);
}
