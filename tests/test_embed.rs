mod utils;
use std::sync::Arc;

use evalit::{Environment, Module, Object, RuntimeError, VM, Value, ValueRef, compile};
use utils::init_logger;

fn run_vm(program: Arc<Module>, env: Environment) -> Result<Value, RuntimeError> {
    let mut vm = VM::new(program, env);
    let ret = vm.run().unwrap();

    Ok(ret.unwrap().take())
}

#[test]
fn test_basic_embedding() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    // 定义一个简单的函数
    fn add(a, b) {
        return a + b;
    }
    
    // 执行一些计算
    let result1 = add(2, 3);
    let result2 = add(result1, 5);
    
    // 返回最终结果
    return result2;
    "#;

    let program = compile(script, &env).unwrap();

    let retval = run_vm(program, env).unwrap();

    assert_eq!(retval, 10);
}

#[test]
fn test_rust_interop() {
    init_logger();

    // 定义一个Rust函数
    fn greet(name: String) -> String {
        format!("Hello, {}!", name)
    }

    let mut env = Environment::new();

    // 将Rust函数注册到脚本环境中
    env.define_function("greet", greet);

    let script = r#"
    // 使用Rust函数
    let message = greet("World");
    return true;
    
    // 操作返回值
    if message != "Hello, World!" {
        return false;
    }
    
    // 测试组合使用
    let composed = greet("Script") + " Welcome to Evalit";
    if composed != "Hello, Script! Welcome to Evalit" {
        return false;
    }
    
    return true;
    "#;

    let program = compile(script, &env).unwrap();
    let retval = run_vm(program, env).unwrap();

    assert_eq!(retval, true);
}

#[test]
fn test_multiple_vm_instances() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    fn add(a, b) {
        return a + b;
    }
    
    return add(1, 1);
    "#;

    let program = compile(script, &env).unwrap();

    let retval1 = run_vm(program.clone(), env.clone()).unwrap();

    let retval2 = run_vm(program, env).unwrap();

    assert_eq!(retval1, 2);
    assert_eq!(retval2, 2);
}

#[test]
fn test_shared_compiled_program() {
    init_logger();

    let env = Environment::new();

    // 创建一个可共享的程序
    let script = r#"
    fn add(a, b) {
        return a + b;
    }
    
    // 测试基础调用
    return add(2, 3);
    "#;

    // 编译一次
    let program = compile(script, &env).unwrap();

    // 在多个线程中复用编译后的程序
    use std::thread;

    for _ in 0..5 {
        let program = program.clone();
        thread::spawn(move || {
            // 每个线程创建一个新的VM来运行相同的程序
            let env = Environment::new();
            let retval = run_vm(program, env).unwrap();
            assert_eq!(retval, 5);
        });
    }
}

#[test]
fn test_rust_object_interop() {
    init_logger();

    // 定义一个简单的结构体
    #[derive(Debug)]
    struct MyStruct {
        value: i64,
    }

    impl Object for MyStruct {
        fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "MyStruct")
        }

        fn property_get(&self, property: &str) -> Result<ValueRef, evalit::RuntimeError> {
            match property {
                "value" => Ok(ValueRef::new(self.value)),
                _ => Err(RuntimeError::missing_property_getter::<Self>(property)),
            }
        }

        fn property_set(&mut self, property: &str, value: ValueRef) -> Result<(), RuntimeError> {
            match property {
                "value" => {
                    match value.value().downcast_ref::<i64>() {
                        Some(value) => self.value = *value,
                        _ => return Err(RuntimeError::invalid_argument::<i64>(0, &value)),
                    }
                    Ok(())
                }
                _ => Err(RuntimeError::missing_property_setter::<Self>(property)),
            }
        }

        fn call_method(
            &mut self,
            method: &str,
            args: &[ValueRef],
        ) -> Result<Option<ValueRef>, evalit::RuntimeError> {
            match method {
                "increase" => {
                    if args.len() != 1 {
                        return Err(evalit::RuntimeError::invalid_argument_count(1, 0));
                    }

                    match args[0].value().downcast_ref::<i64>() {
                        Some(arg) => {
                            self.value += *arg;
                            Ok(None)
                        }
                        None => Err(evalit::RuntimeError::invalid_argument::<i64>(0, &args[0])),
                    }
                }
                "value" => {
                    if !args.is_empty() {
                        return Err(evalit::RuntimeError::invalid_argument_count(0, args.len()));
                    }
                    Ok(Some(ValueRef::new(self.value)))
                }
                _ => Err(evalit::RuntimeError::missing_method::<Self>(method)),
            }
        }
    }

    let mut env = Environment::new();

    let my_struct = MyStruct { value: 42 };

    // 将结构体注册到环境中
    env.insert("myObj", my_struct);

    let script = r#"
        myObj.increase(10);
        if myObj.value != 52 {
            return false;
        }

        myObj.value = 100;


        return myObj.value() == 100;
    "#;

    let program = compile(script, &env).unwrap();
    let retval = run_vm(program, env.clone()).unwrap();

    assert_eq!(retval, true);

    // 从环境中获取变量并还原为原始类型
    let my_obj: MyStruct = env.remove_as("myObj").unwrap();

    assert_eq!(my_obj.value, 100);
}
