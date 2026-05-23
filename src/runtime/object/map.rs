use std::collections::HashMap;

use crate::{Object, RuntimeError, Value, ValueRef};

/// Helper to extract a String key from a Value (handles both inline Str and Object String)
fn try_get_string_key(value: &Value) -> Option<String> {
    match value {
        Value::Str(s) => Some(s.to_string()),
        Value::Object(obj) => {
            let any_ref: &dyn std::any::Any = &**obj;
            if any_ref.type_id() == std::any::TypeId::of::<String>() {
                any_ref.downcast_ref::<String>().map(|s| s.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Specialized Object impl for `HashMap<String, ValueRef>` used by map literals.
/// `String` is stored inline as `Value::Str`, so we use `try_get_string_key()` for key extraction.
impl Object for HashMap<String, ValueRef> {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        if let Some(key) = try_get_string_key(index) {
            if let Some(value) = self.get(&key) {
                return Ok(value.clone());
            }
            return Err(RuntimeError::key_not_found(index));
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexGet,
            format!("cannot index hashmap with {index:?}"),
        ))
    }

    fn index_set(&mut self, index: &Value, value: ValueRef) -> Result<(), RuntimeError> {
        if let Some(key) = try_get_string_key(index) {
            self.insert(key, value);
            return Ok(());
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexSet,
            format!("cannot index hashmap with {index:?}"),
        ))
    }

    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        Ok(Box::new(self.clone().into_iter().map(|(k, v)| {
            ValueRef::new((ValueRef::from(Value::Str(std::rc::Rc::new(k))), v))
        })))
    }

    fn call_method(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        match method {
            "len" => Ok(Some(ValueRef::new(self.len() as i64))),
            "clear" => {
                self.clear();
                Ok(None)
            }
            "insert" => {
                if args.len() == 2 {
                    match try_get_string_key(&args[0].value()) {
                        Some(key) => {
                            self.insert(key, args[1].clone());
                            return Ok(None);
                        }
                        None => {
                            return Err(RuntimeError::invalid_type::<String>(
                                "insert() argument 1 must be a key".to_string(),
                            ));
                        }
                    };
                }
                Err(RuntimeError::invalid_argument_count(1, args.len()))
            }
            "remove" => {
                if args.len() == 1 {
                    match try_get_string_key(&args[0].value()) {
                        Some(key) => {
                            self.remove(&key);
                            return Ok(None);
                        }
                        None => {
                            return Err(RuntimeError::invalid_type::<String>(
                                "insert() argument 1 must be a key".to_string(),
                            ));
                        }
                    };
                }

                Err(RuntimeError::invalid_argument_count(1, args.len()))
            }
            "keys" => {
                if args.is_empty() {
                    let keys = self.keys().cloned().collect::<Vec<_>>();
                    return Ok(Some(ValueRef::new(keys)));
                }

                Err(RuntimeError::invalid_argument_count(0, args.len()))
            }
            "values" => {
                if args.is_empty() {
                    let values = self.values().cloned().collect::<Vec<_>>();
                    return Ok(Some(ValueRef::new(values)));
                }

                Err(RuntimeError::invalid_argument_count(0, args.len()))
            }
            _ => Ok(None),
        }
    }
}