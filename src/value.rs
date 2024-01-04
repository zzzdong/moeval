


#[derive(Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Byte(u8),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
}

