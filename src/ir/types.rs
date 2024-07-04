use core::fmt;
use std::hash::Hash;

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub enum Primitive {
    Boolean(bool),
    Byte(u8),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    #[default]
    Undefined,
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Boolean(b) => write!(f, "{}", b),
            Primitive::Byte(b) => write!(f, "{}", b),
            Primitive::Integer(i) => write!(f, "{}", i),
            Primitive::Float(ff) => write!(f, "{}", ff),
            Primitive::Char(c) => write!(f, "{}", c),
            Primitive::String(s) => write!(f, "{}", s),
            Primitive::Undefined => write!(f, "undefined"),
        }
    }
}

impl Eq for Primitive {
    fn assert_receiver_is_total_eq(&self) {
        match self {
            Primitive::Boolean(b) => b.assert_receiver_is_total_eq(),
            Primitive::Byte(b) => b.assert_receiver_is_total_eq(),
            Primitive::Integer(i) => i.assert_receiver_is_total_eq(),
            Primitive::Float(f) => f.to_bits().assert_receiver_is_total_eq(),
            Primitive::Char(c) => c.assert_receiver_is_total_eq(),
            Primitive::String(s) => s.assert_receiver_is_total_eq(),
            Primitive::Undefined => (),
        }
    }
}

impl Hash for Primitive {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Primitive::Boolean(b) => b.hash(state),
            Primitive::Byte(b) => b.hash(state),
            Primitive::Integer(i) => i.hash(state),
            Primitive::Float(f) => f.to_bits().hash(state),
            Primitive::Char(c) => c.hash(state),
            Primitive::String(s) => s.hash(state),
            Primitive::Undefined => 1.hash(state),
        }
    }
}

macro_rules! id_entity {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(usize);

        impl $name {
            pub fn new(id: usize) -> Self {
                Self(id)
            }

            pub fn id(&self) -> usize {
                self.0
            }

            pub fn as_usize(&self) -> usize {
                self.0
            }
        }
    };
}

id_entity!(ConstantId);

id_entity!(InstId);

id_entity!(BlockId);

id_entity!(FunctionId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Name(Option<String>);

impl Name {
    pub fn new(name: impl ToString) -> Self {
        Self(Some(name.to_string()))
    }

    pub fn anonymous() -> Self {
        Self(None)
    }

    pub fn is_anonymous(&self) -> bool {
        self.0.is_none()
    }
}

impl From<Option<String>> for Name {
    fn from(value: Option<String>) -> Self {
        Self(value)
    }
}

impl From<String> for Name {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

impl Default for Name {
    fn default() -> Self {
        Self::anonymous()
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.as_deref().unwrap_or("<anonymous>"))
    }
}
