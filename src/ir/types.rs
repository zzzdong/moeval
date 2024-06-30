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

macro_rules! id_entity {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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

impl Default for Name {
    fn default() -> Self {
        Self::anonymous()
    }
}

#[derive(Debug)]
pub struct FunctionParam {
    pub name: Name,
}

#[derive(Debug)]
pub struct Function {
    pub name: Name,
    pub params: Vec<FunctionParam>,
}

impl Function {
    pub fn new(name: impl Into<Name>, params: Vec<FunctionParam>) -> Self {
        Self {
            name: name.into(),
            params,
        }
    }
}

