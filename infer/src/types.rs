use itertools::Itertools;
use std::collections::HashMap;
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

use crate::{ast::Identifier, infer_error, InferResult};

pub trait TypeScheme {
    fn instantiate(self, lvl: usize) -> Type;
}

pub trait SimpleType: TypeScheme {
    fn level(&self) -> usize;
}

/// A single identifier.
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct Var {
    pub lower_bounds: Vec<Type>,
    pub upper_bounds: Vec<Type>,
    pub level: usize,
    pub id: usize,
}

impl Var {
    pub fn new(level: usize, id: usize) -> Self {
        Self {
            level,
            id,
            ..Default::default()
        }
    }
}

impl From<&Identifier> for Var {
    fn from(ident: &Identifier) -> Self {
        Self {
            level: ident.level,
            ..Default::default()
        }
    }
}

pub type PolarVar = (Var, bool);

/// A nix language type.
#[derive(Debug, Clone, PartialEq, Eq, Display, EnumDiscriminants, AsRefStr)]
#[strum_discriminants(derive(AsRefStr, Display))]
#[strum_discriminants(name(TypeName))]
pub enum Type {
    Top,
    Bottom,

    Number,
    Bool,
    String,
    Path,
    Null,
    Undefined,

    Var(Var),
    Function(Box<Type>, Box<Type>),
    List(Vec<Type>),
    Record(HashMap<String, Type>),
    Optional(Box<Type>),

    // Complexe types only created by simplification
    Union(Box<Type>, Box<Type>),
    Inter(Box<Type>, Box<Type>),
}

impl Type {
    pub fn get_var(&self) -> InferResult<&Var> {
        match self {
            Type::Var(ident) => Ok(ident),
            t => infer_error(TypeName::Var, t.get_name()),
        }
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // TODO: proper implementation
        core::mem::discriminant(self).hash(state);
    }
}

struct PolymorphicType {
    body: Type,
    level: usize,
}

impl TypeScheme for Type {
    fn instantiate(self, lvl: usize) -> Type {
        self
    }
}

impl SimpleType for Type {
    fn level(&self) -> usize {
        match self {
            Type::Var(ident) => ident.level,
            Type::Optional(ty) => ty.level(),
            Type::Function(lhs, rhs) => lhs.level().max(rhs.level()),
            Type::List(ls) => ls.iter().map(|t| t.level()).max().unwrap_or(0),
            Type::Record(rc) => rc.values().map(|t| t.level()).max().unwrap_or(0),
            Type::Bool
            | Type::Number
            | Type::String
            | Type::Path
            | Type::Null
            | Type::Undefined => 0,
            Type::Top | Type::Bottom | Type::Union(..) | Type::Inter(..) => {
                panic!("Not a simple type")
            }
        }
    }
}

impl Type {
    /// Try to convert this type to a list.
    pub fn into_list(self) -> InferResult<Vec<Type>> {
        match self {
            Type::List(elems) => Ok(elems),
            t => infer_error(TypeName::List, t.get_name()),
        }
    }

    /// Try to convert this type to a function.
    pub fn into_function(self) -> InferResult<(Type, Type)> {
        match self {
            Type::Function(box lhs, box rhs) => Ok((lhs, rhs)),
            t => infer_error(TypeName::Function, t.get_name()),
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Type::Var(_))
    }

    /// Returns the enum descriminant of this type.
    pub fn get_name(&self) -> TypeName {
        match self {
            Type::Var(_) => TypeName::Var,
            Type::List(_) => TypeName::List,
            Type::Function(_, _) => TypeName::Function,
            Type::Union(_, _) => TypeName::Union,
            Type::Record(_) => TypeName::Record,
            Type::Top => TypeName::Top,
            Type::Bottom => TypeName::Bottom,
            Type::Inter(_, _) => TypeName::Inter,
            Type::Optional(_) => TypeName::Optional,
            Type::Number => TypeName::Number,
            Type::Bool => TypeName::Bool,
            Type::String => TypeName::String,
            Type::Path => TypeName::Path,
            Type::Null => TypeName::Null,
            Type::Undefined => TypeName::Undefined,
        }
    }

    pub fn show(&self) -> String {
        match self {
            Type::Var(ident) => format!("Var({})", ident.id),
            t @ Type::Number
            | t @ Type::Bool
            | t @ Type::String
            | t @ Type::Path
            | t @ Type::Null
            | t @ Type::Undefined => t.as_ref().to_string(),
            Type::List(elems) => format!("[ {} ]", elems.iter().map(|t| t.show()).join(" ")),
            Type::Function(lhs, rhs) => format!("{} -> {}", lhs.show(), rhs.show()),
            Type::Union(lhs, rhs) => format!("{} ∨ {}", lhs, rhs),
            Type::Record(fields) => format!("{:?}", fields),
            Type::Top => TypeName::Top.to_string(),
            Type::Bottom => TypeName::Bottom.to_string(),
            Type::Inter(lhs, rhs) => format!("{} ∧ {}", lhs.show(), rhs.show()),
            Type::Optional(t) => format!("Optional<{}>", t.show()),
        }
    }
}
