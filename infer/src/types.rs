use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap};
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

use crate::{infer_error, Context, InferResult};

/// A single identifier.
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Var<'a> {
    pub lower_bounds: RefCell<Vec<Type<'a>>>,
    pub upper_bounds: RefCell<Vec<Type<'a>>>,
    pub level: usize,
    pub id: usize,
}

impl<'a> std::hash::Hash for Var<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<'a> Var<'a> {
    pub fn new(level: usize, id: usize) -> Self {
        Self {
            level,
            id,
            ..Default::default()
        }
    }
}

pub type PolarVar<'a> = (&'a Var<'a>, bool);

/// A nix language type.
#[derive(Debug, Clone, PartialEq, Eq, Display, EnumDiscriminants, AsRefStr)]
#[strum_discriminants(derive(AsRefStr, Display))]
#[strum_discriminants(name(TypeName))]
pub enum Type<'a> {
    Top,
    Bottom,

    Number,
    Bool,
    String,
    Path,
    Null,
    Undefined,

    Var(&'a Var<'a>),
    Function(Box<Type<'a>>, Box<Type<'a>>),
    List(Vec<Type<'a>>),
    Record(HashMap<String, Type<'a>>),
    Optional(Box<Type<'a>>),
    Pattern(HashMap<String, Type<'a>>, bool),

    // Complexe Types only created by simplification
    Union(Box<Type<'a>>, Box<Type<'a>>),
    Inter(Box<Type<'a>>, Box<Type<'a>>),
    Recursive(&'a Var<'a>, Box<Type<'a>>),
}

impl<'a> Type<'a> {
    pub fn get_var(&'a self) -> InferResult<&Var> {
        match self {
            Type::Var(ident) => Ok(ident),
            t => infer_error(TypeName::Var, t.get_name()),
        }
    }
}

impl<'a> std::hash::Hash for Type<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // TODO: proper implementation
        core::mem::discriminant(self).hash(state);
    }
}

pub struct PolymorphicType<'a> {
    pub level: usize,
    pub body: Type<'a>,
}

impl<'a> PolymorphicType<'a> {
    pub fn new(body: Type<'a>, level: usize) -> Self {
        Self { body, level }
    }
}

impl<'a> Type<'a> {
    pub fn instantiate(&self) -> Type<'a> {
        self.clone()
    }

    pub fn level(&self) -> usize {
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
            | Type::Pattern(..)
            | Type::Undefined => 0,
            Type::Top | Type::Bottom | Type::Union(..) | Type::Inter(..) | Type::Recursive(..) => {
                panic!("Not a simple type")
            }
        }
    }

    /// Try to convert this type to a list.
    pub fn into_list(self) -> InferResult<Vec<Type<'a>>> {
        match self {
            Type::List(elems) => Ok(elems),
            t => infer_error(TypeName::List, t.get_name()),
        }
    }

    pub fn into_record(self) -> InferResult<HashMap<String, Type<'a>>> {
        match self {
            Type::Record(fields) => Ok(fields),
            t => infer_error(TypeName::Record, t.get_name()),
        }
    }

    /// Try to convert this type to a function.
    pub fn into_function(self) -> InferResult<(Type<'a>, Type<'a>)> {
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
            Type::Recursive(_, _) => TypeName::Recursive,
            Type::Pattern(_, _) => TypeName::Pattern,
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
            Type::Recursive(ident, ty) => format!("Rec({}) {}", ident.id, ty.show()),
            Type::Pattern(fields, _) => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v.show()))
                    .join(", ")
            ),
        }
    }
}
