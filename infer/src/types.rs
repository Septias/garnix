//! Types that are used during inferences

use crate::{type_mismatch, InferResult};
use itertools::Itertools;
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

/// A forall-quantified variable.
/// This variable has lower and upper bounds on types.
/// TODO: should this be tracked?
#[salsa::tracked]
#[derive(Debug)]
pub struct Var<'db> {
    pub level: usize,
    pub id: usize,
    // pub lower_bounds: &'db [Ty],
    // pub upper_bounds: Arc<Mutex<Vec<Ty>>>,
}

impl<'db> Var<'db> {
    pub fn as_record(&self) -> Option<HashMap<String, Ty>> {
        let up = self.upper_bounds.borrow();
        if up.len() == 1 {
            match up.first().unwrap() {
                Ty::Record(rc) => Some(rc.clone()),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn as_list(&self) -> Option<Vec<Ty>> {
        let up = self.upper_bounds.borrow();
        if up.len() == 1 {
            match up.first().unwrap() {
                Ty::List(ls) => Some(ls.clone()),
                _ => None,
            }
        } else {
            None
        }
    }
}

/// A polar variable.
/// These variables can either be positive or negative, based on their position in functions (argument or return type).
pub type PolarVar<'a> = (TyVar, bool);
type TyVar = u32;

/// A nix language type.
#[derive(Debug, Clone, PartialEq, Eq, Display, EnumDiscriminants, AsRefStr, salsa::Update)]
#[strum_discriminants(derive(AsRefStr, Display))]
#[strum_discriminants(name(TypeName))]
pub enum Ty {
    Top,
    Bottom,

    Number,
    Bool,
    String,
    Path,
    Null,
    Undefined,

    Var(TyVar),
    Function(Box<Ty>, Box<Ty>),
    List(Vec<Ty>),
    Record(HashMap<String, Ty>),
    Optional(Box<Ty>),
    Pattern(HashMap<String, (Ty, Option<Ty>)>, bool),

    // Complex Types only created by simplification
    Union(Box<Ty>, Box<Ty>),
    Inter(Box<Ty>, Box<Ty>),
    Recursive(TyVar, Box<Ty>),
}

impl std::hash::Hash for Ty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl Ty {
    pub fn is_var(&self) -> bool {
        matches!(self, Ty::Var(_))
    }

    pub fn get_var(&self) -> InferResult<&Var> {
        match self {
            Ty::Var(ident) => Ok(ident),
            t => type_mismatch(TypeName::Var, t.get_name()),
        }
    }

    pub fn into_var(self) -> Option<Var> {
        match self {
            Ty::Var(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn instantiate(&self) -> Ty {
        self.clone()
    }

    pub fn level(&self) -> usize {
        match self {
            Ty::Var(ident) => ident.level,
            Ty::Optional(ty) => ty.level(),
            Ty::Function(lhs, rhs) => lhs.level().max(rhs.level()),
            Ty::List(ls) => ls.iter().map(|t| t.level()).max().unwrap_or(0),
            Ty::Record(rc) => rc.values().map(|t| t.level()).max().unwrap_or(0),
            Ty::Pattern(fields, _) => fields.values().map(|(t, _)| t.level()).max().unwrap_or(0),
            Ty::Union(left, right) => left.level().max(right.level()),
            Ty::Bool | Ty::Number | Ty::String | Ty::Path | Ty::Null | Ty::Undefined => 0,
            Ty::Top | Ty::Bottom | Ty::Inter(..) | Ty::Recursive(..) => {
                panic!("Not a simple type")
            }
        }
    }

    /// Returns the enum descriminant of this type.
    pub fn get_name(&self) -> TypeName {
        match self {
            Ty::Var(_) => TypeName::Var,
            Ty::List(_) => TypeName::List,
            Ty::Function(_, _) => TypeName::Function,
            Ty::Union(_, _) => TypeName::Union,
            Ty::Record(_) => TypeName::Record,
            Ty::Top => TypeName::Top,
            Ty::Bottom => TypeName::Bottom,
            Ty::Inter(_, _) => TypeName::Inter,
            Ty::Optional(_) => TypeName::Optional,
            Ty::Number => TypeName::Number,
            Ty::Bool => TypeName::Bool,
            Ty::String => TypeName::String,
            Ty::Path => TypeName::Path,
            Ty::Null => TypeName::Null,
            Ty::Undefined => TypeName::Undefined,
            Ty::Recursive(_, _) => TypeName::Recursive,
            Ty::Pattern(_, _) => TypeName::Pattern,
        }
    }

    pub fn show(&self) -> String {
        match self {
            Ty::Var(ident) => format!("Var({})", ident.id),
            t @ Ty::Number
            | t @ Ty::Bool
            | t @ Ty::String
            | t @ Ty::Path
            | t @ Ty::Null
            | t @ Ty::Undefined => t.as_ref().to_string(),
            Ty::List(elems) => format!("[ {} ]", elems.iter().map(|t| t.show()).join(" ")),
            Ty::Function(lhs, rhs) => format!("{} -> ({})", lhs.show(), rhs.show()),
            Ty::Union(lhs, rhs) => format!("{} ∨ {}", lhs, rhs),
            Ty::Record(fields) => fields
                .iter()
                .map(|(k, v)| format!("{{{}: {}}}", k, v.show()))
                .join(", "),
            Ty::Top => TypeName::Top.to_string(),
            Ty::Bottom => TypeName::Bottom.to_string(),
            Ty::Inter(lhs, rhs) => format!("{} ∧ {}", lhs.show(), rhs.show()),
            Ty::Optional(t) => format!("Optional<{}>", t.show()),
            Ty::Recursive(ident, ty) => format!("Rec({}) {}", ident.id, ty.show()),
            Ty::Pattern(fields, _) => format!(
                "Pat: {{ {} }}",
                fields
                    .iter()
                    .map(|(k, (v, _))| format!("{}: {}", k, v.show()))
                    .join(", ")
            ),
        }
    }
}

/// A polymorphic type.
/// These types are used for let-polymorphism.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct PolymorphicType {
    pub body: Ty,
    pub level: usize,
}

impl PolymorphicType {
    pub fn new(body: Ty, level: usize) -> Self {
        Self { body, level }
    }
}
