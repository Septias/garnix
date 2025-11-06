//! Syntax definitions of nix.
use la_arena::Arena;
use ordered_float::OrderedFloat;
use std::{collections::HashMap, ops};

pub type AstPtr = parser2::SyntaxNodePtr;

#[derive(Debug)]
pub struct Module<'db> {
    exprs: Arena<Expr<'db>>,
    names: Arena<Name<'db>>,
    entry_expr: Expr<'db>,
}

// impl<'db> ops::Index<Expr<'db>> for Module<'db> {
//     type Output = Expr<'db>;
//     fn index(&self, index: Expr<'db>) -> &Self::Output {
//         &self.exprs(self.db())[index]
//     }
// }

// impl<'db> ops::Index<Name<'db>> for Module<'db> {
//     type Output = Name<'db>;
//     fn index(&self, index: Name<'db>) -> &Self::Output {
//         &self.names(self.db())[index]
//     }
// }

/// Resolution of names.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct NameReference<'db> {
    // Assume almost all defs are referenced somewhere.
    def_refs: HashMap<Name<'db>, Vec<Expr<'db>>>,
    // But there are just some "with"s.
    with_refs: HashMap<Expr<'db>, Vec<Expr<'db>>>,
}

/// Module Source Map
/// This map bridges between the lowered [Expr] and syntaxtree ([AstPtr]).
pub struct ModuleSourceMap<'db> {
    expr_map: HashMap<bool, Expr<'db>>,
    expr_map_rev: HashMap<Expr<'db>, AstPtr>,
    name_map: HashMap<AstPtr, Name<'db>>,
    name_map_rev: HashMap<Name<'db>, Vec<AstPtr>>,
}

// impl<'db> ModuleSourceMap<'db> {
//     pub fn expr_for_node(&self, node: AstPtr, db: &'db dyn salsa::Database) -> Option<Expr<'db>> {
//         self.expr_map(db).get(&node)
//     }

//     pub fn node_for_expr(
//         &self,
//         expr_id: Expr<'db>,
//         db: &'db dyn salsa::Database,
//     ) -> Option<AstPtr> {
//         self.expr_map_rev(db).get(&expr_id).cloned()
//     }

//     pub fn name_for_node(&self, node: AstPtr, db: &'db dyn salsa::Database) -> Option<Name<'db>> {
//         self.name_map(db).get(&node).copied()
//     }

//     pub fn nodes_for_name(
//         &self,
//         name_id: Name<'db>,
//         db: &'db dyn salsa::Database,
//     ) -> impl Iterator<Item = AstPtr> + '_ {
//         self.name_map_rev(db)
//             .get(name_id)
//             .into_iter()
//             .flatten()
//             .cloned()
//     }
// }

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct Apply<'db> {
    pub(crate) fun: Expr<'db>,
    pub(crate) arg: Expr<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct Assert<'db> {
    condition: Expr<'db>,
    body: Expr<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct AttrSet<'db> {
    bindings: Bindings<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct Binary<'db> {
    op: Option<BinOp>,
    left: Expr<'db>,
    right: Expr<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct Conditional<'db> {
    condition: Expr<'db>,
    then_branch: Expr<'db>,
    else_branch: Expr<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct HasAttr<'db> {
    expr: Expr<'db>,
    attrpath: Attrpath<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct Lambda<'db> {
    name: Option<Name<'db>>,
    pattern: Option<Pattern<'db>>,
    body: Expr<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct LetAttrset<'db> {
    bindings: Bindings<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct LetIn<'db> {
    bindings: Bindings<'db>,
    body: Expr<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct List<'db> {
    items: Box<[Expr<'db>]>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct PathInterpolation<'db> {
    parts: Box<[Expr<'db>]>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct RecAttrset<'db> {
    bindings: Bindings<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct Select<'db> {
    expr: Expr<'db>,
    attrpath: Attrpath<'db>,
    default: Option<Expr<'db>>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct StringInterpolation<'db> {
    parts: Box<[Expr<'db>]>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct With<'db> {
    namespace: Expr<'db>,
    body: Expr<'db>,
}

#[salsa::tracked]
#[derive(Debug)]
pub(crate) struct Unary<'db> {
    op: Option<UnOp>,
    operand: Expr<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update, salsa::Supertype)]
pub enum Expr<'db> {
    Apply(Apply<'db>),
    Assert(Assert<'db>),
    AttrSet(AttrSet<'db>),
    Binary(Binary<'db>),
    Conditional(Conditional<'db>),
    // // CurPos,
    HasAttr(HasAttr<'db>),
    Lambda(Lambda<'db>),
    LetAttrset(LetAttrset<'db>),
    LetIn(LetIn<'db>),
    List(List<'db>),
    // // Literal(Literal),
    // // Missing,
    PathInterpolation(PathInterpolation<'db>),
    RecAttrset(RecAttrset<'db>),
    // // Reference(SmolStr),
    Select(Select<'db>),
    StringInterpolation(StringInterpolation<'db>),
    With(With<'db>),
    Unary(Unary<'db>),
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Name<'db> {
    pub text: String,
    pub kind: NameKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameKind {
    LetIn,
    PlainAttrset,
    RecAttrset,
    Param,
    PatField,
}

impl NameKind {
    pub fn is_definition(self) -> bool {
        !matches!(self, Self::PlainAttrset)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
    String(String),
    // Path(Path),
    Null,
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Pattern<'db> {
    pub fields: Box<[(Option<Name<'db>>, Option<Expr<'db>>)]>,
    pub ellipsis: bool,
}

pub type Attrpath<'db> = Box<[Expr<'db>]>;

#[salsa::tracked]
#[derive(Debug)]
pub struct Bindings<'db> {
    pub attrs: Box<[(Name<'db>, BindingValue<'db>)]>,
    pub inherit: Box<[Expr<'db>]>,
    pub dynamics: Box<[(Expr<'db>, Expr<'db>)]>,
    pub is_recursive: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum BindingValue<'db> {
    Expr(Expr<'db>),
    Inherit(Expr<'db>),
    InheritFrom(usize),
}

// impl<'db> Bindings<'db> {
//     pub fn walk_child_exprs(&self, db: &'db dyn salsa::Database, mut f: impl FnMut(Expr<'db>)) {
//         for (_, value) in self.attrs.iter() {
//             match value {
//                 BindingValue::Inherit(e) | BindingValue::Expr(e) => f(*e),
//                 BindingValue::InheritFrom(_idx) => {}
//             }
//         }
//         for &e in self.inherit.iter() {
//             f(e);
//         }
//         for &(k, v) in self.dynamics.iter() {
//             f(k);
//             f(v);
//         }
//     }

//     // FIXME: This is currently O(n).
//     pub fn get(
//         &self,
//         name: &str,
//         module: &Module<'db>,
//         db: &'db dyn salsa::Database,
//     ) -> Option<BindingValue<'db>> {
//         self.attrs
//             .iter()
//             .find_map(|&(name_id, value)| (name_id.text(db) == name).then_some(value))
//     }
// }

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
    Imply,
    Or,
    And,

    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    Update,
    Concat,

    Add,
    Sub,
    Mul,
    Div,

    PipeLeft,
    PipeRight,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnOp {
    Not,
    Negate,
}
