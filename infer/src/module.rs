use std::{
    collections::{HashMap, HashSet},
    ops,
    sync::Arc,
};

use la_arena::{Arena, ArenaMap, Idx};
use ordered_float::OrderedFloat;
use parser2::ast::{BinOp, UnOp};
use smol_str::SmolStr;

use crate::{Diagnostic, Span};
pub type ExprId = Idx<ExprData>;
pub type NameId = Idx<Name>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    exprs: Arena<ExprData>,
    names: Arena<Name>,
    entry_expr: ExprId,
}
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct NameReference {
    // Assume almost all defs are referenced somewhere.
    def_refs: ArenaMap<NameId, Vec<ExprId>>,
    // But there are just some "with"s.
    with_refs: HashMap<ExprId, Vec<ExprId>>,
}

pub struct ModuleSourceMap {
    expr_map: HashMap<AstPtr, ExprId>,
    expr_map_rev: HashMap<ExprId, AstPtr>,
    name_map: HashMap<AstPtr, NameId>,
    name_map_rev: ArenaMap<NameId, Vec<AstPtr>>,

    // This contains locations, thus is quite volatile.
    diagnostics: Vec<Diagnostic>,
}

impl ops::Index<ExprId> for Module {
    type Output = ExprData;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index]
    }
}

impl ops::Index<NameId> for Module {
    type Output = Name;
    fn index(&self, index: NameId) -> &Self::Output {
        &self.names[index]
    }
}

impl Module {
    pub fn shrink_to_fit(&mut self) {
        self.exprs.shrink_to_fit();
        self.names.shrink_to_fit();
    }

    pub fn entry_expr(&self) -> ExprId {
        self.entry_expr
    }

    pub fn exprs(&self) -> impl ExactSizeIterator<Item = (ExprId, &'_ ExprData)> + '_ {
        self.exprs.iter()
    }

    pub fn names(&self) -> impl ExactSizeIterator<Item = (NameId, &'_ Name)> + '_ {
        self.names.iter()
    }

    pub(crate) fn module_references_query(
        db: &dyn DefDatabase,
        file_id: FileId,
    ) -> Arc<HashSet<FileId>> {
        let source_root = db.source_root(db.file_source_root(file_id));
        let mut refs = db
            .module(file_id)
            .exprs()
            .filter_map(|(_, kind)| {
                let &ExprData::Literal(Literal::Path(path)) = kind else {
                    return None;
                };
                let mut vpath = path.resolve(db)?;
                source_root.file_for_path(&vpath).or_else(|| {
                    vpath.push(DEFAULT_IMPORT_FILE)?;
                    source_root.file_for_path(&vpath)
                })
            })
            .collect::<HashSet<_>>();
        refs.shrink_to_fit();
        Arc::new(refs)
    }
}

pub type AstPtr = parser2::SyntaxNodePtr;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleSourceMap {
    expr_map: HashMap<AstPtr, ExprId>,
    expr_map_rev: HashMap<ExprId, AstPtr>,
    name_map: HashMap<AstPtr, NameId>,
    name_map_rev: ArenaMap<NameId, Vec<AstPtr>>,

    // This contains locations, thus is quite volatile.
    diagnostics: Vec<Diagnostic>,
}

impl ModuleSourceMap {
    pub fn shrink_to_fit(&mut self) {
        self.expr_map.shrink_to_fit();
        self.expr_map_rev.shrink_to_fit();
        self.name_map.shrink_to_fit();
        self.name_map_rev.shrink_to_fit();
        self.diagnostics.shrink_to_fit();
    }

    pub fn expr_for_node(&self, node: AstPtr) -> Option<ExprId> {
        self.expr_map.get(&node).copied()
    }

    pub fn node_for_expr(&self, expr_id: ExprId) -> Option<AstPtr> {
        self.expr_map_rev.get(&expr_id).cloned()
    }

    pub fn name_for_node(&self, node: AstPtr) -> Option<NameId> {
        self.name_map.get(&node).copied()
    }

    pub fn nodes_for_name(&self, name_id: NameId) -> impl Iterator<Item = AstPtr> + '_ {
        self.name_map_rev
            .get(name_id)
            .into_iter()
            .flatten()
            .cloned()
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

#[salsa::tracked]
pub struct Expr<'db> {
    span: Span<'db>,
    data: ExprData,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprData {
    Apply(ExprId, ExprId), // Binop
    Assert(ExprId, ExprId),
    AttrSet(Bindings),
    // Binary(Option<BinOp>, ExprId, ExprId),
    Conditional(ExprId, ExprId, ExprId),
    CurPos,                    // Added
    HasAttr(ExprId, Attrpath), // Binop
    Lambda(Option<NameId>, Option<Pat>, ExprId),
    LetAttrset(Bindings), // Added
    LetIn(Bindings, ExprId),
    List(Box<[ExprId]>),
    Literal(Literal),
    Missing,
    PathInterpolation(Box<[ExprId]>), // Added
    RecAttrset(Bindings),             // Why two?
    Reference(SmolStr),               // Literal?
    Select(ExprId, Attrpath, Option<ExprId>),
    StringInterpolation(Box<[ExprId]>),
    With(ExprId, ExprId),
    // Unary(Option<UnOp>, ExprId),
}

impl ExprData {
    pub fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        match self {
            Self::Missing | Self::Reference(_) | Self::Literal(_) | Self::CurPos => {}
            Self::Lambda(_, pat, body) => {
                if let Some(p) = pat {
                    p.fields
                        .iter()
                        .filter_map(|&(_, default_expr)| default_expr)
                        .for_each(&mut f);
                }
                f(*body);
            }
            Self::Unary(_, a) => f(*a),
            Self::Assert(a, b) | Self::With(a, b) | Self::Binary(_, a, b) | Self::Apply(a, b) => {
                f(*a);
                f(*b);
            }
            Self::Conditional(a, b, c) => {
                f(*a);
                f(*b);
                f(*c);
            }
            Self::HasAttr(set, path) => {
                f(*set);
                path.iter().copied().for_each(f);
            }
            Self::Select(set, path, default_expr) => {
                f(*set);
                path.iter().copied().for_each(&mut f);
                if let &Some(e) = default_expr {
                    f(e);
                }
            }
            Self::StringInterpolation(xs) | Self::List(xs) | Self::PathInterpolation(xs) => {
                xs.iter().copied().for_each(f);
            }
            Self::LetIn(bindings, body) => {
                bindings.walk_child_exprs(&mut f);
                f(*body);
            }
            Self::AttrSet(bindings) | Self::RecAttrset(bindings) | Self::LetAttrset(bindings) => {
                bindings.walk_child_exprs(f);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub text: SmolStr,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
    String(SmolStr),
    Path(Path),
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pat {
    pub fields: Box<[(Option<NameId>, Option<ExprId>)]>,
    pub ellipsis: bool,
}

pub type Attrpath = Box<[ExprId]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bindings {
    pub attrs: Box<[(NameId, BindingValue)]>,
    pub inherit: Box<[ExprId]>,
    pub dynamics: Box<[(ExprId, ExprId)]>,
    pub is_recursive: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingValue {
    Expr(ExprId),
    Inherit(ExprId),
    InheritFrom(usize),
}

impl Bindings {
    pub fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        for (_, value) in self.attrs.iter() {
            match value {
                BindingValue::Inherit(e) | BindingValue::Expr(e) => f(*e),
                BindingValue::InheritFrom(_idx) => {}
            }
        }
        for &e in self.inherit.iter() {
            f(e);
        }
        for &(k, v) in self.dynamics.iter() {
            f(k);
            f(v);
        }
    }

    // FIXME: This is currently O(n).
    pub fn get(&self, name: &str, module: &Module) -> Option<BindingValue> {
        self.attrs
            .iter()
            .find_map(|&(name_id, value)| (module[name_id].text == name).then_some(value))
    }
}
