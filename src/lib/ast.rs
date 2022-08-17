use std::ops::Range;

use serde::{Deserialize, Serialize};

/// A "located `X`" has a source location of type `Source`.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Loc<X>(pub X, pub Source);

impl<X: std::fmt::Debug> std::fmt::Debug for Loc<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}@{:?}>", self.0, self.1)
    }
}

pub type Node<X> = Loc<Box<X>>;

impl<X> Loc<X> {
    pub fn map<F: Fn(X) -> T, T>(self, map_fn: F) -> Loc<T> {
        Loc(map_fn(self.0), self.1)
    }
}

impl<X> Node<X> {
    pub fn map_box<F: Fn(X) -> T, T>(self, map_fn: F) -> Node<T> {
        Loc(Box::new(map_fn(*self.0)), self.1)
    }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Source {
    Known {
        span: Range<usize>,
        line: usize,
        col: usize,
    },
    Unknown,
}

impl Source {
    pub fn expand(&self, other: &Source) -> Source {
        match (self, other) {
            (Source::Unknown, Source::Unknown) => Source::Unknown,
            (
                Source::Known { span, line, col },
                Source::Known {
                    span: other_span, ..
                },
            ) => Source::Known {
                span: span.start..other_span.end,
                line: *line,
                col: *col,
            },
            (_, Source::Unknown) => self.clone(),
            (Source::Unknown, _) => other.clone(),
        }
    }
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Source::Known { line, col, .. } => write!(f, "{}:{}", line, col),
            Source::Unknown => write!(f, "(unknown source)"),
        }
    }
}

impl std::fmt::Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::default::Default for Source {
    fn default() -> Self {
        Source::Unknown
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Delim<X> {
    pub vec: Vec<X>,
    pub has_trailing: bool,
}

pub type Literal_ = Node<Literal>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Literal {
    Null,
    Bool(bool),
    Unit,
    Nat(String),
    // Int(String),
    Float(String),
    Char(String), // includes quotes
    Text(String), // includes quotes
    Blob(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ObjSort {
    Object,
    Actor,
    Module,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Shared<T /*: std::fmt::Debug + Clone + PartialEq + Eq*/> {
    Local,
    Shared(T),
}

pub type FuncSort = Shared<SharedSort>;

pub type TypId = Id;
pub type TypId_ = Node<TypId>;

pub type Decs = Delim<Dec_>;
pub type Cases = Delim<Case_>;

pub type Prog = Decs;

pub type Dec_ = Node<Dec>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Dec {
    Exp(Exp),
    Let(Pat_, Exp_),
    Var(Pat_, Exp_),
    Typ(TypId_, TypBinds, Type_),
    Class(
        SortPat_,
        TypId_,
        TypBinds,
        Pat_,
        Option<Type_>,
        ObjSort,
        Id_,
        DecFields,
    ),
}

pub type SortPat_ = Node<SortPat>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SortPat {
    Local,
    Shared(SharedSort, Pat_),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SharedSort {
    Query,
    Update,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BindSort {
    Scope,
    Type,
}

pub type TypeBind_ = Node<TypeBind>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeBind {
    pub var: Id_,
    pub sort: BindSort,
    pub bound: Type_,
}

/// Mutability setting, for arrays, record fields and lexically-scoped bindings.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Mut {
    Const,
    Var,
}

pub type TypBinds = Delim<TypeBind_>;
pub type DecFields = Delim<DecField_>;
pub type ExpFields = Delim<ExpField_>;
pub type PatFields = Delim<PatField_>;
pub type TypeFields = Delim<TypeField_>;

pub type Case_ = Node<Case>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Case {
    pub pat: Pat_,
    pub exp: Exp_,
}

pub type ExpField_ = Node<ExpField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExpField {
    pub mut_: Mut,
    pub id: Id_,
    pub typ: Option<Type_>,
    pub exp: Exp_,
}

pub type DecField_ = Node<DecField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DecField {
    pub dec: Dec_,
    pub vis: Option<Vis>,
    pub stab: Option<Stab>,
}

pub type PatField_ = Node<PatField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PatField {
    pub id: Id_,
    pub pat: Pat_,
}

pub type TypeField_ = Node<TypeField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeField {
    pub mut_: Mut,
    pub id: Id_,
    pub typ: Type_,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Vis {
    Public(Option<Id_>),
    Private,
    System,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Stab {
    Stable,
    Flexible,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ResolvedImport {
    Unresolved,
    Lib(String),
    Candid { path: String, bytes: Vec<u8> },
    Prim,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Sugar(bool);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum PrimType {
    Null,
    Unit,
    Bool,
    Nat,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Float,
    Text,
    Char,
    Principal,
}

pub type Type_ = Node<Type>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    // Path (type path)?
    Prim(PrimType),
    Object(ObjSort, TypeFields),
    Array(Mut, Delim<Type_>),
    Optional(Type_),
    // Variant(Vec<>),
    Tuple(Delim<Type_>),
    Function(FuncSort, Delim<TypeBind>, Delim<Type_>, Type_),
    Async(Type_, Type_),
    And(Type_, Type_),
    Or(Type_, Type_),
    Paren(Type_),
    Named(Id_, Type_),
}

pub type Inst = Delim<Type_>;

// Convention: Foo_ = Located<Box<Foo>
// where Foo is an enum for an AST subsort, like Exp.

pub type Exp_ = Node<Exp>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Exp {
    Hole,
    Prim(Id),
    Var(Id),
    Literal(Literal),
    ActorUrl(Exp_),
    Un(UnOp, Exp_),
    Bin(Exp_, BinOp, Exp_),
    Rel(Exp_, RelOp, Exp_),
    Show(Exp_),
    ToCandid(Vec<Exp_>),
    FromCandid(Exp_),
    Tuple(Delim<Exp_>),
    Proj(Exp_, usize),
    Opt(Exp_),
    DoOpt(Exp_),
    Bang(Exp_),
    ObjectBlock(ObjSort, DecFields),
    Object(ExpFields),
    Variant(Id_, Option<Exp_>),
    Dot(Exp_, Id_),
    Assign(Exp_, Exp_),
    Array(Mut, Delim<Exp_>),
    Idx(Exp_, Exp_),
    Function(Id, SortPat_, TypBinds, Pat_, Option<Type_>, Sugar, Exp_),
    Call(Exp_, Option<Inst>, Exp_),
    Block(Delim<Dec_>),
    Do(Exp_),
    Not(Exp_),
    And(Exp_, Exp_),
    Or(Exp_, Exp_),
    If(Exp_, Exp_, Option<Exp_>),
    Switch(Exp_, Cases),
    While(Exp_, Exp_),
    Loop(Exp_, Option<Exp_>),
    For(Pat_, Exp_, Exp_),
    Label(Id_, Option<Type_>, Exp_),
    Break(Id_, Option<Exp_>),
    Return(Option<Exp_>),
    Debug(Exp_),
    Async(TypeBind_, Exp_),
    Await(Exp_),
    Assert(Exp_),
    Annot(Exp_, Type_),
    Import(Id_, ResolvedImport),
    Throw(Exp_),
    Try(Exp_, Vec<Case_>),
    Ignore(Exp_),
    Paren(Exp_),
}

pub type Pat_ = Node<Pat>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Pat {
    Wild,
    Var(Id),
    Literal(Literal),
    Signed(UnOp, Pat_),
    Tuple(Delim<Pat_>),
    Object(Delim<PatFields>),
    Optional(Pat_),
    Variant(Id_, Option<Pat_>),
    Alt(Delim<Pat_>),
    Annot(Pat_, Type_),
    Paren(Pat_),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnOp {
    Pos,
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    Or,
    Xor,
    ShL,
    ShR,
    RotL,
    RotR,
    WAdd,
    WSub,
    WMul,
    WPow,
    Cat,
    BitOr,
    BitAnd,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RelOp {
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

pub type Id = String;
pub type Id_ = Node<Id>;
