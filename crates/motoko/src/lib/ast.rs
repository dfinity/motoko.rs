use crate::shared::{Share, Shared};
use std::ops::Range;

use serde::{Deserialize, Serialize};

/// A "located `X`" has a source location of type `Source`.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Loc<X>(pub X, pub Source);

impl<X: std::fmt::Debug> std::fmt::Debug for Loc<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}@{:?}>", self.0, self.1)
    }
}

pub type Node<X> = Shared<NodeData<X>>;

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash, Debug)]
pub struct NodeData<X>(pub X, pub Source);

impl<X> Loc<X> {
    pub fn map<F: Fn(X) -> T, T>(self, map_fn: F) -> Loc<T> {
        Loc(map_fn(self.0), self.1)
    }
}

impl<X: Clone> Node<X> {
    pub fn without_source(value: X) -> Self {
        NodeData(value, Source::Unknown).share()
    }

    pub fn map_node<F: Fn(&X) -> T, T: Clone>(self, map_fn: F) -> Node<T> {
        NodeData(map_fn(&self.0), self.1.clone()).share()
    }
}

pub type Span = Range<usize>;

impl<X: Clone> NodeData<X> {
    pub fn new(x: X, s: Source) -> Self {
        NodeData(x, s)
    }
    pub fn data_clone(self) -> X {
        self.0.clone()
    }
    pub fn data_ref<'a>(&'a self) -> &'a X {
        &self.0
    }
    pub fn source_clone(self) -> Source {
        self.1.clone()
    }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(tag = "source_type")]
pub enum Source {
    Known { span: Span, line: usize, col: usize },
    ExpStep { source: Box<Source> },
    Unknown,
    Evaluation,
    CoreInit,
}

impl Source {
    pub fn span(&self) -> Option<Span> {
        use Source::*;
        match self {
            Known { span, .. } => Some(span.clone()),
            _ => None,
        }
    }
    pub fn expand(&self, other: &Source) -> Source {
        use Source::*;
        match (self, other) {
            (Unknown, Unknown) => Source::Unknown,
            (
                Known { span, line, col },
                Known {
                    span: other_span, ..
                },
            ) => Known {
                span: span.start..other_span.end,
                line: *line,
                col: *col,
            },
            (_, Unknown) => self.clone(),
            (Unknown, _) => other.clone(),
            (CoreInit, _) => todo!(),
            (_, CoreInit) => todo!(),
            (Evaluation, _) => todo!(),
            (_, Evaluation) => todo!(),
            (ExpStep { .. }, _) => todo!(),
            (_, ExpStep { .. }) => todo!(),
        }
    }
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            //Source::Known { line, col, .. } => write!(f, "{}:{}", line, col),
            Source::Known { span, line, col } => {
                write!(f, "{}..{} @ {}:{}", span.start, span.end, line, col)
            }
            Source::ExpStep { source } => {
                write!(f, "ExpStep({})", source)
            }
            Source::Unknown => write!(f, "(unknown source)"),
            Source::Evaluation => write!(f, "(evaluation)"),
            Source::CoreInit => write!(f, "(full program, via core init)"),
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Delim<X> {
    pub vec: Vec<X>,
    pub has_trailing: bool,
}

impl<X> Delim<X> {
    pub fn new() -> Self {
        Delim {
            vec: vec![],
            has_trailing: false,
        }
    }
    pub fn from(vec: Vec<X>) -> Self {
        Delim {
            vec,
            has_trailing: false,
        }
    }
}

pub type Literal_ = Node<Literal>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum ObjSort {
    Object,
    Actor,
    Module,
}

pub type TypId = Id;
pub type TypId_ = Node<TypId>;

pub type Decs = Delim<Dec_>;
pub type Cases = Delim<Case_>;

pub type Prog = Decs;

pub type Dec_ = Node<Dec>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Dec {
    Exp(Exp),
    Let(Pat_, Exp_),
    LetModule(Option<Id_>, Sugar, DecFields),
    Func(Function),
    Var(Pat_, Exp_),
    Type(TypId_, TypeBinds, Type_),
    Class(Class),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Class {
    pub shared: Option<SortPat>,
    pub typ_id: TypId_,
    pub binds: Option<TypeBinds>,
    pub input: Pat_,
    pub typ: Option<Type_>,
    pub sort: ObjSort,
    pub name: Option<Id_>,
    pub fields: DecFields,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct SortPat {
    pub shared_keyword: bool, // explicit 'shared' keyword
    pub sort: SharedSort,
    pub pat: Pat_,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum SharedSort {
    Query,
    Update,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum BindSort {
    Scope,
    Type,
}

pub type TypeBind_ = Node<TypeBind>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct TypeBind {
    pub var: Id_,
    pub sort: BindSort,
    pub bound: Type_,
}

/// Mutability setting, for arrays, record fields and lexically-scoped bindings.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Mut {
    Const,
    Var,
}

pub type TypeBinds = Delim<TypeBind_>;
pub type DecFields = Delim<DecField_>;
pub type ExpFields = Delim<ExpField_>;
pub type PatFields = Delim<PatField_>;
pub type TypeFields = Delim<TypeField_>;

pub type Case_ = Node<Case>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Case {
    pub pat: Pat_,
    pub exp: Exp_,
}

pub type ExpField_ = Node<ExpField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct ExpField {
    pub mut_: Mut,
    pub id: Id_,
    pub typ: Option<Type_>,
    pub exp: Exp_,
}

pub type DecField_ = Node<DecField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct DecField {
    pub dec: Dec_,
    pub vis: Option<Vis_>,
    pub stab: Option<Stab_>,
}

pub type PatField_ = Node<PatField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct PatField {
    pub id: Id_,
    pub pat: Pat_,
}

pub type TypeField_ = Node<TypeField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct TypeField {
    pub mut_: Mut,
    pub id: Id_,
    pub typ: Type_,
}

pub type Vis_ = Node<Vis>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Vis {
    Public(Option<Id_>),
    Private,
    System,
}

pub type Stab_ = Node<Stab>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Stab {
    Stable,
    Flexible,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum ResolvedImport {
    Unresolved,
    Lib(String),
    Candid { path: String, bytes: Vec<u8> },
    Prim,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash, Default)]
pub struct Sugar(pub bool);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
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

impl PrimType {
    pub fn from_ident(name: &str) -> Option<PrimType> {
        use PrimType::*;
        Some(match name {
            "Bool" => Bool,
            "Nat" => Nat,
            "Nat8" => Nat8,
            "Nat16" => Nat16,
            "Nat32" => Nat32,
            "Nat64" => Nat64,
            "Int" => Int,
            "Int8" => Int8,
            "Int16" => Int16,
            "Int32" => Int32,
            "Int64" => Int64,
            "Principal" => Principal,
            "Text" => Text,
            _ => None?,
        })
    }
}

pub type Type_ = Node<Type>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Type {
    // Path (type path)?
    Prim(PrimType),
    Object(ObjSort, TypeFields),
    Array(Mut, Delim<Type_>),
    Optional(Type_),
    // Variant(Vec<>),
    Tuple(Delim<Type_>),
    Function(Option<SortPat>, TypeBinds, Delim<Type_>, Type_),
    Async(Type_, Type_),
    And(Type_, Type_),
    Or(Type_, Type_),
    Paren(Type_),
    Unknown(Id_),
    Known(Id_, Type_),
}

pub type Inst = Delim<Type_>;

// Convention: Foo_ = Located<Box<Foo>
// where Foo is an enum for an AST subsort, like Exp.

pub type Exp_ = Node<Exp>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Function {
    pub name: Option<Id_>,
    pub shared: Option<SortPat>,
    pub binds: Option<TypeBinds>,
    pub input: Pat_,
    pub output: Option<Type_>,
    #[serde(default = "Default::default")]
    pub sugar: Sugar,
    pub exp: Exp_,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
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
    Index(Exp_, Exp_),
    Function(Function),
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Pat {
    Wild,
    Var(Id_),
    Literal(Literal),
    Signed(UnOp, Pat_),
    Tuple(Delim<Pat_>),
    Object(PatFields),
    Optional(Pat_),
    Variant(Id_, Option<Pat_>),
    Alt(Delim<Pat_>),
    Annot(Pat_, Type_),
    Paren(Pat_),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum UnOp {
    Pos,
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
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
