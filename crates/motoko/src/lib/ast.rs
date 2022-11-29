use crate::shared::{Share, Shared};
use crate::value::PrimFunction;
use serde::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::Range;

/// A "located `X`" has a source location of type `Source`.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Loc<X>(pub X, pub Source);

impl<X: std::fmt::Debug> std::fmt::Debug for Loc<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}@{:?}>", self.0, self.1)
    }
}

pub type Node<X> = Shared<NodeData<X>>;

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct NodeData<X>(pub X, pub Source);

impl<X: std::fmt::Debug> std::fmt::Debug for NodeData<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}@{:?}>", self.0, self.1)
    }
}

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
    pub fn data_ref(&self) -> &X {
        &self.0
    }
    pub fn data_clone(&self) -> X {
        self.0.clone()
    }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(tag = "source_type")]
pub enum Source {
    Known { span: Span, line: usize, col: usize },
    // ExpStep { source: Shared<Source> },
    Unknown,
    Evaluation,
    CoreInit,
    CoreCreateActor,
    CoreUpgradeActor,
    CoreSetModule,
    CoreCall,
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
            _ => todo!(),
            // (ExpStep { .. }, _) => todo!(),
            // (_, ExpStep { .. }) => todo!(),
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
            // Source::ExpStep { source } => {
            //     write!(f, "ExpStep({})", source)
            // }
            Source::Unknown => write!(f, "(unknown source)"),
            Source::Evaluation => write!(f, "(evaluation)"),
            Source::CoreInit => write!(f, "(full program, via core init)"),
            Source::CoreCreateActor => write!(f, "(Core.create_actor())"),
            Source::CoreUpgradeActor => write!(f, "(Core.upgrade_actor())"),
            Source::CoreCall => write!(f, "(Core.call())"),
            Source::CoreSetModule => write!(f, "(Core.set_module())"),
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
pub struct Delim<X: Clone> {
    pub vec: im_rc::Vector<X>,
    pub has_trailing: bool,
}

impl<X: Clone> Delim<X> {
    pub fn new() -> Self {
        Delim {
            vec: im_rc::vector![],
            has_trailing: false,
        }
    }
    pub fn from(vec: Vec<X>) -> Self {
        Delim {
            vec: vec.into(),
            has_trailing: false,
        }
    }
}
impl<X: Clone> Default for Delim<X> {
    fn default() -> Self {
        Self::new()
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
    Exp(Exp_),
    Let(Pat_, Exp_),
    LetImport(Pat_, Sugar, String),
    LetModule(Option<Id_>, Sugar, DecFields),
    LetActor(Option<Id_>, Sugar, DecFields),
    Func(Function),
    Var(Pat_, Exp_),
    Type(TypId_, Option<TypeBinds>, Type_),
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
pub enum TypeField {
    Val(ValTypeField),
    Type, // to do -- represent AST here.
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct ValTypeField {
    pub mut_: Mut,
    pub id: Id_,
    pub typ: Type_,
}

pub type TypeTag_ = Node<TypeTag>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct TypeTag {
    pub id: Id_,
    pub typ: Option<Type_>,
}

pub type Vis_ = Node<Vis>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Vis {
    Public(Option<Id_>),
    Private,
    System,
}

impl Vis {
    pub fn is_public(&self) -> bool {
        match self {
            Vis::Public(..) => true,
            _ => false,
        }
    }
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
    pub fn from_ident(i: &Id) -> Option<PrimType> {
        use PrimType::*;
        Some(match i.string.as_str() {
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
    Item(Id_, Type_),
    Path(TypePath, Option<Delim<Type_>>),
    Prim(PrimType),
    Object(ObjSort, TypeFields),
    Array(Mut, Type_),
    Optional(Type_),
    Variant(Delim<TypeTag_>),
    Tuple(Delim<Type_>),
    Function(Option<SortPat>, Option<TypeBinds>, Type_, Type_),
    // Async(Type_, Type_), -- to do -- use scope variables
    Async(Type_),
    And(Type_, Type_),
    Or(Type_, Type_),
    Paren(Type_),
    Unknown(Id_),
    Known(Id_, Type_),
}

pub type TypePath_ = Node<TypePath>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum TypePath {
    Id(Id_),
    Dot(TypePath_, Id_),
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
    Prim(Result<PrimFunction, String>),
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
    BinAssign(Exp_, BinOp, Exp_),
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
    //Import(Id_, ResolvedImport),
    Import(String),
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
    // used by the VM to pattern-match values.
    TempVar(u16),
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

#[derive(Clone)]
pub struct Id {
    pub string: Shared<String>,
    // todo -- use https://docs.rs/crate/nohash-hasher/latest to avoid
    // hashing anything (not even the pre-computed hash) and just
    // *use* the pre-computed hash.
    pub hash: u64,
}
pub type Id_ = Node<Id>;

impl Id {
    pub fn new(s: String) -> Self {
        let hash: u64 = {
            let mut h = DefaultHasher::new();
            s.hash(&mut h);
            h.finish()
        };
        Id {
            // to do -- memoize the strings, and avoid duplicate copies.
            string: Shared::new(s),
            hash,
        }
    }
    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }
    pub fn to_string(&self) -> String {
        self.string.to_string()
    }
}

impl std::fmt::Debug for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.string)
    }
}

// TODO: see whether this is faster than the default `PartialEq` impl
impl PartialEq for Id {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.string == other.string
    }

    fn ne(&self, other: &Self) -> bool {
        self.hash != other.hash || self.string != other.string
    }
}
impl Eq for Id {}

impl Hash for Id {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}

impl Serialize for Id {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serde::Serialize::serialize(&self.string, serializer)
    }
}

impl<'de> Deserialize<'de> for Id {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde::Deserialize::deserialize(deserializer).map(Id::new)
    }
}

pub trait ToId {
    fn to_id(self) -> Id;
}

impl ToId for Id {
    fn to_id(self) -> Id {
        self
    }
}

impl ToId for &str {
    fn to_id(self) -> Id {
        Id::new(self.to_string())
    }
}

impl ToId for String {
    fn to_id(self) -> Id {
        Id::new(self)
    }
}
