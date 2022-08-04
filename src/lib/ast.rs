//use num_bigint::{BigInt, BigUint};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Delim<X> {
    pub vec: Vec<X>,
    pub has_trailing: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Null,
    Bool(bool),
    Unit,
    Nat(String),
    Int(String),
    Float(String),
    Char(String), // includes quotes
    Text(String), // includes quotes
    Blob(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjSort {
    Object,
    Actor,
    Module,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Shared<T /*: std::fmt::Debug + Clone + PartialEq + Eq*/> {
    Local,
    Shared(T),
}

pub type FuncSort = Shared<SharedSort>;

pub type TypId = Id;

pub type Decs = Delim<Dec>;
pub type Cases = Delim<Case>;

pub type Prog = Decs;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Dec {
    Exp(Exp),
    Let(Pat, Exp),
    Var(Pat, Exp),
    Typ(TypId, TypBinds, Type),
    Class(
        SortPat,
        TypId,
        TypBinds,
        Pat_,
        Option<Type>,
        ObjSort,
        Id,
        DecFields,
    ),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SortPat {
    Local,
    Shared(SharedSort, Pat),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SharedSort {
    Query,
    Update,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindSort {
    Scope,
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeBind {
    pub var: Id,
    pub sort: BindSort,
    pub bound: Type,
}

/// Mutability setting, for arrays, record fields and lexically-scoped bindings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mut {
    Const,
    Var,
}

pub type TypBinds = Delim<TypeBind>;
pub type DecFields = Delim<DecField>;
pub type ExpFields = Delim<ExpField>;
pub type PatFields = Delim<PatField>;
pub type TypeFields = Delim<TypeField>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Case {
    pub pat: Pat,
    pub exp: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpField {
    pub mut_: Mut,
    pub id: Id,
    pub typ: Option<Type>,
    pub exp: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecField {
    pub dec: Dec,
    pub vis: Option<Vis>,
    pub stab: Option<Stab>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatField {
    pub id: Id,
    pub pat: Pat,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeField {
    pub mut_: Mut,
    pub id: Id,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Vis {
    Public(Option<Id>),
    Private,
    System,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stab {
    Stable,
    Flexible,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedImport {
    Unresolved,
    Lib(String),
    Candid { path: String, bytes: String },
    Prim,
}

// | Unresolved
// | LibPath of string
// | IDLPath of (string * string) (* filepath * bytes *)
// | PrimPath (* the built-in prim module *)

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Sugar(bool);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimType {
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
    Principal,
    Text,
}

pub type Type_ = Box<Type>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // Path (type path)?
    Prim(PrimType),
    Object(ObjSort, TypeFields),
    Array(Mut, Delim<Type>),
    Optional(Type_),
    // Variant(Vec<>),
    Tuple(Delim<Type>),
    Function(FuncSort, Delim<TypeBind>, Delim<Type>, Type_),
    Async(Type_, Type_),
    And(Type_, Type_),
    Or(Type_, Type_),
    Paren(Type_),
    Named(Id, Type_),
}

pub type Inst = Delim<Type>;

// Convention: Foo_ = Box<Foo>
// where Foo is an enum for an AST subsort, like Exp.

pub type Exp_ = Box<Exp>;

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Tuple(Delim<Exp>),
    Proj(Exp_, usize),
    Opt(Exp_),
    DoOpt(Exp_),
    Bang(Exp_),
    ObjectBlock(ObjSort, DecFields),
    Object(ExpFields),
    Variant(Id, Option<Exp_>),
    Dot(Exp_, Id),
    Assign(Exp_, Exp_),
    Array(Mut, Delim<Exp>),
    Idx(Exp_, Exp_),
    Function(Id, SortPat, TypBinds, Pat_, Option<Type_>, Sugar, Exp_),
    Call(Exp_, Inst, Exp_),
    Block(Delim<Dec>),
    DoBlock(Delim<Dec>),
    Not(Exp_),
    And(Exp_, Exp_),
    Or(Exp_, Exp_),
    If(Exp_, Exp_, Option<Exp_>),
    Switch(Exp_, Cases),
    While(Exp_, Exp_),
    Loop(Exp_, Option<Exp_>),
    For(Pat, Exp_, Exp_),
    Label(Id, Option<Type_>, Exp_),
    Break(Id, Option<Exp_>),
    Return(Exp_),
    Debug(Exp_),
    Async(TypeBind, Exp_),
    Await(Exp_),
    Assert(Exp_),
    Annot(Exp_, Type_),
    Import(String, ResolvedImport),
    Throw(Exp_),
    Try(Exp_, Vec<Case>),
    Ignore(Exp_),
    Paren(Exp_),
}

pub type Pat_ = Box<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Wild,
    Var(Id),
    Literal(Literal),
    Signed(UnOp, Pat_),
    Tuple(Delim<Pat>),
    Object(Delim<PatFields>),
    Optional(Pat_),
    Variant(Id, Option<Pat_>),
    Alt(Delim<Pat>),
    Annot(Pat_, Type),
    Paren(Pat_),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnOp {
    Pos,
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelOp {
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

pub type Id = String;
