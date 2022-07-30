//use num_bigint::{BigInt, BigUint};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Null,
    Bool(bool),
    Unit,
    Nat(String),
    Nat8(u8),
    Nat16(u16),
    Nat32(u32),
    Nat64(u64),
    Int(String),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Float(String),
    Char(char),
    Text(String),
    Blob(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjSort {
    Object,
    Actor,
    Module,
}

pub type TypId = Id;

pub type Decs = Vec<Dec>;
pub type Cases = Vec<Case>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Dec {
    Exp(Exp),
    Let(Pat, Exp),
    Var(Id, Exp),
    Typ(TypId, TypBinds, Type_),
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
    Shared(SharedSort, Pat_),
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
pub struct TypBind {
    pub var: Id,
    pub sort: BindSort,
    pub bound: Type_,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mut {
    Const,
    Var,
}

pub type TypBinds = Vec<TypBind>;
pub type DecFields = Vec<DecField>;
pub type ExpFields = Vec<ExpField>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Case {
    pub pat: Pat,
    pub exp: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpField {
    pub mut_: Mut,
    pub id: Id,
    pub exp: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecField {
    pub dec: Dec,
    pub vis: Vis,
    pub stab: Option<Stab>,
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
    Object(Vec<(Id, Type)>),
    Array(Vec<Type>),
    Optional(Type_),
    // Variant(Vec<>),
    Tuple(Vec<Type>),
    Function(
        (),      /* FuncSort */
        Vec<()>, /* TypeBind */
        Type_,
        Type_,
    ),
    Async(Type_),
    And(Type_, Type_),
    Or(Type_, Type_),
    // Parenthesized(Type_),
    Named(Id, Type_),
}

pub type Inst = Vec<Type>;

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
    Rel(RelOp, Exp_),
    Show(Exp_),
    ToCandid(Vec<Exp_>),
    FromCandid(Exp_),
    Tuple(Vec<Exp>),
    Proj(Exp_, usize),
    Opt(Exp_),
    DoOpt(Exp_),
    Bang(Exp_),
    ObjectBlock(ObjSort, DecFields),
    Object(ExpFields),
    Variant(Id, Exp_),
    Dot(Exp_, Id),
    Assign(Exp_, Exp_),
    Array(Mut, Vec<Exp>),
    Idx(Exp_, Exp_),
    Func(Id, SortPat, TypBinds, Pat_, Option<Type_>, Exp_),
    Call(Exp_, Inst, Exp_),
    Block(Decs),
    Not(Exp_),
    And(Exp_, Exp_),
    Or(Exp_, Exp_),
    Of(Exp_, Exp_, Exp_),
    Switch(Exp_, Cases),
    While(Exp_, Exp_),
    Loop(Exp_, Option<Exp_>),
    For(Pat_, Exp_, Exp_),
    Label(Id, Type_, Exp_),
    Break(Id, Exp_),
    Return(Exp_),
    Debug(Exp_),
    Async(TypBind, Exp_),
    Await(Exp_),
    Assert(Exp_),
    Annot(Exp_, Type_),
    Import(String),
    Throw(Exp_),
    Try(Exp_, Cases),
    Ignore(Exp_),
}

pub type Pat_ = Box<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Wild,
    Var(Id),
    Literal(Literal),
    Signed(Vec<UnOp>, Pat_),
    Tuple(Vec<Pat>),
    Object(Vec<(Id, Pat)>),
    Optional(Pat_),
    Variant(Id, Pat_),
    Alt(Vec<Pat>),
    Annot(Pat_, Type),
    // Parenthesized(Pat_),
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
