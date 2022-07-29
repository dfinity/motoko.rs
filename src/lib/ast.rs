use num_bigint::{BigInt, BigUint};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Null,
    Bool(bool),
    Unit,
    Nat(BigUint),
    Nat8(u8),
    Nat16(u16),
    Nat32(u32),
    Nat64(u64),
    Int(BigInt),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Float(String),
    Char(char),
    Text(String),
    Blob(String),
    Pre(String, () /* Type.prim? */),
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
    Typ(TypId, TypBinds, Type),
    Class(
        SortPat,
        TypId,
        TypBinds,
        Pat,
        Option<Type_>,
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
pub struct TypBind {
    var: Id,
    sort: BindSort,
    bound: Type,
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
    pat: Pat,
    exp: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpField {
    mut_: Mut,
    id: Id,
    exp: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecField {
    dec: Dec,
    vis: Vis,
    stab: Option<Stab>,
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

type Type = Box<Type_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type_ {
    // Path (type path)?
    Prim(PrimType),
    Object(Vec<(Id, Type_)>),
    Array(Vec<Type_>),
    Optional(Type),
    // Variant(Vec<>),
    Tuple(Vec<Type_>),
    Function(
        (),      /* FuncSort */
        Vec<()>, /* TypeBind */
        Type,
        Type,
    ),
    Async(Type),
    And(Type, Type),
    Or(Type, Type),
    Parenthesized(Type),
    Named(Id, Type),
}

pub type Inst = Vec<Type_>;

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
    Bin(BinOp, Exp_),
    Rel(RelOp, Exp_),
    Show(Exp_),
    ToCandid(Vec<Exp_>),
    FromCandid(Exp_),
    Tup(Vec<Exp_>),
    Proj(Exp_, usize),
    Opt(Exp_),
    DoOpt(Exp_),
    Bang(Exp_),
    ObjBlock(ObjSort, DecFields),
    Obj(ExpFields),
    Tag(Id, Exp_),
    Dot(Exp_, Id),
    Assign(Exp_, Exp_),
    Array(Mut, Vec<Exp_>),
    Idx(Exp_, Exp_),
    Func(Id, SortPat, TypBinds, Pat, Option<Type>, Exp_),
    Call(Exp_, Inst, Exp_),
    Block(Decs),
    Not(Exp_),
    And(Exp_, Exp_),
    Or(Exp_, Exp_),
    Of(Exp_, Exp_, Exp_),
    Switch(Exp_, Cases),
    While(Exp_, Exp_),
    Loop(Exp_, Option<Exp_>),
    For(Pat, Exp_, Exp_),
    Label(Id, Type, Exp_),
    Break(Id, Exp_),
    Return(Exp_),
    Debug(Exp_),
    Async(TypBind, Exp_),
    Await(Exp_),
    Assert(Exp_),
    Annot(Exp_, Type),
    Import(String),
    Throw(Exp_),
    Try(Exp_, Cases),
    Ignore(Exp_),
}

pub type Pat = Box<Pat_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat_ {
    Wild,
    Var(Id),
    Lit(Literal),
    Sign(Vec<UnOp>, Pat), // signed literal?
    Tuple(Vec<Pat_>),
    Object(Vec<(Id, Pat_)>),
    Optional(Pat),
    Tag(Id, Pat),
    Alt(Pat, Pat),
    Annot(Pat, Type),
    Parenthesized(Pat),
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
