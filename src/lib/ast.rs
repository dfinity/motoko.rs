use num_bigint::{BigInt, BigUint};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Null,
    Bool,
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
    Class(SortPat, TypId, TypBinds, Pat, Option<Type>, ObjSort, Id, DecFields)
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
    Type
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
    Var
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
    stab: Option<Stab>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Vis {
    Public(Option<String>),
    Private,
    System
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stab {
    Stable,
    Flexible
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // Path (type path)?
    Prim(PrimType),
    Object(Vec<(String, Type)>),
    Array(Vec<Type>),
    Optional(TypeRef),
    // Variant(Vec<>),
    Tuple(Vec<Type>),
    Function(
        (),      /* FuncSort */
        Vec<()>, /* TypeBind */
        TypeRef,
        TypeRef,
    ),
    Async(TypeRef),
    And(TypeRef, TypeRef),
    Or(TypeRef, TypeRef),
    Parenthesized(TypeRef),
    Named(String, TypeRef),
}
type TypeRef = Box<Type>;

pub type Inst = Vec<Type>;

pub type Exp = Box<Exp_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp_ {
    Prim(String),
    Var(Id),
    Literal(Literal),
    ActorUrl(Exp),
    Un(UnOp, Exp),
    Bin(BinOp, Exp),
    Rel(RelOp, Exp),
    Show(Exp),
    ToCandid(Vec<Exp>),
    FromCandid(Exp),
    Tup(Vec<Exp>),
    Proj(Exp, usize),
    Opt(Exp),
    DoOpt(Exp),
    Bang(Exp),
    ObjBlock(ObjSort, DecFields),
    Obj(ExpxpFields),
    Tag(Id, Exp),
    Dot(Exp, Id),
    Assign(Exp, Exp),
    Array(Mut, Vec<Exp>),
    Idx(Exp, Exp),
    Func(String, SortPat, TypBinds, Pat, Option<Type>, Exp),
    Call(Exp, Inst, Exp),
    Block(Decs),
    Not(Exp),
    And(Exp, Exp),
    Or(Exp, Exp),
    Of(Exp, Exp, Exp),
    Switch(Exp, Cases),
    While(Exp, Exp),
    Loop(Exp, Option<Exp>),
    For(Pat, Exp, Exp),
    Label(Id, Type, Exp),
    Break(Id, Exp),
    Ret(Exp),
    Debug(Exp),
    Async(TypBind, Exp),
    Await(Exp),
    Assert(Exp),
    Annot(Exp, Type),
    Import(String),
    Throw(Exp),
    Try(Exp, Cases),
    Ignore(Exp)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Wild(),
    Var(String),
    Lit(Literal),
    Sign(), // signed literal?
    Tuple(Vec<(String, Pat)>),
    Object(Vec<(String, Pat)>),
    Optional(PatRef),
    Tag(String, PatRef),
    Alt(PatRef, PatRef),
    Annot(PatRef, Type),
    Parenthesis(Pat),
}
type PatRef = Box<Pat>;

pub enum UnOp{
    Pos,
    Neg,
    Not,
}

pub enum BinOp{
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

pub enum RelOp {
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

pub type Id = String;
