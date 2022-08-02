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

pub type Decs = Delim<Dec>;
pub type Prog = Decs;
pub type Cases = Delim<Case>;

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

pub type TypBinds = Delim<TypBind>;
pub type DecFields = Delim<DecField>;
pub type ExpFields = Delim<ExpField>;

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
    Object(Vec<(Id, Type)>),
    Array(Delim<Type>),
    Optional(Type_),
    // Variant(Vec<>),
    Tuple(Delim<Type>),
    Function(
        (),      /* FuncSort */
        Vec<()>, /* TypeBind */
        Delim<Type_>,
        Type_,
    ),
    Async(Type_),
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
    Rel(RelOp, Exp_),
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
    Variant(Id, Exp_),
    Dot(Exp_, Id),
    Assign(Exp_, Exp_),
    Array(Mut, Delim<Exp>),
    Idx(Exp_, Exp_),
    Function(Id, SortPat, TypBinds, Pat_, Option<Type_>, Sugar, Exp_),
    Call(Exp_, Inst, Exp_),
    Block(Delim<Dec>),
    Not(Exp_),
    And(Exp_, Exp_),
    Or(Exp_, Exp_),
    Switch(Exp_, Cases),
    While(Exp_, Exp_),
    Loop(Exp_, Option<Exp_>),
    For(Pat, Exp_, Exp_),
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
    Paren(Exp_),
}

pub type Pat_ = Box<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Wild,
    Var(Id),
    Literal(Literal),
    Signed(Vec<UnOp>, Pat_),
    Tuple(Delim<Pat>),
    Object(Delim<(Id, Pat)>),
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
