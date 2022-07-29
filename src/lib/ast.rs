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

// type lit =
//   | NullLit
//   | BoolLit of bool
//   | NatLit of Numerics.Nat.t
//   | Nat8Lit of Numerics.Nat8.t
//   | Nat16Lit of Numerics.Nat16.t
//   | Nat32Lit of Numerics.Nat32.t
//   | Nat64Lit of Numerics.Nat64.t
//   | IntLit of Numerics.Int.t
//   | Int8Lit of Numerics.Int_8.t
//   | Int16Lit of Numerics.Int_16.t
//   | Int32Lit of Numerics.Int_32.t
//   | Int64Lit of Numerics.Int_64.t
//   | FloatLit of Numerics.Float.t
//   | CharLit of Value.unicode
//   | TextLit of string
//   | BlobLit of string
//   | PreLit of string * Type.prim

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

#[Derive(Debug, Clone, PartialEq, Eq)]
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

// and typ' =
//   | PathT of path * typ list                       (* type path *)
//   | PrimT of string                                (* primitive *)
//   | ObjT of obj_sort * typ_field list              (* object *)
//   | ArrayT of mut * typ                            (* array *)
//   | OptT of typ                                    (* option *)
//   | VariantT of typ_tag list                       (* variant *)
//   | TupT of typ_item list                          (* tuple *)
//   | FuncT of func_sort * typ_bind list * typ * typ (* function *)
//   | AsyncT of scope * typ                          (* future *)
//   | AndT of typ * typ                              (* intersection *)
//   | OrT of typ * typ                               (* union *)
//   | ParT of typ                                    (* parentheses, used to control function arity only *)
//   | NamedT of id * typ                             (* parenthesized single element named "tuple" *)

pub type Exp_ = Box<Exp>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp {
    Prim(String),
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
    Func(String, SortPat, TypBinds, Pat, Option<Type>, Exp_),
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
    Ignore(Exp_)
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
    Parenthesis(PatRef),
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

// type pat = (pat', Type.typ) Source.annotated_phrase
// and pat' =
//   | WildP                                      (* wildcard *)
//   | VarP of id                                 (* variable *)
//   | LitP of lit ref                            (* literal *)
//   | SignP of unop * lit ref                    (* signed literal *)
//   | TupP of pat list                           (* tuple *)
//   | ObjP of pat_field list                     (* object *)
//   | OptP of pat                                (* option *)
//   | TagP of id * pat                           (* tagged variant *)
//   | AltP of pat * pat                          (* disjunctive *)
//   | AnnotP of pat * typ                        (* type annotation *)
//   | ParP of pat                                (* parenthesis *)

pub type Id = String;
