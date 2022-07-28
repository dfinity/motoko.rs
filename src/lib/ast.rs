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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp {
    Literal(Literal),

    Hole,
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
