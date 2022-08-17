use crate::ast::{Dec, Dec_, Exp, Exp_, Loc, Node, Pat, Pat_, Source, Type, Type_};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SyntaxTree {
    Exp(Exp_),
    Dec(Dec_),
    Pat(Pat_),
    Type(Type_),
}

pub trait Syntax: Sized {
    fn node_tree(node: Node<Self>) -> SyntaxTree;

    fn node(self, src: Source) -> Node<Self> {
        Loc(Box::new(self), src)
    }
}

pub trait SyntaxNode {
    fn tree(self) -> SyntaxTree;
}

impl<S: Syntax> SyntaxNode for Node<S> {
    fn tree(self) -> SyntaxTree {
        S::node_tree(self)
    }
}

impl Syntax for Exp {
    fn node_tree(node: Node<Self>) -> SyntaxTree {
        SyntaxTree::Exp(node)
    }
}

impl Syntax for Dec {
    fn node_tree(node: Node<Self>) -> SyntaxTree {
        SyntaxTree::Dec(node)
    }
}

impl Syntax for Pat {
    fn node_tree(node: Node<Self>) -> SyntaxTree {
        SyntaxTree::Pat(node)
    }
}

impl Syntax for Type {
    fn node_tree(node: Node<Self>) -> SyntaxTree {
        SyntaxTree::Type(node)
    }
}
