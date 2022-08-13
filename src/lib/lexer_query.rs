// use crate::lexer::TokenTree;

// pub trait TokenQuery {
//     fn iter_trees<'a, I: Iterator<Item = &'a TokenTree>>(&self) -> I;
//     fn iter_tokens<'a, I: Iterator<Item = &'a TokenTree>>(&self) -> I;

//     fn find_tree<T>(&self, f: &dyn Fn(&TokenTree) -> Option<T>) -> Option<T> {
//         for t in self.iter_trees() {
//             if let Some(result) = f {
//                 Some(result)
//             }
//         }
//         None
//     }
// }

// impl<'a> TokenQuery for &'a TokenTree {
//     fn iter_trees<I: Iterator<Item = &'a TokenTree>>(&'a self) -> I {
//         [self].iter()
//     }
// }

// impl<'a> TokenQuery for &'a Vec<TokenTree> {
//     fn iter_trees<I: Iterator<Item = &'a TokenTree>>(&'a self) -> I {
//         self.iter()
//     }
// }
