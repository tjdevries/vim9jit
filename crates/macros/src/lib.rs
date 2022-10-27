extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, visit_mut::VisitMut, ImplItem};

struct ReplaceQuestions {
    name: syn::Type,
}

impl VisitMut for ReplaceQuestions {
    fn visit_expr_mut(&mut self, i: &mut syn::Expr) {
        match i {
            syn::Expr::Try(try_expr) => {
                syn::visit_mut::visit_expr_try_mut(self, try_expr);

                // let name = syn::LitStr::new(&self.name, self.span);
                let e = &try_expr.expr;
                let name = &self.name;
                *i = syn::parse_quote! {
                    match #e {
                        ::core::result::Result::Ok(v) => v,
                        ::core::result::Result::Err(e) => {
                            // panic!(#name);
                            return Err(::anyhow::anyhow!(
                                format!("{}\n- {}::{}", e, stringify!(#name), stringify!(#e))
                            ));
                        }
                    }
                };

                // *i = syn::parse_quote! {
                //     #e.expect(&format!("{}.{}", stringify!(#name), stringify!(#e)))
                // }
            }
            _ => syn::visit_mut::visit_expr_mut(self, i),
        }
    }
}

#[proc_macro_attribute]
pub fn parse_context(_attr: TokenStream, tokens: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(tokens as syn::ItemImpl);
    for mut item in input.items.iter_mut() {
        match &item {
            ImplItem::Const(_) => todo!(),
            ImplItem::Method(_) => {
                let mut replacer = ReplaceQuestions {
                    name: *input.self_ty.clone(),
                };

                replacer.visit_impl_item_mut(&mut item);
            }
            ImplItem::Type(_) => todo!(),
            ImplItem::Macro(_) => todo!(),
            ImplItem::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }

    TokenStream::from(quote! {
        #input
    })
}
