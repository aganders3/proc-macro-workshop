use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};

use syn::{parse_macro_input, DeriveInput};
use quote::quote;

fn optional_inner_type(ty: &syn::Type) -> &syn::Type {
    match ty {
        syn::Type::Path(ty) => {
            if let syn::PathArguments::AngleBracketed(generic_args) = &ty.path.segments[0].arguments {
                if let syn::GenericArgument::Type(ty) = &generic_args.args[0] {
                    ty
                } else {
                    unimplemented!("unrecognized generic argument type for Option");
                }
            } else {
                unimplemented!("unrecognized Option syntax");
            }
        }
        _ => unimplemented!("unrecognized type"),
    }
}

fn is_optional(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(t) => {
            if t.path.segments[0].ident == "Option" {
                true
            } else {
                false
            }
        },
        _ => false,
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let name_builder = Ident::new(&format!("{}Builder", name), Span::call_site());

    let fields = match input.data {
        syn::Data::Struct(data) => data.fields,
        _ => unimplemented!(),
    };

    let field_names = fields.iter().map(|field| &field.ident);
    let field_types = fields.iter().map(|field| &field.ty);

    let builder_decl = quote! {
        // generated builder struct
        pub struct #name_builder {
            #( #field_names: Option<#field_types> ),*
        }
    };

    let required_field_names = fields.iter().filter_map(|field| {
        if is_optional(&field.ty) {
            None
        } else {
            Some(&field.ident)
        }
    });

    let optional_field_names = fields.iter().filter_map(|field| {
        if !is_optional(&field.ty) {
            None
        } else {
            Some(&field.ident)
        }
    });

    let og_impl = quote! {
        // generated impl
        impl #name {
            pub fn builder() -> #name_builder {
                // doing this breaks the field order, but only in the builder struct
                #name_builder {
                    #( #required_field_names: None, )*
                    #( #optional_field_names: Some(None), )*
                }
            }
        }
    };


    let required_setter_names = fields.iter().filter_map(|field| {
        if is_optional(&field.ty) {
            None
        } else {
            Some(&field.ident)
        }
    });

    let required_setter_types = fields.iter().filter_map(|field| {
        if is_optional(&field.ty) {
            None
        } else {
            Some(&field.ty)
        }
    });

    let optional_setter_names = fields.iter().filter_map(|field| {
        if !is_optional(&field.ty) {
            None
        } else {
            Some(&field.ident)
        }
    });

    let optional_setter_types = fields.iter().filter_map(|field| {
        if !is_optional(&field.ty) {
            None
        } else {
            Some(optional_inner_type(&field.ty))
        }
    });

    // get field names as strings for error messages
    // still generated for optional types even though it doesn't matter
    let field_names = fields.iter().map(|field| &field.ident);
    let field_not_set = fields.iter().map(|field| format!("{} not set", field.ident.as_ref().unwrap()));

    let builder_impl = quote! {
        use std::error::Error;
        // generated impl
        impl #name_builder {
            #(
                pub fn #required_setter_names(&mut self, #required_setter_names: #required_setter_types) -> &mut Self {
                    self.#required_setter_names = Some(#required_setter_names);
                    self
                }
            )*
            #(
                pub fn #optional_setter_names(&mut self, #optional_setter_names: #optional_setter_types) -> &mut Self {
                    // optional attributes are double-wrapped in the builder
                    self.#optional_setter_names = Some(Some(#optional_setter_names));
                    self
                }
            )*
            pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                Ok(#name {
                    #( #field_names: self.#field_names.take().ok_or(#field_not_set)? ),*
                })
            }
        }
    };

    let expanded = quote! {
        #builder_decl
        #builder_impl
        #og_impl
    };

    TokenStream::from(expanded)
}
