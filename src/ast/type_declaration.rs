use crate::parser::Parse;

// The following builtin types are supported:
//         bool
//         number
//         float
//         string
//         blob
//         list<{type}>
//         dict<{type}>
//         job
//         channel
//         func
//         func: {type}
//         func({type}, ...)
//         func({type}, ...): {type}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDeclaration {
    Bool,
    Number,
    Float,
    String,
    Blob,
    List(Box<TypeDeclaration>),
    Dict(Box<TypeDeclaration>),
    Job,
    Channel,

    // TODO: Bunch of func types
    Func,
}

impl Parse for TypeDeclaration {
    fn parse(p: &mut crate::parser::Parser) -> crate::parser::ParseResult<Self> {
        use TypeDeclaration::*;

        let token = p.next_token();
        let text = token.text.trim();

        Ok(match text {
            "bool" => Bool,
            "number" => Number,
            "float" => Float,
            "string" => String,
            "blob" => Blob,
            "job" => Job,
            "channel" => Channel,
            "func" => Func,

            _ => unimplemented!(),
        })

        // Ok(TypeDeclaration::Number)
    }
}
