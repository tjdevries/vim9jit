#![feature(test)]

extern crate test;

#[bench]
fn lexer_handler(b: &mut test::Bencher) {
    b.iter(|| {
        let contents = include_str!("/home/tjdevries/git/lsp/src/autoload/lsp/handlers.vim");
        let lexer = vim9_lexer::Lexer::new(contents);

        let mut res = vec![];
        loop {
            match test::black_box(lexer.next_token()) {
                Ok(tok) => {
                    if tok.kind == vim9_lexer::TokenKind::EndOfFile {
                        break;
                    }

                    res.push(tok);
                }
                Err(err) => panic!("error: {err:?}"),
            }
        }

        assert!(!res.is_empty(), "Must have some items");
    });
}
