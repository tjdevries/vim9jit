#![cfg(test)]

use crate::*;

macro_rules! snapshot {
    ($name:tt, $path:tt) => {
        #[test]
        fn $name() {
            setup_trace();

            let contents = include_str!($path);
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("../testdata/output/");
            settings.bind(|| {
                insta::assert_snapshot!(snapshot_parsing(contents));
            });
        }
    };
}

snapshot!(test_var, "../testdata/snapshots/simple_var.vim");
snapshot!(test_ret, "../testdata/snapshots/simple_ret.vim");
snapshot!(test_shared, "../testdata/snapshots/shared.vim");
snapshot!(test_comment, "../testdata/snapshots/comment.vim");
snapshot!(test_header, "../testdata/snapshots/header.vim");
snapshot!(test_expr, "../testdata/snapshots/expr.vim");
snapshot!(test_echo, "../testdata/snapshots/echo.vim");
snapshot!(test_scopes, "../testdata/snapshots/scopes.vim");
snapshot!(test_autocmd, "../testdata/snapshots/autocmd.vim");
snapshot!(test_array, "../testdata/snapshots/array.vim");
snapshot!(test_dict, "../testdata/snapshots/dict.vim");
snapshot!(test_if, "../testdata/snapshots/if.vim");
snapshot!(test_call, "../testdata/snapshots/call.vim");
snapshot!(test_concat, "../testdata/snapshots/concat.vim");
snapshot!(test_unpack, "../testdata/snapshots/unpack.vim");
snapshot!(test_assign, "../testdata/snapshots/assign.vim");
snapshot!(test_vimvar, "../testdata/snapshots/vimvar.vim");
snapshot!(test_busted, "../testdata/snapshots/busted.vim");
snapshot!(test_heredoc, "../testdata/snapshots/heredoc.vim");
snapshot!(test_typed_params, "../testdata/snapshots/typed_params.vim");
snapshot!(test_index, "../testdata/snapshots/index.vim");
snapshot!(test_adv_index, "../testdata/snapshots/adv_index.vim");
snapshot!(test_multiline, "../testdata/snapshots/multiline.vim");
snapshot!(test_cfilter, "../testdata/snapshots/cfilter.vim");
snapshot!(test_lambda, "../testdata/snapshots/lambda.vim");
snapshot!(test_comparisons, "../testdata/snapshots/comparisons.vim");
snapshot!(test_methods, "../testdata/snapshots/methods.vim");
snapshot!(test_eval, "../testdata/snapshots/eval.vim");
snapshot!(test_export, "../testdata/snapshots/export.vim");
snapshot!(test_import, "../testdata/snapshots/import.vim");
snapshot!(
    test_plugin_fileselect,
    "../testdata/snapshots/plugin_fileselect.vim"
);

#[test]
fn test_peek_n() {
    let input = "vim9script\nvar x = true\n";
    let lexer = new_lexer(input);
    let mut parser = Parser::new(lexer);
    assert_eq!(parser.peek_token.kind, TokenKind::EndOfLine);
    assert_eq!(parser.peek_n(0).kind, TokenKind::Identifier);
    assert_eq!(parser.peek_n(1).kind, TokenKind::EndOfLine);
    assert_eq!(parser.peek_n(2).kind, TokenKind::Identifier);
    assert_eq!(parser.peek_n(3).kind, TokenKind::Identifier);
    assert_eq!(parser.peek_n(4).kind, TokenKind::Equal);
    assert_eq!(parser.peek_n(1).kind, TokenKind::EndOfLine);
}

// TODO: Slowly but surely, we can work towards this
// snapshot!(test_matchparen, "../../shared/snapshots/matchparen.vim");
