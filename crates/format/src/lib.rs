use stylua_lib::OutputVerification;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

fn get_stylua_config() -> stylua_lib::Config {
    stylua_lib::Config::new()
        .with_column_width(100)
        .with_line_endings(stylua_lib::LineEndings::Unix)
        .with_indent_type(stylua_lib::IndentType::Spaces)
        .with_indent_width(2)
        .with_quote_style(stylua_lib::QuoteStyle::AutoPreferSingle)
        .with_call_parentheses(stylua_lib::CallParenType::Always)
        .with_collapse_simple_statement(stylua_lib::CollapseSimpleStatement::Never)
}

pub fn lua(code: &str) -> Result<String, (String, String)> {
    let conf = get_stylua_config();
    let code = match stylua_lib::format_code(code, conf, None, OutputVerification::None) {
        Ok(res) => res,
        Err(err) => return Err((code.to_string(), err.to_string())),
    };

    let code = match stylua_lib::format_code(&code, conf, None, OutputVerification::None) {
        Ok(res) => res,
        Err(err) => return Err((code.to_string(), err.to_string())),
    };

    Ok(code)
}
