pub mod cursor;

pub fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\t' | '\n' => true,
        _ => false,
    }
}

pub fn is_delimiter(c: char) -> bool {
    match c {
        '(' | ')' | '"' | ';' => true,
        c => is_whitespace(c),
    }
}

pub fn is_initial(c: char) -> bool {
    match c {
        '!' | '$' | '%' | '&' | '*' | '/' | ':' => true,
        '<' | '=' | '>' | '?' | '^' | '_' | '~' => true,
        c if 'a' <= c && c <= 'z' => true,
        c if 'A' <= c && c <= 'Z' => true,
        _ => false,
    }
}

pub fn is_subsequent(c: char) -> bool {
    match c {
        c if is_initial(c) => true,
        c if c.is_digit(10) => true,
        '+' | '-' | '.' | '@' => true,
        _ => false,
    }
}
