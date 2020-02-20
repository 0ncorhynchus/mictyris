use std::str::Chars;

pub struct Cursor<'a> {
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn eat(&mut self) -> Option<char> {
        self.chars.next()
    }
}
