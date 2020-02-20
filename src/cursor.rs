use std::str::Chars;

pub struct Cursor<'a> {
    chars: Chars<'a>,
    last_len: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            last_len: input.len(),
            chars: input.chars(),
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn eat(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub fn terminated<P: Fn(char) -> bool>(&self, pred: P) -> bool {
        if let Some(c) = self.peek() {
            pred(c)
        } else {
            true
        }
    }

    pub fn len_eaten(&mut self) -> usize {
        let current = self.chars.as_str().len();
        let len = self.last_len - current;
        self.last_len = current;
        len
    }
}
