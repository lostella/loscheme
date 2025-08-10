pub fn char_from_external(s: &str) -> Option<char> {
    if s.len() == 1 {
        return s.chars().next();
    }
    if s == "newline" {
        return Some('\n');
    }
    None
}

pub fn char_to_external(c: char) -> String {
    if c == '\n' {
        return "newline".to_string();
    }
    c.to_string()
}
