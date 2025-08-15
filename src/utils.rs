use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn read_code(filename: &str) -> Result<String, String> {
    let file = File::open(filename)
        .ok()
        .ok_or("Unable to open file".to_string())?;
    let reader = BufReader::new(file);
    let mut code = String::new();

    for res in reader.lines() {
        let line = res.ok().ok_or("Unable to read line".to_string())?;
        match line.find(';') {
            Some(idx) => code.push_str(&line[..idx]),
            None => code.push_str(&line),
        }
        code.push('\n');
    }
    Ok(code)
}
