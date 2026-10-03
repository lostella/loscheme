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

#[cfg(all(target_arch = "wasm32", feature = "web"))]
thread_local! {
    static OUTPUT: std::cell::RefCell<String> = const { std::cell::RefCell::new(String::new()) };
}

/// Write program output: to stdout natively, to an in-memory buffer on the web.
pub fn emit(s: &str) {
    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    OUTPUT.with(|out| out.borrow_mut().push_str(s));
    #[cfg(not(all(target_arch = "wasm32", feature = "web")))]
    print!("{s}");
}

/// Take everything written through `emit` so far, leaving the buffer empty.
#[cfg(all(target_arch = "wasm32", feature = "web"))]
pub fn take_output() -> String {
    OUTPUT.with(|out| std::mem::take(&mut *out.borrow_mut()))
}
