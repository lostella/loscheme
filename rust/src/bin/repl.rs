use std::io::{self, BufRead, Write};

fn main() {
    println!("Enter text (Ctrl+D to skip, Ctrl+C to quit):");

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("> ");
        stdout.flush().unwrap();

        let mut input = String::new();

        match stdin.lock().read_line(&mut input) {
            Ok(0) => {  // Ctrl+D sends EOF (read_line returns 0 bytes read)
                println!("\nSkipped!");
                continue; // Skip this iteration and prompt for input again
            },
            Ok(_) => {
                input = input.trim().to_string();
                // Modify the input here (e.g., convert to uppercase)
                let output = input.to_uppercase();

                println!("Modified: {}", output);
            },
            Err(err) => {
                println!("Error: {}", err);
                break;
            }
        }
    }

    println!("Exiting...");
}
